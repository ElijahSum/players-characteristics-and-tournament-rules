#!/usr/bin/env python3
"""Train a contrastive neural encoder for player style from pre-change games.

The script consumes the JSONL sequence dataset produced by
build_contrastive_style_dataset.py. Each sequence is one player's non-opening,
non-terminal moves in one game, encoded as compact categorical move features.

Training objective:
- games by the same player should have similar embeddings;
- games by different players in the same batch should have dissimilar embeddings.

Outputs are deliberately thesis-friendly:
- game-level neural embeddings;
- player-level embeddings averaged over pre-change games;
- K-means style clusters;
- optional cluster profiles using interpretable style features.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import os
import random
import time
import warnings
from collections import defaultdict
from pathlib import Path

import numpy as np
import torch

if not os.environ.get("LOKY_MAX_CPU_COUNT"):
    os.environ["LOKY_MAX_CPU_COUNT"] = str(os.cpu_count() or 1)
warnings.filterwarnings(
    "ignore",
    message="Could not find the number of physical cores.*",
    category=UserWarning,
)

from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from torch import nn
from torch.nn.utils.rnn import pack_padded_sequence


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_INPUT = (
    ROOT
    / "outputs"
    / "whole_dataset_2022_2026"
    / "contrastive_style"
    / "prechange_contrastive_sequences.jsonl"
)
DEFAULT_OUTPUT_DIR = ROOT / "outputs" / "whole_dataset_2022_2026" / "contrastive_style" / "neural_style"
DEFAULT_STYLE_FEATURES = (
    ROOT
    / "outputs"
    / "whole_dataset_2022_2026"
    / "style_features"
    / "prechange_player_style_features.csv"
)


TOKEN_FIELD_NAMES = [
    "piece_id",
    "from_square_id",
    "to_square_id",
    "is_capture",
    "gives_check",
    "phase_id",
    "cp_loss_bin",
    "eval_before_bin",
    "move_number_bin",
]
FIELD_CARDINALITIES = [8, 65, 65, 2, 2, 5, 6, 21, 21]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-jsonl", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--style-features-csv", type=Path, default=DEFAULT_STYLE_FEATURES)
    parser.add_argument("--min-sequences-per-player", type=int, default=2)
    parser.add_argument("--max-sequences", type=int, default=0, help="Optional cap for smoke tests.")
    parser.add_argument("--epochs", type=int, default=15)
    parser.add_argument("--steps-per-epoch", type=int, default=0, help="0 chooses a data-size based default.")
    parser.add_argument("--batch-players", type=int, default=64)
    parser.add_argument("--games-per-player", type=int, default=2)
    parser.add_argument("--field-embedding-dim", type=int, default=8)
    parser.add_argument("--hidden-dim", type=int, default=128)
    parser.add_argument("--embedding-dim", type=int, default=64)
    parser.add_argument("--dropout", type=float, default=0.15)
    parser.add_argument("--temperature", type=float, default=0.10)
    parser.add_argument("--learning-rate", type=float, default=1e-3)
    parser.add_argument("--weight-decay", type=float, default=1e-4)
    parser.add_argument(
        "--validation-share",
        type=float,
        default=0.10,
        help="Per-player sequence share held out for contrastive validation when enough games exist.",
    )
    parser.add_argument("--validation-batches", type=int, default=20)
    parser.add_argument(
        "--early-stopping-patience",
        type=int,
        default=0,
        help="Stop after this many non-improving validation epochs. 0 disables early stopping.",
    )
    parser.add_argument("--encode-batch-size", type=int, default=512)
    parser.add_argument("--cluster-k", type=int, default=5)
    parser.add_argument("--cluster-k-grid", default="3,4,5,6,7,8")
    parser.add_argument("--seed", type=int, default=20260513)
    parser.add_argument("--device", default="auto", choices=["auto", "cpu", "mps", "cuda"])
    parser.add_argument("--no-game-embeddings", action="store_true")
    return parser.parse_args()


def choose_device(requested: str) -> torch.device:
    if requested != "auto":
        return torch.device(requested)
    if torch.backends.mps.is_available():
        return torch.device("mps")
    if torch.cuda.is_available():
        return torch.device("cuda")
    return torch.device("cpu")


def set_seed(seed: int) -> None:
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def clamp_token(token: list[int]) -> list[int]:
    out = []
    for value, cardinality in zip(token, FIELD_CARDINALITIES):
        if value < 0:
            out.append(0)
        elif value >= cardinality:
            out.append(cardinality - 1)
        else:
            out.append(int(value))
    return out


def load_sequences(
    path: Path,
    min_sequences_per_player: int,
    max_sequences: int,
) -> tuple[list[dict[str, object]], dict[str, list[int]]]:
    by_player: dict[str, list[int]] = defaultdict(list)
    raw: list[dict[str, object]] = []
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            if max_sequences and len(raw) >= max_sequences:
                break
            line = line.strip()
            if not line:
                continue
            rec = json.loads(line)
            player = str(rec["player"])
            tokens = [clamp_token(token) for token in rec["tokens"]]
            if not tokens:
                continue
            rec = {
                "player": player,
                "game_id": str(rec.get("game_id", "")),
                "mover_color": str(rec.get("mover_color", "")),
                "date": str(rec.get("date", "")),
                "tokens": tokens,
            }
            by_player[player].append(len(raw))
            raw.append(rec)

    keep_players = {player for player, indices in by_player.items() if len(indices) >= min_sequences_per_player}
    sequences: list[dict[str, object]] = []
    remap: dict[int, int] = {}
    for old_idx, rec in enumerate(raw):
        if rec["player"] not in keep_players:
            continue
        remap[old_idx] = len(sequences)
        sequences.append(rec)

    final_by_player: dict[str, list[int]] = defaultdict(list)
    for old_idx, new_idx in remap.items():
        final_by_player[str(raw[old_idx]["player"])].append(new_idx)
    return sequences, dict(final_by_player)


def split_train_validation_players(
    by_player: dict[str, list[int]],
    validation_share: float,
    games_per_player: int,
    seed: int,
) -> tuple[dict[str, list[int]], dict[str, list[int]], dict[str, int | float]]:
    """Hold out sequences within player, so validation still tests same-player pulls."""
    validation_share = max(0.0, min(validation_share, 0.5))
    min_train = max(2, games_per_player)
    min_validation = max(2, games_per_player)
    rng = random.Random(seed + 1009)
    train_by_player: dict[str, list[int]] = {}
    validation_by_player: dict[str, list[int]] = {}

    for player, indices in by_player.items():
        shuffled = list(indices)
        rng.shuffle(shuffled)
        val_count = 0
        if validation_share > 0 and len(shuffled) >= min_train + min_validation:
            val_count = max(min_validation, int(round(len(shuffled) * validation_share)))
            val_count = min(val_count, len(shuffled) - min_train)
        if val_count:
            validation_by_player[player] = sorted(shuffled[:val_count])
            train_by_player[player] = sorted(shuffled[val_count:])
        else:
            train_by_player[player] = sorted(shuffled)

    trainable_players = sum(1 for indices in train_by_player.values() if len(indices) >= min_train)
    validation_players = sum(1 for indices in validation_by_player.values() if len(indices) >= min_validation)
    summary = {
        "validation_share": validation_share,
        "train_players": trainable_players,
        "validation_players": validation_players,
        "train_sequences": sum(len(indices) for indices in train_by_player.values()),
        "validation_sequences": sum(len(indices) for indices in validation_by_player.values()),
    }
    return train_by_player, validation_by_player, summary


def make_batch(
    sequences: list[dict[str, object]],
    by_player: dict[str, list[int]],
    players: list[str],
    games_per_player: int,
    device: torch.device,
) -> tuple[torch.Tensor, torch.Tensor, list[int]]:
    batch_indices: list[int] = []
    labels: list[int] = []
    for label, player in enumerate(players):
        choices = by_player[player]
        if len(choices) >= games_per_player:
            picked = random.sample(choices, games_per_player)
        else:
            picked = random.choices(choices, k=games_per_player)
        batch_indices.extend(picked)
        labels.extend([label] * len(picked))

    lengths = [len(sequences[i]["tokens"]) for i in batch_indices]
    max_len = max(lengths)
    batch = torch.zeros((len(batch_indices), max_len, len(FIELD_CARDINALITIES)), dtype=torch.long)
    for row_idx, seq_idx in enumerate(batch_indices):
        tokens = sequences[seq_idx]["tokens"]
        batch[row_idx, : len(tokens), :] = torch.tensor(tokens, dtype=torch.long)
    return batch.to(device), torch.tensor(labels, dtype=torch.long, device=device), lengths


class StyleEncoder(nn.Module):
    def __init__(
        self,
        field_embedding_dim: int,
        hidden_dim: int,
        embedding_dim: int,
        dropout: float,
    ) -> None:
        super().__init__()
        self.field_embeddings = nn.ModuleList(
            nn.Embedding(cardinality, field_embedding_dim) for cardinality in FIELD_CARDINALITIES
        )
        token_dim = field_embedding_dim * len(FIELD_CARDINALITIES)
        self.input = nn.Sequential(
            nn.Linear(token_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
        )
        self.gru = nn.GRU(
            input_size=hidden_dim,
            hidden_size=hidden_dim,
            batch_first=True,
            bidirectional=True,
        )
        self.output = nn.Sequential(
            nn.Linear(hidden_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, embedding_dim),
        )

    def forward(self, tokens: torch.Tensor, lengths: list[int]) -> torch.Tensor:
        embedded = []
        for field_idx, emb in enumerate(self.field_embeddings):
            embedded.append(emb(tokens[:, :, field_idx]))
        x = torch.cat(embedded, dim=-1)
        x = self.input(x)
        packed = pack_padded_sequence(
            x,
            lengths=torch.tensor(lengths, device="cpu"),
            batch_first=True,
            enforce_sorted=False,
        )
        _, hidden = self.gru(packed)
        pooled = torch.cat([hidden[-2], hidden[-1]], dim=-1)
        z = self.output(pooled)
        return torch.nn.functional.normalize(z, p=2, dim=1)


def supervised_contrastive_loss(embeddings: torch.Tensor, labels: torch.Tensor, temperature: float) -> torch.Tensor:
    logits = embeddings @ embeddings.T / temperature
    n = logits.shape[0]
    self_mask = torch.eye(n, dtype=torch.bool, device=logits.device)
    logits = logits.masked_fill(self_mask, -1e9)
    same_player = labels[:, None].eq(labels[None, :]) & ~self_mask
    log_prob = logits - torch.logsumexp(logits, dim=1, keepdim=True)
    positive_counts = same_player.sum(dim=1).clamp_min(1)
    loss = -(log_prob * same_player).sum(dim=1) / positive_counts
    return loss.mean()


def train_model(
    model: StyleEncoder,
    sequences: list[dict[str, object]],
    by_player: dict[str, list[int]],
    validation_by_player: dict[str, list[int]],
    args: argparse.Namespace,
    device: torch.device,
) -> list[dict[str, float]]:
    trainable_players = [
        player for player, indices in by_player.items() if len(indices) >= max(2, args.games_per_player)
    ]
    if len(trainable_players) < 2:
        raise ValueError("Need at least two players with enough sequences for contrastive training.")

    batch_players = min(args.batch_players, len(trainable_players))
    steps_per_epoch = args.steps_per_epoch
    if steps_per_epoch <= 0:
        train_sequence_count = sum(len(indices) for indices in by_player.values())
        approx_batches = math.ceil(train_sequence_count / max(batch_players * args.games_per_player, 1))
        steps_per_epoch = max(10, approx_batches)

    optimizer = torch.optim.AdamW(
        model.parameters(),
        lr=args.learning_rate,
        weight_decay=args.weight_decay,
    )
    history: list[dict[str, float]] = []
    best_val_loss = float("inf")
    best_state: dict[str, torch.Tensor] | None = None
    stale_epochs = 0
    model.train()
    for epoch in range(1, args.epochs + 1):
        start = time.perf_counter()
        losses = []
        for step in range(1, steps_per_epoch + 1):
            players = random.sample(trainable_players, batch_players)
            batch, labels, lengths = make_batch(sequences, by_player, players, args.games_per_player, device)
            optimizer.zero_grad(set_to_none=True)
            embeddings = model(batch, lengths)
            loss = supervised_contrastive_loss(embeddings, labels, args.temperature)
            loss.backward()
            torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=2.0)
            optimizer.step()
            losses.append(float(loss.detach().cpu()))
        record = {
            "epoch": epoch,
            "mean_loss": float(np.mean(losses)),
            "seconds": time.perf_counter() - start,
            "steps": steps_per_epoch,
        }
        validation_loss = evaluate_contrastive_loss(
            model,
            sequences,
            validation_by_player,
            args,
            device,
            args.validation_batches,
        )
        if validation_loss is not None:
            record["validation_loss"] = validation_loss
            if args.early_stopping_patience > 0:
                if validation_loss < best_val_loss - 1e-4:
                    best_val_loss = validation_loss
                    best_state = {
                        key: value.detach().cpu().clone()
                        for key, value in model.state_dict().items()
                    }
                    stale_epochs = 0
                else:
                    stale_epochs += 1
                    record["stale_epochs"] = stale_epochs
        history.append(record)
        print(json.dumps(record), flush=True)
        if args.early_stopping_patience > 0 and validation_loss is not None:
            if stale_epochs >= args.early_stopping_patience:
                print(
                    json.dumps(
                        {
                            "early_stop_epoch": epoch,
                            "best_validation_loss": best_val_loss,
                            "patience": args.early_stopping_patience,
                        }
                    ),
                    flush=True,
                )
                break
    if best_state is not None:
        model.load_state_dict(best_state)
    return history


@torch.no_grad()
def evaluate_contrastive_loss(
    model: StyleEncoder,
    sequences: list[dict[str, object]],
    by_player: dict[str, list[int]],
    args: argparse.Namespace,
    device: torch.device,
    batches: int,
) -> float | None:
    trainable_players = [
        player for player, indices in by_player.items() if len(indices) >= max(2, args.games_per_player)
    ]
    if batches <= 0 or len(trainable_players) < 2:
        return None

    was_training = model.training
    model.eval()
    batch_players = min(args.batch_players, len(trainable_players))
    losses = []
    for _ in range(batches):
        players = random.sample(trainable_players, batch_players)
        batch, labels, lengths = make_batch(sequences, by_player, players, args.games_per_player, device)
        embeddings = model(batch, lengths)
        loss = supervised_contrastive_loss(embeddings, labels, args.temperature)
        losses.append(float(loss.detach().cpu()))
    if was_training:
        model.train()
    return float(np.mean(losses))


@torch.no_grad()
def encode_sequences(
    model: StyleEncoder,
    sequences: list[dict[str, object]],
    device: torch.device,
    batch_size: int = 512,
) -> np.ndarray:
    model.eval()
    all_embeddings = []
    for start in range(0, len(sequences), batch_size):
        chunk = sequences[start : start + batch_size]
        lengths = [len(rec["tokens"]) for rec in chunk]
        max_len = max(lengths)
        batch = torch.zeros((len(chunk), max_len, len(FIELD_CARDINALITIES)), dtype=torch.long)
        for row_idx, rec in enumerate(chunk):
            tokens = rec["tokens"]
            batch[row_idx, : len(tokens), :] = torch.tensor(tokens, dtype=torch.long)
        embeddings = model(batch.to(device), lengths).cpu().numpy()
        all_embeddings.append(embeddings)
    return np.vstack(all_embeddings)


def write_game_embeddings(path: Path, sequences: list[dict[str, object]], embeddings: np.ndarray) -> None:
    fieldnames = ["player", "game_id", "mover_color", "date"] + [
        f"emb_{i:03d}" for i in range(embeddings.shape[1])
    ]
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for rec, emb in zip(sequences, embeddings):
            row = {
                "player": rec["player"],
                "game_id": rec["game_id"],
                "mover_color": rec["mover_color"],
                "date": rec["date"],
            }
            row.update({f"emb_{i:03d}": f"{value:.8g}" for i, value in enumerate(emb)})
            writer.writerow(row)


def average_player_embeddings(
    sequences: list[dict[str, object]],
    game_embeddings: np.ndarray,
) -> tuple[list[str], np.ndarray, dict[str, int]]:
    sums: dict[str, np.ndarray] = {}
    counts: dict[str, int] = defaultdict(int)
    for rec, emb in zip(sequences, game_embeddings):
        player = str(rec["player"])
        if player not in sums:
            sums[player] = np.zeros(game_embeddings.shape[1], dtype=np.float64)
        sums[player] += emb
        counts[player] += 1
    players = sorted(sums)
    matrix = np.vstack([sums[player] / counts[player] for player in players])
    norms = np.linalg.norm(matrix, axis=1, keepdims=True)
    matrix = matrix / np.maximum(norms, 1e-12)
    return players, matrix.astype(np.float32), dict(counts)


def write_player_embeddings(path: Path, players: list[str], embeddings: np.ndarray, counts: dict[str, int]) -> None:
    fieldnames = ["player", "prechange_sequences"] + [f"emb_{i:03d}" for i in range(embeddings.shape[1])]
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for player, emb in zip(players, embeddings):
            row = {"player": player, "prechange_sequences": counts[player]}
            row.update({f"emb_{i:03d}": f"{value:.8g}" for i, value in enumerate(emb)})
            writer.writerow(row)


def parse_k_grid(value: str, main_k: int) -> list[int]:
    out = {main_k}
    for item in value.split(","):
        item = item.strip()
        if not item:
            continue
        out.add(int(item))
    return sorted(k for k in out if k >= 2)


def cluster_embeddings(
    players: list[str],
    embeddings: np.ndarray,
    counts: dict[str, int],
    args: argparse.Namespace,
    output_dir: Path,
) -> list[dict[str, float]]:
    diagnostics = []
    for k in parse_k_grid(args.cluster_k_grid, args.cluster_k):
        if k >= len(players):
            continue
        km = KMeans(n_clusters=k, n_init=50, random_state=args.seed)
        labels = km.fit_predict(embeddings)
        inertia = float(km.inertia_)
        silhouette = float(silhouette_score(embeddings, labels)) if len(set(labels)) > 1 else float("nan")
        diagnostics.append({"k": k, "inertia": inertia, "silhouette": silhouette})
        if k == args.cluster_k:
            cluster_path = output_dir / f"player_style_clusters_k{k}.csv"
            with cluster_path.open("w", encoding="utf-8", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=["player", "style_cluster", "prechange_sequences"])
                writer.writeheader()
                for player, label in zip(players, labels):
                    writer.writerow(
                        {
                            "player": player,
                            "style_cluster": int(label),
                            "prechange_sequences": counts[player],
                        }
                    )
    return diagnostics


def write_cluster_diagnostics(path: Path, diagnostics: list[dict[str, float]]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["k", "inertia", "silhouette"])
        writer.writeheader()
        writer.writerows(diagnostics)


def load_style_features(path: Path) -> tuple[list[str], dict[str, dict[str, str]]]:
    if not path.exists():
        return [], {}
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        fields = list(reader.fieldnames or [])
        rows = {row["player"]: row for row in reader if row.get("player")}
    return fields, rows


def try_float(value: str) -> float | None:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def write_cluster_profile(
    path: Path,
    clusters_path: Path,
    style_features_csv: Path,
) -> int:
    fields, feature_rows = load_style_features(style_features_csv)
    if not fields or not feature_rows or not clusters_path.exists():
        return 0

    clusters: dict[int, list[str]] = defaultdict(list)
    with clusters_path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.DictReader(f):
            player = row.get("player", "")
            if player in feature_rows:
                clusters[int(row["style_cluster"])].append(player)

    numeric_fields = []
    for field in fields:
        if field == "player":
            continue
        values = [try_float(row.get(field, "")) for row in feature_rows.values()]
        values = [value for value in values if value is not None]
        if len(values) >= 10:
            numeric_fields.append(field)

    rows_written = 0
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["style_cluster", "players", "feature", "mean"])
        writer.writeheader()
        for cluster_id in sorted(clusters):
            cluster_players = clusters[cluster_id]
            for field in numeric_fields:
                values = [
                    try_float(feature_rows[player].get(field, ""))
                    for player in cluster_players
                    if player in feature_rows
                ]
                values = [value for value in values if value is not None]
                if not values:
                    continue
                writer.writerow(
                    {
                        "style_cluster": cluster_id,
                        "players": len(cluster_players),
                        "feature": field,
                        "mean": f"{float(np.mean(values)):.8g}",
                    }
                )
                rows_written += 1
    return rows_written


def main() -> int:
    args = parse_args()
    set_seed(args.seed)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    device = choose_device(args.device)
    start = time.perf_counter()
    if not args.input_jsonl.exists():
        raise FileNotFoundError(
            f"Input sequence JSONL not found: {args.input_jsonl}. "
            "Build it first with build_contrastive_style_dataset.py."
        )

    sequences, by_player = load_sequences(
        args.input_jsonl,
        args.min_sequences_per_player,
        args.max_sequences,
    )
    if not sequences:
        raise ValueError(f"No usable sequences found in {args.input_jsonl}")
    train_by_player, validation_by_player, split_summary = split_train_validation_players(
        by_player,
        args.validation_share,
        args.games_per_player,
        args.seed,
    )

    model = StyleEncoder(
        field_embedding_dim=args.field_embedding_dim,
        hidden_dim=args.hidden_dim,
        embedding_dim=args.embedding_dim,
        dropout=args.dropout,
    ).to(device)
    history = train_model(model, sequences, train_by_player, validation_by_player, args, device)

    checkpoint_path = args.output_dir / "contrastive_style_encoder.pt"
    torch.save(
        {
            "model_state_dict": model.state_dict(),
            "args": vars(args),
            "token_field_names": TOKEN_FIELD_NAMES,
            "field_cardinalities": FIELD_CARDINALITIES,
            "history": history,
        },
        checkpoint_path,
    )

    game_embeddings = encode_sequences(model, sequences, device, batch_size=args.encode_batch_size)
    if not args.no_game_embeddings:
        write_game_embeddings(args.output_dir / "game_style_embeddings.csv", sequences, game_embeddings)
    players, player_embeddings, player_counts = average_player_embeddings(sequences, game_embeddings)
    write_player_embeddings(args.output_dir / "player_style_embeddings.csv", players, player_embeddings, player_counts)

    cluster_diagnostics = cluster_embeddings(players, player_embeddings, player_counts, args, args.output_dir)
    write_cluster_diagnostics(args.output_dir / "cluster_diagnostics.csv", cluster_diagnostics)
    profile_rows = write_cluster_profile(
        args.output_dir / f"cluster_profile_k{args.cluster_k}.csv",
        args.output_dir / f"player_style_clusters_k{args.cluster_k}.csv",
        args.style_features_csv,
    )

    summary = {
        "input_jsonl": str(args.input_jsonl),
        "output_dir": str(args.output_dir),
        "style_features_csv": str(args.style_features_csv),
        "device": str(device),
        "sequences_used": len(sequences),
        "players_used": len(by_player),
        "split_summary": split_summary,
        "epochs": args.epochs,
        "epochs_completed": len(history),
        "batch_players": args.batch_players,
        "games_per_player": args.games_per_player,
        "validation_share": args.validation_share,
        "validation_batches": args.validation_batches,
        "early_stopping_patience": args.early_stopping_patience,
        "embedding_dim": args.embedding_dim,
        "checkpoint": str(checkpoint_path),
        "player_embeddings_csv": str(args.output_dir / "player_style_embeddings.csv"),
        "cluster_diagnostics_csv": str(args.output_dir / "cluster_diagnostics.csv"),
        "cluster_diagnostics": cluster_diagnostics,
        "cluster_profile_rows": profile_rows,
        "seconds": time.perf_counter() - start,
    }
    (args.output_dir / "training_summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
