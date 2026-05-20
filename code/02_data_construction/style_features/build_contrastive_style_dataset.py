#!/usr/bin/env python3
"""Build pre-change move-sequence data for contrastive style learning.

The output is a JSONL file of player-game sequences. Each line is one player's
moves in one game, after excluding opening theory and final moves. It is built
from currently available PGN + Stockfish centipawn rows and is safe to rerun as
those files grow.

Eligibility is computed from the full regression/player-game dataset:
- at least --min-total-games total player-game observations;
- at least one pre-change and one post-change observation.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
import sys
import time
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Sequence

import chess.pgn


ROOT = Path(__file__).resolve().parents[3]
PROJECT_ROOT = ROOT
DEFAULT_METADATA = PROJECT_ROOT / "data" / "final_regression_data_tournaments_2022_2026.csv"
DEFAULT_DATASET_DIR = ROOT / "outputs" / "whole_dataset_2022_2026"
DEFAULT_COMBINED_PGN = DEFAULT_DATASET_DIR / "whole_dataset_2022_2026.pgn"
DEFAULT_SOURCE_PGNS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "whole_dataset_2022_2024.pgn",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "whole_dataset_2024_2026.pgn",
]
DEFAULT_PGNS = [DEFAULT_COMBINED_PGN] if DEFAULT_COMBINED_PGN.exists() else DEFAULT_SOURCE_PGNS
DEFAULT_CENTIPAWNS = [
    ROOT
    / "outputs"
    / "whole_dataset_2022_2024"
    / "centipawn_loss_nodes2000_watch"
    / "centipawn_loss_watch.csv",
    ROOT
    / "outputs"
    / "whole_dataset_2024_2026"
    / "centipawn_loss_nodes2000_watch"
    / "centipawn_loss_watch.csv",
]
DEFAULT_STYLE_FEATURES = (
    ROOT
    / "outputs"
    / "whole_dataset_2022_2026"
    / "style_features"
    / "prechange_player_style_features.csv"
)
DEFAULT_OUTPUT_DIR = DEFAULT_DATASET_DIR / "contrastive_style"


PIECE_TO_ID = {
    "pawn": 0,
    "N": 1,
    "B": 2,
    "R": 3,
    "Q": 4,
    "K": 5,
    "castle": 6,
    "unknown": 7,
}
PHASE_TO_ID = {
    "opening_1_10": 0,
    "early_middlegame_11_20": 1,
    "late_middlegame_21_35": 2,
    "endgame_36_plus": 3,
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--metadata-csv", type=Path, default=DEFAULT_METADATA)
    parser.add_argument(
        "--pgn",
        dest="pgn",
        type=Path,
        action="append",
        default=None,
        help="PGN file. Can be repeated. Defaults to the two 2022-2026 source windows.",
    )
    parser.add_argument(
        "--pgns",
        default="",
        help="Comma-separated PGN files; appended to --pgn values.",
    )
    parser.add_argument(
        "--centipawn-csv",
        dest="centipawn_csv",
        type=Path,
        action="append",
        default=None,
        help="Centipawn-loss CSV. Can be repeated. Defaults to the two 2022-2026 source windows.",
    )
    parser.add_argument(
        "--centipawn-csvs",
        default="",
        help="Comma-separated centipawn-loss CSVs; appended to --centipawn-csv values.",
    )
    parser.add_argument("--style-features-csv", type=Path, default=DEFAULT_STYLE_FEATURES)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--rule-change-date", default="2025-09-01")
    parser.add_argument("--min-total-games", type=int, default=100)
    parser.add_argument("--min-pre-games-in-current-pgn", type=int, default=2)
    parser.add_argument("--exclude-first-fullmoves", type=int, default=10)
    parser.add_argument("--exclude-last-plies", type=int, default=10)
    parser.add_argument("--min-seq-len", type=int, default=12)
    parser.add_argument("--max-seq-len", type=int, default=80)
    parser.add_argument("--progress-every", type=int, default=500000)
    parser.add_argument("--no-progress", action="store_true")
    args = parser.parse_args()
    args.pgns = parse_path_list(args.pgn, args.pgns, DEFAULT_PGNS)
    args.centipawn_csvs = parse_path_list(args.centipawn_csv, args.centipawn_csvs, DEFAULT_CENTIPAWNS)
    return args


def parse_path_list(
    repeated_paths: Sequence[Path] | None,
    comma_separated_paths: str,
    defaults: Sequence[Path],
) -> list[Path]:
    paths = [Path(path) for path in repeated_paths or []]
    for item in comma_separated_paths.split(","):
        item = item.strip()
        if item:
            paths.append(Path(item))
    return paths or list(defaults)


def game_id_from_text(value: str) -> str:
    match = re.search(r"/(?:game|analysis/game)/live/(\d+)", value or "")
    if match:
        return match.group(1)
    match = re.search(r"(\d{8,})", value or "")
    return match.group(1) if match else ""


def game_id_from_headers(headers: chess.pgn.Headers) -> str:
    for key in ("Link", "Site", "URL"):
        game_id = game_id_from_text(headers.get(key, ""))
        if game_id:
            return game_id
    return ""


def parse_pgn_date(headers: chess.pgn.Headers) -> datetime | None:
    date = headers.get("UTCDate") or headers.get("Date")
    if not date or "?" in date:
        return None
    try:
        return datetime.strptime(date, "%Y.%m.%d")
    except ValueError:
        return None


def parse_metadata_date(value: str) -> datetime:
    return datetime.fromisoformat(value.strip())


def load_eligible_players(metadata_csv: Path, rule_change_date: datetime, min_total_games: int) -> dict[str, dict[str, int]]:
    stats: dict[str, dict[str, int]] = defaultdict(lambda: {"total": 0, "pre": 0, "post": 0})
    with metadata_csv.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            player = row.get("player_name", "").strip()
            if not player:
                continue
            dt = parse_metadata_date(row["date"])
            stats[player]["total"] += 1
            if dt < rule_change_date:
                stats[player]["pre"] += 1
            else:
                stats[player]["post"] += 1
    return {
        player: values
        for player, values in stats.items()
        if values["total"] >= min_total_games and values["pre"] > 0 and values["post"] > 0
    }


def load_style_feature_players(path: Path) -> set[str]:
    if not path.exists():
        return set()
    with path.open("r", encoding="utf-8", newline="") as f:
        return {row["player"] for row in csv.DictReader(f) if row.get("player")}


def load_prechange_pgn_game_ids(
    pgn_paths: Sequence[Path],
    rule_change_date: datetime,
    eligible_players: set[str],
) -> tuple[set[str], dict[str, dict[str, object]], dict[str, object]]:
    game_ids: set[str] = set()
    metadata: dict[str, dict[str, object]] = {}
    start = time.perf_counter()
    scanned = 0
    duplicate_selected_game_ids = 0
    file_summaries = []
    for pgn_path in pgn_paths:
        file_scanned = 0
        file_selected = 0
        file_duplicates = 0
        with pgn_path.open("r", encoding="utf-8", errors="replace") as f:
            while True:
                game = chess.pgn.read_game(f)
                if game is None:
                    break
                scanned += 1
                file_scanned += 1
                game_id = game_id_from_headers(game.headers)
                game_date = parse_pgn_date(game.headers)
                if not game_id or game_date is None or game_date >= rule_change_date:
                    continue
                white = game.headers.get("White", "")
                black = game.headers.get("Black", "")
                if white not in eligible_players and black not in eligible_players:
                    continue
                if game_id in game_ids:
                    duplicate_selected_game_ids += 1
                    file_duplicates += 1
                    continue
                ply_count = sum(1 for _ in game.mainline_moves())
                game_ids.add(game_id)
                file_selected += 1
                metadata[game_id] = {
                    "date": game_date.strftime("%Y-%m-%d"),
                    "white": white,
                    "black": black,
                    "ply_count": ply_count,
                }
                if scanned % 50000 == 0:
                    elapsed = time.perf_counter() - start
                    print(
                        f"PGN scan: scanned={scanned:,} selected_prechange_games={len(game_ids):,} "
                        f"elapsed={elapsed:,.1f}s",
                        file=sys.stderr,
                    )
        file_summaries.append(
            {
                "pgn": str(pgn_path),
                "games_scanned": file_scanned,
                "selected_prechange_games_added": file_selected,
                "duplicate_selected_game_ids": file_duplicates,
            }
        )
    return game_ids, metadata, {
        "pgn_files": file_summaries,
        "games_scanned": scanned,
        "duplicate_selected_game_ids": duplicate_selected_game_ids,
    }


def square_id(square: str) -> int:
    if not square or len(square) != 2:
        return 64
    file_char, rank_char = square[0], square[1]
    if file_char < "a" or file_char > "h" or rank_char < "1" or rank_char > "8":
        return 64
    return (int(rank_char) - 1) * 8 + (ord(file_char) - ord("a"))


def piece_id_from_san(san: str) -> int:
    if not san:
        return PIECE_TO_ID["unknown"]
    if san.startswith("O-O"):
        return PIECE_TO_ID["castle"]
    san = san.lstrip("+#?!")
    first = san[0]
    if first in "NBRQK":
        return PIECE_TO_ID[first]
    if first in "abcdefgh":
        return PIECE_TO_ID["pawn"]
    return PIECE_TO_ID["unknown"]


def cp_loss_bin(value: float) -> int:
    if value < 10:
        return 0
    if value < 25:
        return 1
    if value < 50:
        return 2
    if value < 100:
        return 3
    if value < 200:
        return 4
    return 5


def eval_bin(value: float) -> int:
    capped = max(-1000.0, min(1000.0, value))
    return int(math.floor((capped + 1000.0) / 100.0))


def move_number_bin(fullmove: int) -> int:
    return min(max((fullmove - 1) // 5, 0), 20)


def safe_float(value: str | None, default: float = 0.0) -> float:
    try:
        return float(value) if value not in (None, "") else default
    except ValueError:
        return default


def safe_int(value: str | None, default: int = 0) -> int:
    try:
        return int(float(value)) if value not in (None, "") else default
    except ValueError:
        return default


def token_from_row(row: dict[str, str]) -> list[int]:
    uci = row.get("uci", "")
    from_sq = square_id(uci[:2] if len(uci) >= 4 else "")
    to_sq = square_id(uci[2:4] if len(uci) >= 4 else "")
    return [
        piece_id_from_san(row.get("san", "")),
        from_sq,
        to_sq,
        safe_int(row.get("is_capture")),
        safe_int(row.get("gives_check")),
        PHASE_TO_ID.get(row.get("phase", ""), 4),
        cp_loss_bin(safe_float(row.get("cp_loss"))),
        eval_bin(safe_float(row.get("eval_before_mover_cp"))),
        move_number_bin(safe_int(row.get("fullmove_number"))),
    ]


def flush_sequence(
    out_f,
    game_id: str,
    color: str,
    player: str,
    game_meta: dict[str, object],
    tokens: list[list[int]],
    min_seq_len: int,
    max_seq_len: int,
) -> bool:
    if not player or len(tokens) < min_seq_len:
        return False
    if len(tokens) > max_seq_len:
        tokens = tokens[:max_seq_len]
    record = {
        "player": player,
        "game_id": game_id,
        "mover_color": color,
        "date": game_meta["date"],
        "sequence_length": len(tokens),
        "tokens": tokens,
    }
    out_f.write(json.dumps(record, separators=(",", ":")))
    out_f.write("\n")
    return True


def build_sequences(
    centipawn_csvs: Sequence[Path],
    output_jsonl: Path,
    selected_game_ids: set[str],
    pgn_metadata: dict[str, dict[str, object]],
    eligible_players: set[str],
    exclude_first_fullmoves: int,
    exclude_last_plies: int,
    min_seq_len: int,
    max_seq_len: int,
    progress_every: int,
    no_progress: bool,
) -> dict[str, object]:
    player_sequence_counts: dict[str, int] = defaultdict(int)
    rows = 0
    used_move_rows = 0
    sequences_written = 0
    current_game_id = None
    current_tokens = {"white": [], "black": []}
    current_players = {"white": "", "black": ""}
    completed_game_ids: set[str] = set()
    skipped_duplicate_game_rows = 0
    file_summaries = []
    start = time.perf_counter()

    def flush_current(out_f) -> None:
        nonlocal current_game_id, current_tokens, current_players, sequences_written
        if current_game_id is None:
            return
        meta = pgn_metadata[current_game_id]
        for color in ("white", "black"):
            player = current_players[color]
            if player not in eligible_players:
                continue
            ok = flush_sequence(
                out_f,
                current_game_id,
                color,
                player,
                meta,
                current_tokens[color],
                min_seq_len,
                max_seq_len,
            )
            if ok:
                sequences_written += 1
                player_sequence_counts[player] += 1
        completed_game_ids.add(current_game_id)
        current_game_id = None
        current_tokens = {"white": [], "black": []}
        current_players = {"white": "", "black": ""}

    with output_jsonl.open("w", encoding="utf-8") as out_f:
        for centipawn_csv in centipawn_csvs:
            file_rows_start = rows
            file_used_start = used_move_rows
            with centipawn_csv.open("r", encoding="utf-8", newline="") as in_f:
                reader = csv.DictReader(in_f)
                for row in reader:
                    rows += 1
                    game_id = row.get("game_id", "")
                    if game_id in completed_game_ids:
                        skipped_duplicate_game_rows += 1
                        continue
                    if game_id not in selected_game_ids:
                        continue
                    if game_id != current_game_id:
                        flush_current(out_f)
                        current_game_id = game_id
                        meta = pgn_metadata[game_id]
                        current_tokens = {"white": [], "black": []}
                        current_players = {"white": str(meta["white"]), "black": str(meta["black"])}

                    meta = pgn_metadata[game_id]
                    ply = safe_int(row.get("ply"))
                    fullmove = safe_int(row.get("fullmove_number"))
                    color = row.get("mover_color", "")
                    if color not in ("white", "black"):
                        continue
                    if fullmove <= exclude_first_fullmoves:
                        continue
                    if ply > int(meta["ply_count"]) - exclude_last_plies:
                        continue
                    player = current_players[color]
                    if player not in eligible_players:
                        continue
                    current_tokens[color].append(token_from_row(row))
                    used_move_rows += 1

                    if progress_every and not no_progress and rows % progress_every == 0:
                        elapsed = time.perf_counter() - start
                        print(
                            f"Centipawn scan: rows={rows:,} used_moves={used_move_rows:,} "
                            f"sequences={sequences_written:,} players={len(player_sequence_counts):,} "
                            f"elapsed={elapsed:,.1f}s",
                            file=sys.stderr,
                        )
                flush_current(out_f)
            file_summaries.append(
                {
                    "centipawn_csv": str(centipawn_csv),
                    "rows_read": rows - file_rows_start,
                    "move_rows_used_in_sequences": used_move_rows - file_used_start,
                }
            )

    return {
        "centipawn_rows_read": rows,
        "move_rows_used_in_sequences": used_move_rows,
        "sequences_written": sequences_written,
        "players_with_sequences": len(player_sequence_counts),
        "rows_skipped_duplicate_game": skipped_duplicate_game_rows,
        "centipawn_file_summaries": file_summaries,
        "player_sequence_counts": dict(sorted(player_sequence_counts.items())),
    }


def write_csv(path: Path, rows: list[dict[str, object]], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    start = time.perf_counter()
    rule_change_date = datetime.strptime(args.rule_change_date, "%Y-%m-%d")

    eligible = load_eligible_players(args.metadata_csv, rule_change_date, args.min_total_games)
    style_players = load_style_feature_players(args.style_features_csv)
    eligible_with_style = set(eligible)
    if style_players:
        eligible_with_style &= style_players

    eligible_rows = [
        {
            "player": player,
            "total_games": values["total"],
            "pre_games": values["pre"],
            "post_games": values["post"],
            "has_interpretable_style_features": int(player in style_players) if style_players else "",
        }
        for player, values in sorted(eligible.items())
    ]
    write_csv(
        args.output_dir / "eligible_players_total100_prepost.csv",
        eligible_rows,
        ["player", "total_games", "pre_games", "post_games", "has_interpretable_style_features"],
    )

    print(
        f"Eligible players from metadata: {len(eligible):,}; "
        f"eligible with current style-feature row: {len(eligible_with_style):,}",
        file=sys.stderr,
    )
    selected_game_ids, pgn_metadata, pgn_summary = load_prechange_pgn_game_ids(
        args.pgns,
        rule_change_date,
        eligible_with_style,
    )
    sequence_path = args.output_dir / "prechange_contrastive_sequences.jsonl"
    sequence_summary = build_sequences(
        args.centipawn_csvs,
        sequence_path,
        selected_game_ids,
        pgn_metadata,
        eligible_with_style,
        args.exclude_first_fullmoves,
        args.exclude_last_plies,
        args.min_seq_len,
        args.max_seq_len,
        args.progress_every,
        args.no_progress,
    )

    summary = {
        "metadata_csv": str(args.metadata_csv),
        "pgns": [str(path) for path in args.pgns],
        "centipawn_csvs": [str(path) for path in args.centipawn_csvs],
        "style_features_csv": str(args.style_features_csv),
        "rule_change_date": args.rule_change_date,
        "min_total_games": args.min_total_games,
        "eligible_players_metadata": len(eligible),
        "eligible_players_with_style_features": len(eligible_with_style),
        "selected_prechange_games_in_pgns": len(selected_game_ids),
        "exclude_first_fullmoves": args.exclude_first_fullmoves,
        "exclude_last_plies": args.exclude_last_plies,
        "min_seq_len": args.min_seq_len,
        "max_seq_len": args.max_seq_len,
        "sequence_jsonl": str(sequence_path),
        "pgn_summary": pgn_summary,
        "seconds": time.perf_counter() - start,
        **{k: v for k, v in sequence_summary.items() if k != "player_sequence_counts"},
    }
    (args.output_dir / "contrastive_dataset_summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8"
    )
    (args.output_dir / "contrastive_player_sequence_counts.json").write_text(
        json.dumps(sequence_summary["player_sequence_counts"], indent=2), encoding="utf-8"
    )
    vocab = {
        "token_fields": [
            "piece_id",
            "from_square_id",
            "to_square_id",
            "is_capture",
            "gives_check",
            "phase_id",
            "cp_loss_bin",
            "eval_before_bin",
            "move_number_bin",
        ],
        "piece_to_id": PIECE_TO_ID,
        "phase_to_id": PHASE_TO_ID,
        "from_square_id": "0-63 are a1-h8, 64 is unknown",
        "to_square_id": "0-63 are a1-h8, 64 is unknown",
        "cp_loss_bin": ["<10", "10-24", "25-49", "50-99", "100-199", ">=200"],
        "eval_before_bin": "floor((clamp(eval_before_mover_cp,-1000,1000)+1000)/100), 0-20",
        "move_number_bin": "5-move bins, capped at 20",
    }
    (args.output_dir / "contrastive_token_vocab.json").write_text(
        json.dumps(vocab, indent=2), encoding="utf-8"
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
