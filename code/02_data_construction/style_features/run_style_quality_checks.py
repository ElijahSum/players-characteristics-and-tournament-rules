#!/usr/bin/env python3
"""Quality checks for neural style clusters and rule-change benefits.

The checks are intentionally light on dependencies and write flat files that
are easy to inspect:
- cluster sizes and embedding silhouette;
- cluster profiles from interpretable pre-change style variables;
- top standardized feature differences by cluster;
- pre/post player outcomes by style cluster.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import numpy as np
from sklearn.metrics import silhouette_score


ROOT = Path(__file__).resolve().parents[3]
PROJECT_ROOT = ROOT
DEFAULT_DATASET_DIR = ROOT / "outputs" / "whole_dataset_2022_2026"
DEFAULT_NEURAL_DIR = DEFAULT_DATASET_DIR / "contrastive_style" / "neural_style"
DEFAULT_CLUSTERS = DEFAULT_NEURAL_DIR / "player_style_clusters_k5.csv"
DEFAULT_EMBEDDINGS = DEFAULT_NEURAL_DIR / "player_style_embeddings.csv"
DEFAULT_STYLE_FEATURES = DEFAULT_DATASET_DIR / "style_features" / "prechange_player_style_features.csv"
DEFAULT_REGRESSION = PROJECT_ROOT / "data" / "final_regression_data_tournaments_2022_2026.csv"
DEFAULT_OUTPUT_DIR = DEFAULT_DATASET_DIR / "contrastive_style" / "qc"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--clusters-csv", type=Path, default=DEFAULT_CLUSTERS)
    parser.add_argument("--player-embeddings-csv", type=Path, default=DEFAULT_EMBEDDINGS)
    parser.add_argument("--style-features-csv", type=Path, default=DEFAULT_STYLE_FEATURES)
    parser.add_argument("--regression-csv", type=Path, default=DEFAULT_REGRESSION)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--rule-change-date", default="2025-09-01")
    parser.add_argument("--outcomes", default="player_result,player_accuracy")
    parser.add_argument("--top-features-per-cluster", type=int, default=8)
    parser.add_argument("--min-feature-n", type=int, default=5)
    parser.add_argument("--min-period-games", type=int, default=3)
    return parser.parse_args()


def safe_float(value: str | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        out = float(value)
    except ValueError:
        return None
    if math.isnan(out):
        return None
    return out


def parse_date(value: str) -> datetime | None:
    value = (value or "").strip()
    if not value:
        return None
    for candidate in (value[:10], value):
        for fmt in ("%Y-%m-%d", "%Y.%m.%d"):
            try:
                return datetime.strptime(candidate, fmt)
            except ValueError:
                continue
    return None


def mean(values: list[float]) -> float | None:
    return float(np.mean(values)) if values else None


def sd(values: list[float]) -> float:
    if len(values) <= 1:
        return 0.0
    return float(np.std(values, ddof=1))


def fmt(value: object) -> object:
    if value is None:
        return ""
    if isinstance(value, float):
        if math.isnan(value):
            return ""
        return f"{value:.8g}"
    return value


def write_csv(path: Path, rows: list[dict[str, object]], fieldnames: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow({field: fmt(row.get(field, "")) for field in fieldnames})


def load_clusters(path: Path) -> tuple[dict[str, int], list[dict[str, object]]]:
    clusters: dict[str, int] = {}
    rows = []
    with path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.DictReader(f):
            player = row.get("player", "")
            if not player:
                continue
            cluster = int(row["style_cluster"])
            clusters[player] = cluster
            rows.append(
                {
                    "player": player,
                    "style_cluster": cluster,
                    "prechange_sequences": int(float(row.get("prechange_sequences") or 0)),
                }
            )
    return clusters, rows


def cluster_size_rows(cluster_rows: list[dict[str, object]]) -> list[dict[str, object]]:
    counts: dict[int, dict[str, int]] = defaultdict(lambda: {"players": 0, "prechange_sequences": 0})
    for row in cluster_rows:
        cluster = int(row["style_cluster"])
        counts[cluster]["players"] += 1
        counts[cluster]["prechange_sequences"] += int(row["prechange_sequences"])
    return [
        {
            "style_cluster": cluster,
            "players": values["players"],
            "prechange_sequences": values["prechange_sequences"],
        }
        for cluster, values in sorted(counts.items())
    ]


def embedding_qc(path: Path, clusters: dict[str, int]) -> dict[str, object]:
    if not path.exists():
        return {"embedding_file_exists": False}
    players = []
    labels = []
    vectors = []
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        emb_fields = [field for field in reader.fieldnames or [] if field.startswith("emb_")]
        for row in reader:
            player = row.get("player", "")
            if player not in clusters:
                continue
            vector = [safe_float(row.get(field)) for field in emb_fields]
            if any(value is None for value in vector):
                continue
            players.append(player)
            labels.append(clusters[player])
            vectors.append([float(value) for value in vector if value is not None])
    unique_labels = sorted(set(labels))
    silhouette = None
    if len(vectors) > len(unique_labels) and len(unique_labels) > 1:
        silhouette = float(silhouette_score(np.asarray(vectors, dtype=np.float32), np.asarray(labels)))
    return {
        "embedding_file_exists": True,
        "embedding_players_joined": len(players),
        "embedding_dimensions": len(vectors[0]) if vectors else 0,
        "embedding_clusters": len(unique_labels),
        "embedding_silhouette": silhouette,
    }


def style_feature_profiles(
    path: Path,
    clusters: dict[str, int],
    min_feature_n: int,
    top_features_per_cluster: int,
) -> tuple[list[dict[str, object]], list[dict[str, object]], dict[str, object]]:
    feature_values: dict[str, dict[int, list[float]]] = defaultdict(lambda: defaultdict(list))
    global_values: dict[str, list[float]] = defaultdict(list)
    cluster_players: dict[int, set[str]] = defaultdict(set)
    rows_read = 0
    rows_joined = 0

    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        fields = [field for field in reader.fieldnames or [] if field != "player"]
        for row in reader:
            rows_read += 1
            player = row.get("player", "")
            if player not in clusters:
                continue
            rows_joined += 1
            cluster = clusters[player]
            cluster_players[cluster].add(player)
            for field in fields:
                value = safe_float(row.get(field))
                if value is None:
                    continue
                feature_values[field][cluster].append(value)
                global_values[field].append(value)

    profile_rows = []
    for field in sorted(feature_values):
        all_values = global_values[field]
        if len(all_values) < min_feature_n:
            continue
        global_mean = mean(all_values)
        global_sd = sd(all_values)
        for cluster in sorted(cluster_players):
            values = feature_values[field].get(cluster, [])
            if len(values) < min_feature_n:
                continue
            cluster_mean = mean(values)
            standardized_difference = None
            if global_mean is not None and global_sd > 0 and cluster_mean is not None:
                standardized_difference = (cluster_mean - global_mean) / global_sd
            profile_rows.append(
                {
                    "style_cluster": cluster,
                    "feature": field,
                    "players_with_feature": len(values),
                    "cluster_players": len(cluster_players[cluster]),
                    "mean": cluster_mean,
                    "global_mean": global_mean,
                    "global_sd": global_sd,
                    "standardized_difference": standardized_difference,
                }
            )

    top_rows = []
    by_cluster: dict[int, list[dict[str, object]]] = defaultdict(list)
    for row in profile_rows:
        by_cluster[int(row["style_cluster"])].append(row)
    for cluster, rows in sorted(by_cluster.items()):
        rows = sorted(
            rows,
            key=lambda item: abs(float(item["standardized_difference"] or 0.0)),
            reverse=True,
        )
        for rank, row in enumerate(rows[:top_features_per_cluster], start=1):
            top_rows.append({"rank": rank, **row})

    summary = {
        "style_feature_rows_read": rows_read,
        "style_feature_players_joined": rows_joined,
        "style_feature_profile_rows": len(profile_rows),
    }
    return profile_rows, top_rows, summary


def new_stats() -> dict[str, object]:
    return {"n": 0, "players": set(), "sums": defaultdict(float), "counts": defaultdict(int)}


def add_observation(stats: dict[str, object], player: str) -> None:
    stats["n"] = int(stats["n"]) + 1
    stats["players"].add(player)


def add_outcome(stats: dict[str, object], player: str, outcome: str, value: float | None) -> None:
    if value is None:
        return
    stats["sums"][outcome] += value
    stats["counts"][outcome] += 1


def outcome_checks(
    regression_csv: Path,
    clusters: dict[str, int],
    rule_change_date: datetime,
    outcomes: list[str],
    min_period_games: int,
) -> tuple[list[dict[str, object]], list[dict[str, object]], list[dict[str, object]], dict[str, object]]:
    cluster_period_stats: dict[tuple[int, str], dict[str, object]] = defaultdict(new_stats)
    player_period_stats: dict[tuple[str, str], dict[str, object]] = defaultdict(new_stats)
    rows_read = 0
    rows_joined = 0
    rows_bad_date = 0

    with regression_csv.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows_read += 1
            player = row.get("player_name", "")
            if player not in clusters:
                continue
            dt = parse_date(row.get("date", ""))
            if dt is None:
                rows_bad_date += 1
                continue
            rows_joined += 1
            period = "post" if dt >= rule_change_date else "pre"
            cluster = clusters[player]
            cluster_stats = cluster_period_stats[(cluster, period)]
            player_stats = player_period_stats[(player, period)]
            add_observation(cluster_stats, player)
            add_observation(player_stats, player)
            for outcome in outcomes:
                value = safe_float(row.get(outcome))
                add_outcome(cluster_stats, player, outcome, value)
                add_outcome(player_stats, player, outcome, value)

    period_rows = []
    for (cluster, period), stats in sorted(cluster_period_stats.items()):
        out = {
            "style_cluster": cluster,
            "period": period,
            "n_observations": int(stats["n"]),
            "n_players": len(stats["players"]),
        }
        for outcome in outcomes:
            count = int(stats["counts"][outcome])
            out[f"{outcome}_n"] = count
            out[f"{outcome}_mean"] = (
                float(stats["sums"][outcome]) / count if count else None
            )
        period_rows.append(out)

    player_rows = []
    players_by_cluster = {player: cluster for player, cluster in clusters.items()}
    for player, cluster in sorted(players_by_cluster.items()):
        pre = player_period_stats.get((player, "pre"))
        post = player_period_stats.get((player, "post"))
        if pre is None and post is None:
            continue
        out = {
            "player": player,
            "style_cluster": cluster,
            "pre_observations": int(pre["n"]) if pre else 0,
            "post_observations": int(post["n"]) if post else 0,
        }
        for outcome in outcomes:
            pre_count = int(pre["counts"][outcome]) if pre else 0
            post_count = int(post["counts"][outcome]) if post else 0
            pre_mean = float(pre["sums"][outcome]) / pre_count if pre_count else None
            post_mean = float(post["sums"][outcome]) / post_count if post_count else None
            out[f"pre_{outcome}_n"] = pre_count
            out[f"post_{outcome}_n"] = post_count
            out[f"pre_{outcome}_mean"] = pre_mean
            out[f"post_{outcome}_mean"] = post_mean
            out[f"delta_{outcome}"] = (
                post_mean - pre_mean
                if pre_mean is not None
                and post_mean is not None
                and pre_count >= min_period_games
                and post_count >= min_period_games
                else None
            )
        player_rows.append(out)

    ranking_rows = []
    for outcome in outcomes:
        for cluster in sorted(set(clusters.values())):
            cluster_player_rows = [row for row in player_rows if row["style_cluster"] == cluster]
            deltas = [
                float(row[f"delta_{outcome}"])
                for row in cluster_player_rows
                if row.get(f"delta_{outcome}") is not None
            ]
            pre_stats = cluster_period_stats.get((cluster, "pre"))
            post_stats = cluster_period_stats.get((cluster, "post"))
            pre_count = int(pre_stats["counts"][outcome]) if pre_stats else 0
            post_count = int(post_stats["counts"][outcome]) if post_stats else 0
            pre_mean = float(pre_stats["sums"][outcome]) / pre_count if pre_count else None
            post_mean = float(post_stats["sums"][outcome]) / post_count if post_count else None
            ranking_rows.append(
                {
                    "outcome": outcome,
                    "style_cluster": cluster,
                    "players_with_prepost": len(deltas),
                    "mean_player_delta": mean(deltas),
                    "pre_mean": pre_mean,
                    "post_mean": post_mean,
                    "observation_weighted_delta": (
                        post_mean - pre_mean if pre_mean is not None and post_mean is not None else None
                    ),
                    "pre_observations": pre_count,
                    "post_observations": post_count,
                }
            )
    ranking_rows.sort(
        key=lambda row: (
            str(row["outcome"]),
            float("-inf") if row["mean_player_delta"] is None else -float(row["mean_player_delta"]),
        )
    )
    summary = {
        "regression_rows_read": rows_read,
        "regression_rows_joined_to_clusters": rows_joined,
        "regression_rows_bad_date": rows_bad_date,
        "player_outcome_rows": len(player_rows),
    }
    return period_rows, player_rows, ranking_rows, summary


def main() -> int:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    outcomes = [item.strip() for item in args.outcomes.split(",") if item.strip()]
    rule_change_date = datetime.strptime(args.rule_change_date, "%Y-%m-%d")

    clusters, cluster_rows_raw = load_clusters(args.clusters_csv)
    size_rows = cluster_size_rows(cluster_rows_raw)
    write_csv(
        args.output_dir / "cluster_sizes.csv",
        size_rows,
        ["style_cluster", "players", "prechange_sequences"],
    )

    embed_summary = embedding_qc(args.player_embeddings_csv, clusters)
    profile_rows, top_rows, feature_summary = style_feature_profiles(
        args.style_features_csv,
        clusters,
        args.min_feature_n,
        args.top_features_per_cluster,
    )
    write_csv(
        args.output_dir / "cluster_feature_profile.csv",
        profile_rows,
        [
            "style_cluster",
            "feature",
            "players_with_feature",
            "cluster_players",
            "mean",
            "global_mean",
            "global_sd",
            "standardized_difference",
        ],
    )
    write_csv(
        args.output_dir / "cluster_top_features.csv",
        top_rows,
        [
            "style_cluster",
            "rank",
            "feature",
            "players_with_feature",
            "cluster_players",
            "mean",
            "global_mean",
            "global_sd",
            "standardized_difference",
        ],
    )

    period_rows, player_rows, ranking_rows, outcome_summary = outcome_checks(
        args.regression_csv,
        clusters,
        rule_change_date,
        outcomes,
        args.min_period_games,
    )
    period_fields = ["style_cluster", "period", "n_observations", "n_players"]
    for outcome in outcomes:
        period_fields.extend([f"{outcome}_n", f"{outcome}_mean"])
    write_csv(args.output_dir / "cluster_prepost_outcomes.csv", period_rows, period_fields)

    player_fields = ["player", "style_cluster", "pre_observations", "post_observations"]
    for outcome in outcomes:
        player_fields.extend(
            [
                f"pre_{outcome}_n",
                f"post_{outcome}_n",
                f"pre_{outcome}_mean",
                f"post_{outcome}_mean",
                f"delta_{outcome}",
            ]
        )
    write_csv(args.output_dir / "player_prepost_outcomes_by_style.csv", player_rows, player_fields)
    write_csv(
        args.output_dir / "style_benefit_ranking.csv",
        ranking_rows,
        [
            "outcome",
            "style_cluster",
            "players_with_prepost",
            "mean_player_delta",
            "pre_mean",
            "post_mean",
            "observation_weighted_delta",
            "pre_observations",
            "post_observations",
        ],
    )

    summary = {
        "clusters_csv": str(args.clusters_csv),
        "player_embeddings_csv": str(args.player_embeddings_csv),
        "style_features_csv": str(args.style_features_csv),
        "regression_csv": str(args.regression_csv),
        "output_dir": str(args.output_dir),
        "rule_change_date": args.rule_change_date,
        "players_in_clusters": len(clusters),
        "clusters": len(set(clusters.values())),
        "outcomes": outcomes,
        **embed_summary,
        **feature_summary,
        **outcome_summary,
    }
    for outcome in outcomes:
        candidates = [
            row
            for row in ranking_rows
            if row["outcome"] == outcome and row["mean_player_delta"] is not None
        ]
        if candidates:
            best = max(candidates, key=lambda row: float(row["mean_player_delta"]))
            summary[f"best_cluster_by_{outcome}_mean_player_delta"] = int(best["style_cluster"])
            summary[f"best_{outcome}_mean_player_delta"] = float(best["mean_player_delta"])

    summary_path = args.output_dir / "style_qc_summary.json"
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
