#!/usr/bin/env python3
"""Create interactive Plotly maps of player style clusters.

Outputs:
- interpretable-feature PCA coordinates;
- neural-embedding PCA coordinates;
- interpretable-feature PCA loadings;
- an HTML report with hoverable player names and optional text labels.

The HTML uses Plotly from a CDN, so the Python plotly package is not required.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
from sklearn.decomposition import PCA


ROOT = Path(__file__).resolve().parents[3]
PROJECT_ROOT = ROOT
DATASET_DIR = ROOT / "outputs" / "whole_dataset_2022_2026"
DEFAULT_CLUSTERS = DATASET_DIR / "contrastive_style" / "neural_style" / "player_style_clusters_k5.csv"
DEFAULT_EMBEDDINGS = DATASET_DIR / "contrastive_style" / "neural_style" / "player_style_embeddings.csv"
DEFAULT_STYLE_FEATURES = DATASET_DIR / "style_features" / "prechange_player_style_features.csv"
DEFAULT_OUTCOMES = DATASET_DIR / "contrastive_style" / "qc" / "player_prepost_outcomes_by_style.csv"
DEFAULT_METADATA = PROJECT_ROOT / "data" / "final_regression_data_tournaments_2022_2026.csv"
DEFAULT_OUTPUT_DIR = DATASET_DIR / "contrastive_style" / "visualizations"

STYLE_FEATURE_COLUMNS = [
    "avg_game_length_ply",
    "long_game_share",
    "draw_rate",
    "decisive_game_rate",
    "prechange_result_mean",
    "capture_rate",
    "check_rate",
    "own_blunder_rate",
    "own_mistake_rate",
    "own_inaccuracy_rate",
    "low_cp_loss_rate",
    "mean_cp_loss",
    "sd_cp_loss",
    "p90_cp_loss",
    "opening_cp_loss",
    "middlegame_cp_loss",
    "endgame_cp_loss",
    "last10_cp_loss",
    "no_blunder_game_rate",
    "first_blunder_move_mean",
    "conversion_rate_from_plus_2",
    "escape_rate_from_minus_2",
    "opponent_next_blunder_rate",
    "opponent_next_mistake_rate",
    "eval_swing_rate",
]

CLUSTER_LABELS = {
    0: "Error-prone lower-score",
    1: "Clean high-score",
    2: "Volatile high-error",
    3: "Average-profile",
    4: "Clean solid",
}

CLUSTER_COLORS = {
    0: "#4E79A7",
    1: "#59A14F",
    2: "#E15759",
    3: "#9C755F",
    4: "#F28E2B",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--clusters-csv", type=Path, default=DEFAULT_CLUSTERS)
    parser.add_argument("--player-embeddings-csv", type=Path, default=DEFAULT_EMBEDDINGS)
    parser.add_argument("--style-features-csv", type=Path, default=DEFAULT_STYLE_FEATURES)
    parser.add_argument("--player-outcomes-csv", type=Path, default=DEFAULT_OUTCOMES)
    parser.add_argument("--metadata-csv", type=Path, default=DEFAULT_METADATA)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--html-name", default="player_style_cluster_map.html")
    parser.add_argument("--min-nonmissing", type=int, default=20)
    return parser.parse_args()


def safe_float(value: str | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        out = float(value)
    except ValueError:
        return None
    return None if math.isnan(out) else out


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


def read_by_player(path: Path) -> dict[str, dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as f:
        return {row["player"]: row for row in csv.DictReader(f) if row.get("player")}


def load_real_names(path: Path) -> tuple[dict[str, str], dict[str, int]]:
    counts: dict[str, Counter[str]] = defaultdict(Counter)
    rows = 0
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows += 1
            player = row.get("player_name", "").strip()
            real_name = row.get("real_name", "").strip()
            if player and real_name:
                counts[player][real_name] += 1
    names = {
        player: counter.most_common(1)[0][0]
        for player, counter in counts.items()
        if counter
    }
    return names, {"metadata_rows_read": rows, "players_with_real_name": len(names)}


def load_clusters(path: Path) -> dict[str, dict[str, object]]:
    out = {}
    with path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.DictReader(f):
            player = row.get("player", "")
            if not player:
                continue
            cluster = int(row["style_cluster"])
            out[player] = {
                "player": player,
                "style_cluster": cluster,
                "style_label": CLUSTER_LABELS.get(cluster, f"Cluster {cluster}"),
                "prechange_sequences": int(float(row.get("prechange_sequences") or 0)),
            }
    return out


def numeric_feature_matrix(
    players: list[str],
    rows_by_player: dict[str, dict[str, str]],
    candidate_fields: list[str],
    min_nonmissing: int,
) -> tuple[np.ndarray, list[str], dict[str, dict[str, float | int]]]:
    fields = []
    columns = []
    feature_summary: dict[str, dict[str, float | int]] = {}

    for field in candidate_fields:
        values = [safe_float(rows_by_player.get(player, {}).get(field)) for player in players]
        nonmissing = [value for value in values if value is not None]
        if len(nonmissing) < min_nonmissing:
            continue
        sd = float(np.std(nonmissing, ddof=1)) if len(nonmissing) > 1 else 0.0
        if sd <= 1e-12:
            continue
        median = float(np.median(nonmissing))
        filled = [median if value is None else float(value) for value in values]
        fields.append(field)
        columns.append(filled)
        feature_summary[field] = {
            "nonmissing": len(nonmissing),
            "mean": float(np.mean(nonmissing)),
            "sd": sd,
            "median_imputed": median,
        }

    matrix = np.asarray(columns, dtype=np.float64).T
    means = matrix.mean(axis=0)
    sds = matrix.std(axis=0, ddof=1)
    matrix = (matrix - means) / np.maximum(sds, 1e-12)
    return matrix, fields, feature_summary


def embedding_matrix(
    players: list[str],
    rows_by_player: dict[str, dict[str, str]],
) -> tuple[np.ndarray, list[str]]:
    if not rows_by_player:
        raise ValueError("No player embedding rows found.")
    first = next(iter(rows_by_player.values()))
    fields = sorted(field for field in first if field.startswith("emb_"))
    matrix = []
    kept_players = []
    for player in players:
        row = rows_by_player.get(player)
        if not row:
            continue
        values = [safe_float(row.get(field)) for field in fields]
        if any(value is None for value in values):
            continue
        matrix.append([float(value) for value in values if value is not None])
        kept_players.append(player)
    return np.asarray(matrix, dtype=np.float64), kept_players


def run_pca(matrix: np.ndarray) -> tuple[np.ndarray, PCA]:
    pca = PCA(n_components=2, random_state=0)
    coords = pca.fit_transform(matrix)
    return coords, pca


def enrich_row(
    player: str,
    base: dict[str, object],
    style_rows: dict[str, dict[str, str]],
    outcome_rows: dict[str, dict[str, str]],
    real_names: dict[str, str],
) -> dict[str, object]:
    style = style_rows.get(player, {})
    outcome = outcome_rows.get(player, {})
    real_name = real_names.get(player)
    row = dict(base)
    row["username"] = player
    row["real_name"] = real_name or ""
    row["display_name"] = real_name or player
    row["has_real_name"] = int(real_name is not None)
    row["name_source"] = "FIDE/metadata real name" if real_name else "Chess.com handle fallback"
    for field in [
        "prechange_games",
        "prechange_result_mean",
        "mean_cp_loss",
        "p90_cp_loss",
        "own_mistake_rate",
        "own_blunder_rate",
        "low_cp_loss_rate",
        "no_blunder_game_rate",
        "capture_rate",
        "check_rate",
        "long_game_share",
    ]:
        row[field] = safe_float(style.get(field))
    for field in ["delta_player_accuracy", "delta_player_result"]:
        row[field] = safe_float(outcome.get(field))
    return row


def pca_rows(
    players: list[str],
    coords: np.ndarray,
    clusters: dict[str, dict[str, object]],
    style_rows: dict[str, dict[str, str]],
    outcome_rows: dict[str, dict[str, str]],
    real_names: dict[str, str],
    prefix: str,
) -> list[dict[str, object]]:
    rows = []
    for player, xy in zip(players, coords):
        rows.append(
            enrich_row(
                player,
                {
                    "player": player,
                    "style_cluster": clusters[player]["style_cluster"],
                    "style_label": clusters[player]["style_label"],
                    "prechange_sequences": clusters[player]["prechange_sequences"],
                    f"{prefix}_pc1": float(xy[0]),
                    f"{prefix}_pc2": float(xy[1]),
                },
                style_rows,
                outcome_rows,
                real_names,
            )
        )
    return rows


def loadings_rows(pca: PCA, fields: list[str]) -> list[dict[str, object]]:
    rows = []
    for feature, pc1, pc2 in zip(fields, pca.components_[0], pca.components_[1]):
        rows.append(
            {
                "feature": feature,
                "pc1_loading": float(pc1),
                "pc2_loading": float(pc2),
                "abs_pc1_loading": abs(float(pc1)),
                "abs_pc2_loading": abs(float(pc2)),
            }
        )
    return sorted(rows, key=lambda row: max(row["abs_pc1_loading"], row["abs_pc2_loading"]), reverse=True)


def cluster_summary(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    by_cluster: dict[int, list[dict[str, object]]] = defaultdict(list)
    for row in rows:
        by_cluster[int(row["style_cluster"])].append(row)
    out = []
    for cluster, group in sorted(by_cluster.items()):
        out.append(
            {
                "style_cluster": cluster,
                "style_label": CLUSTER_LABELS.get(cluster, f"Cluster {cluster}"),
                "players": len(group),
                "mean_accuracy_delta": mean_nonmissing(group, "delta_player_accuracy"),
                "mean_result_delta": mean_nonmissing(group, "delta_player_result"),
                "mean_cp_loss": mean_nonmissing(group, "mean_cp_loss"),
                "mean_prechange_result": mean_nonmissing(group, "prechange_result_mean"),
            }
        )
    return out


def mean_nonmissing(rows: list[dict[str, object]], field: str) -> float | None:
    values = [row.get(field) for row in rows]
    values = [float(value) for value in values if value is not None]
    return float(np.mean(values)) if values else None


def plot_trace_rows(rows: list[dict[str, object]], x_field: str, y_field: str) -> list[dict[str, object]]:
    traces = []
    for cluster in sorted({int(row["style_cluster"]) for row in rows}):
        group = [row for row in rows if int(row["style_cluster"]) == cluster]
        traces.append(
            {
                "type": "scattergl",
                "mode": "markers",
                "name": f"{cluster}: {CLUSTER_LABELS.get(cluster, f'Cluster {cluster}')}",
                "x": [row[x_field] for row in group],
                "y": [row[y_field] for row in group],
                "text": [row["display_name"] for row in group],
                "customdata": [
                    [
                        row["name_source"],
                        row["style_label"],
                        fmt(row.get("prechange_games")),
                        fmt(row.get("prechange_sequences")),
                        fmt(row.get("prechange_result_mean")),
                        fmt(row.get("mean_cp_loss")),
                        fmt(row.get("own_mistake_rate")),
                        fmt(row.get("own_blunder_rate")),
                        fmt(row.get("delta_player_accuracy")),
                        fmt(row.get("delta_player_result")),
                    ]
                    for row in group
                ],
                "marker": {
                    "size": 8,
                    "opacity": 0.78,
                    "color": CLUSTER_COLORS.get(cluster, "#666666"),
                    "line": {"width": 0.5, "color": "rgba(20,20,20,0.35)"},
                },
                "textfont": {"size": 8, "color": "#222"},
                "textposition": "top center",
                "hovertemplate": (
                    "<b>%{text}</b><br>"
                    "name source: %{customdata[0]}<br>"
                    "style: %{customdata[1]}<br>"
                    "pre-change games: %{customdata[2]}<br>"
                    "sequences: %{customdata[3]}<br>"
                    "pre result mean: %{customdata[4]}<br>"
                    "mean CP loss: %{customdata[5]}<br>"
                    "mistake rate: %{customdata[6]}<br>"
                    "blunder rate: %{customdata[7]}<br>"
                    "accuracy delta: %{customdata[8]}<br>"
                    "result delta: %{customdata[9]}<extra></extra>"
                ),
            }
        )
    return traces


def html_report(
    feature_rows: list[dict[str, object]],
    embedding_rows: list[dict[str, object]],
    feature_explained: list[float],
    embedding_explained: list[float],
    cluster_rows: list[dict[str, object]],
) -> str:
    feature_traces = plot_trace_rows(feature_rows, "feature_pc1", "feature_pc2")
    embedding_traces = plot_trace_rows(embedding_rows, "embedding_pc1", "embedding_pc2")
    cluster_table = "\n".join(
        "<tr>"
        f"<td>{row['style_cluster']}</td>"
        f"<td>{row['style_label']}</td>"
        f"<td>{row['players']}</td>"
        f"<td>{fmt(row['mean_accuracy_delta'])}</td>"
        f"<td>{fmt(row['mean_result_delta'])}</td>"
        f"<td>{fmt(row['mean_cp_loss'])}</td>"
        f"<td>{fmt(row['mean_prechange_result'])}</td>"
        "</tr>"
        for row in cluster_rows
    )
    feature_pct = [100 * value for value in feature_explained]
    embedding_pct = [100 * value for value in embedding_explained]
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Titled Tuesday Player Style Cluster Map</title>
  <script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
  <style>
    body {{ margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #202124; background: #f7f7f4; }}
    main {{ max-width: 1280px; margin: 0 auto; padding: 24px; }}
    h1 {{ margin: 0 0 6px; font-size: 24px; letter-spacing: 0; }}
    h2 {{ margin: 28px 0 8px; font-size: 18px; letter-spacing: 0; }}
    p {{ max-width: 960px; line-height: 1.45; }}
    .controls {{ display: flex; gap: 8px; margin: 10px 0 12px; flex-wrap: wrap; }}
    button {{ border: 1px solid #b8b8b0; background: white; border-radius: 6px; padding: 7px 10px; cursor: pointer; }}
    button:hover {{ background: #efefea; }}
    .plot {{ width: 100%; height: 760px; background: white; border: 1px solid #d6d6cf; }}
    table {{ border-collapse: collapse; width: 100%; background: white; margin-top: 12px; }}
    th, td {{ border: 1px solid #d6d6cf; padding: 8px 10px; text-align: right; font-size: 13px; }}
    th:nth-child(2), td:nth-child(2) {{ text-align: left; }}
    th {{ background: #ecece5; }}
    .note {{ color: #5f6368; font-size: 13px; }}
  </style>
</head>
<body>
<main>
  <h1>Titled Tuesday Player Style Cluster Map</h1>
  <p>
    Each point is one eligible player. Colors are the neural style clusters.
    Hover over points for real player names and style metrics. Chess.com
    handles are not shown in the plot unless no real-name match was available
    for that player. Use the buttons to show or hide all player-name labels;
    labels are intentionally hidden by default because there are {len(feature_rows):,}
    players.
  </p>

  <h2>Interpretable Style Feature PCA</h2>
  <p class="note">
    PCA uses standardized pre-change interpretable style variables from
    <code>build_interpretable_style_features.py</code>. PC1 explains
    {feature_pct[0]:.1f}% and PC2 explains {feature_pct[1]:.1f}% of the
    selected feature variance.
  </p>
  <div class="controls">
    <button onclick="setMode('featurePlot', 'markers')">Hide labels</button>
    <button onclick="setMode('featurePlot', 'markers+text')">Show labels</button>
  </div>
  <div id="featurePlot" class="plot"></div>

  <h2>Neural Style Embedding PCA</h2>
  <p class="note">
    PCA of the learned 64-dimensional contrastive style embeddings. This is the
    closest 2D view to the neural clustering similarity space. PC1 explains
    {embedding_pct[0]:.1f}% and PC2 explains {embedding_pct[1]:.1f}% of embedding
    variance.
  </p>
  <div class="controls">
    <button onclick="setMode('embeddingPlot', 'markers')">Hide labels</button>
    <button onclick="setMode('embeddingPlot', 'markers+text')">Show labels</button>
  </div>
  <div id="embeddingPlot" class="plot"></div>

  <h2>Cluster Summary</h2>
  <table>
    <thead>
      <tr>
        <th>Cluster</th><th>Data-driven label</th><th>Players</th>
        <th>Mean accuracy delta</th><th>Mean result delta</th>
        <th>Mean CP loss</th><th>Pre-change result mean</th>
      </tr>
    </thead>
    <tbody>
      {cluster_table}
    </tbody>
  </table>
</main>
<script>
const featureTraces = {json.dumps(feature_traces)};
const embeddingTraces = {json.dumps(embedding_traces)};
const baseLayout = {{
  margin: {{l: 58, r: 24, t: 28, b: 56}},
  paper_bgcolor: 'white',
  plot_bgcolor: 'white',
  legend: {{orientation: 'h', y: -0.16}},
  hovermode: 'closest',
  xaxis: {{zeroline: true, zerolinecolor: '#cccccc', gridcolor: '#eeeeee'}},
  yaxis: {{zeroline: true, zerolinecolor: '#cccccc', gridcolor: '#eeeeee'}},
}};
Plotly.newPlot('featurePlot', featureTraces, {{
  ...baseLayout,
  xaxis: {{...baseLayout.xaxis, title: 'Feature PC1 ({feature_pct[0]:.1f}%)'}},
  yaxis: {{...baseLayout.yaxis, title: 'Feature PC2 ({feature_pct[1]:.1f}%)'}},
}}, {{responsive: true, displaylogo: false}});
Plotly.newPlot('embeddingPlot', embeddingTraces, {{
  ...baseLayout,
  xaxis: {{...baseLayout.xaxis, title: 'Embedding PC1 ({embedding_pct[0]:.1f}%)'}},
  yaxis: {{...baseLayout.yaxis, title: 'Embedding PC2 ({embedding_pct[1]:.1f}%)'}},
}}, {{responsive: true, displaylogo: false}});
function setMode(divId, mode) {{
  const div = document.getElementById(divId);
  Plotly.restyle(div, {{mode: mode}}, div.data.map((_, i) => i));
}}
</script>
</body>
</html>
"""


def main() -> int:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    clusters = load_clusters(args.clusters_csv)
    style_rows = read_by_player(args.style_features_csv)
    outcome_rows = read_by_player(args.player_outcomes_csv)
    embedding_rows_by_player = read_by_player(args.player_embeddings_csv)
    real_names, real_name_summary = load_real_names(args.metadata_csv)
    players = sorted(player for player in clusters if player in style_rows)
    if not players:
        raise ValueError("No players overlap between clusters and style features.")

    feature_matrix, feature_fields, feature_summary = numeric_feature_matrix(
        players,
        style_rows,
        STYLE_FEATURE_COLUMNS,
        args.min_nonmissing,
    )
    feature_coords, feature_pca = run_pca(feature_matrix)
    feature_rows = pca_rows(players, feature_coords, clusters, style_rows, outcome_rows, real_names, "feature")

    emb_matrix, emb_players = embedding_matrix(players, embedding_rows_by_player)
    emb_coords, emb_pca = run_pca(emb_matrix)
    embedding_rows = pca_rows(emb_players, emb_coords, clusters, style_rows, outcome_rows, real_names, "embedding")

    common_fields = [
        "player",
        "display_name",
        "real_name",
        "username",
        "has_real_name",
        "name_source",
        "style_cluster",
        "style_label",
        "prechange_sequences",
        "prechange_games",
        "prechange_result_mean",
        "mean_cp_loss",
        "p90_cp_loss",
        "own_mistake_rate",
        "own_blunder_rate",
        "low_cp_loss_rate",
        "no_blunder_game_rate",
        "capture_rate",
        "check_rate",
        "long_game_share",
        "delta_player_accuracy",
        "delta_player_result",
    ]
    write_csv(
        args.output_dir / "player_style_feature_pca_coordinates.csv",
        feature_rows,
        common_fields + ["feature_pc1", "feature_pc2"],
    )
    write_csv(
        args.output_dir / "player_style_embedding_pca_coordinates.csv",
        embedding_rows,
        common_fields + ["embedding_pc1", "embedding_pc2"],
    )
    write_csv(
        args.output_dir / "style_feature_pca_loadings.csv",
        loadings_rows(feature_pca, feature_fields),
        ["feature", "pc1_loading", "pc2_loading", "abs_pc1_loading", "abs_pc2_loading"],
    )
    write_csv(
        args.output_dir / "cluster_style_map_summary.csv",
        cluster_summary(feature_rows),
        [
            "style_cluster",
            "style_label",
            "players",
            "mean_accuracy_delta",
            "mean_result_delta",
            "mean_cp_loss",
            "mean_prechange_result",
        ],
    )

    html = html_report(
        feature_rows,
        embedding_rows,
        list(feature_pca.explained_variance_ratio_),
        list(emb_pca.explained_variance_ratio_),
        cluster_summary(feature_rows),
    )
    html_path = args.output_dir / args.html_name
    html_path.write_text(html, encoding="utf-8")

    players_without_real_names = [
        str(row["username"])
        for row in feature_rows
        if not int(row["has_real_name"])
    ]
    summary = {
        "players_plotted": len(feature_rows),
        "embedding_players_plotted": len(embedding_rows),
        "metadata_csv": str(args.metadata_csv),
        "players_plotted_with_real_names": sum(int(row["has_real_name"]) for row in feature_rows),
        "players_plotted_without_real_names": len(players_without_real_names),
        "players_without_real_names": players_without_real_names,
        **real_name_summary,
        "feature_pca_fields": feature_fields,
        "feature_pca_explained_variance_ratio": list(map(float, feature_pca.explained_variance_ratio_)),
        "embedding_pca_explained_variance_ratio": list(map(float, emb_pca.explained_variance_ratio_)),
        "feature_summary": feature_summary,
        "html": str(html_path),
        "feature_coordinates_csv": str(args.output_dir / "player_style_feature_pca_coordinates.csv"),
        "embedding_coordinates_csv": str(args.output_dir / "player_style_embedding_pca_coordinates.csv"),
        "feature_loadings_csv": str(args.output_dir / "style_feature_pca_loadings.csv"),
    }
    (args.output_dir / "style_cluster_map_summary.json").write_text(
        json.dumps(summary, indent=2),
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
