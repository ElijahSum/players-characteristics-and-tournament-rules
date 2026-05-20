#!/usr/bin/env python3
"""Create the static interpretable-style PCA figure for the thesis."""

from __future__ import annotations

import csv
import math
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D


ROOT = Path(__file__).resolve().parents[3]
OVERLEAF = ROOT / "paper"
VIS = ROOT / "outputs" / "whole_dataset_2022_2026" / "contrastive_style" / "visualizations"
COORDS = VIS / "player_style_feature_pca_coordinates.csv"
LOADINGS = VIS / "style_feature_pca_loadings.csv"
OUT = OVERLEAF / "figures" / "figure11_interpretable_style_feature_pca.png"

PALETTE = {
    "Clean solid": "#2e8b57",
    "Average-profile": "#2d6cdf",
    "Elite clean": "#1f3a5f",
    "Volatile high-error": "#b84a62",
    "Lower-accuracy volatile": "#d77a33",
}

FALLBACK = ["#7460a8", "#31a7c8", "#6b7280", "#9a6b3f", "#7c3aed"]


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(value: str) -> float:
    value = value.replace(",", "").strip()
    if value == "" or value.lower() == "nan":
        return math.nan
    return float(value)


def pretty_feature_name(name: str) -> str:
    labels = {
        "capture_rate": "capture rate",
        "avg_game_length_ply": "game length",
        "long_game_share": "long games",
        "mean_cp_loss": "mean CPL",
        "own_mistake_rate": "mistakes",
        "p90_cp_loss": "tail CPL",
        "middlegame_cp_loss": "middlegame CPL",
        "own_inaccuracy_rate": "inaccuracies",
        "low_cp_loss_rate": "low-CPL moves",
        "own_blunder_rate": "blunders",
        "eval_swing_rate": "eval swings",
        "check_rate": "checks",
        "sd_cp_loss": "CPL volatility",
        "no_blunder_game_rate": "no-blunder games",
        "last10_cp_loss": "last-10 CPL",
        "endgame_cp_loss": "endgame CPL",
        "prechange_result_mean": "pre result",
        "first_blunder_move_mean": "first blunder later",
        "opponent_next_mistake_rate": "induced mistakes",
    }
    return labels.get(name, name.replace("_", " "))


def percentile(values: list[float], q: float) -> float:
    clean = sorted(v for v in values if not math.isnan(v))
    if not clean:
        return math.nan
    pos = (len(clean) - 1) * q
    lo = math.floor(pos)
    hi = math.ceil(pos)
    if lo == hi:
        return clean[int(pos)]
    return clean[lo] * (hi - pos) + clean[hi] * (pos - lo)


def main() -> None:
    rows = read_csv(COORDS)
    points = []
    for row in rows:
        x = fnum(row["feature_pc1"])
        y = fnum(row["feature_pc2"])
        if math.isnan(x) or math.isnan(y):
            continue
        label = row.get("style_label", "Unclassified") or "Unclassified"
        points.append(
            {
                "x": x,
                "y": y,
                "label": label,
                "player": row.get("display_name") or row.get("player") or "",
                "pre_games": fnum(row.get("prechange_games", "")),
                "mean_cp_loss": fnum(row.get("mean_cp_loss", "")),
            }
        )

    labels = sorted({p["label"] for p in points})
    color_map = {label: PALETTE.get(label, FALLBACK[i % len(FALLBACK)]) for i, label in enumerate(labels)}

    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 8.8,
            "axes.titlesize": 11,
            "axes.labelsize": 9,
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.edgecolor": "#d5dde5",
            "xtick.color": "#6b7280",
            "ytick.color": "#6b7280",
        }
    )

    fig, (ax, ax_load) = plt.subplots(
        1,
        2,
        figsize=(13.2, 7.2),
        gridspec_kw={"width_ratios": [1.75, 1.0], "wspace": 0.18},
    )
    fig.patch.set_facecolor("white")
    ax.set_facecolor("#fbfcfd")

    for label in labels:
        subset = [p for p in points if p["label"] == label]
        ax.scatter(
            [p["x"] for p in subset],
            [p["y"] for p in subset],
            s=28,
            alpha=0.62,
            color=color_map[label],
            edgecolor="white",
            linewidth=0.35,
            label=f"{label} (n={len(subset)})",
        )

    grouped = defaultdict(list)
    for p in points:
        grouped[p["label"]].append(p)
    for label, subset in grouped.items():
        cx = sum(p["x"] for p in subset) / len(subset)
        cy = sum(p["y"] for p in subset) / len(subset)
        ax.scatter(cx, cy, s=155, color=color_map[label], edgecolor="white", linewidth=1.5, zorder=5)
        ax.text(
            cx,
            cy,
            f" {label}",
            va="center",
            ha="left",
            fontsize=8.4,
            fontweight="bold",
            color="#1f2937",
            zorder=6,
        )

    ax.axhline(0, color="#d5dde5", linewidth=1)
    ax.axvline(0, color="#d5dde5", linewidth=1)
    ax.grid(color="#e7edf3", linewidth=0.8, alpha=0.85)

    xs = [p["x"] for p in points]
    ys = [p["y"] for p in points]
    xmin, xmax = percentile(xs, 0.01), percentile(xs, 0.99)
    ymin, ymax = percentile(ys, 0.01), percentile(ys, 0.99)
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)

    loadings = read_csv(LOADINGS)
    loadings = sorted(
        loadings,
        key=lambda r: max(abs(fnum(r["pc1_loading"])), abs(fnum(r["pc2_loading"]))),
        reverse=True,
    )[:9]
    features = [pretty_feature_name(row["feature"]) for row in loadings]
    pc1 = [fnum(row["pc1_loading"]) for row in loadings]
    pc2 = [fnum(row["pc2_loading"]) for row in loadings]
    ypos = list(range(len(features)))
    h = 0.34
    ax_load.axvline(0, color="#d5dde5", linewidth=1)
    ax_load.barh([y - h / 2 for y in ypos], pc1, height=h, color="#1f3a5f", alpha=0.88, label="PC1")
    ax_load.barh([y + h / 2 for y in ypos], pc2, height=h, color="#d77a33", alpha=0.88, label="PC2")
    ax_load.set_yticks(ypos, features)
    ax_load.invert_yaxis()
    ax_load.set_xlim(-0.56, 0.56)
    ax_load.grid(axis="x", color="#e7edf3", linewidth=0.8)
    ax_load.set_title("Largest Feature Loadings", loc="left", fontweight="bold", color="#1f3a5f")
    ax_load.set_xlabel("PCA loading")
    ax_load.legend(frameon=False, loc="lower right")

    ax.set_title("Player Map", loc="left", fontweight="bold", color="#1f3a5f")
    fig.text(
        0.08,
        0.94,
        "Interpretable Feature PCA with Neural Style-Embedding Clusters",
        fontsize=14,
        fontweight="bold",
        color="#1f3a5f",
    )
    fig.text(
        0.08,
        0.905,
        "Points are players. Axes use PCA of standardized pre-change interpretable features; colors are k-means clusters learned from contrastive neural embeddings.",
        fontsize=8.7,
        color="#6b7280",
    )
    ax.set_xlabel("Feature PC1: error, volatility, and overall move-quality gradient")
    ax.set_ylabel("Feature PC2: tactical activity versus longer, quieter games")
    ax.legend(
        handles=[
            Line2D([0], [0], marker="o", color="w", markerfacecolor=color_map[label], markeredgecolor="white", markersize=8, label=f"{label} (n={sum(1 for p in points if p['label'] == label)})")
            for label in labels
        ],
        frameon=True,
        facecolor="white",
        edgecolor="#d5dde5",
        loc="upper right",
        fontsize=7.4,
    )

    fig.subplots_adjust(left=0.06, right=0.985, bottom=0.13, top=0.78, wspace=0.24)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(OUT)


if __name__ == "__main__":
    main()
