#!/usr/bin/env python3
"""Create polished thesis figures from existing analysis outputs.

The script intentionally uses only the Python standard library plus matplotlib.
It reads the already materialized CSV summaries from analysis_outputs and writes
static PNG figures into the Overleaf project.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib import gridspec
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch, Rectangle


ROOT = Path(__file__).resolve().parents[3]
OVERLEAF = ROOT / "paper"
OUT = OVERLEAF / "figures"
ASSETS = ROOT / "analysis_outputs" / "master_thesis_results_assets"


COLORS = {
    "navy": "#1f3a5f",
    "blue": "#2d6cdf",
    "cyan": "#31a7c8",
    "green": "#2e8b57",
    "teal": "#1b998b",
    "orange": "#d77a33",
    "red": "#b84a62",
    "purple": "#7460a8",
    "gray": "#6b7280",
    "light": "#f3f6f8",
    "line": "#d5dde5",
    "text": "#1f2937",
}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(value: str) -> float:
    value = value.replace(",", "").strip()
    if value == "":
        return math.nan
    return float(value)


def setup_style() -> None:
    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 10,
            "axes.titlesize": 12,
            "axes.labelsize": 10,
            "figure.titlesize": 15,
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.edgecolor": COLORS["line"],
            "axes.linewidth": 0.8,
            "xtick.color": COLORS["gray"],
            "ytick.color": COLORS["gray"],
            "text.color": COLORS["text"],
        }
    )


def add_panel_label(ax, label: str) -> None:
    ax.text(
        -0.08,
        1.08,
        label,
        transform=ax.transAxes,
        fontsize=12,
        fontweight="bold",
        color=COLORS["navy"],
        va="top",
    )


def draw_node(ax, xy, w, h, title, subtitle, color, text_color="white"):
    x, y = xy
    box = FancyBboxPatch(
        (x, y),
        w,
        h,
        boxstyle="round,pad=0.02,rounding_size=0.025",
        facecolor=color,
        edgecolor="white",
        linewidth=1.4,
    )
    ax.add_patch(box)
    ax.text(
        x + w / 2,
        y + h * 0.62,
        title,
        ha="center",
        va="center",
        fontsize=10,
        color=text_color,
        fontweight="bold",
        wrap=True,
    )
    ax.text(
        x + w / 2,
        y + h * 0.28,
        subtitle,
        ha="center",
        va="center",
        fontsize=8.2,
        color=text_color,
        wrap=True,
    )


def arrow(ax, start, end, color=COLORS["gray"]):
    ax.add_patch(
        FancyArrowPatch(
            start,
            end,
            arrowstyle="-|>",
            mutation_scale=13,
            linewidth=1.2,
            color=color,
            alpha=0.8,
            connectionstyle="arc3,rad=0.0",
        )
    )


def make_data_design_map() -> None:
    fig, ax = plt.subplots(figsize=(13.5, 8.0))
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis("off")

    fig.suptitle(
        "Data Architecture and Empirical Layers",
        x=0.055,
        ha="left",
        fontweight="bold",
        color=COLORS["navy"],
    )
    ax.text(
        0.055,
        0.925,
        "The thesis links platform metadata, external player data, PGNs, and engine evaluations into four empirical designs.",
        fontsize=10.5,
        color=COLORS["gray"],
    )

    columns = [
        ("Raw sources", 0.06),
        ("Constructed data", 0.32),
        ("Empirical design", 0.58),
        ("Interpretation", 0.81),
    ]
    for title, x in columns:
        ax.text(x, 0.86, title, ha="left", fontsize=11, fontweight="bold", color=COLORS["navy"])

    source_nodes = [
        ((0.06, 0.69), "Chess.com tournaments", "pairings, ratings,\nrounds, results", COLORS["blue"]),
        ((0.06, 0.50), "Chess.com PGNs", "move sequences,\nECO metadata", COLORS["cyan"]),
        ((0.06, 0.31), "FIDE profiles", "real names, age,\ngender, federation", COLORS["purple"]),
        ((0.06, 0.12), "World Bank WDI", "GDP per capita PPP\nby federation", COLORS["orange"]),
    ]
    constructed = [
        ((0.32, 0.69), "Player-game panel", "results, accuracy,\nratings, round state", COLORS["navy"]),
        ((0.32, 0.50), "Stockfish move file", "CPL, blunders,\nconversion states", COLORS["teal"]),
        ((0.32, 0.31), "Demographic links", "age, gender,\nFIDE ratings", COLORS["purple"]),
        ((0.32, 0.12), "Country covariates", "GDP and local-time\nexposure variables", COLORS["orange"]),
    ]
    designs = [
        ((0.58, 0.69), "Game-level FE", "player + event FE,\npaired-game checks", COLORS["blue"]),
        ((0.58, 0.50), "Move-level FE", "player + tournament +\nmove-number FE", COLORS["teal"]),
        ((0.58, 0.31), "Participation models", "slot exposure,\ncompletion, selection", COLORS["orange"]),
        ((0.58, 0.12), "Style models", "pre-change indexes,\ncontrastive embeddings", COLORS["purple"]),
    ]
    results = [
        ((0.81, 0.69), "Skill premium", "rating and accuracy\nmatter more", COLORS["green"]),
        ((0.81, 0.50), "Conversion channel", "+2 conversion,\n-2 recovery", COLORS["green"]),
        ((0.81, 0.31), "Schedule selection", "late-slot players\nparticipate less", COLORS["orange"]),
        ((0.81, 0.12), "Adaptability", "age, online capital,\nstyle heterogeneity", COLORS["red"]),
    ]

    for nodes in (source_nodes, constructed, designs, results):
        for xy, title, subtitle, color in nodes:
            draw_node(ax, xy, 0.16, 0.115, title, subtitle, color)

    for y in [0.747, 0.557, 0.367, 0.177]:
        arrow(ax, (0.22, y), (0.32, y), COLORS["gray"])
        arrow(ax, (0.48, y), (0.58, y), COLORS["gray"])
        arrow(ax, (0.74, y), (0.81, y), COLORS["gray"])

    arrow(ax, (0.48, 0.557), (0.58, 0.747), COLORS["gray"])
    arrow(ax, (0.48, 0.367), (0.58, 0.747), COLORS["gray"])
    arrow(ax, (0.48, 0.367), (0.58, 0.177), COLORS["gray"])

    ax.text(
        0.06,
        0.035,
        "Note: the September 2025 reform is bundled: no increment, larger base time, and removal of the old late slot.",
        fontsize=9,
        color=COLORS["gray"],
    )
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    fig.savefig(OUT / "figure7_data_design_map.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def make_rating_robustness() -> None:
    rows = read_csv(ASSETS / "tables" / "table2_rating_advantage_robustness.csv")
    specs = []
    for row in rows:
        est = fnum(row["Estimate"]) * 100
        se = fnum(row["SE"]) * 100
        specs.append((row["Spec"], est, est - 1.96 * se, est + 1.96 * se, "Placebo" in row["Spec"]))

    fig, ax = plt.subplots(figsize=(10.5, 5.8))
    y = list(range(len(specs)))
    labels = [s[0] for s in specs]
    for yi, (_, est, lo, hi, is_placebo) in zip(y, specs):
        color = COLORS["gray"] if is_placebo else COLORS["green"]
        alpha = 0.65 if is_placebo else 1
        ax.plot([lo, hi], [yi, yi], color=color, linewidth=3, alpha=alpha)
        ax.scatter(est, yi, s=78, color=color, edgecolor="white", linewidth=1.2, zorder=3, alpha=alpha)
        ax.text(hi + 0.08, yi, f"{est:.2f} pp", va="center", fontsize=9, color=COLORS["text"])

    placebo = [s for s in specs if s[4]][0]
    ax.axvspan(placebo[2], placebo[3], color=COLORS["gray"], alpha=0.12, label="Placebo-grid mean 95% CI")
    ax.axvline(0, color=COLORS["line"], linewidth=1)
    ax.set_yticks(y, labels)
    ax.invert_yaxis()
    ax.set_xlabel("Post-change effect per +100 rating advantage, percentage points")
    fig.suptitle(
        "Rating Advantage Effect: Stable Across Designs and Far Above Placebo",
        x=0.055,
        y=0.98,
        ha="left",
        fontweight="bold",
        color=COLORS["navy"],
    )
    fig.text(
        0.055,
        0.92,
        "All rows estimate Post x rating_diff100 on player result. Horizontal lines show 95% confidence intervals.",
        fontsize=9.5,
        color=COLORS["gray"],
    )
    ax.grid(axis="x", color=COLORS["line"], linewidth=0.8, alpha=0.8)
    fig.tight_layout(rect=[0, 0, 1, 0.90])
    fig.savefig(OUT / "figure8_rating_robustness_advanced.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def make_move_dashboard() -> None:
    phase = read_csv(ROOT / "analysis_outputs" / "stockfish_move_mechanisms_full_2022_2026" / "phase_descriptives.csv")
    hazard = read_csv(ROOT / "analysis_outputs" / "missing_move_mechanisms_full_2022_2026" / "hazard_descriptives.csv")
    conv = read_csv(ASSETS / "figure_data" / "conversion_escape_by_rating_bin.csv")
    deep = read_csv(ROOT / "analysis_outputs" / "deeper_research_ideas_full_2022_2026" / "headline_deeper_research_results.csv")

    phases = ["opening_1_10", "early_middlegame_11_20", "late_middlegame_21_35", "endgame_36_plus"]
    phase_labels = ["Opening\n1-10", "Early mid.\n11-20", "Late mid.\n21-35", "Endgame\n36+"]
    periods = {"0": "3+1", "1": "5+0"}
    period_colors = {"3+1": COLORS["blue"], "5+0": COLORS["orange"]}

    fig = plt.figure(figsize=(14, 9))
    gs = gridspec.GridSpec(2, 2, figure=fig, hspace=0.36, wspace=0.24)

    ax1 = fig.add_subplot(gs[0, 0])
    for fmt, period in periods.items():
        vals = []
        for ph in phases:
            r = next(x for x in phase if x["format_5_0"] == fmt and x["phase_group"] == ph)
            vals.append(fnum(r["mean_cp_loss_cap"]))
        ax1.plot(range(len(phases)), vals, marker="o", linewidth=2.8, color=period_colors[period], label=period)
    ax1.set_xticks(range(len(phases)), phase_labels)
    ax1.set_ylabel("Mean capped centipawn loss")
    ax1.set_title("Error Profile by Game Phase", loc="left", fontweight="bold", color=COLORS["navy"])
    ax1.grid(axis="y", color=COLORS["line"], alpha=0.8)
    ax1.legend(frameon=False, loc="upper left")
    add_panel_label(ax1, "A")

    ax2 = fig.add_subplot(gs[0, 1])
    for fmt, period in periods.items():
        vals = []
        for ph in phases:
            r = next(x for x in hazard if x["format_5_0"] == fmt and x["phase_group"] == ph)
            vals.append(fnum(r["event_rate"]) * 100)
        ax2.plot(range(len(phases)), vals, marker="o", linewidth=2.8, color=period_colors[period], label=period)
    ax2.set_xticks(range(len(phases)), phase_labels)
    ax2.set_ylabel("First-blunder event rate, percent")
    ax2.set_title("First Major Mistake Hazard", loc="left", fontweight="bold", color=COLORS["navy"])
    ax2.grid(axis="y", color=COLORS["line"], alpha=0.8)
    ax2.legend(frameon=False, loc="upper left")
    add_panel_label(ax2, "B")

    ax3 = fig.add_subplot(gs[1, 0])
    bins = ["Underdog >300", "Underdog 100-300", "Close +/-100", "Favorite 100-300", "Favorite >300"]
    x = list(range(len(bins)))
    for outcome, linestyle in [("conversion_rate", "-"), ("escape_rate", "--")]:
        for fmt, period in periods.items():
            vals = []
            for b in bins:
                r = next(row for row in conv if row["format_5_0"] == fmt and row["rating_bin"] == b)
                vals.append(fnum(r[outcome]) * 100)
            label = f"{period} {'conversion from +2' if outcome == 'conversion_rate' else 'escape from -2'}"
            ax3.plot(x, vals, marker="o", linewidth=2.3, linestyle=linestyle, color=period_colors[period], label=label)
    ax3.set_xticks(x, ["Underdog\n>300", "Underdog\n100-300", "Close\n+/-100", "Favorite\n100-300", "Favorite\n>300"])
    ax3.set_ylabel("Probability, percent")
    ax3.set_title("Advantage Management by Rating Bin", loc="left", fontweight="bold", color=COLORS["navy"])
    ax3.grid(axis="y", color=COLORS["line"], alpha=0.8)
    ax3.legend(frameon=False, fontsize=8, loc="upper left")
    add_panel_label(ax3, "C")

    ax4 = fig.add_subplot(gs[1, 1])
    wanted = [
        ("conversion_deep", "defensive_recovery_cp_after_minus2", "Recovery after -2\n(+100 rating)", "cp"),
        ("conversion_deep", "defensive_recovery_after_minus2", "Escape after -2\n(+100 rating)", "pp"),
        ("conversion_deep", "conversion_speed_after_plus2", "Speed after +2\n(+100 rating)", "plies"),
        ("conversion_deep", "dropped_below_equal_after_plus2", "Drop below equality\n(+100 rating)", "pp"),
        ("stress_test_traits", "pre_traits_cp_loss_combined", "Pre-change defensive skill\n(1 SD)", "cpl"),
        ("stress_test_traits", "pre_traits_blunder_rate_combined", "Pre-change defensive skill\n(1 SD)", "pp"),
    ]
    bars = []
    for fam, model, label, unit in wanted:
        r = next(row for row in deep if row["family"] == fam and row["model"] == model)
        est = fnum(r["estimate"])
        if unit == "pp":
            est *= 100
            txt = f"{est:+.2f} pp"
        elif unit == "plies":
            txt = f"{est:+.2f} plies"
        elif unit == "cp":
            txt = f"{est:+.1f} cp"
        else:
            txt = f"{est:+.2f}"
        bars.append((label, est, txt))
    y = range(len(bars))
    vals = [b[1] for b in bars]
    colors = [COLORS["green"] if v > 0 else COLORS["red"] for v in vals]
    ax4.barh(y, vals, color=colors, alpha=0.88)
    ax4.axvline(0, color=COLORS["line"], linewidth=1)
    ax4.set_yticks(y, [b[0] for b in bars])
    ax4.invert_yaxis()
    ax4.set_title("Deep Conversion and Predetermined Traits", loc="left", fontweight="bold", color=COLORS["navy"])
    ax4.set_xlabel("Natural units; labels report original scale")
    ax4.grid(axis="x", color=COLORS["line"], alpha=0.65)
    for yi, (_, val, txt) in zip(y, bars):
        ax4.text(val + (0.18 if val >= 0 else -0.18), yi, txt, va="center", ha="left" if val >= 0 else "right", fontsize=8.8)
    add_panel_label(ax4, "D")

    fig.suptitle("Stockfish Mechanism Dashboard: Phase, Blunder Timing, and Conversion", x=0.02, ha="left", fontweight="bold", color=COLORS["navy"])
    fig.text(
        0.02,
        0.945,
        "Move-level evidence links the 5+0 skill premium to later phases, first major mistakes, and advantage management.",
        fontsize=10,
        color=COLORS["gray"],
    )
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    fig.savefig(OUT / "figure9_stockfish_mechanism_dashboard.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def make_evidence_matrix() -> None:
    rows = [
        ("Skill premium", "Rating advantage", "Robust", "+1.2 pp per +100 rating"),
        ("Skill premium", "Accuracy-to-points", "Robust", "+1.4 pp per +10 accuracy"),
        ("Skill premium", "Online capital", "Suggestive", "positive; pretrend caveat"),
        ("Adaptability", "Age/youth", "Robust", "younger players gain; placebo caveat"),
        ("Adaptability", "Late-slot displacement", "Robust", "participation/completion margins"),
        ("Move quality", "Late/final-move CPL", "Robust", "+2 to +3 capped cp"),
        ("Move quality", "Critical equal positions", "Robust", "higher CPL and blunder risk"),
        ("Conversion technology", "+2 conversion/-2 recovery", "Robust", "favorites manage advantages"),
        ("Conversion technology", "Deep conversion", "Robust", "faster +2 conversion; fewer drops"),
        ("Clock mechanism", "Ordinary low-clock exposure", "Robust", "lower <=10s exposure"),
        ("Clock mechanism", "Severe low-clock cost", "Robust", "+19.8 cp at <=10s"),
        ("Clock mechanism", "Increment insurance", "Robust", "recovery above 10s falls 16.5 pp"),
        ("Clock mechanism", "Clock and conversion", "Robust", "opponent low clock becomes valuable"),
        ("Predetermined traits", "Defensive skill", "Robust", "lower post-change errors"),
        ("Predetermined traits", "Complexity tolerance", "Robust", "lower post-change error rates"),
        ("Style", "Chaos creator index", "Robust", "negative post interaction"),
        ("Style", "Neural style clusters", "Data-science", "all clusters gain raw accuracy"),
        ("Rejected mechanisms", "GDP/openings/White", "Null", "not central channels"),
        ("Rejected mechanisms", "Simplification/cascades", "Null", "not robust"),
        ("Rejected mechanisms", "Broad gender mechanisms", "Null", "not robust"),
    ]
    status_color = {
        "Robust": COLORS["green"],
        "Suggestive": COLORS["orange"],
        "Data-science": COLORS["purple"],
        "Null": COLORS["gray"],
    }
    theme_color = {
        "Skill premium": "#e7f0ff",
        "Adaptability": "#e9f8f4",
        "Move quality": "#fff4e6",
        "Conversion technology": "#fef3c7",
        "Clock mechanism": "#e6f4f1",
        "Predetermined traits": "#eef2ff",
        "Style": "#f0ecff",
        "Rejected mechanisms": "#f4f5f7",
    }

    fig, ax = plt.subplots(figsize=(13.8, 10.2))
    ax.set_xlim(0, 1)
    ax.set_ylim(0, len(rows) + 1.8)
    ax.axis("off")
    fig.suptitle("Updated Evidence Map: Main Results After Move, Clock, and Style Analyses", x=0.04, ha="left", fontweight="bold", color=COLORS["navy"])
    ax.text(0.04, len(rows) + 1.15, "The map separates robust mechanisms from suggestive, data-science, and null findings.", color=COLORS["gray"], fontsize=10)

    headers = [("Theme", 0.04), ("Mechanism", 0.25), ("Status", 0.58), ("Main reading", 0.73)]
    for h, x in headers:
        ax.text(x, len(rows) + 0.55, h, fontweight="bold", color=COLORS["navy"], fontsize=10.5)
    ax.plot([0.04, 0.96], [len(rows) + 0.25, len(rows) + 0.25], color=COLORS["line"], linewidth=1.2)

    for idx, (theme, mechanism, status, note) in enumerate(rows):
        y = len(rows) - idx - 0.2
        ax.add_patch(Rectangle((0.035, y - 0.35), 0.93, 0.62, facecolor=theme_color[theme], edgecolor="white", linewidth=1.0))
        ax.text(0.04, y, theme, va="center", fontsize=9.2, color=COLORS["text"])
        ax.text(0.25, y, mechanism, va="center", fontsize=9.5, color=COLORS["text"])
        pill = FancyBboxPatch((0.58, y - 0.18), 0.12, 0.36, boxstyle="round,pad=0.02,rounding_size=0.08", facecolor=status_color[status], edgecolor="none")
        ax.add_patch(pill)
        ax.text(0.64, y, status, ha="center", va="center", fontsize=8.1, color="white", fontweight="bold")
        ax.text(0.73, y, note, va="center", fontsize=9.2, color=COLORS["text"])

    fig.tight_layout(rect=[0, 0, 1, 0.95])
    fig.savefig(OUT / "figure10_clean_outline_evidence_map.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    setup_style()
    make_data_design_map()
    make_rating_robustness()
    make_move_dashboard()
    make_evidence_matrix()


if __name__ == "__main__":
    main()
