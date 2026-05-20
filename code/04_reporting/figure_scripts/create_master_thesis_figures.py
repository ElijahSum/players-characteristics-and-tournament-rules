#!/usr/bin/env python3

import csv
import math
import os
from collections import defaultdict

os.environ.setdefault("MPLCONFIGDIR", "/private/tmp/chess_master_thesis_mpl")
os.makedirs(os.environ["MPLCONFIGDIR"], exist_ok=True)

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

import plotly.graph_objects as go
from plotly.offline import plot as plotly_plot


OUT_ROOT = "analysis_outputs/master_thesis_results_assets"
FIG_DIR = os.path.join(OUT_ROOT, "figures")
FIGDATA_DIR = os.path.join(OUT_ROOT, "figure_data")
os.makedirs(FIG_DIR, exist_ok=True)


def read_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(value, default=float("nan")):
    try:
        if value is None or value == "":
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def label_period(value):
    return "5+0" if str(value) in {"1", "1.0", "5+0"} else "3+1"


def setup_style():
    try:
        plt.style.use("seaborn-v0_8-whitegrid")
    except Exception:
        plt.style.use("default")
    plt.rcParams.update(
        {
            "figure.dpi": 160,
            "savefig.dpi": 220,
            "font.size": 10,
            "axes.titlesize": 12,
            "axes.labelsize": 10,
            "legend.fontsize": 9,
            "axes.spines.top": False,
            "axes.spines.right": False,
        }
    )


def savefig(name):
    path = os.path.join(FIG_DIR, name)
    plt.tight_layout()
    plt.savefig(path, bbox_inches="tight")
    plt.close()
    print(path)


def figure1_rating_diff_event_study():
    rows = read_csv("analysis_outputs/rule_change_mechanism_validation/event_study_mechanism_coefficients.csv")
    rows = [
        r
        for r in rows
        if r.get("variable") == "rating_diff100" and r.get("outcome") == "player_result"
    ]
    rows.sort(key=lambda r: fnum(r["event_month"]))
    clock_desc = read_csv("analysis_outputs/clock_time_mechanisms_2022_2026/clock_descriptives_by_format.csv")
    clock_coefs = read_csv("analysis_outputs/exact_clock_mechanisms_2022_2026/exact_requested_headline_coefficients.csv")

    x = [fnum(r["event_month"]) for r in rows]
    y = [100 * fnum(r["estimate"]) for r in rows]
    lo = [100 * fnum(r["conf.low"]) for r in rows]
    hi = [100 * fnum(r["conf.high"]) for r in rows]
    yerr = [[yy - ll for yy, ll in zip(y, lo)], [hh - yy for yy, hh in zip(y, hi)]]

    desc_by_format = {r["format_5_0"]: r for r in clock_desc}
    pre = desc_by_format["0"]
    post = desc_by_format["1"]

    def coef(model_id, term):
        for row in clock_coefs:
            if row.get("model_id") == model_id and row.get("term") == term:
                return row
        raise KeyError((model_id, term))

    clock_bins = [
        ("60-120s", "clock_bin_f::60_120:format_5_0"),
        ("30-60s", "clock_bin_f::30_60:format_5_0"),
        ("10-30s", "clock_bin_f::10_30:format_5_0"),
        ("5-10s", "clock_bin_f::05_10:format_5_0"),
        ("0-5s", "clock_bin_f::00_05:format_5_0"),
    ]
    bin_est = [fnum(coef("E03", term)["estimate"]) for _, term in clock_bins]
    bin_se = [fnum(coef("E03", term)["std_error"]) for _, term in clock_bins]
    bin_err = [1.96 * se for se in bin_se]

    fig = plt.figure(figsize=(13.2, 4.8), constrained_layout=True)
    gs = fig.add_gridspec(1, 3, width_ratios=[1.45, 1.05, 1.15], wspace=0.35)

    ax = fig.add_subplot(gs[0, 0])
    ax.axhline(0, color="#555555", lw=0.8)
    ax.axvline(0, color="#b23b3b", lw=1.0, linestyle="--", label="Rule change")
    ax.errorbar(x, y, yerr=yerr, fmt="o-", color="#235789", ecolor="#8aa7c7", capsize=2, lw=1.6, ms=4)
    ax.set_title("A. Return to 100 rating points")
    ax.set_xlabel("Months relative to September 2025")
    ax.set_ylabel("Score-share effect, pp")
    ax.legend(frameon=False, loc="upper left")

    ax = fig.add_subplot(gs[0, 1])
    metrics = [
        ("Moves\n<=10s", "low_before_10_share"),
        ("Last 10\n<=10s", "last10_low_before_10_share"),
        ("Final clock\nreserve", "final_clock_fraction"),
    ]
    xpos = list(range(len(metrics)))
    width = 0.36
    pre_vals = [100 * fnum(pre[key]) for _, key in metrics]
    post_vals = [100 * fnum(post[key]) for _, key in metrics]
    ax.bar([i - width / 2 for i in xpos], pre_vals, width=width, color="#6c757d", label="3+1")
    ax.bar([i + width / 2 for i in xpos], post_vals, width=width, color="#235789", label="5+0")
    ax.set_title("B. Clock states")
    ax.set_ylabel("Percent")
    ax.set_xticks(xpos)
    ax.set_xticklabels([label for label, _ in metrics])
    ax.legend(frameon=False)
    ax.set_ylim(0, max(pre_vals + post_vals) * 1.25)

    ax = fig.add_subplot(gs[0, 2])
    xpos = list(range(len(clock_bins)))
    colors = ["#8aa7c7", "#8aa7c7", "#8aa7c7", "#d28f3d", "#b23b3b"]
    ax.bar(xpos, bin_est, color=colors)
    ax.errorbar(xpos, bin_est, yerr=bin_err, fmt="none", ecolor="#222222", capsize=2, lw=0.8)
    ax.axhline(0, color="#555555", lw=0.8)
    ax.set_title("C. Extra CPL under 5+0")
    ax.set_ylabel("Post interaction, cp")
    ax.set_xticks(xpos)
    ax.set_xticklabels([label for label, _ in clock_bins], rotation=25, ha="right")
    ax.text(
        0.04,
        0.94,
        "Reference: >120s,\nwith move-number FE",
        transform=ax.transAxes,
        va="top",
        ha="left",
        fontsize=8,
        color="#4a4a4a",
    )

    fig.suptitle("Skill Premium and Clock-Time Mechanisms Around the Format Change", y=1.02, fontsize=13)
    path = os.path.join(FIG_DIR, "figure1_rating_diff_event_study.png")
    fig.savefig(path, bbox_inches="tight")
    plt.close(fig)
    print(path)


def figure2_age_gap_event_study():
    rows = read_csv("analysis_outputs/rule_change_metadata_econometric_novelty/age_gap_event_study.csv")
    rows.sort(key=lambda r: fnum(r["event_month_bin"]))
    x = [fnum(r["event_month_bin"]) for r in rows]
    y = [100 * fnum(r["estimate"]) for r in rows]
    lo = [100 * fnum(r["conf.low"]) for r in rows]
    hi = [100 * fnum(r["conf.high"]) for r in rows]
    yerr = [[yy - ll for yy, ll in zip(y, lo)], [hh - yy for yy, hh in zip(y, hi)]]

    plt.figure(figsize=(8.2, 4.6))
    plt.axhline(0, color="#555555", lw=0.8)
    plt.axvline(0, color="#b23b3b", lw=1.0, linestyle="--", label="Rule change month")
    plt.errorbar(x, y, yerr=yerr, fmt="o-", color="#2a7f62", ecolor="#98c8b8", capsize=2, lw=1.6, ms=4)
    plt.title("Event Study: Older-vs-Younger Within-Game Result Gap")
    plt.xlabel("Months relative to September 2025")
    plt.ylabel("Effect of being 10 years older, pp")
    plt.legend(frameon=False)
    savefig("figure2_age_gap_event_study.png")


def figure3_rank_movement_by_rating_bin():
    rows = read_csv(os.path.join(FIGDATA_DIR, "rank_movement_by_rating_bin.csv"))
    bins = ["Underdog >300", "Underdog 100-300", "Close +/-100", "Favorite 100-300", "Favorite >300"]
    period_rows = defaultdict(dict)
    for r in rows:
        period_rows[r["period"]][r["rating_bin"]] = r

    x = list(range(len(bins)))
    width = 0.36
    colors = {"3+1": "#6c757d", "5+0": "#235789"}
    plt.figure(figsize=(9.0, 4.9))
    for offset, period in [(-width / 2, "3+1"), (width / 2, "5+0")]:
        vals = [100 * fnum(period_rows[period].get(b, {}).get("mean_rank_improvement")) for b in bins]
        ses = [100 * fnum(period_rows[period].get(b, {}).get("se_rank_improvement"), 0.0) for b in bins]
        plt.bar([i + offset for i in x], vals, width=width, label=period, color=colors[period], alpha=0.9)
        plt.errorbar([i + offset for i in x], vals, yerr=ses, fmt="none", ecolor="#222222", capsize=2, lw=0.8)
    plt.axhline(0, color="#555555", lw=0.8)
    plt.xticks(x, bins, rotation=20, ha="right")
    plt.title("Rank Movement by Rating Advantage Bin")
    plt.xlabel("Player rating advantage before the game")
    plt.ylabel("Mean rank improvement, percentage points")
    plt.legend(frameon=False)
    savefig("figure3_rank_movement_by_rating_advantage.png")


def figure4_phase_and_last10_cp_loss():
    phase_rows = read_csv("analysis_outputs/stockfish_move_mechanisms_full_2022_2026/phase_descriptives.csv")
    last10_rows = read_csv(os.path.join(FIGDATA_DIR, "last10_cp_loss_by_format.csv"))
    phase_order = ["opening_1_10", "early_middlegame_11_20", "late_middlegame_21_35", "endgame_36_plus"]
    phase_label = {
        "opening_1_10": "Opening\n1-10",
        "early_middlegame_11_20": "Early middle\n11-20",
        "late_middlegame_21_35": "Late middle\n21-35",
        "endgame_36_plus": "Endgame\n36+",
    }
    phase_by = defaultdict(dict)
    for r in phase_rows:
        phase_by[label_period(r["format_5_0"])][r["phase_group"]] = r

    x = list(range(len(phase_order)))
    width = 0.36
    colors = {"3+1": "#6c757d", "5+0": "#235789"}
    fig, axes = plt.subplots(1, 2, figsize=(11.0, 4.7), gridspec_kw={"width_ratios": [3.0, 1.15]})

    for offset, period in [(-width / 2, "3+1"), (width / 2, "5+0")]:
        vals = [fnum(phase_by[period][p]["mean_cp_loss_cap"]) for p in phase_order]
        axes[0].bar([i + offset for i in x], vals, width=width, color=colors[period], label=period, alpha=0.9)
    axes[0].set_xticks(x)
    axes[0].set_xticklabels([phase_label[p] for p in phase_order])
    axes[0].set_title("Capped centipawn loss by phase")
    axes[0].set_ylabel("Mean capped centipawn loss")
    axes[0].legend(frameon=False)

    last_by = {r["period"]: r for r in last10_rows}
    axes[1].bar(
        [0, 1],
        [fnum(last_by["3+1"]["mean_last10_cp_loss"]), fnum(last_by["5+0"]["mean_last10_cp_loss"])],
        color=[colors["3+1"], colors["5+0"]],
        alpha=0.9,
    )
    axes[1].set_xticks([0, 1])
    axes[1].set_xticklabels(["3+1", "5+0"])
    axes[1].set_title("Last 10 plies")
    axes[1].set_ylabel("Mean CPL")
    fig.suptitle("Late-Game and Last-10-Move Position Quality by Format", y=1.02, fontsize=13)
    savefig("figure4_phase_last10_cp_loss.png")


def figure5_conversion_recovery_by_rating_bin():
    rows = read_csv(os.path.join(FIGDATA_DIR, "conversion_escape_by_rating_bin.csv"))
    bins = ["Underdog >300", "Underdog 100-300", "Close +/-100", "Favorite 100-300", "Favorite >300"]
    by = defaultdict(dict)
    for r in rows:
        by[r["period"]][r["rating_bin"]] = r
    colors = {"3+1": "#6c757d", "5+0": "#235789"}
    x = list(range(len(bins)))
    fig, axes = plt.subplots(1, 2, figsize=(11.2, 4.7), sharex=True)
    for period in ["3+1", "5+0"]:
        conv = [100 * fnum(by[period].get(b, {}).get("conversion_rate")) for b in bins]
        esc = [100 * fnum(by[period].get(b, {}).get("escape_rate")) for b in bins]
        axes[0].plot(x, conv, marker="o", lw=1.8, color=colors[period], label=period)
        axes[1].plot(x, esc, marker="o", lw=1.8, color=colors[period], label=period)
    axes[0].set_title("Conversion after reaching +2")
    axes[0].set_ylabel("Conversion rate, %")
    axes[1].set_title("Escape after reaching -2")
    axes[1].set_ylabel("Draw/win escape rate, %")
    for ax in axes:
        ax.set_xticks(x)
        ax.set_xticklabels(bins, rotation=20, ha="right")
        ax.set_xlabel("Player rating advantage before the game")
        ax.legend(frameon=False)
    fig.suptitle("Conversion and Defensive Recovery by Rating Advantage", y=1.02, fontsize=13)
    savefig("figure5_conversion_recovery_by_rating_advantage.png")


def figure6_neural_style_cluster_map():
    rows = read_csv("outputs/whole_dataset_2022_2026/contrastive_style/visualizations/player_style_embedding_pca_coordinates.csv")
    clusters = sorted({r["style_label"] for r in rows})
    palette = [
        "#235789",
        "#2a7f62",
        "#c44536",
        "#7d5fff",
        "#f0a202",
        "#58508d",
        "#008585",
    ]
    color = {cluster: palette[i % len(palette)] for i, cluster in enumerate(clusters)}

    fig = go.Figure()
    for cluster in clusters:
        subset = [r for r in rows if r["style_label"] == cluster]
        fig.add_trace(
            go.Scattergl(
                x=[fnum(r["embedding_pc1"]) for r in subset],
                y=[fnum(r["embedding_pc2"]) for r in subset],
                mode="markers",
                name=cluster,
                marker=dict(size=7, color=color[cluster], opacity=0.75),
                text=[
                    (
                        f"{r.get('display_name') or r.get('player')}<br>"
                        f"Cluster: {cluster}<br>"
                        f"Pre games: {r.get('prechange_games')}<br>"
                        f"Mean CPL: {fnum(r.get('mean_cp_loss')):.2f}<br>"
                        f"Pre result: {fnum(r.get('prechange_result_mean')):.3f}<br>"
                        f"Post accuracy delta: {fnum(r.get('delta_player_accuracy')):.3f}<br>"
                        f"Post result delta: {fnum(r.get('delta_player_result')):.3f}"
                    )
                    for r in subset
                ],
                hoverinfo="text",
            )
        )
    fig.update_layout(
        title="Neural Contrastive Player-Style Embeddings",
        xaxis_title="Embedding PC1",
        yaxis_title="Embedding PC2",
        template="plotly_white",
        legend_title="Style cluster",
        width=1050,
        height=720,
    )
    html_path = os.path.join(FIG_DIR, "figure6_neural_style_cluster_map.html")
    plotly_plot(fig, filename=html_path, auto_open=False, include_plotlyjs=True)
    print(html_path)

    plt.figure(figsize=(8.2, 6.2))
    for cluster in clusters:
        subset = [r for r in rows if r["style_label"] == cluster]
        plt.scatter(
            [fnum(r["embedding_pc1"]) for r in subset],
            [fnum(r["embedding_pc2"]) for r in subset],
            s=16,
            alpha=0.75,
            color=color[cluster],
            label=cluster,
            linewidths=0,
        )
    plt.title("Neural Contrastive Player-Style Embeddings")
    plt.xlabel("Embedding PC1")
    plt.ylabel("Embedding PC2")
    plt.legend(frameon=False, loc="best", fontsize=8)
    savefig("figure6_neural_style_cluster_map_static.png")


def write_manifest():
    path = os.path.join(OUT_ROOT, "README.md")
    with open(path, "a", encoding="utf-8") as f:
        f.write("\n## Figures\n\n")
        for name in [
            "figure1_rating_diff_event_study.png",
            "figure2_age_gap_event_study.png",
            "figure3_rank_movement_by_rating_advantage.png",
            "figure4_phase_last10_cp_loss.png",
            "figure5_conversion_recovery_by_rating_advantage.png",
            "figure6_neural_style_cluster_map.html",
            "figure6_neural_style_cluster_map_static.png",
        ]:
            f.write(f"- `figures/{name}`\n")


def main():
    setup_style()
    figure1_rating_diff_event_study()
    figure2_age_gap_event_study()
    figure3_rank_movement_by_rating_bin()
    figure4_phase_and_last10_cp_loss()
    figure5_conversion_recovery_by_rating_bin()
    figure6_neural_style_cluster_map()
    write_manifest()


if __name__ == "__main__":
    main()
