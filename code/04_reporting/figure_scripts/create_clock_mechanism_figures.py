#!/usr/bin/env python3
"""Create thesis figures for the clock-time mechanism results."""

from __future__ import annotations

import csv
import math
import os
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib-cache")

import matplotlib.pyplot as plt


ROOT = Path(__file__).resolve().parents[3]
FIG_DIR = ROOT / "paper" / "figures"
CLOCK_DIR = ROOT / "analysis_outputs" / "clock_time_mechanisms_2022_2026"
DEEP_DIR = ROOT / "analysis_outputs" / "deep_clock_mechanisms_2022_2026"
EXACT_DIR = ROOT / "analysis_outputs" / "exact_clock_mechanisms_2022_2026"

COLORS = {
    "pre": "#2f5d7c",
    "post": "#c76f3a",
    "accent": "#b84a62",
    "green": "#2e8b57",
    "navy": "#1f3a5f",
    "gray": "#6b7280",
    "light": "#eef3f7",
    "line": "#d6dde5",
    "text": "#202938",
}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(value: str | None) -> float:
    if value is None or value == "":
        return math.nan
    return float(value)


def setup() -> None:
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
        0.00,
        1.06,
        label,
        transform=ax.transAxes,
        fontsize=12,
        fontweight="bold",
        color=COLORS["navy"],
        va="top",
    )


def add_value_labels(ax, bars, fmt="{:.1f}") -> None:
    for bar in bars:
        height = bar.get_height()
        ax.text(
            bar.get_x() + bar.get_width() / 2,
            height + (ax.get_ylim()[1] - ax.get_ylim()[0]) * 0.025,
            fmt.format(height),
            ha="center",
            va="bottom",
            fontsize=9,
            color=COLORS["text"],
        )


def coef_lookup(rows: list[dict[str, str]], model_id: str, term: str) -> dict[str, str]:
    for row in rows:
        if row.get("model_id") == model_id and row.get("term") == term:
            return row
    raise KeyError((model_id, term))


def make_clock_exposure_figure() -> None:
    desc = read_csv(CLOCK_DIR / "clock_descriptives_by_format.csv")
    deep = {row["metric"]: row for row in read_csv(DEEP_DIR / "deep_clock_descriptives.csv")}
    clock_coef = read_csv(CLOCK_DIR / "clock_headline_coefficients.csv")

    pre = next(row for row in desc if row["format_5_0"] == "0")
    post = next(row for row in desc if row["format_5_0"] == "1")
    age = coef_lookup(clock_coef, "A06", "format_5_0:age10_c")
    age_est = 100 * fnum(age["estimate"])
    age_se = 100 * fnum(age["std_error"])

    fig, axes = plt.subplots(2, 2, figsize=(11.2, 7.8))
    fig.suptitle(
        "Clock States Before and After the 5+0 Reform",
        x=0.02,
        ha="left",
        y=0.98,
        fontsize=15,
        fontweight="bold",
        color=COLORS["navy"],
    )
    fig.text(
        0.02,
        0.935,
        "The reform reduced ordinary low-clock exposure, but removed the increment-based recovery margin.",
        color=COLORS["gray"],
        fontsize=10,
    )

    ax = axes[0, 0]
    add_panel_label(ax, "A")
    labels = [r"$\leq 10$s", r"$\leq 30$s"]
    pre_vals = [100 * fnum(pre["low_before_10_share"]), 100 * fnum(pre["low_before_30_share"])]
    post_vals = [100 * fnum(post["low_before_10_share"]), 100 * fnum(post["low_before_30_share"])]
    x = [0, 1]
    width = 0.35
    b1 = ax.bar([v - width / 2 for v in x], pre_vals, width, label="3+1 pre", color=COLORS["pre"])
    b2 = ax.bar([v + width / 2 for v in x], post_vals, width, label="5+0 post", color=COLORS["post"])
    ax.set_title("Low-clock exposure falls")
    ax.set_ylabel("Share of moves (%)")
    ax.set_xticks(x, labels)
    ax.set_ylim(0, max(pre_vals + post_vals) * 1.28)
    ax.grid(axis="y", alpha=0.25)
    ax.legend(frameon=False, loc="upper right")
    add_value_labels(ax, b1)
    add_value_labels(ax, b2)

    ax = axes[0, 1]
    add_panel_label(ax, "B")
    vals = [fnum(pre["final_time_after"]), fnum(post["final_time_after"])]
    bars = ax.bar(["3+1 pre", "5+0 post"], vals, color=[COLORS["pre"], COLORS["post"]])
    ax.set_title("Final clock reserve rises")
    ax.set_ylabel("Seconds after player's last move")
    ax.set_ylim(0, max(vals) * 1.30)
    ax.grid(axis="y", alpha=0.25)
    add_value_labels(ax, bars)

    ax = axes[1, 0]
    add_panel_label(ax, "C")
    recovery = deep["recovered_low10_given_reached"]
    vals = [100 * fnum(recovery["pre"]), 100 * fnum(recovery["post"])]
    bars = ax.bar(["3+1 pre", "5+0 post"], vals, color=[COLORS["pre"], COLORS["post"]])
    ax.set_title("Recovery from severe time trouble disappears")
    ax.set_ylabel("Recovered above 10 seconds (%)")
    ax.set_ylim(0, max(vals) * 1.45 + 1)
    ax.grid(axis="y", alpha=0.25)
    add_value_labels(ax, bars)

    ax = axes[1, 1]
    add_panel_label(ax, "D")
    ax.axvline(0, color=COLORS["line"], linewidth=1)
    ax.errorbar(
        [age_est],
        [0],
        xerr=[1.96 * age_se],
        fmt="o",
        color=COLORS["accent"],
        ecolor=COLORS["accent"],
        capsize=4,
        markersize=7,
    )
    ax.set_title("Older players are more exposed post-reform")
    ax.set_xlabel("Post x age effect on <=10s exposure, pp per 10 years")
    ax.set_yticks([])
    ax.set_ylim(-0.35, 0.35)
    ax.set_xlim(0, max(3.2, age_est + 2.2 * age_se))
    ax.text(
        age_est,
        0.14,
        f"{age_est:.2f} pp",
        ha="center",
        va="bottom",
        fontsize=10,
        color=COLORS["accent"],
        fontweight="bold",
    )
    ax.grid(axis="x", alpha=0.25)

    fig.subplots_adjust(left=0.08, right=0.98, bottom=0.09, top=0.82, wspace=0.24, hspace=0.52)
    fig.savefig(FIG_DIR / "figure12_clock_exposure_insurance.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def make_clock_mechanism_figure() -> None:
    exact = read_csv(EXACT_DIR / "exact_requested_model_coefficients.csv")
    deep = read_csv(DEEP_DIR / "deep_clock_headline_coefficients.csv")

    bins = [
        ("60_120", "60-120s"),
        ("30_60", "30-60s"),
        ("10_30", "10-30s"),
        ("05_10", "5-10s"),
        ("00_05", "0-5s"),
    ]
    interactions = []
    for key, label in bins:
        row = coef_lookup(exact, "E03", f"clock_bin_f::{key}:format_5_0")
        interactions.append((label, fnum(row["estimate"]), fnum(row["std_error"])))

    def total_effect(main_term: str, int_term: str) -> tuple[float, float]:
        main = coef_lookup(deep, "D06" if "winning" in main_term else "D07", main_term)
        inter = coef_lookup(deep, "D06" if "winning" in main_term else "D07", int_term)
        return 100 * fnum(main["estimate"]), 100 * (fnum(main["estimate"]) + fnum(inter["estimate"]))

    conversion_rows = [
        ("Own low clock\nwhen +2", *total_effect("first_winning_own_low10", "first_winning_own_low10:format_5_0")),
        ("Opponent low clock\nwhen +2", *total_effect("first_winning_opp_low10", "first_winning_opp_low10:format_5_0")),
        ("Own low clock\nwhen -2", *total_effect("first_losing_own_low10", "first_losing_own_low10:format_5_0")),
        ("Opponent low clock\nwhen -2", *total_effect("first_losing_opp_low10", "first_losing_opp_low10:format_5_0")),
    ]

    fig, axes = plt.subplots(1, 2, figsize=(12.2, 5.7), gridspec_kw={"width_ratios": [1.05, 1.25]})
    fig.suptitle(
        "How Clock Pressure Changes Move Quality and Conversion",
        x=0.02,
        ha="left",
        y=0.98,
        fontsize=15,
        fontweight="bold",
        color=COLORS["navy"],
    )
    fig.text(
        0.02,
        0.925,
        "Severe time trouble has the largest post-change quality penalty; opponent clock weakness becomes a conversion resource.",
        color=COLORS["gray"],
        fontsize=10,
    )

    ax = axes[0]
    add_panel_label(ax, "A")
    labels = [x[0] for x in interactions]
    est = [x[1] for x in interactions]
    se = [x[2] for x in interactions]
    colors = [COLORS["pre"], COLORS["pre"], COLORS["pre"], COLORS["post"], COLORS["accent"]]
    bars = ax.bar(labels, est, color=colors)
    ax.errorbar(labels, est, yerr=[1.96 * s for s in se], fmt="none", ecolor=COLORS["text"], capsize=3, linewidth=0.9)
    ax.axhline(0, color=COLORS["line"], linewidth=1)
    ax.set_title("Post-change extra CPL by clock bin")
    ax.set_ylabel("Additional capped centipawns")
    ax.set_ylim(0, max(est) * 1.30)
    ax.grid(axis="y", alpha=0.25)
    ax.tick_params(axis="x", rotation=25)
    add_value_labels(ax, bars)

    ax = axes[1]
    add_panel_label(ax, "B")
    y = list(range(len(conversion_rows)))
    pre_vals = [row[1] for row in conversion_rows]
    post_vals = [row[2] for row in conversion_rows]
    offset = 0.18
    ax.barh([v + offset for v in y], pre_vals, height=0.32, label="3+1 pre effect", color=COLORS["pre"])
    ax.barh([v - offset for v in y], post_vals, height=0.32, label="5+0 post total effect", color=COLORS["post"])
    ax.axvline(0, color=COLORS["line"], linewidth=1)
    ax.set_yticks(y, [row[0] for row in conversion_rows])
    ax.set_title("Low-clock states reshape conversion")
    ax.set_xlabel("Change in conversion or escape probability, percentage points")
    ax.grid(axis="x", alpha=0.25)
    ax.legend(frameon=False, loc="lower right")
    ax.set_xlim(min(pre_vals + post_vals) * 1.18, max(pre_vals + post_vals) * 1.18)
    for yi, val in zip([v + offset for v in y], pre_vals):
        ax.text(val + (1 if val >= 0 else -1), yi, f"{val:.1f}", va="center", ha="left" if val >= 0 else "right", fontsize=8.5)
    for yi, val in zip([v - offset for v in y], post_vals):
        ax.text(val + (1 if val >= 0 else -1), yi, f"{val:.1f}", va="center", ha="left" if val >= 0 else "right", fontsize=8.5)

    fig.subplots_adjust(left=0.08, right=0.98, bottom=0.15, top=0.78, wspace=0.34)
    fig.savefig(FIG_DIR / "figure13_clock_panic_conversion.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    setup()
    make_clock_exposure_figure()
    make_clock_mechanism_figure()
    print("Wrote clock figures to", FIG_DIR)


if __name__ == "__main__":
    main()
