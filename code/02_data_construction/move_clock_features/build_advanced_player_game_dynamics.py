#!/usr/bin/env python3
"""Build compact dynamic player-game features from centipawn rows.

The output supports deeper analyses:
- adaptation and treatment heterogeneity use player-game outcomes;
- error cascades after the first blunder;
- richer conversion/defensive recovery measures after +2/-2 positions.

The script streams centipawn CSVs and writes one row per player-game.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_CPS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
]
DEFAULT_PLAYER_GAME_OUTCOMES = (
    ROOT / "analysis_outputs"
    / "stockfish_move_mechanisms_full_2022_2026"
    / "player_game_move_outcomes.csv"
)
DEFAULT_OUTPUT_DIR = ROOT / "analysis_outputs" / "advanced_player_game_dynamics"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--centipawn-csv", action="append", type=Path, default=[])
    parser.add_argument("--player-game-outcomes", type=Path, default=DEFAULT_PLAYER_GAME_OUTCOMES)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--winning-cp", type=float, default=200.0)
    parser.add_argument("--equal-cp", type=float, default=100.0)
    parser.add_argument("--cap-cp", type=float, default=5000.0)
    parser.add_argument("--progress-every-games", type=int, default=50000)
    return parser.parse_args()


def safe_float(value: str | None, default: float = math.nan) -> float:
    try:
        return float(value) if value not in (None, "") else default
    except ValueError:
        return default


def safe_int(value: str | None, default: int = 0) -> int:
    try:
        return int(float(value)) if value not in (None, "") else default
    except ValueError:
        return default


def load_keep_player_games(path: Path) -> set[tuple[str, str]]:
    keep: set[tuple[str, str]] = set()
    with path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.DictReader(f):
            game_id = row.get("game_id", "")
            color = row.get("mover_color", "")
            if game_id and color:
                keep.add((game_id, color))
    return keep


def cp_game_groups(paths: list[Path]):
    for path in paths:
        with path.open("r", encoding="utf-8", newline="") as f:
            reader = csv.DictReader(f)
            current_id = None
            rows: list[dict[str, str]] = []
            for row in reader:
                game_id = row.get("game_id", "")
                if current_id is None:
                    current_id = game_id
                if game_id != current_id:
                    yield current_id, rows
                    current_id = game_id
                    rows = []
                rows.append(row)
            if current_id is not None:
                yield current_id, rows


def capped(value: float, cap: float) -> float:
    if math.isnan(value):
        return value
    return max(-cap, min(cap, value))


def player_eval_white(eval_white: float, color: str, cap: float) -> float:
    value = eval_white if color == "white" else -eval_white
    return capped(value, cap)


def blank_if_nan(value: float | int | None) -> str | int:
    if value is None:
        return ""
    if isinstance(value, float) and math.isnan(value):
        return ""
    return value


def first_index(values: list[float], predicate) -> int | None:
    for idx, value in enumerate(values):
        if predicate(value):
            return idx
    return None


def summarize_color(
    game_id: str,
    color: str,
    rows: list[dict[str, str]],
    keep: set[tuple[str, str]],
    winning_cp: float,
    equal_cp: float,
    cap_cp: float,
) -> dict[str, object] | None:
    if (game_id, color) not in keep:
        return None
    if not rows:
        return None

    timeline = []
    own_moves = []
    for row in rows:
        ply = safe_int(row.get("ply"))
        fullmove = safe_int(row.get("fullmove_number"))
        eval_white = safe_float(row.get("eval_after_white_cp"))
        eval_after_player = player_eval_white(eval_white, color, cap_cp)
        timeline.append((ply, fullmove, eval_after_player))
        if row.get("mover_color") == color:
            cp_loss = safe_float(row.get("cp_loss"), 0.0)
            own_moves.append(
                {
                    "ply": ply,
                    "fullmove": fullmove,
                    "cp_loss": cp_loss,
                    "eval_after_player": player_eval_white(
                        safe_float(row.get("eval_after_white_cp")), color, cap_cp
                    ),
                }
            )

    if not timeline or not own_moves:
        return None

    evals = [x[2] for x in timeline if not math.isnan(x[2])]
    final_eval = evals[-1] if evals else math.nan
    max_eval = max(evals) if evals else math.nan
    min_eval = min(evals) if evals else math.nan

    first_win_idx = first_index([x[2] for x in timeline], lambda v: not math.isnan(v) and v >= winning_cp)
    first_loss_idx = first_index([x[2] for x in timeline], lambda v: not math.isnan(v) and v <= -winning_cp)

    first_win_ply = timeline[first_win_idx][0] if first_win_idx is not None else None
    first_loss_ply = timeline[first_loss_idx][0] if first_loss_idx is not None else None
    max_after_plus2 = min_after_plus2 = final_after_plus2 = math.nan
    advantage_loss_after_plus2 = math.nan
    dropped_below_equal_after_plus2 = ""
    dropped_below_plus1_after_plus2 = ""
    conversion_speed_plies = ""
    if first_win_idx is not None:
        after = [x[2] for x in timeline[first_win_idx:] if not math.isnan(x[2])]
        if after:
            max_after_plus2 = max(after)
            min_after_plus2 = min(after)
            final_after_plus2 = after[-1]
            advantage_loss_after_plus2 = max_after_plus2 - min_after_plus2
            dropped_below_equal_after_plus2 = int(any(v < 0 for v in after))
            dropped_below_plus1_after_plus2 = int(any(v < equal_cp for v in after))
            conversion_speed_plies = timeline[-1][0] - timeline[first_win_idx][0]

    best_after_minus2 = final_after_minus2 = math.nan
    recovered_to_equal_after_minus2 = ""
    recovered_to_plus1_after_minus2 = ""
    defensive_recovery_cp = math.nan
    if first_loss_idx is not None:
        after = [x[2] for x in timeline[first_loss_idx:] if not math.isnan(x[2])]
        if after:
            best_after_minus2 = max(after)
            final_after_minus2 = after[-1]
            recovered_to_equal_after_minus2 = int(any(v >= -equal_cp for v in after))
            recovered_to_plus1_after_minus2 = int(any(v >= equal_cp for v in after))
            defensive_recovery_cp = best_after_minus2 - after[0]

    first_blunder_idx = first_index(own_moves, lambda m: m["cp_loss"] >= 200.0)
    first_mistake_idx = first_index(own_moves, lambda m: m["cp_loss"] >= 100.0)
    cascade_blunder_next5 = ""
    cascade_mistake_next5 = ""
    mean_cp_loss_next5 = math.nan
    max_cp_loss_next5 = math.nan
    eval_after_first_blunder = math.nan
    best_eval_next5 = math.nan
    worst_eval_next5 = math.nan
    recovered_to_equal_next5 = ""
    additional_eval_drop_next5 = math.nan
    if first_blunder_idx is not None:
        first = own_moves[first_blunder_idx]
        next5 = own_moves[first_blunder_idx + 1 : first_blunder_idx + 6]
        eval_after_first_blunder = first["eval_after_player"]
        if next5:
            cp_next = [m["cp_loss"] for m in next5]
            ev_next = [m["eval_after_player"] for m in next5 if not math.isnan(m["eval_after_player"])]
            cascade_blunder_next5 = int(any(v >= 200.0 for v in cp_next))
            cascade_mistake_next5 = int(any(v >= 100.0 for v in cp_next))
            mean_cp_loss_next5 = sum(cp_next) / len(cp_next)
            max_cp_loss_next5 = max(cp_next)
            if ev_next:
                best_eval_next5 = max(ev_next)
                worst_eval_next5 = min(ev_next)
                recovered_to_equal_next5 = int(best_eval_next5 >= -equal_cp)
                additional_eval_drop_next5 = eval_after_first_blunder - worst_eval_next5

    return {
        "game_id": game_id,
        "mover_color": color,
        "position_count": len(timeline),
        "own_move_count": len(own_moves),
        "max_eval_player_cp": blank_if_nan(max_eval),
        "min_eval_player_cp": blank_if_nan(min_eval),
        "final_eval_player_cp": blank_if_nan(final_eval),
        "first_plus2_ply": blank_if_nan(first_win_ply),
        "first_minus2_ply": blank_if_nan(first_loss_ply),
        "max_after_plus2_cp": blank_if_nan(max_after_plus2),
        "min_after_plus2_cp": blank_if_nan(min_after_plus2),
        "final_after_plus2_cp": blank_if_nan(final_after_plus2),
        "advantage_loss_after_plus2_cp": blank_if_nan(advantage_loss_after_plus2),
        "dropped_below_equal_after_plus2": dropped_below_equal_after_plus2,
        "dropped_below_plus1_after_plus2": dropped_below_plus1_after_plus2,
        "conversion_speed_plies": conversion_speed_plies,
        "best_after_minus2_cp": blank_if_nan(best_after_minus2),
        "final_after_minus2_cp": blank_if_nan(final_after_minus2),
        "recovered_to_equal_after_minus2": recovered_to_equal_after_minus2,
        "recovered_to_plus1_after_minus2": recovered_to_plus1_after_minus2,
        "defensive_recovery_after_minus2_cp": blank_if_nan(defensive_recovery_cp),
        "first_blunder_ply_dynamic": blank_if_nan(
            own_moves[first_blunder_idx]["ply"] if first_blunder_idx is not None else None
        ),
        "first_blunder_move_dynamic": blank_if_nan(
            own_moves[first_blunder_idx]["fullmove"] if first_blunder_idx is not None else None
        ),
        "first_mistake_move_dynamic": blank_if_nan(
            own_moves[first_mistake_idx]["fullmove"] if first_mistake_idx is not None else None
        ),
        "cascade_blunder_next5": cascade_blunder_next5,
        "cascade_mistake_next5": cascade_mistake_next5,
        "mean_cp_loss_next5_after_blunder": blank_if_nan(mean_cp_loss_next5),
        "max_cp_loss_next5_after_blunder": blank_if_nan(max_cp_loss_next5),
        "eval_after_first_blunder_cp": blank_if_nan(eval_after_first_blunder),
        "best_eval_next5_after_blunder_cp": blank_if_nan(best_eval_next5),
        "worst_eval_next5_after_blunder_cp": blank_if_nan(worst_eval_next5),
        "recovered_to_equal_next5_after_blunder": recovered_to_equal_next5,
        "additional_eval_drop_next5_after_blunder_cp": blank_if_nan(additional_eval_drop_next5),
    }


def main() -> int:
    args = parse_args()
    cp_paths = args.centipawn_csv or DEFAULT_CPS
    args.output_dir.mkdir(parents=True, exist_ok=True)
    keep = load_keep_player_games(args.player_game_outcomes)
    output_path = args.output_dir / "advanced_player_game_dynamics.csv"

    fieldnames = [
        "game_id", "mover_color", "position_count", "own_move_count",
        "max_eval_player_cp", "min_eval_player_cp", "final_eval_player_cp",
        "first_plus2_ply", "first_minus2_ply", "max_after_plus2_cp",
        "min_after_plus2_cp", "final_after_plus2_cp", "advantage_loss_after_plus2_cp",
        "dropped_below_equal_after_plus2", "dropped_below_plus1_after_plus2",
        "conversion_speed_plies", "best_after_minus2_cp", "final_after_minus2_cp",
        "recovered_to_equal_after_minus2", "recovered_to_plus1_after_minus2",
        "defensive_recovery_after_minus2_cp", "first_blunder_ply_dynamic",
        "first_blunder_move_dynamic", "first_mistake_move_dynamic",
        "cascade_blunder_next5", "cascade_mistake_next5",
        "mean_cp_loss_next5_after_blunder", "max_cp_loss_next5_after_blunder",
        "eval_after_first_blunder_cp", "best_eval_next5_after_blunder_cp",
        "worst_eval_next5_after_blunder_cp", "recovered_to_equal_next5_after_blunder",
        "additional_eval_drop_next5_after_blunder_cp",
    ]

    start = time.perf_counter()
    games = 0
    rows_written = 0
    with output_path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for game_id, rows in cp_game_groups(cp_paths):
            games += 1
            for color in ("white", "black"):
                out = summarize_color(
                    game_id,
                    color,
                    rows,
                    keep,
                    args.winning_cp,
                    args.equal_cp,
                    args.cap_cp,
                )
                if out is not None:
                    writer.writerow(out)
                    rows_written += 1
            if args.progress_every_games and games % args.progress_every_games == 0:
                elapsed = time.perf_counter() - start
                print(
                    f"Dynamics: games={games:,} player_games={rows_written:,} elapsed={elapsed:,.1f}s",
                    flush=True,
                )

    summary = {
        "centipawn_csvs": [str(p) for p in cp_paths],
        "player_game_outcomes": str(args.player_game_outcomes),
        "keep_player_games": len(keep),
        "games_read": games,
        "player_games_written": rows_written,
        "output_csv": str(output_path),
        "seconds": time.perf_counter() - start,
    }
    (args.output_dir / "advanced_player_game_dynamics_summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8"
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
