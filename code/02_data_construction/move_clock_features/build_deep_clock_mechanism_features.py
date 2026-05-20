#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import sys
import time
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_CPS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
]
DEFAULT_OUTPUT_DIR = ROOT / "analysis_outputs" / "deep_clock_mechanisms_2022_2026"
SNAPSHOT_MOVES = (10, 20, 30)
CLOCK_THRESHOLDS = (10.0, 30.0)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build deeper clock mechanism features from move-level Stockfish/clock CSVs."
    )
    parser.add_argument("--centipawn-csv", action="append", type=Path, default=[])
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--cp-loss-cap", type=float, default=1000.0)
    parser.add_argument("--progress-every-games", type=int, default=25000)
    parser.add_argument("--limit-games", type=int, default=0)
    return parser.parse_args()


def safe_float(value: str, default: float = math.nan) -> float:
    try:
        return float(value) if value != "" else default
    except ValueError:
        return default


def safe_int(value: str, default: int = 0) -> int:
    try:
        return int(float(value)) if value != "" else default
    except ValueError:
        return default


def fmt(value) -> str:
    if value is None:
        return ""
    if isinstance(value, float):
        if math.isnan(value) or math.isinf(value):
            return ""
        return f"{value:.6f}".rstrip("0").rstrip(".")
    return str(value)


def player_result_from_game(result: str, color: str) -> float:
    if result == "1-0":
        return 1.0 if color == "white" else 0.0
    if result == "0-1":
        return 0.0 if color == "white" else 1.0
    if result in {"1/2-1/2", "0.5-0.5"}:
        return 0.5
    return math.nan


def clock_bin(seconds: float) -> str:
    if seconds <= 5:
        return "00_05"
    if seconds <= 10:
        return "05_10"
    if seconds <= 30:
        return "10_30"
    if seconds <= 60:
        return "30_60"
    if seconds <= 120:
        return "60_120"
    return "gt120"


def time_spent_bin(seconds: float) -> str:
    if seconds <= 1:
        return "le1"
    if seconds <= 3:
        return "01_03"
    if seconds <= 5:
        return "03_05"
    if seconds <= 10:
        return "05_10"
    if seconds <= 20:
        return "10_20"
    return "gt20"


def iter_game_groups(path: Path):
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        required = {
            "game_id",
            "date",
            "result",
            "white",
            "black",
            "ply",
            "fullmove_number",
            "mover_color",
            "mover",
            "eval_after_white_cp",
            "eval_before_mover_cp",
            "cp_loss",
            "time_before_move",
            "time_after_move",
            "time_spent",
        }
        missing = required.difference(reader.fieldnames or [])
        if missing:
            raise RuntimeError(f"{path} is missing required columns: {sorted(missing)}")

        current_game_id = None
        rows: list[dict[str, str]] = []
        for row in reader:
            game_id = row["game_id"]
            if current_game_id is None:
                current_game_id = game_id
            if game_id != current_game_id:
                yield current_game_id, rows
                current_game_id = game_id
                rows = []
            rows.append(row)
        if current_game_id is not None:
            yield current_game_id, rows


def mean_or_blank(values: list[float]) -> float:
    clean = [v for v in values if not math.isnan(v)]
    if not clean:
        return math.nan
    return sum(clean) / len(clean)


def summarize_window(seq: list[dict], start: int, end: int) -> tuple[int, float, float]:
    vals = seq[max(0, start) : min(len(seq), end)]
    if not vals:
        return 0, math.nan, math.nan
    cp = [v["cp_cap"] for v in vals]
    blunders = [1 if v["cp_raw"] >= 200 else 0 for v in vals]
    return len(vals), mean_or_blank(cp), mean_or_blank(blunders)


class SumAgg:
    __slots__ = ("n", "cp_sum", "blunders", "critical", "time_sum")

    def __init__(self) -> None:
        self.n = 0
        self.cp_sum = 0.0
        self.blunders = 0
        self.critical = 0
        self.time_sum = 0.0

    def add(self, cp: float, blunder: bool, critical: bool, time_spent: float) -> None:
        self.n += 1
        self.cp_sum += cp
        self.blunders += int(blunder)
        self.critical += int(critical)
        self.time_sum += time_spent

    def row_values(self) -> dict[str, str]:
        return {
            "n_moves": self.n,
            "mean_cp_loss": self.cp_sum / self.n if self.n else math.nan,
            "blunder_rate": self.blunders / self.n if self.n else math.nan,
            "critical_share": self.critical / self.n if self.n else math.nan,
            "mean_time_spent": self.time_sum / self.n if self.n else math.nan,
        }


def parse_game_rows(rows: list[dict[str, str]], cp_loss_cap: float) -> list[dict]:
    parsed = []
    for row in rows:
        time_before = safe_float(row["time_before_move"])
        time_after = safe_float(row["time_after_move"])
        time_spent = safe_float(row["time_spent"])
        if math.isnan(time_before) or math.isnan(time_after) or math.isnan(time_spent):
            continue
        cp_raw = safe_float(row["cp_loss"], 0.0)
        eval_before_mover = safe_float(row["eval_before_mover_cp"])
        parsed.append(
            {
                "game_id": row["game_id"],
                "date": row["date"],
                "result": row["result"],
                "white": row["white"],
                "black": row["black"],
                "ply": safe_int(row["ply"]),
                "fullmove": safe_int(row["fullmove_number"]),
                "color": row["mover_color"],
                "mover": row["mover"],
                "eval_after_white": safe_float(row["eval_after_white_cp"]),
                "eval_before_mover": eval_before_mover,
                "critical": (not math.isnan(eval_before_mover)) and abs(eval_before_mover) <= 100,
                "cp_raw": cp_raw,
                "cp_cap": min(cp_raw, cp_loss_cap),
                "time_before": time_before,
                "time_after": time_after,
                "time_spent": time_spent,
            }
        )
    return parsed


def first_threshold_features(
    seq: list[dict], threshold: float
) -> dict[str, float | int]:
    first_idx = None
    for i, row in enumerate(seq):
        if row["time_before"] <= threshold:
            first_idx = i
            break
    if first_idx is None:
        return {
            "reached": 0,
            "recovered": 0,
            "first_ply": 0,
            "pre5_n": 0,
            "post5_n": 0,
            "post10_n": 0,
            "pre5_cp": math.nan,
            "post5_cp": math.nan,
            "post10_cp": math.nan,
            "delta_post5_cp": math.nan,
            "delta_post10_cp": math.nan,
            "pre5_blunder": math.nan,
            "post5_blunder": math.nan,
            "post10_blunder": math.nan,
        }
    recovered = any(row["time_before"] > threshold for row in seq[first_idx + 1 :])
    pre5_n, pre5_cp, pre5_blunder = summarize_window(seq, first_idx - 5, first_idx)
    post5_n, post5_cp, post5_blunder = summarize_window(seq, first_idx, first_idx + 5)
    post10_n, post10_cp, post10_blunder = summarize_window(seq, first_idx, first_idx + 10)
    return {
        "reached": 1,
        "recovered": int(recovered),
        "first_ply": seq[first_idx]["ply"],
        "pre5_n": pre5_n,
        "post5_n": post5_n,
        "post10_n": post10_n,
        "pre5_cp": pre5_cp,
        "post5_cp": post5_cp,
        "post10_cp": post10_cp,
        "delta_post5_cp": post5_cp - pre5_cp if not math.isnan(pre5_cp) and not math.isnan(post5_cp) else math.nan,
        "delta_post10_cp": post10_cp - pre5_cp if not math.isnan(pre5_cp) and not math.isnan(post10_cp) else math.nan,
        "pre5_blunder": pre5_blunder,
        "post5_blunder": post5_blunder,
        "post10_blunder": post10_blunder,
    }


def board_threshold_features(parsed: list[dict], color: str) -> dict[str, float | int]:
    white = parsed[0]["white"] if parsed else ""
    black = parsed[0]["black"] if parsed else ""
    result = parsed[0]["result"] if parsed else ""
    player_result = player_result_from_game(result, color)
    opp = "black" if color == "white" else "white"
    start_clock = {
        "white": next((r["time_before"] for r in parsed if r["color"] == "white"), math.nan),
        "black": next((r["time_before"] for r in parsed if r["color"] == "black"), math.nan),
    }
    current_clock = dict(start_clock)

    first_winning = None
    first_losing = None
    for row in parsed:
        current_clock[row["color"]] = row["time_after"]
        eval_white = row["eval_after_white"]
        if math.isnan(eval_white):
            continue
        eval_player = eval_white if color == "white" else -eval_white
        own_time = current_clock[color]
        opp_time = current_clock[opp]
        if first_winning is None and eval_player >= 200:
            first_winning = (row["ply"], own_time, opp_time)
        if first_losing is None and eval_player <= -200:
            first_losing = (row["ply"], own_time, opp_time)

    player_name = white if color == "white" else black
    opponent_name = black if color == "white" else white
    out = {
        "player_name": player_name,
        "opponent_name": opponent_name,
        "mover_color": color,
        "player_result_pgn": player_result,
        "board_reached_winning": int(first_winning is not None),
        "board_reached_losing": int(first_losing is not None),
        "board_converted_winning": int(first_winning is not None and player_result == 1.0),
        "board_escaped_losing": int(first_losing is not None and player_result >= 0.5),
        "first_winning_ply_board": first_winning[0] if first_winning else 0,
        "first_losing_ply_board": first_losing[0] if first_losing else 0,
        "first_winning_own_time": first_winning[1] if first_winning else math.nan,
        "first_winning_opp_time": first_winning[2] if first_winning else math.nan,
        "first_losing_own_time": first_losing[1] if first_losing else math.nan,
        "first_losing_opp_time": first_losing[2] if first_losing else math.nan,
    }
    for prefix in ("first_winning", "first_losing"):
        own_time = out[f"{prefix}_own_time"]
        opp_time = out[f"{prefix}_opp_time"]
        out[f"{prefix}_own_low10"] = int(not math.isnan(own_time) and own_time <= 10)
        out[f"{prefix}_opp_low10"] = int(not math.isnan(opp_time) and opp_time <= 10)
        out[f"{prefix}_own_low30"] = int(not math.isnan(own_time) and own_time <= 30)
        out[f"{prefix}_opp_low30"] = int(not math.isnan(opp_time) and opp_time <= 30)
    return out


def write_dict(writer: csv.DictWriter, row: dict) -> None:
    writer.writerow({key: fmt(row.get(key, "")) for key in writer.fieldnames or []})


def build_features(cp_paths: list[Path], output_dir: Path, cp_loss_cap: float, progress_every: int, limit_games: int) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    snapshot_path = output_dir / "deep_clock_snapshots.csv"
    clock_bins_path = output_dir / "deep_clock_bins_player_game.csv"
    time_bins_path = output_dir / "deep_time_spent_bins_player_game.csv"
    recovery_path = output_dir / "deep_clock_recovery_player_game.csv"
    event_path = output_dir / "deep_low_clock_event_player_game.csv"

    snapshot_fields = [
        "game_id", "date", "snapshot_move", "player_name", "opponent_name", "mover_color",
        "own_clock", "opp_clock", "clock_advantage_seconds", "eval_advantage_cp",
    ]
    bin_fields = [
        "game_id", "date", "player_name", "opponent_name", "mover_color", "clock_bin",
        "n_moves", "mean_cp_loss", "blunder_rate", "critical_share", "mean_time_spent",
    ]
    time_bin_fields = [
        "game_id", "date", "player_name", "opponent_name", "mover_color", "time_spent_bin",
        "critical", "n_moves", "mean_cp_loss", "blunder_rate", "critical_share", "mean_time_spent",
    ]
    recovery_fields = [
        "game_id", "date", "player_name", "opponent_name", "mover_color", "player_result_pgn",
        "n_moves", "clock_gain_move_share", "min_time_before", "final_time_after",
        "reached_low10", "recovered_low10", "first_low10_ply",
        "reached_low30", "recovered_low30", "first_low30_ply",
        "board_reached_winning", "board_converted_winning", "first_winning_ply_board",
        "first_winning_own_time", "first_winning_opp_time",
        "first_winning_own_low10", "first_winning_opp_low10",
        "first_winning_own_low30", "first_winning_opp_low30",
        "board_reached_losing", "board_escaped_losing", "first_losing_ply_board",
        "first_losing_own_time", "first_losing_opp_time",
        "first_losing_own_low10", "first_losing_opp_low10",
        "first_losing_own_low30", "first_losing_opp_low30",
    ]
    event_fields = [
        "game_id", "date", "player_name", "opponent_name", "mover_color", "threshold",
        "reached", "recovered", "first_ply", "pre5_n", "post5_n", "post10_n",
        "pre5_cp", "post5_cp", "post10_cp", "delta_post5_cp", "delta_post10_cp",
        "pre5_blunder", "post5_blunder", "post10_blunder",
    ]

    start = time.monotonic()
    games_seen = 0
    written = defaultdict(int)
    with (
        snapshot_path.open("w", encoding="utf-8", newline="") as snapshot_f,
        clock_bins_path.open("w", encoding="utf-8", newline="") as clock_bins_f,
        time_bins_path.open("w", encoding="utf-8", newline="") as time_bins_f,
        recovery_path.open("w", encoding="utf-8", newline="") as recovery_f,
        event_path.open("w", encoding="utf-8", newline="") as event_f,
    ):
        snapshot_writer = csv.DictWriter(snapshot_f, fieldnames=snapshot_fields)
        clock_bins_writer = csv.DictWriter(clock_bins_f, fieldnames=bin_fields)
        time_bins_writer = csv.DictWriter(time_bins_f, fieldnames=time_bin_fields)
        recovery_writer = csv.DictWriter(recovery_f, fieldnames=recovery_fields)
        event_writer = csv.DictWriter(event_f, fieldnames=event_fields)
        for writer in (snapshot_writer, clock_bins_writer, time_bins_writer, recovery_writer, event_writer):
            writer.writeheader()

        for path in cp_paths:
            for game_id, rows in iter_game_groups(path):
                games_seen += 1
                parsed = parse_game_rows(rows, cp_loss_cap)
                if not parsed:
                    continue

                white = parsed[0]["white"]
                black = parsed[0]["black"]
                by_color = {
                    "white": [row for row in parsed if row["color"] == "white"],
                    "black": [row for row in parsed if row["color"] == "black"],
                }

                by_color_move = {(row["color"], row["fullmove"]): row for row in parsed}
                for move_no in SNAPSHOT_MOVES:
                    white_row = by_color_move.get(("white", move_no))
                    black_row = by_color_move.get(("black", move_no))
                    if white_row is None or black_row is None:
                        continue
                    eval_white = black_row["eval_after_white"]
                    if math.isnan(eval_white):
                        continue
                    white_clock = white_row["time_after"]
                    black_clock = black_row["time_after"]
                    snap_rows = (
                        {
                            "game_id": game_id,
                            "date": parsed[0]["date"],
                            "snapshot_move": move_no,
                            "player_name": white,
                            "opponent_name": black,
                            "mover_color": "white",
                            "own_clock": white_clock,
                            "opp_clock": black_clock,
                            "clock_advantage_seconds": white_clock - black_clock,
                            "eval_advantage_cp": eval_white,
                        },
                        {
                            "game_id": game_id,
                            "date": parsed[0]["date"],
                            "snapshot_move": move_no,
                            "player_name": black,
                            "opponent_name": white,
                            "mover_color": "black",
                            "own_clock": black_clock,
                            "opp_clock": white_clock,
                            "clock_advantage_seconds": black_clock - white_clock,
                            "eval_advantage_cp": -eval_white,
                        },
                    )
                    for snap_row in snap_rows:
                        write_dict(snapshot_writer, snap_row)
                        written["snapshots"] += 1

                for color, seq in by_color.items():
                    if not seq:
                        continue
                    player_name = white if color == "white" else black
                    opponent_name = black if color == "white" else white
                    base = {
                        "game_id": game_id,
                        "date": parsed[0]["date"],
                        "player_name": player_name,
                        "opponent_name": opponent_name,
                        "mover_color": color,
                    }
                    clock_aggs: dict[str, SumAgg] = defaultdict(SumAgg)
                    time_aggs: dict[tuple[str, int], SumAgg] = defaultdict(SumAgg)
                    clock_gain_moves = 0
                    min_time_before = math.inf

                    for row in seq:
                        blunder = row["cp_raw"] >= 200
                        critical = row["critical"]
                        min_time_before = min(min_time_before, row["time_before"])
                        clock_gain_moves += int(row["time_after"] > row["time_before"] + 0.05)
                        clock_aggs[clock_bin(row["time_before"])].add(
                            row["cp_cap"], blunder, critical, row["time_spent"]
                        )
                        time_aggs[(time_spent_bin(row["time_spent"]), int(critical))].add(
                            row["cp_cap"], blunder, critical, row["time_spent"]
                        )

                    for bin_name, agg in clock_aggs.items():
                        write_dict(clock_bins_writer, {**base, "clock_bin": bin_name, **agg.row_values()})
                        written["clock_bins"] += 1
                    for (bin_name, critical), agg in time_aggs.items():
                        write_dict(
                            time_bins_writer,
                            {**base, "time_spent_bin": bin_name, "critical": critical, **agg.row_values()},
                        )
                        written["time_bins"] += 1

                    threshold10 = first_threshold_features(seq, 10.0)
                    threshold30 = first_threshold_features(seq, 30.0)
                    board_features = board_threshold_features(parsed, color)
                    recovery_row = {
                        **base,
                        "player_result_pgn": player_result_from_game(parsed[0]["result"], color),
                        "n_moves": len(seq),
                        "clock_gain_move_share": clock_gain_moves / len(seq),
                        "min_time_before": min_time_before if min_time_before < math.inf else math.nan,
                        "final_time_after": seq[-1]["time_after"],
                        "reached_low10": threshold10["reached"],
                        "recovered_low10": threshold10["recovered"],
                        "first_low10_ply": threshold10["first_ply"],
                        "reached_low30": threshold30["reached"],
                        "recovered_low30": threshold30["recovered"],
                        "first_low30_ply": threshold30["first_ply"],
                        **board_features,
                    }
                    write_dict(recovery_writer, recovery_row)
                    written["recovery"] += 1

                    for threshold, features in ((10, threshold10), (30, threshold30)):
                        if features["reached"] != 1:
                            continue
                        event_row = {**base, "threshold": threshold, **features}
                        write_dict(event_writer, event_row)
                        written["events"] += 1

                if progress_every > 0 and games_seen % progress_every == 0:
                    elapsed = time.monotonic() - start
                    print(
                        "Deep clock features: "
                        f"games={games_seen:,} snapshots={written['snapshots']:,} "
                        f"clock_bins={written['clock_bins']:,} recovery={written['recovery']:,} "
                        f"rate={games_seen / elapsed:,.0f} games/s",
                        file=sys.stderr,
                        flush=True,
                    )
                if limit_games and games_seen >= limit_games:
                    break
            if limit_games and games_seen >= limit_games:
                break

    elapsed = time.monotonic() - start
    print(
        {
            "output_dir": str(output_dir),
            "games_seen": games_seen,
            **dict(written),
            "seconds": round(elapsed, 3),
        }
    )


def main() -> int:
    args = parse_args()
    cp_paths = args.centipawn_csv or DEFAULT_CPS
    for path in cp_paths:
        if not path.exists():
            raise FileNotFoundError(path)
    build_features(args.centipawn_csv or DEFAULT_CPS, args.output_dir, args.cp_loss_cap, args.progress_every_games, args.limit_games)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
