#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import shutil
import sys
import time
from collections import defaultdict
from datetime import datetime
from pathlib import Path

import duckdb


ROOT = Path(__file__).resolve().parents[3]
PROJECT_ROOT = ROOT
DEFAULT_CPS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
]
DEFAULT_BRIDGES = [
    PROJECT_ROOT / "data" / "tournaments_1_261_final_v6.csv",
    PROJECT_ROOT / "data" / "merged_tournaments_1_150_added_missed_links_3.csv",
]
DEFAULT_OUTPUT_DIR = PROJECT_ROOT / "analysis_outputs" / "interaction_clock_mechanisms_2022_2026"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build compact cells for interaction-based clock mechanisms.")
    parser.add_argument("--centipawn-csv", action="append", type=Path, default=[])
    parser.add_argument("--bridge-csv", action="append", type=Path, default=[])
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--cp-loss-cap", type=float, default=1000.0)
    parser.add_argument("--flush-every-games", type=int, default=25000)
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


def normalize_event_datetime(value: str) -> str:
    value = str(value)
    for fmt in ("%Y-%m-%d %H:%M:%S", "%b %d, %Y, %I:%M %p"):
        try:
            return datetime.strptime(value, fmt).strftime("%Y-%m-%d %H:%M:%S")
        except ValueError:
            pass
    return ""


def extract_game_id(value: str) -> str:
    marker = "/live/"
    value = str(value)
    if marker not in value:
        return ""
    tail = value.split(marker, 1)[1]
    digits = []
    for ch in tail:
        if ch.isdigit():
            digits.append(ch)
        else:
            break
    return "".join(digits)


def format_flag(date_key: str) -> int:
    return int(date_key >= "2025-09-02 00:00:00")


def phase_from_fullmove(fullmove: int) -> str:
    if fullmove <= 10:
        return "opening_1_10"
    if fullmove <= 20:
        return "early_11_20"
    if fullmove <= 35:
        return "late_21_35"
    return "endgame_36_plus"


def eval_swing_bin(cp: float) -> tuple[str, float]:
    if cp <= 25:
        return "000_025", 0.125
    if cp <= 50:
        return "025_050", 0.375
    if cp <= 100:
        return "050_100", 0.75
    if cp <= 200:
        return "100_200", 1.5
    if cp <= 500:
        return "200_500", 3.5
    return "gt500", 7.5


def time_bin(seconds: float) -> tuple[str, float]:
    if seconds <= 1:
        return "le1", 0.5
    if seconds <= 3:
        return "01_03", 2.0
    if seconds <= 5:
        return "03_05", 4.0
    if seconds <= 10:
        return "05_10", 7.5
    if seconds <= 20:
        return "10_20", 15.0
    if seconds <= 40:
        return "20_40", 30.0
    return "gt40", 50.0


def build_bridge(paths: list[Path]) -> dict[str, str]:
    bridge: dict[str, str] = {}
    for path in paths:
        with path.open("r", encoding="utf-8", newline="") as f:
            reader = csv.DictReader(f)
            required = {"date", "game_link"}
            missing = required.difference(reader.fieldnames or [])
            if missing:
                raise RuntimeError(f"{path} missing bridge columns: {sorted(missing)}")
            for row in reader:
                game_id = extract_game_id(row["game_link"])
                date_key = normalize_event_datetime(row["date"])
                if game_id and date_key:
                    bridge[game_id] = date_key
    return bridge


def iter_game_groups(path: Path):
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        required = {
            "game_id", "ply", "fullmove_number", "mover", "is_capture", "gives_check",
            "eval_before_white_cp", "eval_after_white_cp", "eval_before_mover_cp",
            "cp_loss", "time_after_move", "time_spent",
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


class PressureAgg:
    __slots__ = ("n", "next_time_sum", "next_low10", "next_low30")

    def __init__(self) -> None:
        self.n = 0
        self.next_time_sum = 0.0
        self.next_low10 = 0
        self.next_low30 = 0

    def add(self, next_time_spent: float, next_time_after: float) -> None:
        self.n += 1
        self.next_time_sum += next_time_spent
        self.next_low10 += int(next_time_after <= 10)
        self.next_low30 += int(next_time_after <= 30)


class RhythmAgg:
    __slots__ = ("n", "cp_sum", "blunders")

    def __init__(self) -> None:
        self.n = 0
        self.cp_sum = 0.0
        self.blunders = 0

    def add(self, cp_loss: float, blunder: bool) -> None:
        self.n += 1
        self.cp_sum += cp_loss
        self.blunders += int(blunder)


def write_pressure_partial(path: Path, fieldnames: list[str], rows: dict[tuple, PressureAgg]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(fieldnames)
        for key, agg in rows.items():
            writer.writerow([*key, agg.n, f"{agg.next_time_sum:.6f}", agg.next_low10, agg.next_low30])


def write_rhythm_partial(path: Path, fieldnames: list[str], rows: dict[tuple, RhythmAgg]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(fieldnames)
        for key, agg in rows.items():
            writer.writerow([*key, agg.n, f"{agg.cp_sum:.6f}", agg.blunders])


def finalize_pressure(tmp_dir: Path, output_path: Path) -> None:
    con = duckdb.connect(":memory:")
    con.execute("PRAGMA threads=4")
    con.execute(
        f"""
        COPY (
          SELECT
            player_name,
            opponent_name,
            format_5_0,
            phase,
            is_capture,
            gives_check,
            critical_position,
            eval_swing_bin,
            eval_swing_midpoint,
            SUM(n)::BIGINT AS n_moves,
            SUM(next_time_sum) / SUM(n) AS mean_opponent_time_spent_next,
            SUM(next_low10) / SUM(n) AS opponent_low10_after_next_rate,
            SUM(next_low30) / SUM(n) AS opponent_low30_after_next_rate
          FROM read_csv_auto('{tmp_dir / "pressure_*.csv"}', union_by_name=true)
          GROUP BY
            player_name, opponent_name, format_5_0, phase, is_capture, gives_check,
            critical_position, eval_swing_bin, eval_swing_midpoint
        )
        TO '{output_path}' (HEADER, DELIMITER ',')
        """
    )
    con.close()


def finalize_rhythm(tmp_dir: Path, output_path: Path) -> None:
    con = duckdb.connect(":memory:")
    con.execute("PRAGMA threads=4")
    con.execute(
        f"""
        COPY (
          SELECT
            player_name,
            format_5_0,
            fullmove_number,
            previous_time_bin,
            previous_time_midpoint,
            SUM(n)::BIGINT AS n_moves,
            SUM(cp_sum) / SUM(n) AS mean_cp_loss,
            SUM(blunders) / SUM(n) AS blunder_rate
          FROM read_csv_auto('{tmp_dir / "rhythm_*.csv"}', union_by_name=true)
          GROUP BY player_name, format_5_0, fullmove_number, previous_time_bin, previous_time_midpoint
        )
        TO '{output_path}' (HEADER, DELIMITER ',')
        """
    )
    con.close()


def build_features(
    cp_paths: list[Path],
    bridge_paths: list[Path],
    output_dir: Path,
    cp_loss_cap: float,
    flush_every_games: int,
    progress_every_games: int,
    limit_games: int,
) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    tmp_dir = output_dir / "_tmp_interaction_cells"
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    tmp_dir.mkdir(parents=True)

    bridge = build_bridge(bridge_paths)
    pressure: dict[tuple, PressureAgg] = defaultdict(PressureAgg)
    rhythm: dict[tuple, RhythmAgg] = defaultdict(RhythmAgg)
    pressure_fields = [
        "player_name", "opponent_name", "format_5_0", "phase", "is_capture", "gives_check",
        "critical_position", "eval_swing_bin", "eval_swing_midpoint",
        "n", "next_time_sum", "next_low10", "next_low30",
    ]
    rhythm_fields = [
        "player_name", "format_5_0", "fullmove_number", "previous_time_bin",
        "previous_time_midpoint", "n", "cp_sum", "blunders",
    ]

    games_seen = 0
    skipped_pairs = 0
    chunks = 0
    start = time.monotonic()

    def flush() -> None:
        nonlocal pressure, rhythm, chunks
        if not pressure and not rhythm:
            return
        chunks += 1
        if pressure:
            write_pressure_partial(tmp_dir / f"pressure_{chunks:04d}.csv", pressure_fields, pressure)
        if rhythm:
            write_rhythm_partial(tmp_dir / f"rhythm_{chunks:04d}.csv", rhythm_fields, rhythm)
        pressure = defaultdict(PressureAgg)
        rhythm = defaultdict(RhythmAgg)

    for path in cp_paths:
        for game_id, rows in iter_game_groups(path):
            games_seen += 1
            date_key = bridge.get(game_id)
            if not date_key:
                skipped_pairs += max(0, len(rows) - 1)
                continue
            fmt = format_flag(date_key)
            parsed = []
            for row in rows:
                time_spent = safe_float(row["time_spent"])
                time_after = safe_float(row["time_after_move"])
                cp_raw = safe_float(row["cp_loss"], 0.0)
                if math.isnan(time_spent) or math.isnan(time_after):
                    continue
                parsed.append(
                    {
                        "player": row["mover"],
                        "fullmove": min(safe_int(row["fullmove_number"]), 80),
                        "phase": phase_from_fullmove(safe_int(row["fullmove_number"])),
                        "is_capture": safe_int(row["is_capture"]),
                        "gives_check": safe_int(row["gives_check"]),
                        "critical": int(abs(safe_float(row["eval_before_mover_cp"])) <= 100),
                        "eval_swing_cp": abs(
                            safe_float(row["eval_after_white_cp"], 0.0)
                            - safe_float(row["eval_before_white_cp"], 0.0)
                        ),
                        "cp_cap": min(cp_raw, cp_loss_cap),
                        "cp_raw": cp_raw,
                        "time_after": time_after,
                        "time_spent": time_spent,
                    }
                )

            for i in range(len(parsed)):
                row = parsed[i]
                if i > 0:
                    prev = parsed[i - 1]
                    tb, tm = time_bin(prev["time_spent"])
                    rhythm[(row["player"], fmt, row["fullmove"], tb, tm)].add(
                        row["cp_cap"], row["cp_raw"] >= 200
                    )
                if i + 1 < len(parsed):
                    nxt = parsed[i + 1]
                    if row["player"] == nxt["player"]:
                        skipped_pairs += 1
                        continue
                    eb, em = eval_swing_bin(row["eval_swing_cp"])
                    pressure[
                        (
                            row["player"],
                            nxt["player"],
                            fmt,
                            row["phase"],
                            row["is_capture"],
                            row["gives_check"],
                            row["critical"],
                            eb,
                            em,
                        )
                    ].add(nxt["time_spent"], nxt["time_after"])

            if flush_every_games and games_seen % flush_every_games == 0:
                flush()
            if progress_every_games and games_seen % progress_every_games == 0:
                elapsed = time.monotonic() - start
                print(
                    f"Interaction cells: games={games_seen:,} chunks={chunks} "
                    f"pressure_cells={len(pressure):,} rhythm_cells={len(rhythm):,} "
                    f"rate={games_seen / elapsed:,.0f} games/s",
                    file=sys.stderr,
                    flush=True,
                )
            if limit_games and games_seen >= limit_games:
                break
        if limit_games and games_seen >= limit_games:
            break
    flush()
    finalize_pressure(tmp_dir, output_dir / "time_pressure_production_cells.csv")
    finalize_rhythm(tmp_dir, output_dir / "opponent_long_think_disruption_cells.csv")
    shutil.rmtree(tmp_dir)
    elapsed = time.monotonic() - start
    print(
        {
            "output_dir": str(output_dir),
            "games_seen": games_seen,
            "skipped_pairs": skipped_pairs,
            "chunks": chunks,
            "seconds": round(elapsed, 3),
        }
    )


def main() -> int:
    args = parse_args()
    cp_paths = args.centipawn_csv or DEFAULT_CPS
    bridge_paths = args.bridge_csv or DEFAULT_BRIDGES
    for path in [*cp_paths, *bridge_paths]:
        if not path.exists():
            raise FileNotFoundError(path)
    build_features(
        cp_paths,
        bridge_paths,
        args.output_dir,
        args.cp_loss_cap,
        args.flush_every_games,
        args.progress_every_games,
        args.limit_games,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
