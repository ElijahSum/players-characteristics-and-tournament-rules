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
DEFAULT_OUTPUT_DIR = PROJECT_ROOT / "analysis_outputs" / "exact_clock_mechanisms_2022_2026"
EVENT_K_MIN = -5
EVENT_K_MAX = 10


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build aggregated cells for exact requested clock-econometric specifications."
    )
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
    value = str(value)
    marker = "/live/"
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


def format_flag(date_key: str) -> int:
    return int(date_key >= "2025-09-02 00:00:00")


def build_bridge(paths: list[Path]) -> dict[tuple[str, str], str]:
    bridge: dict[tuple[str, str], str] = {}
    for path in paths:
        with path.open("r", encoding="utf-8", newline="") as f:
            reader = csv.DictReader(f)
            required = {"white_name", "black_name", "date", "game_link"}
            missing = required.difference(reader.fieldnames or [])
            if missing:
                raise RuntimeError(f"{path} is missing bridge columns: {sorted(missing)}")
            for row in reader:
                game_id = extract_game_id(row["game_link"])
                date_key = normalize_event_datetime(row["date"])
                if not game_id or not date_key:
                    continue
                bridge[(game_id, row["white_name"])] = date_key
                bridge[(game_id, row["black_name"])] = date_key
    return bridge


def iter_game_groups(path: Path):
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        required = {
            "game_id", "ply", "fullmove_number", "mover_color", "mover",
            "eval_before_mover_cp", "cp_loss", "time_before_move", "time_spent",
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


class Agg:
    __slots__ = ("n", "cp_sum", "blunders")

    def __init__(self) -> None:
        self.n = 0
        self.cp_sum = 0.0
        self.blunders = 0

    def add(self, cp: float, blunder: bool) -> None:
        self.n += 1
        self.cp_sum += cp
        self.blunders += int(blunder)


def write_partial(path: Path, fieldnames: list[str], rows: dict[tuple, Agg]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(fieldnames)
        key_len = len(fieldnames) - 3
        for key, agg in rows.items():
            if len(key) != key_len:
                raise RuntimeError("Internal key length mismatch")
            writer.writerow([*key, agg.n, f"{agg.cp_sum:.6f}", agg.blunders])


def finalize_partials(tmp_dir: Path, pattern: str, group_cols: list[str], output_path: Path) -> None:
    files = sorted(tmp_dir.glob(pattern))
    if not files:
        raise RuntimeError(f"No partial files found for {pattern}")
    glob_path = str(tmp_dir / pattern)
    group_expr = ", ".join(group_cols)
    con = duckdb.connect(":memory:")
    con.execute("PRAGMA threads=4")
    con.execute(
        f"""
        COPY (
          SELECT
            {group_expr},
            SUM(n)::BIGINT AS n_moves,
            SUM(cp_sum) / SUM(n) AS mean_cp_loss,
            SUM(blunders) / SUM(n) AS blunder_rate
          FROM read_csv_auto('{glob_path}', union_by_name=true)
          GROUP BY {group_expr}
        )
        TO '{output_path}' (HEADER, DELIMITER ',')
        """
    )
    con.close()


def build_cells(
    cp_paths: list[Path],
    bridge_paths: list[Path],
    output_dir: Path,
    cp_loss_cap: float,
    flush_every_games: int,
    progress_every_games: int,
    limit_games: int,
) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    tmp_dir = output_dir / "_tmp_exact_cells"
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    tmp_dir.mkdir(parents=True)

    bridge = build_bridge(bridge_paths)
    panic: dict[tuple, Agg] = defaultdict(Agg)
    thinking: dict[tuple, Agg] = defaultdict(Agg)
    event: dict[tuple, Agg] = defaultdict(Agg)

    panic_fields = ["player_name", "date_key", "format_5_0", "fullmove_number", "clock_bin", "n", "cp_sum", "blunders"]
    thinking_fields = [
        "player_name", "date_key", "format_5_0", "fullmove_number",
        "critical", "time_bin", "time_midpoint", "n", "cp_sum", "blunders",
    ]
    event_fields = [
        "player_name", "date_key", "format_5_0", "fullmove_number",
        "threshold", "event_k", "n", "cp_sum", "blunders",
    ]
    games_seen = 0
    skipped_rows = 0
    chunk = 0
    start = time.monotonic()

    def flush() -> None:
        nonlocal chunk, panic, thinking, event
        if not panic and not thinking and not event:
            return
        chunk += 1
        if panic:
            write_partial(tmp_dir / f"panic_{chunk:04d}.csv", panic_fields, panic)
        if thinking:
            write_partial(tmp_dir / f"thinking_{chunk:04d}.csv", thinking_fields, thinking)
        if event:
            write_partial(tmp_dir / f"event_{chunk:04d}.csv", event_fields, event)
        panic = defaultdict(Agg)
        thinking = defaultdict(Agg)
        event = defaultdict(Agg)

    for path in cp_paths:
        for game_id, rows in iter_game_groups(path):
            games_seen += 1
            seq_by_player: dict[str, list[tuple[int, int, float, bool]]] = defaultdict(list)
            for row in rows:
                player = row["mover"]
                date_key = bridge.get((game_id, player))
                if date_key is None:
                    skipped_rows += 1
                    continue
                time_before = safe_float(row["time_before_move"])
                time_spent = safe_float(row["time_spent"])
                if math.isnan(time_before) or math.isnan(time_spent):
                    skipped_rows += 1
                    continue
                fullmove = safe_int(row["fullmove_number"])
                cp_raw = safe_float(row["cp_loss"], 0.0)
                cp_cap = min(cp_raw, cp_loss_cap)
                blunder = cp_raw >= 200
                fmt = format_flag(date_key)
                critical = int(abs(safe_float(row["eval_before_mover_cp"])) <= 100)

                panic[(player, date_key, fmt, fullmove, clock_bin(time_before))].add(cp_cap, blunder)
                tb, midpoint = time_bin(time_spent)
                thinking[(player, date_key, fmt, fullmove, critical, tb, midpoint)].add(cp_cap, blunder)
                seq_by_player[player].append((fullmove, int(row["ply"]), time_before, cp_cap, blunder, date_key, fmt))

            for player, seq in seq_by_player.items():
                seq.sort(key=lambda item: item[1])
                for threshold in (10, 30):
                    first_idx = None
                    for i, (_, _, time_before, *_rest) in enumerate(seq):
                        if time_before <= threshold:
                            first_idx = i
                            break
                    if first_idx is None:
                        continue
                    for i in range(max(0, first_idx + EVENT_K_MIN), min(len(seq), first_idx + EVENT_K_MAX + 1)):
                        fullmove, _ply, _time_before, cp_cap, blunder, date_key, fmt = seq[i]
                        event[(player, date_key, fmt, fullmove, threshold, i - first_idx)].add(cp_cap, blunder)

            if flush_every_games > 0 and games_seen % flush_every_games == 0:
                flush()
            if progress_every_games > 0 and games_seen % progress_every_games == 0:
                elapsed = time.monotonic() - start
                print(
                    f"Exact cells: games={games_seen:,} chunks={chunk} "
                    f"skipped_rows={skipped_rows:,} rate={games_seen / elapsed:,.0f} games/s",
                    file=sys.stderr,
                    flush=True,
                )
            if limit_games and games_seen >= limit_games:
                break
        if limit_games and games_seen >= limit_games:
            break
    flush()

    finalize_partials(
        tmp_dir,
        "panic_*.csv",
        ["player_name", "date_key", "format_5_0", "fullmove_number", "clock_bin"],
        output_dir / "exact_clock_bin_move_cells.csv",
    )
    finalize_partials(
        tmp_dir,
        "thinking_*.csv",
        ["player_name", "date_key", "format_5_0", "fullmove_number", "critical", "time_bin", "time_midpoint"],
        output_dir / "exact_thinking_time_move_cells.csv",
    )
    finalize_partials(
        tmp_dir,
        "event_*.csv",
        ["player_name", "date_key", "format_5_0", "fullmove_number", "threshold", "event_k"],
        output_dir / "exact_first_time_trouble_event_cells.csv",
    )
    shutil.rmtree(tmp_dir)

    elapsed = time.monotonic() - start
    print(
        {
            "output_dir": str(output_dir),
            "games_seen": games_seen,
            "skipped_rows": skipped_rows,
            "chunks": chunk,
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
    build_cells(
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
