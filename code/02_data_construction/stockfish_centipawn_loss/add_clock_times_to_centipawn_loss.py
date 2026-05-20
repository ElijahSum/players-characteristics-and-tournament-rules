#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import os
import re
import sys
import time
from pathlib import Path
from typing import Iterator


CLOCK_RE = re.compile(r"\{\s*\[%clk\s+([^\]]+)\]\s*\}")
HEADER_RE = re.compile(r'^\[([A-Za-z0-9_]+)\s+"(.*)"\]$')
GAME_ID_RE = re.compile(r"/(?:live|daily)/(\d+)")
NEW_COLUMNS = ("time_before_move", "time_after_move", "time_spent")
REFORM_DATE = (2025, 9, 2)
MISSING = {"", "NA", "NaN", "nan", "None", "null", "NULL"}


class ClockBuildError(RuntimeError):
    pass


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Add per-move clock variables from Chess.com PGN [%clk] annotations "
            "to a centipawn-loss CSV."
        )
    )
    parser.add_argument("--pgn", required=True, help="Path to the PGN file with clock tags.")
    parser.add_argument(
        "--centipawn-csv",
        required=True,
        help="Path to the centipawn-loss CSV to update.",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Output CSV path. If omitted with --in-place, writes a temporary file and replaces input.",
    )
    parser.add_argument(
        "--in-place",
        action="store_true",
        help="Replace the input centipawn CSV after the new file is fully written.",
    )
    parser.add_argument(
        "--limit-rows",
        type=int,
        default=None,
        help="Optional row limit for smoke tests.",
    )
    parser.add_argument(
        "--progress-every",
        type=int,
        default=1_000_000,
        help="Print progress after this many centipawn rows.",
    )
    return parser.parse_args()


def parse_clock_seconds(raw: str) -> float:
    parts = raw.strip().split(":")
    if len(parts) == 3:
        hours = float(parts[0])
        minutes = float(parts[1])
        seconds = float(parts[2])
        return hours * 3600.0 + minutes * 60.0 + seconds
    if len(parts) == 2:
        minutes = float(parts[0])
        seconds = float(parts[1])
        return minutes * 60.0 + seconds
    return float(parts[0])


def format_seconds(value: float | None) -> str:
    if value is None:
        return ""
    if abs(value) < 0.0005:
        value = 0.0
    return f"{value:.3f}".rstrip("0").rstrip(".")


def parse_date_tuple(headers: dict[str, str]) -> tuple[int, int, int] | None:
    raw = headers.get("UTCDate") or headers.get("Date")
    if not raw or raw in MISSING:
        return None
    parts = raw.replace("-", ".").split(".")
    if len(parts) < 3:
        return None
    try:
        return int(parts[0]), int(parts[1]), int(parts[2])
    except ValueError:
        return None


def infer_time_control(headers: dict[str, str]) -> tuple[float, float]:
    raw = (headers.get("TimeControl") or "").strip()
    if raw and raw not in MISSING and raw != "-":
        first_control = raw.split(":")[0]
        if "+" in first_control:
            base_raw, inc_raw = first_control.split("+", 1)
            return float(base_raw), float(inc_raw)
        if first_control.isdigit():
            return float(first_control), 0.0

    date_tuple = parse_date_tuple(headers)
    if date_tuple is not None and date_tuple >= REFORM_DATE:
        return 300.0, 0.0
    return 180.0, 1.0


def extract_game_id(headers: dict[str, str]) -> str | None:
    link = headers.get("Link", "")
    match = GAME_ID_RE.search(link)
    if match:
        return match.group(1)
    site = headers.get("Site", "")
    match = GAME_ID_RE.search(site)
    if match:
        return match.group(1)
    return None


def build_clock_rows(headers: dict[str, str], move_text: str) -> tuple[str, list[tuple[str, str, str]]]:
    game_id = extract_game_id(headers)
    if not game_id:
        raise ClockBuildError(f"Could not extract game id from PGN headers: {headers!r}")

    base_seconds, increment_seconds = infer_time_control(headers)
    previous_after = {"white": base_seconds, "black": base_seconds}
    rows: list[tuple[str, str, str]] = []

    for ply, match in enumerate(CLOCK_RE.finditer(move_text), start=1):
        color = "white" if ply % 2 == 1 else "black"
        before = previous_after[color]
        after = parse_clock_seconds(match.group(1))
        spent = before + increment_seconds - after

        # Chess.com clock tags are rounded and can occasionally contain clock
        # corrections. Negative thinking time is not meaningful for the final
        # dataset, so clamp these display anomalies to zero.
        if spent < 0:
            spent = 0.0

        rows.append((format_seconds(before), format_seconds(after), format_seconds(spent)))
        previous_after[color] = after

    return game_id, rows


def iter_pgn_clock_games(pgn_path: Path) -> Iterator[tuple[str, list[tuple[str, str, str]]]]:
    headers: dict[str, str] = {}
    move_lines: list[str] = []

    def maybe_yield_current() -> tuple[str, list[tuple[str, str, str]]] | None:
        if headers and move_lines:
            return build_clock_rows(headers, " ".join(move_lines))
        return None

    with pgn_path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped:
                current = maybe_yield_current()
                if current is not None:
                    yield current
                    headers = {}
                    move_lines = []
                continue

            header_match = HEADER_RE.match(stripped)
            if header_match:
                current = maybe_yield_current()
                if current is not None:
                    yield current
                    headers = {}
                    move_lines = []
                headers[header_match.group(1)] = header_match.group(2)
                continue

            move_lines.append(stripped)

    current = maybe_yield_current()
    if current is not None:
        yield current


def output_path_for(args: argparse.Namespace, centipawn_path: Path) -> Path:
    if args.output:
        return Path(args.output)
    if args.in_place:
        return centipawn_path.with_name(f"{centipawn_path.name}.clock_tmp")
    raise ClockBuildError("Use either --output or --in-place.")


def advance_to_game(
    pgn_iter: Iterator[tuple[str, list[tuple[str, str, str]]]],
    target_game_id: str,
    current_game: tuple[str, list[tuple[str, str, str]]] | None,
) -> tuple[tuple[str, list[tuple[str, str, str]]] | None, int]:
    skipped = 0
    while current_game is None or current_game[0] != target_game_id:
        try:
            current_game = next(pgn_iter)
        except StopIteration:
            return None, skipped
        if current_game[0] != target_game_id:
            skipped += 1
    return current_game, skipped


def add_clock_times(
    *,
    pgn_path: Path,
    centipawn_path: Path,
    output_path: Path,
    limit_rows: int | None,
    progress_every: int,
) -> dict[str, int | str | float]:
    start_time = time.monotonic()
    pgn_iter = iter_pgn_clock_games(pgn_path)
    current_game: tuple[str, list[tuple[str, str, str]]] | None = None
    current_game_id: str | None = None
    input_size = centipawn_path.stat().st_size

    rows_read = 0
    rows_with_clock = 0
    rows_missing_clock = 0
    pgn_games_skipped = 0
    pgn_games_used = 0

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with (
        centipawn_path.open("r", encoding="utf-8", newline="") as src,
        output_path.open("w", encoding="utf-8", newline="") as dst,
    ):
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise ClockBuildError(f"Centipawn CSV has no header: {centipawn_path}")
        if "game_id" not in reader.fieldnames or "ply" not in reader.fieldnames:
            raise ClockBuildError("Centipawn CSV must contain game_id and ply columns.")

        base_fieldnames = [field for field in reader.fieldnames if field not in NEW_COLUMNS]
        writer = csv.DictWriter(dst, fieldnames=base_fieldnames + list(NEW_COLUMNS))
        writer.writeheader()

        for row in reader:
            rows_read += 1
            game_id = row["game_id"].strip()
            if game_id != current_game_id:
                current_game, skipped = advance_to_game(pgn_iter, game_id, current_game)
                pgn_games_skipped += skipped
                if current_game is None:
                    raise ClockBuildError(f"Failed to find PGN clock data for game_id={game_id}")
                current_game_id = game_id
                pgn_games_used += 1

            for column in NEW_COLUMNS:
                row.pop(column, None)

            clock_values: tuple[str, str, str] | None = None
            try:
                ply = int(row["ply"])
            except ValueError:
                ply = -1

            if current_game is not None and 1 <= ply <= len(current_game[1]):
                clock_values = current_game[1][ply - 1]

            if clock_values is None:
                row["time_before_move"] = ""
                row["time_after_move"] = ""
                row["time_spent"] = ""
                rows_missing_clock += 1
            else:
                row["time_before_move"], row["time_after_move"], row["time_spent"] = clock_values
                rows_with_clock += 1

            writer.writerow({field: row.get(field, "") for field in writer.fieldnames})

            if limit_rows is not None and rows_read >= limit_rows:
                break

            if progress_every > 0 and rows_read % progress_every == 0:
                elapsed = time.monotonic() - start_time
                rate = rows_read / elapsed if elapsed > 0 else 0
                print(
                    f"Clock merge {centipawn_path.name}: rows={rows_read:,} "
                    f"clock={rows_with_clock:,} missing={rows_missing_clock:,} "
                    f"rate={rate:,.0f} rows/s",
                    file=sys.stderr,
                    flush=True,
                )

    return {
        "centipawn_csv": str(centipawn_path),
        "pgn": str(pgn_path),
        "output": str(output_path),
        "rows_read": rows_read,
        "rows_with_clock": rows_with_clock,
        "rows_missing_clock": rows_missing_clock,
        "pgn_games_used": pgn_games_used,
        "pgn_games_skipped": pgn_games_skipped,
        "seconds": round(time.monotonic() - start_time, 3),
    }


def main() -> int:
    args = parse_args()
    pgn_path = Path(args.pgn)
    centipawn_path = Path(args.centipawn_csv)
    output_path = output_path_for(args, centipawn_path)

    if not pgn_path.exists():
        raise FileNotFoundError(f"PGN not found: {pgn_path}")
    if not centipawn_path.exists():
        raise FileNotFoundError(f"Centipawn CSV not found: {centipawn_path}")
    if output_path.exists():
        output_path.unlink()

    stats = add_clock_times(
        pgn_path=pgn_path,
        centipawn_path=centipawn_path,
        output_path=output_path,
        limit_rows=args.limit_rows,
        progress_every=args.progress_every,
    )

    if args.in_place and args.limit_rows is None:
        os.replace(output_path, centipawn_path)
        stats["replaced_input"] = str(centipawn_path)

    print(stats)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
