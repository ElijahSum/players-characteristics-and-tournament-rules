#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path


DEFAULT_INPUT_2026 = Path(
    "outputs/final_regression_data_tournaments_2024_2026_with_opponents_sum_score_with_rank.csv"
)
DEFAULT_INPUT_2024 = Path(
    "outputs/regression_data_tournaments_2024_with_opponents_sum_score_with_rank.csv"
)
DEFAULT_OUTPUT = Path(
    "outputs/final_regression_data_tournaments_2024_2026.csv"
)
UNIQUE_KEY_COLUMNS = ("date", "round", "player_name")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Concatenate the ranked 2024 and 2026 regression datasets into one final CSV."
    )
    parser.add_argument(
        "--input-2026",
        default=str(DEFAULT_INPUT_2026),
        help="Path to the 2026 ranked regression dataset.",
    )
    parser.add_argument(
        "--input-2024",
        default=str(DEFAULT_INPUT_2024),
        help="Path to the 2024 ranked regression dataset.",
    )
    parser.add_argument(
        "--output",
        default=str(DEFAULT_OUTPUT),
        help="Path to the final concatenated dataset.",
    )
    return parser.parse_args()


def validate_input(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"Input file not found: {path}")


def read_header(path: Path) -> list[str]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.reader(handle)
        try:
            return next(reader)
        except StopIteration as exc:
            raise ValueError(f"Input file is empty: {path}") from exc


def build_key_indices(header: list[str]) -> tuple[int, ...]:
    missing_columns = [column for column in UNIQUE_KEY_COLUMNS if column not in header]
    if missing_columns:
        raise ValueError(
            "Input files are missing required deduplication columns: "
            + ", ".join(missing_columns)
        )
    return tuple(header.index(column) for column in UNIQUE_KEY_COLUMNS)


def append_rows(
    writer: csv.writer,
    path: Path,
    expected_header: list[str],
    key_indices: tuple[int, ...],
    seen_keys: set[tuple[str, ...]],
) -> tuple[int, int]:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.reader(handle)
        header = next(reader, None)
        if header is None:
            raise ValueError(f"Input file is empty: {path}")
        if header != expected_header:
            raise ValueError(
                f"Input schema mismatch for {path}. Expected columns: {expected_header}. "
                f"Found: {header}."
            )

        kept_rows = 0
        skipped_rows = 0
        for row in reader:
            row_key = tuple(row[index] for index in key_indices)
            if row_key in seen_keys:
                skipped_rows += 1
                continue
            seen_keys.add(row_key)
            writer.writerow(row)
            kept_rows += 1
        return kept_rows, skipped_rows


def main() -> int:
    args = parse_args()
    input_2026 = Path(args.input_2026)
    input_2024 = Path(args.input_2024)
    output_path = Path(args.output)

    validate_input(input_2026)
    validate_input(input_2024)

    print(f"Reading header from: {input_2026}")
    header_2026 = read_header(input_2026)
    print(f"Reading header from: {input_2024}")
    header_2024 = read_header(input_2024)

    if header_2026 != header_2024:
        raise ValueError(
            "Input files do not share the same columns and cannot be concatenated safely."
        )

    key_indices = build_key_indices(header_2026)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    print(f"Writing final dataset to: {output_path}")
    seen_keys: set[tuple[str, ...]] = set()
    with output_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(header_2026)
        rows_2026, duplicates_2026 = append_rows(
            writer, input_2026, header_2026, key_indices, seen_keys
        )
        rows_2024, duplicates_2024 = append_rows(
            writer, input_2024, header_2026, key_indices, seen_keys
        )

    total_rows = rows_2026 + rows_2024
    total_duplicates = duplicates_2026 + duplicates_2024
    print(f"Appended {rows_2026} rows from: {input_2026}")
    print(f"Skipped {duplicates_2026} duplicate rows from: {input_2026}")
    print(f"Appended {rows_2024} rows from: {input_2024}")
    print(f"Skipped {duplicates_2024} duplicate rows from: {input_2024}")
    print(f"Skipped {total_duplicates} duplicate rows in total")
    print(f"Wrote {total_rows} rows to: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
