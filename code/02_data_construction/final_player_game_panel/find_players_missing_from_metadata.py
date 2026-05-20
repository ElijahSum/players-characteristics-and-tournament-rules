#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path


DEFAULT_REGRESSION_INPUT = Path("outputs/final_regression_data_tournaments_2024_2026.csv")
DEFAULT_METADATA_INPUT = Path("data/players_final_data.csv")
DEFAULT_OUTPUT = Path("outputs/players_in_regression_missing_from_players_final_data.csv")
PLAYER_NAME_COLUMN = "player_name"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Find player names that appear in the regression dataset but are missing from "
            "players_final_data.csv."
        )
    )
    parser.add_argument(
        "--regression-input",
        default=str(DEFAULT_REGRESSION_INPUT),
        help="Path to the regression CSV.",
    )
    parser.add_argument(
        "--metadata-input",
        default=str(DEFAULT_METADATA_INPUT),
        help="Path to the players metadata CSV.",
    )
    parser.add_argument(
        "--output",
        default=str(DEFAULT_OUTPUT),
        help="Path to the output CSV of missing player names.",
    )
    return parser.parse_args()


def load_unique_player_names(csv_path: Path) -> set[str]:
    with csv_path.open("r", encoding="utf-8", newline="") as src:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise ValueError(f"CSV has no header row: {csv_path}")
        if PLAYER_NAME_COLUMN not in reader.fieldnames:
            raise ValueError(
                f"CSV is missing required column {PLAYER_NAME_COLUMN!r}: {csv_path}"
            )

        names: set[str] = set()
        for row in reader:
            raw_name = row.get(PLAYER_NAME_COLUMN, "")
            player_name = raw_name.strip()
            if player_name:
                names.add(player_name)
        return names


def write_missing_names(output_path: Path, missing_names: list[str]) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8", newline="") as dst:
        writer = csv.writer(dst)
        writer.writerow([PLAYER_NAME_COLUMN])
        for player_name in missing_names:
            writer.writerow([player_name])


def main() -> int:
    args = parse_args()
    regression_input = Path(args.regression_input)
    metadata_input = Path(args.metadata_input)
    output_path = Path(args.output)

    if not regression_input.exists():
        raise FileNotFoundError(f"Regression input not found: {regression_input}")
    if not metadata_input.exists():
        raise FileNotFoundError(f"Metadata input not found: {metadata_input}")

    print(f"Reading regression players: {regression_input}")
    regression_names = load_unique_player_names(regression_input)
    print(f"Found {len(regression_names)} unique player names in regression data.")

    print(f"Reading metadata players: {metadata_input}")
    metadata_names = load_unique_player_names(metadata_input)
    print(f"Found {len(metadata_names)} unique player names in metadata.")

    missing_names = sorted(regression_names - metadata_names, key=str.casefold)
    print(f"Found {len(missing_names)} player names missing from metadata.")

    write_missing_names(output_path, missing_names)
    print(f"Wrote missing player names to: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
