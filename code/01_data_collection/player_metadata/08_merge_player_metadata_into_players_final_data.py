#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path


DEFAULT_EXISTING_INPUT = Path("data/players_final_data.csv")
DEFAULT_NEW_INPUT = Path("outputs/player_metadata_from_fide_missing_players.csv")
DEFAULT_OUTPUT = Path("outputs/players_final_data_merged.csv")
OUTPUT_COLUMNS = [
    "player_name",
    "real_name",
    "classic_rating",
    "rapid_rating",
    "blitz_rating",
    "federation",
    "country_name",
    "gdp_per_capita_ppp",
    "gdp_per_capita_ppp_logged",
    "birthday",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Merge newly created player metadata into players_final_data.csv and write "
            "a unified output CSV."
        )
    )
    parser.add_argument(
        "--existing-input",
        type=Path,
        default=DEFAULT_EXISTING_INPUT,
        help=f"Path to the existing players_final_data CSV (default: {DEFAULT_EXISTING_INPUT})",
    )
    parser.add_argument(
        "--new-input",
        type=Path,
        default=DEFAULT_NEW_INPUT,
        help=f"Path to the newly created metadata CSV (default: {DEFAULT_NEW_INPUT})",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"Path to the merged output CSV (default: {DEFAULT_OUTPUT})",
    )
    return parser.parse_args()


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8-sig", newline="") as src:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise ValueError(f"CSV has no header row: {path}")
        rows: list[dict[str, str]] = []
        for row in reader:
            cleaned = {}
            for key, value in row.items():
                if key is None or not key.strip():
                    continue
                cleaned[key] = value.strip() if value is not None else ""
            rows.append(cleaned)
        return rows


def pick_real_name(new_row: dict[str, str]) -> str:
    chesscom_real_name = new_row.get("chesscom_real_name", "").strip()
    if chesscom_real_name:
        return chesscom_real_name

    fide_real_name = new_row.get("real_name", "").strip()
    if "," in fide_real_name:
        left, right = [part.strip() for part in fide_real_name.split(",", 1)]
        if left and right:
            return f"{right} {left}"
    return fide_real_name


def format_birthday(new_row: dict[str, str]) -> str:
    birth_year = new_row.get("birth_year", "").strip()
    if not birth_year:
        return ""
    return f"{birth_year}.0"


def build_federation_lookup(existing_rows: list[dict[str, str]]) -> dict[str, tuple[str, str, str]]:
    lookup: dict[str, tuple[str, str, str]] = {}
    for row in existing_rows:
        federation = row.get("federation", "").strip()
        if not federation:
            continue
        payload = (
            row.get("country_name", "").strip(),
            row.get("gdp_per_capita_ppp", "").strip(),
            row.get("gdp_per_capita_ppp_logged", "").strip(),
        )
        if federation not in lookup:
            lookup[federation] = payload
    return lookup


def project_new_row(
    new_row: dict[str, str],
    federation_lookup: dict[str, tuple[str, str, str]],
) -> dict[str, str]:
    federation = new_row.get("federation", "").strip()
    country_name, gdp, gdp_logged = federation_lookup.get(federation, ("", "", ""))
    return {
        "player_name": new_row.get("player_name", "").strip(),
        "real_name": pick_real_name(new_row),
        "classic_rating": new_row.get("classic_rating", "").strip(),
        "rapid_rating": new_row.get("rapid_rating", "").strip(),
        "blitz_rating": new_row.get("blitz_rating", "").strip(),
        "federation": federation,
        "country_name": country_name,
        "gdp_per_capita_ppp": gdp,
        "gdp_per_capita_ppp_logged": gdp_logged,
        "birthday": format_birthday(new_row),
    }


def normalize_existing_row(row: dict[str, str]) -> dict[str, str]:
    return {column: row.get(column, "").strip() for column in OUTPUT_COLUMNS}


def merge_rows(
    existing_rows: list[dict[str, str]],
    new_rows: list[dict[str, str]],
) -> tuple[list[dict[str, str]], int, int]:
    federation_lookup = build_federation_lookup(existing_rows)
    merged_rows = [normalize_existing_row(row) for row in existing_rows]
    row_index_by_player = {
        row["player_name"]: index
        for index, row in enumerate(merged_rows)
        if row.get("player_name")
    }

    appended = 0
    updated = 0
    for new_row in new_rows:
        projected = project_new_row(new_row, federation_lookup)
        player_name = projected["player_name"]
        if not player_name:
            continue

        existing_index = row_index_by_player.get(player_name)
        if existing_index is None:
            merged_rows.append(projected)
            row_index_by_player[player_name] = len(merged_rows) - 1
            appended += 1
            continue

        target = merged_rows[existing_index]
        row_changed = False
        for column in OUTPUT_COLUMNS:
            if not target[column] and projected[column]:
                target[column] = projected[column]
                row_changed = True
        if row_changed:
            updated += 1

    return merged_rows, appended, updated


def write_rows(path: Path, rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as dst:
        writer = csv.DictWriter(dst, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    args = parse_args()
    if not args.existing_input.exists():
        raise FileNotFoundError(f"Existing input not found: {args.existing_input}")
    if not args.new_input.exists():
        raise FileNotFoundError(f"New input not found: {args.new_input}")

    print(f"Reading existing player metadata: {args.existing_input}")
    existing_rows = read_csv_rows(args.existing_input)
    print(f"Loaded {len(existing_rows)} existing rows.")

    print(f"Reading new player metadata: {args.new_input}")
    new_rows = read_csv_rows(args.new_input)
    print(f"Loaded {len(new_rows)} new rows.")

    merged_rows, appended, updated = merge_rows(existing_rows, new_rows)
    print(f"Appended {appended} new players.")
    print(f"Updated {updated} existing players with previously missing values.")

    write_rows(args.output, merged_rows)
    print(f"Wrote {len(merged_rows)} merged rows to: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
