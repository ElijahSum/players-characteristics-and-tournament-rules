#!/usr/bin/env python3
from __future__ import annotations

import csv
import os
from collections import Counter, defaultdict
from pathlib import Path


PLAYERS = Path("data/players_final_data_merged.csv")
REGRESSION = Path("data/final_regression_data_tournaments_2022_2026.csv")
TMP = REGRESSION.with_suffix(".csv.tmp")

PLAYER_ATTRS = [
    "real_name",
    "classic_rating",
    "rapid_rating",
    "blitz_rating",
    "federation",
    "country_name",
    "gdp_per_capita_ppp",
    "gdp_per_capita_ppp_logged",
    "birthday",
    "player_title_values",
    "female",
]

PLACEHOLDERS = {"", "Not found", "nan", "NaN", "None"}


def nonblank(value: str | None) -> bool:
    return (value or "").strip() not in PLACEHOLDERS


def choose_value(values: list[str]) -> str:
    non_empty = [value for value in values if nonblank(value)]
    if not non_empty:
        return ""
    counts = Counter(non_empty)
    return counts.most_common(1)[0][0]


def load_player_lookup() -> dict[str, dict[str, str]]:
    grouped: dict[str, list[dict[str, str]]] = defaultdict(list)
    with PLAYERS.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        required = ["player_name", *[field for field in PLAYER_ATTRS if field != "female"]]
        missing = [field for field in required if field not in (reader.fieldnames or [])]
        if missing:
            raise ValueError(f"{PLAYERS} is missing expected columns: {missing}")
        for row in reader:
            if "female" not in row:
                row["female"] = row.get("gender", "")
            grouped[row["player_name"]].append(row)

    lookup: dict[str, dict[str, str]] = {}
    for player, rows in grouped.items():
        lookup[player] = {
            field: choose_value([row.get(field, "") for row in rows])
            for field in PLAYER_ATTRS
        }
    return lookup


def output_fieldnames(input_fields: list[str]) -> list[str]:
    base = [field for field in input_fields if field not in PLAYER_ATTRS]
    insert_at = len(base)
    if "round_11" in base:
        insert_at = base.index("round_11") + 1
    elif "player_title" in base:
        insert_at = base.index("player_title") + 1
    return base[:insert_at] + PLAYER_ATTRS + base[insert_at:]


def main() -> None:
    lookup = load_player_lookup()
    rows_written = 0
    missing_players: Counter[str] = Counter()

    with REGRESSION.open(newline="", encoding="utf-8") as in_fh:
        reader = csv.DictReader(in_fh)
        input_fields = reader.fieldnames or []
        if "player_name" not in input_fields:
            raise ValueError(f"{REGRESSION} has no player_name column")
        fields = output_fieldnames(input_fields)

        with TMP.open("w", newline="", encoding="utf-8") as out_fh:
            writer = csv.DictWriter(out_fh, fieldnames=fields)
            writer.writeheader()
            for row in reader:
                player = row.get("player_name", "")
                attrs = lookup.get(player)
                if attrs is None:
                    missing_players[player] += 1
                    attrs = {field: "" for field in PLAYER_ATTRS}

                output_row = {field: row.get(field, "") for field in fields if field not in PLAYER_ATTRS}
                output_row.update(attrs)
                writer.writerow(output_row)
                rows_written += 1

    os.replace(TMP, REGRESSION)
    print(f"rows_written={rows_written}")
    print(f"players_in_lookup={len(lookup)}")
    print(f"missing_player_rows={sum(missing_players.values())}")
    print(f"missing_unique_players={len(missing_players)}")
    if missing_players:
        print(f"missing_player_sample={missing_players.most_common(10)}")
    print(f"wrote={REGRESSION}")


if __name__ == "__main__":
    main()
