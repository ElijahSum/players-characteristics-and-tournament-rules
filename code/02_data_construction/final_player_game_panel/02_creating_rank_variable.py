#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path


REQUIRED_COLUMNS = (
    "player_name",
    "opponent_name",
    "date",
    "round",
    "opponents_sum_score",
    "opponents_sum_score_end_round",
    "buchholz_score",
    "buchholz_score_end_round",
    "sonneborn_berger_score",
    "sonneborn_berger_score_end_round",
)
START_SCORE_COLUMNS = (
    "final_score_round_start",
    "final_score_before_round",
    "final_score_pregame",
)
END_SCORE_COLUMNS = ("final_score_round_end", "final_score")
RANK_COLUMN = "rank"
RANK_END_COLUMN = "rank_end_round"
LEADER_COLUMN = "leader"
IN_PRIZES_COLUMN = "in_prizes"
BUBBLE_COLUMN = "bubble"
ELIMINATED_COLUMN = "eliminated"
PLAYED_AGAINST_BUBBLE_COLUMN = "played_against_bubble"
PLAYED_AGAINST_PRIZES_COLUMN = "played_against_prizes"
PLAYED_AGAINST_ELIMINATED_COLUMN = "played_against_eliminated"
PLAYED_AGAINST_LEADER_COLUMN = "played_against_leader"
PLAYER_STATUS_COLUMNS = (
    LEADER_COLUMN,
    IN_PRIZES_COLUMN,
    BUBBLE_COLUMN,
    ELIMINATED_COLUMN,
)
OPPONENT_STATUS_COLUMNS = (
    PLAYED_AGAINST_BUBBLE_COLUMN,
    PLAYED_AGAINST_PRIZES_COLUMN,
    PLAYED_AGAINST_ELIMINATED_COLUMN,
    PLAYED_AGAINST_LEADER_COLUMN,
)
MISSING_TOKENS = {"", "NA", "NaN", "nan", "None", "null", "NULL"}


class RankBuildError(RuntimeError):
    pass


def resolve_column_name(
    fieldnames: list[str], candidates: tuple[str, ...], *, label: str
) -> str:
    for candidate in candidates:
        if candidate in fieldnames:
            return candidate
    raise RankBuildError(f"Input CSV is missing a {label} column. Tried: {', '.join(candidates)}")


def parse_half_units(raw: str, *, field: str, line_no: int) -> int:
    value = raw.strip()
    if value in MISSING_TOKENS:
        raise RankBuildError(f"Line {line_no}: required field {field!r} is missing")

    try:
        number = float(value)
    except ValueError as exc:
        raise RankBuildError(f"Line {line_no}: field {field!r} is not numeric: {raw!r}") from exc

    units = number * 2.0
    rounded = round(units)
    if abs(units - rounded) > 1e-9:
        raise RankBuildError(
            f"Line {line_no}: field {field!r} must be in 0.5 increments, got {raw!r}"
        )
    return int(rounded)


def parse_quarter_units(raw: str, *, field: str, line_no: int) -> int:
    value = raw.strip()
    if value in MISSING_TOKENS:
        raise RankBuildError(f"Line {line_no}: required field {field!r} is missing")

    try:
        number = float(value)
    except ValueError as exc:
        raise RankBuildError(f"Line {line_no}: field {field!r} is not numeric: {raw!r}") from exc

    units = number * 4.0
    rounded = round(units)
    if abs(units - rounded) > 1e-9:
        raise RankBuildError(
            f"Line {line_no}: field {field!r} must be in 0.25 increments, got {raw!r}"
        )
    return int(rounded)


def assign_competition_ranks(
    player_scores: dict[str, tuple[int, ...]],
) -> dict[str, int]:
    sorted_players = sorted(
        player_scores.items(),
        key=lambda item: tuple(-value for value in item[1]) + (item[0],),
    )

    ranks: dict[str, int] = {}
    current_rank = 1
    i = 0
    while i < len(sorted_players):
        player_name, score_key = sorted_players[i]
        tie_count = 1
        j = i + 1
        while j < len(sorted_players) and sorted_players[j][1] == score_key:
            tie_count += 1
            j += 1

        for k in range(i, j):
            tied_player = sorted_players[k][0]
            ranks[tied_player] = current_rank

        current_rank += tie_count
        i = j

    return ranks


def compute_ranks(
    rows: list[dict[str, str]],
    *,
    start_score_column: str,
    end_score_column: str,
) -> tuple[list[int], list[int]]:
    start_scores_by_group: defaultdict[tuple[str, int], dict[str, tuple[int, ...]]] = defaultdict(dict)
    end_scores_by_group: defaultdict[tuple[str, int], dict[str, tuple[int, ...]]] = defaultdict(dict)

    for row_index, row in enumerate(rows):
        line_no = row_index + 2
        try:
            round_no = int(row["round"])
        except ValueError as exc:
            raise RankBuildError(f"Line {line_no}: round is not an integer: {row['round']!r}") from exc

        date = row["date"]
        player_name = row["player_name"]

        start_score = parse_half_units(
            row[start_score_column], field=start_score_column, line_no=line_no
        )
        start_tiebreak = parse_half_units(
            row["opponents_sum_score"], field="opponents_sum_score", line_no=line_no
        )
        start_buchholz = parse_half_units(
            row["buchholz_score"], field="buchholz_score", line_no=line_no
        )
        start_sonneborn_berger = parse_quarter_units(
            row["sonneborn_berger_score"],
            field="sonneborn_berger_score",
            line_no=line_no,
        )
        end_score = parse_half_units(
            row[end_score_column], field=end_score_column, line_no=line_no
        )
        end_tiebreak = parse_half_units(
            row["opponents_sum_score_end_round"],
            field="opponents_sum_score_end_round",
            line_no=line_no,
        )
        end_buchholz = parse_half_units(
            row["buchholz_score_end_round"],
            field="buchholz_score_end_round",
            line_no=line_no,
        )
        end_sonneborn_berger = parse_quarter_units(
            row["sonneborn_berger_score_end_round"],
            field="sonneborn_berger_score_end_round",
            line_no=line_no,
        )

        group_key = (date, round_no)
        start_tuple = (start_score, start_tiebreak, start_buchholz, start_sonneborn_berger)
        end_tuple = (end_score, end_tiebreak, end_buchholz, end_sonneborn_berger)

        prev_start = start_scores_by_group[group_key].get(player_name)
        if prev_start is None:
            start_scores_by_group[group_key][player_name] = start_tuple
        elif prev_start != start_tuple:
            # Keep strongest tuple if duplicates disagree.
            start_scores_by_group[group_key][player_name] = max(prev_start, start_tuple)

        prev_end = end_scores_by_group[group_key].get(player_name)
        if prev_end is None:
            end_scores_by_group[group_key][player_name] = end_tuple
        elif prev_end != end_tuple:
            end_scores_by_group[group_key][player_name] = max(prev_end, end_tuple)

    start_rank_map: dict[tuple[str, int, str], int] = {}
    end_rank_map: dict[tuple[str, int, str], int] = {}

    for group_key, player_scores in start_scores_by_group.items():
        ranks = assign_competition_ranks(player_scores)
        date, round_no = group_key
        for player_name, rank_value in ranks.items():
            start_rank_map[(date, round_no, player_name)] = rank_value

    for group_key, player_scores in end_scores_by_group.items():
        ranks = assign_competition_ranks(player_scores)
        date, round_no = group_key
        for player_name, rank_value in ranks.items():
            end_rank_map[(date, round_no, player_name)] = rank_value

    start_ranks: list[int] = []
    end_ranks: list[int] = []
    for row_index, row in enumerate(rows):
        line_no = row_index + 2
        round_no = int(row["round"])
        key = (row["date"], round_no, row["player_name"])
        start_rank = start_rank_map.get(key)
        end_rank = end_rank_map.get(key)
        if start_rank is None or end_rank is None:
            raise RankBuildError(
                f"Line {line_no}: failed to find rank for key (date={key[0]!r}, round={key[1]}, player={key[2]!r})"
            )
        start_ranks.append(start_rank)
        end_ranks.append(end_rank)

    return start_ranks, end_ranks


def classify_rank(rank_value: int) -> tuple[bool, bool, bool, bool]:
    return (
        rank_value == 1,
        rank_value <= 6,
        7 <= rank_value <= 15,
        rank_value >= 16,
    )


def build_player_status_values(rank_value: int) -> dict[str, str]:
    is_leader, in_prizes, in_bubble, eliminated = classify_rank(rank_value)
    return {
        LEADER_COLUMN: "1" if is_leader else "0",
        IN_PRIZES_COLUMN: "1" if in_prizes else "0",
        BUBBLE_COLUMN: "1" if in_bubble else "0",
        ELIMINATED_COLUMN: "1" if eliminated else "0",
    }


def build_opponent_rank_lookup(
    rows: list[dict[str, str]],
    start_ranks: list[int],
) -> dict[tuple[str, int, str], int]:
    opponent_rank_lookup: dict[tuple[str, int, str], int] = {}

    for row_index, (row, rank_value) in enumerate(zip(rows, start_ranks)):
        line_no = row_index + 2
        try:
            round_no = int(row["round"])
        except ValueError as exc:
            raise RankBuildError(f"Line {line_no}: round is not an integer: {row['round']!r}") from exc

        key = (row["date"], round_no, row["player_name"])
        existing_rank = opponent_rank_lookup.get(key)
        if existing_rank is None:
            opponent_rank_lookup[key] = rank_value
        elif existing_rank != rank_value:
            raise RankBuildError(
                "Conflicting ranks for key "
                f"(date={key[0]!r}, round={key[1]}, player={key[2]!r})"
            )

    return opponent_rank_lookup


def build_opponent_status_values(
    row: dict[str, str],
    *,
    line_no: int,
    opponent_rank_lookup: dict[tuple[str, int, str], int],
) -> dict[str, str]:
    opponent_name = row["opponent_name"].strip()
    if opponent_name in MISSING_TOKENS:
        return {
            PLAYED_AGAINST_BUBBLE_COLUMN: "0",
            PLAYED_AGAINST_PRIZES_COLUMN: "0",
            PLAYED_AGAINST_ELIMINATED_COLUMN: "0",
            PLAYED_AGAINST_LEADER_COLUMN: "0",
        }

    try:
        round_no = int(row["round"])
    except ValueError as exc:
        raise RankBuildError(f"Line {line_no}: round is not an integer: {row['round']!r}") from exc

    opponent_key = (row["date"], round_no, opponent_name)
    opponent_rank = opponent_rank_lookup.get(opponent_key)
    if opponent_rank is None:
        raise RankBuildError(
            "Line "
            f"{line_no}: failed to find opponent rank for "
            f"(date={opponent_key[0]!r}, round={opponent_key[1]}, player={opponent_key[2]!r})"
        )

    is_leader, in_prizes, in_bubble, eliminated = classify_rank(opponent_rank)
    return {
        PLAYED_AGAINST_BUBBLE_COLUMN: "1" if in_bubble else "0",
        PLAYED_AGAINST_PRIZES_COLUMN: "1" if in_prizes else "0",
        PLAYED_AGAINST_ELIMINATED_COLUMN: "1" if eliminated else "0",
        PLAYED_AGAINST_LEADER_COLUMN: "1" if is_leader else "0",
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create rank and rank_end_round columns per tournament date/round."
    )
    parser.add_argument(
        "--input",
        default="data/players_regression_data_6_with_opponents_sum_score.csv",
        help="Path to input CSV.",
    )
    parser.add_argument(
        "--output",
        default="data/players_regression_data_6_with_opponents_sum_score_with_rank.csv",
        help="Path to output CSV.",
    )
    return parser.parse_args()


def load_rows(input_path: Path) -> tuple[list[str], list[dict[str, str]], str, str]:
    with input_path.open("r", encoding="utf-8", newline="") as src:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise RankBuildError("Input CSV has no header row.")

        missing_columns = [column for column in REQUIRED_COLUMNS if column not in reader.fieldnames]
        if missing_columns:
            raise RankBuildError(
                f"Input CSV is missing required columns: {', '.join(missing_columns)}"
            )

        rows = list(reader)
        fieldnames = list(reader.fieldnames)
        start_score_column = resolve_column_name(
            fieldnames, START_SCORE_COLUMNS, label="round-start score"
        )
        end_score_column = resolve_column_name(
            fieldnames, END_SCORE_COLUMNS, label="round-end score"
        )
        return fieldnames, rows, start_score_column, end_score_column


def build_output_columns(input_columns: list[str]) -> list[str]:
    excluded_columns = {
        RANK_COLUMN,
        RANK_END_COLUMN,
        *PLAYER_STATUS_COLUMNS,
        *OPPONENT_STATUS_COLUMNS,
    }
    return [column for column in input_columns if column not in excluded_columns] + [
        RANK_COLUMN,
        RANK_END_COLUMN,
        *PLAYER_STATUS_COLUMNS,
        *OPPONENT_STATUS_COLUMNS,
    ]


def write_rows(
    output_path: Path,
    output_columns: list[str],
    rows: list[dict[str, str]],
    start_ranks: list[int],
    end_ranks: list[int],
) -> None:
    if len(rows) != len(start_ranks) or len(rows) != len(end_ranks):
        raise RankBuildError("Rank output length does not match input row count.")

    opponent_rank_lookup = build_opponent_rank_lookup(rows, start_ranks)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8", newline="") as dst:
        writer = csv.DictWriter(dst, fieldnames=output_columns)
        writer.writeheader()
        for row_index, (row, rank_value, rank_end_value) in enumerate(
            zip(rows, start_ranks, end_ranks)
        ):
            line_no = row_index + 2
            row_out = dict(row)
            row_out[RANK_COLUMN] = str(rank_value)
            row_out[RANK_END_COLUMN] = str(rank_end_value)
            row_out.update(build_player_status_values(rank_value))
            row_out.update(
                build_opponent_status_values(
                    row,
                    line_no=line_no,
                    opponent_rank_lookup=opponent_rank_lookup,
                )
            )
            writer.writerow({column: row_out.get(column, "") for column in output_columns})


def main() -> int:
    args = parse_args()
    input_path = Path(args.input)
    output_path = Path(args.output)

    if not input_path.exists():
        raise RankBuildError(f"Input file not found: {input_path}")

    print(f"Reading input: {input_path}")
    input_columns, rows, start_score_column, end_score_column = load_rows(input_path)
    print(f"Loaded {len(rows)} rows.")

    print("Computing rank columns...")
    start_ranks, end_ranks = compute_ranks(
        rows,
        start_score_column=start_score_column,
        end_score_column=end_score_column,
    )

    print(f"Writing output: {output_path}")
    output_columns = build_output_columns(input_columns)
    write_rows(output_path, output_columns, rows, start_ranks, end_ranks)
    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
