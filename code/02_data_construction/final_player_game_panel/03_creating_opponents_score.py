#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import sys
from collections import defaultdict
from pathlib import Path
from typing import DefaultDict


TARGET_COLUMN = "opponents_sum_score"
TARGET_COLUMN_END = "opponents_sum_score_end_round"
BUCHHOLZ_COLUMN = "buchholz_score"
BUCHHOLZ_COLUMN_END = "buchholz_score_end_round"
SONNEBORN_BERGER_COLUMN = "sonneborn_berger_score"
SONNEBORN_BERGER_COLUMN_END = "sonneborn_berger_score_end_round"
REQUIRED_COLUMNS = ("date", "player_name", "round", "player_result")
PAIRINGS_REQUIRED_COLUMNS = ("white_name", "black_name", "date", "round")
SCORE_BEFORE_COLUMNS = (
    "final_score_before_round",
    "final_score_round_start",
    "final_score_pregame",
)
SCORE_END_COLUMNS = ("final_score_round_end", "final_score")
MISSING_TOKENS = {"", "NA", "NaN", "nan", "None", "null", "NULL"}


class DataIntegrityError(RuntimeError):
    pass


def parse_half_point_units(raw_value: str, line_no: int, field_name: str) -> int:
    try:
        value = float(raw_value)
    except ValueError as exc:
        raise DataIntegrityError(
            f"Line {line_no}: {field_name} is not numeric: {raw_value!r}"
        ) from exc

    units = value * 2.0
    rounded = round(units)
    if abs(units - rounded) > 1e-9:
        raise DataIntegrityError(
            "Line "
            f"{line_no}: {field_name} must be in 0.5 increments, got {raw_value!r}"
        )
    return int(rounded)


def parse_optional_half_point_units(raw_value: str, line_no: int, field_name: str) -> int | None:
    if raw_value.strip() in MISSING_TOKENS:
        return None
    return parse_half_point_units(raw_value, line_no, field_name)


def format_half_point_units(units: int) -> str:
    if units % 2 == 0:
        return str(units // 2)
    return f"{units / 2:.1f}"


def format_quarter_point_units(units: int) -> str:
    if units % 4 == 0:
        return str(units // 4)
    value = units / 4
    text = f"{value:.2f}"
    if text.endswith("00"):
        return text[:-3]
    if text.endswith("0"):
        return text[:-1]
    return text


def resolve_output_path(input_path: Path, output_path: str | None) -> Path:
    if output_path:
        return Path(output_path)
    return input_path.with_name(f"{input_path.stem}_with_opponents_sum_score{input_path.suffix}")


def resolve_column_name(
    fieldnames: list[str], candidates: tuple[str, ...], *, label: str, required: bool
) -> str | None:
    for candidate in candidates:
        if candidate in fieldnames:
            return candidate
    if required:
        raise DataIntegrityError(
            f"Input CSV is missing a {label} column. Tried: {', '.join(candidates)}"
        )
    return None


def parse_is_white(raw_value: str, line_no: int) -> str:
    value = raw_value.strip().lower()
    if value in {"1", "true", "t", "white"}:
        return "1"
    if value in {"0", "false", "f", "black"}:
        return "0"
    raise DataIntegrityError(f"Line {line_no}: is_white must be one of 0/1/true/false, got {raw_value!r}")


def load_pairings_lookup(pairings_path: Path) -> dict[tuple[str, int, str, str], str]:
    if not pairings_path.exists():
        raise DataIntegrityError(f"Pairings file not found: {pairings_path}")

    lookup: dict[tuple[str, int, str, str], str] = {}
    with pairings_path.open("r", encoding="utf-8", newline="") as src:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise DataIntegrityError(f"Pairings CSV has no header row: {pairings_path}")

        missing_columns = [
            column for column in PAIRINGS_REQUIRED_COLUMNS if column not in reader.fieldnames
        ]
        if missing_columns:
            raise DataIntegrityError(
                f"Pairings CSV is missing required columns: {', '.join(missing_columns)}"
            )

        for row_index, row in enumerate(reader):
            line_no = row_index + 2
            try:
                round_no = int(row["round"])
            except ValueError as exc:
                raise DataIntegrityError(
                    f"Pairings line {line_no}: round is not an integer: {row['round']!r}"
                ) from exc

            date = sys.intern(row["date"])
            white_name = sys.intern(row["white_name"])
            black_name = sys.intern(row["black_name"])

            white_key = (date, round_no, white_name, "1")
            black_key = (date, round_no, black_name, "0")

            existing_white = lookup.get(white_key)
            if existing_white is not None and existing_white != black_name:
                raise DataIntegrityError(
                    "Pairings lookup collision for "
                    f"(date={date!r}, round={round_no}, player={white_name!r}, is_white=1)"
                )
            lookup[white_key] = black_name

            existing_black = lookup.get(black_key)
            if existing_black is not None and existing_black != white_name:
                raise DataIntegrityError(
                    "Pairings lookup collision for "
                    f"(date={date!r}, round={round_no}, player={black_name!r}, is_white=0)"
                )
            lookup[black_key] = white_name

    return lookup


def load_indexes(
    input_path: Path,
    pairings_lookup: dict[tuple[str, int, str, str], str] | None,
) -> tuple[
    list[str],
    dict[tuple[str, str, int], int],
    dict[tuple[str, str, int], int],
    DefaultDict[tuple[str, str], list[tuple[int, str, int, int]]],
    int,
]:
    score_before_by_key: dict[tuple[str, str, int], int] = {}
    score_end_by_key: dict[tuple[str, str, int], int] = {}
    games_by_player: DefaultDict[tuple[str, str], list[tuple[int, str, int, int]]] = defaultdict(list)

    with input_path.open("r", encoding="utf-8", newline="") as src:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            raise DataIntegrityError("Input CSV has no header row.")

        missing_columns = [column for column in REQUIRED_COLUMNS if column not in reader.fieldnames]
        if missing_columns:
            raise DataIntegrityError(
                f"Input CSV is missing required columns: {', '.join(missing_columns)}"
            )
        score_before_column = resolve_column_name(
            list(reader.fieldnames),
            SCORE_BEFORE_COLUMNS,
            label="round-start score",
            required=True,
        )
        score_end_column = resolve_column_name(
            list(reader.fieldnames),
            SCORE_END_COLUMNS,
            label="round-end score",
            required=False,
        )
        opponent_name_column = resolve_column_name(
            list(reader.fieldnames),
            ("opponent_name",),
            label="opponent_name",
            required=False,
        )
        if opponent_name_column is None and "is_white" not in reader.fieldnames:
            raise DataIntegrityError(
                "Input CSV must contain opponent_name or is_white so opponents can be resolved."
            )

        row_count = 0
        for row_index, row in enumerate(reader):
            line_no = row_index + 2

            raw_round = row["round"]
            try:
                round_no = int(raw_round)
            except ValueError as exc:
                raise DataIntegrityError(
                    f"Line {line_no}: round is not an integer: {raw_round!r}"
                ) from exc

            date = sys.intern(row["date"])
            player_name = sys.intern(row["player_name"])
            if opponent_name_column is not None and row[opponent_name_column].strip() not in MISSING_TOKENS:
                opponent_name = sys.intern(row[opponent_name_column])
            else:
                if pairings_lookup is None:
                    raise DataIntegrityError(
                        "Input CSV is missing opponent_name values and no pairings lookup is available."
                    )
                is_white = parse_is_white(row["is_white"], line_no)
                lookup_key = (date, round_no, player_name, is_white)
                opponent_name_raw = pairings_lookup.get(lookup_key)
                if opponent_name_raw is None:
                    raise DataIntegrityError(
                        "Failed to resolve opponent_name from pairings lookup for "
                        f"(date={date!r}, round={round_no}, player={player_name!r}, is_white={is_white})."
                    )
                opponent_name = sys.intern(opponent_name_raw)
            score_before_units = parse_half_point_units(
                row[score_before_column], line_no, score_before_column
            )
            result_units = parse_half_point_units(row["player_result"], line_no, "player_result")
            if result_units not in (0, 1, 2):
                raise DataIntegrityError(
                    f"Line {line_no}: player_result must be one of 0, 0.5, 1, got {row['player_result']!r}"
                )
            score_end_units: int | None = None
            if score_end_column is not None:
                score_end_units = parse_optional_half_point_units(
                    row[score_end_column], line_no, score_end_column
                )
            if score_end_units is None:
                score_end_units = score_before_units + result_units

            key = (date, player_name, round_no)
            prev_before = score_before_by_key.get(key)
            prev_end = score_end_by_key.get(key)
            if prev_before is None:
                score_before_by_key[key] = score_before_units
                score_end_by_key[key] = score_end_units
            else:
                # Keep the larger score snapshot for robustness if duplicates disagree.
                score_before_by_key[key] = max(prev_before, score_before_units)
                score_end_by_key[key] = max(prev_end, score_end_units)

            games_by_player[(date, player_name)].append(
                (round_no, opponent_name, row_index, result_units)
            )
            row_count += 1

    return list(reader.fieldnames), score_before_by_key, score_end_by_key, games_by_player, row_count


def compute_tiebreak_scores(
    score_before_by_key: dict[tuple[str, str, int], int],
    score_end_by_key: dict[tuple[str, str, int], int],
    games_by_player: DefaultDict[tuple[str, str], list[tuple[int, str, int, int]]],
    row_count: int,
    missing_opponent_policy: str,
) -> tuple[list[int], list[int], list[int], list[int], list[int], list[int]]:
    # Chess.com Buchholz/Cut-1 uses opponents' final tournament score.
    # Build final score per (date, player) from the player's last available round.
    final_score_by_player: dict[tuple[str, str], int] = {}
    for date_player, games in games_by_player.items():
        if not games:
            continue
        last_round = max(game[0] for game in games)
        date, player_name = date_player
        key = (date, player_name, last_round)
        final_score = score_end_by_key.get(key)
        if final_score is None:
            raise DataIntegrityError(
                f"Missing final score for date/player/round key: {date!r}, {player_name!r}, {last_round}"
            )
        final_score_by_player[date_player] = final_score

    results_before: list[int | None] = [None] * row_count
    results_end: list[int | None] = [None] * row_count
    buchholz_before: list[int | None] = [None] * row_count
    buchholz_end: list[int | None] = [None] * row_count
    sb_before: list[int | None] = [None] * row_count
    sb_end: list[int | None] = [None] * row_count
    missing_count = 0
    missing_examples: list[tuple[str, str, int, str]] = []

    for (date, player_name), games in games_by_player.items():
        games.sort(key=lambda item: item[0])
        games_by_round: DefaultDict[int, list[tuple[str, int, int]]] = defaultdict(list)
        for round_no, opponent_name, row_index, result_units in games:
            games_by_round[round_no].append((opponent_name, row_index, result_units))

        prior_opponent_final_scores: list[int] = []
        prior_sum = 0
        prior_sonneborn_berger = 0

        for round_no in sorted(games_by_round):
            current_round_games = games_by_round[round_no]
            buchholz_before_value = prior_sum
            sb_before_value = prior_sonneborn_berger
            if len(prior_opponent_final_scores) < 2:
                cut1_before_value = 0
            else:
                cut1_before_value = prior_sum - min(prior_opponent_final_scores)

            round_history_candidate: int | None = None
            round_sb_candidate: int | None = None
            for opponent_name, row_index, result_units in current_round_games:
                results_before[row_index] = cut1_before_value
                buchholz_before[row_index] = buchholz_before_value
                sb_before[row_index] = sb_before_value

                opponent_final_score = final_score_by_player.get((date, opponent_name))
                if opponent_final_score is None:
                    if missing_opponent_policy == "error":
                        missing_count += 1
                        if len(missing_examples) < 20:
                            missing_examples.append((date, player_name, round_no, opponent_name))
                        opponent_final_score = 0
                    if missing_opponent_policy == "zero":
                        opponent_final_score = 0
                    elif missing_opponent_policy != "error":
                        raise DataIntegrityError(
                            f"Unsupported missing_opponent_policy: {missing_opponent_policy!r}"
                        )

                buchholz_end[row_index] = prior_sum + opponent_final_score
                sb_contribution = opponent_final_score * result_units
                sb_end[row_index] = prior_sonneborn_berger + sb_contribution
                end_count = len(prior_opponent_final_scores) + 1
                if end_count < 2:
                    results_end[row_index] = 0
                else:
                    results_end[row_index] = prior_sum + opponent_final_score - min(
                        min(prior_opponent_final_scores), opponent_final_score
                    )

                if round_history_candidate is None:
                    round_history_candidate = opponent_final_score
                if round_sb_candidate is None:
                    round_sb_candidate = sb_contribution

            if round_history_candidate is not None:
                prior_opponent_final_scores.append(round_history_candidate)
                prior_sum += round_history_candidate
            if round_sb_candidate is not None:
                prior_sonneborn_berger += round_sb_candidate

    if missing_count > 0:
        sample_text = "\n".join(
            f"  date={date}, player={player}, round={round_no}, missing_opponent={opponent}"
            for date, player, round_no, opponent in missing_examples
        )
        raise DataIntegrityError(
            f"Missing opponent score rows for {missing_count} games.\nSample missing keys:\n{sample_text}"
        )

    if any(value is None for value in results_before):
        raise DataIntegrityError("Internal error: some rows did not receive opponents_sum_score.")
    if any(value is None for value in results_end):
        raise DataIntegrityError(
            "Internal error: some rows did not receive opponents_sum_score_end_round."
        )
    if any(value is None for value in buchholz_before):
        raise DataIntegrityError("Internal error: some rows did not receive buchholz_score.")
    if any(value is None for value in buchholz_end):
        raise DataIntegrityError("Internal error: some rows did not receive buchholz_score_end_round.")
    if any(value is None for value in sb_before):
        raise DataIntegrityError("Internal error: some rows did not receive sonneborn_berger_score.")
    if any(value is None for value in sb_end):
        raise DataIntegrityError(
            "Internal error: some rows did not receive sonneborn_berger_score_end_round."
        )

    return (
        [int(value) for value in results_before],
        [int(value) for value in results_end],
        [int(value) for value in buchholz_before],
        [int(value) for value in buchholz_end],
        [int(value) for value in sb_before],
        [int(value) for value in sb_end],
    )


def write_output(
    input_path: Path,
    output_path: Path,
    fieldnames: list[str],
    cut1_scores_before: list[int],
    cut1_scores_end: list[int],
    buchholz_scores_before: list[int],
    buchholz_scores_end: list[int],
    sb_scores_before: list[int],
    sb_scores_end: list[int],
) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames_out = [
        column
        for column in fieldnames
        if column not in (TARGET_COLUMN, TARGET_COLUMN_END, BUCHHOLZ_COLUMN, BUCHHOLZ_COLUMN_END)
        and column not in (SONNEBORN_BERGER_COLUMN, SONNEBORN_BERGER_COLUMN_END)
    ] + [
        TARGET_COLUMN,
        TARGET_COLUMN_END,
        BUCHHOLZ_COLUMN,
        BUCHHOLZ_COLUMN_END,
        SONNEBORN_BERGER_COLUMN,
        SONNEBORN_BERGER_COLUMN_END,
    ]

    with (
        input_path.open("r", encoding="utf-8", newline="") as src,
        output_path.open("w", encoding="utf-8", newline="") as dst,
    ):
        reader = csv.DictReader(src)
        writer = csv.DictWriter(dst, fieldnames=fieldnames_out)
        writer.writeheader()

        written_rows = 0
        for row_index, row in enumerate(reader):
            row[TARGET_COLUMN] = format_half_point_units(cut1_scores_before[row_index])
            row[TARGET_COLUMN_END] = format_half_point_units(cut1_scores_end[row_index])
            row[BUCHHOLZ_COLUMN] = format_half_point_units(buchholz_scores_before[row_index])
            row[BUCHHOLZ_COLUMN_END] = format_half_point_units(buchholz_scores_end[row_index])
            row[SONNEBORN_BERGER_COLUMN] = format_quarter_point_units(sb_scores_before[row_index])
            row[SONNEBORN_BERGER_COLUMN_END] = format_quarter_point_units(sb_scores_end[row_index])
            writer.writerow({column: row.get(column, "") for column in fieldnames_out})
            written_rows += 1

    if (
        written_rows != len(cut1_scores_before)
        or written_rows != len(cut1_scores_end)
        or written_rows != len(buchholz_scores_before)
        or written_rows != len(buchholz_scores_end)
        or written_rows != len(sb_scores_before)
        or written_rows != len(sb_scores_end)
    ):
        raise DataIntegrityError(
            "Row count mismatch while writing output: "
            f"written={written_rows}, expected_before={len(cut1_scores_before)}, "
            f"expected_end={len(cut1_scores_end)}, "
            f"expected_buchholz_before={len(buchholz_scores_before)}, "
            f"expected_buchholz_end={len(buchholz_scores_end)}, "
            f"expected_sb_before={len(sb_scores_before)}, "
            f"expected_sb_end={len(sb_scores_end)}"
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Add Buchholz Cut 1, Buchholz, and Sonneborn-Berger tiebreak columns to a tournament CSV "
            "using opponents' final tournament scores."
        )
    )
    parser.add_argument(
        "--input",
        default="data/players_regression_data_test_with_banned_plus_federations_5.csv",
        help="Path to input CSV file.",
    )
    parser.add_argument(
        "--output",
        default=None,
        help=(
            "Path to output CSV file. "
            "If omitted, writes <input_stem>_with_opponents_sum_score.csv next to the input."
        ),
    )
    parser.add_argument(
        "--pairings-input",
        default="data/merged_tournaments_1_150_added_missed_links.csv",
        help=(
            "Path to the original pairings CSV. Used only when the input dataset does not "
            "contain opponent_name."
        ),
    )
    parser.add_argument(
        "--missing-opponent-policy",
        choices=("error", "zero"),
        default="error",
        help=(
            "How to handle games where (date, opponent_name, round) row is missing. "
            "'error' fails fast, 'zero' uses opponent score 0."
        ),
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    input_path = Path(args.input)
    output_path = resolve_output_path(input_path, args.output)
    pairings_path = Path(args.pairings_input)

    if not input_path.exists():
        print(f"Input file not found: {input_path}", file=sys.stderr)
        return 1

    print(f"Reading and indexing: {input_path}")
    try:
        pairings_lookup = load_pairings_lookup(pairings_path)
        fieldnames, score_before_by_key, score_end_by_key, games_by_player, row_count = load_indexes(
            input_path,
            pairings_lookup=pairings_lookup,
        )
        print(
            "Indexed "
            f"{row_count} rows across {len(games_by_player)} (date, player) groups."
        )

        print(
            "Computing opponents_sum_score / opponents_sum_score_end_round "
            "and buchholz_score / buchholz_score_end_round "
            "and sonneborn_berger_score / sonneborn_berger_score_end_round..."
        )
        (
            cut1_scores_before,
            cut1_scores_end,
            buchholz_scores_before,
            buchholz_scores_end,
            sb_scores_before,
            sb_scores_end,
        ) = compute_tiebreak_scores(
            score_before_by_key=score_before_by_key,
            score_end_by_key=score_end_by_key,
            games_by_player=games_by_player,
            row_count=row_count,
            missing_opponent_policy=args.missing_opponent_policy,
        )

        print(f"Writing output: {output_path}")
        write_output(
            input_path=input_path,
            output_path=output_path,
            fieldnames=fieldnames,
            cut1_scores_before=cut1_scores_before,
            cut1_scores_end=cut1_scores_end,
            buchholz_scores_before=buchholz_scores_before,
            buchholz_scores_end=buchholz_scores_end,
            sb_scores_before=sb_scores_before,
            sb_scores_end=sb_scores_end,
        )
    except DataIntegrityError as exc:
        print(f"Data integrity error: {exc}", file=sys.stderr)
        return 1

    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
