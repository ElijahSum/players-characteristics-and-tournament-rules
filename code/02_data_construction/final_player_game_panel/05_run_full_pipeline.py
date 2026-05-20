#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


DEFAULT_INPUT = "data/merged_tournaments_1_150_added_missed_links_3.csv"
DEFAULT_PLAYERS_INPUT = "data/players_final_data.csv"
DEFAULT_REGRESSION_OUTPUT = "outputs/final_regression_data_tournaments_2026.csv"
STEP_01_SCRIPT = "01_merging_datasets_final.py"
STEP_04_SCRIPT = "04_run_buchholz_cut1_pipeline.py"


def default_opponents_output(regression_output: Path) -> Path:
    return regression_output.with_name(
        f"{regression_output.stem}_with_opponents_sum_score{regression_output.suffix}"
    )


def default_final_output(opponents_output: Path) -> Path:
    return opponents_output.with_name(
        f"{opponents_output.stem}_with_rank{opponents_output.suffix}"
    )


def run_step(command: list[str]) -> None:
    print("Running:", " ".join(command))
    subprocess.run(command, check=True)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run the full numbered pipeline from merged tournaments input to the final "
            "ranked dataset in outputs/. The execution order is 01 -> 03 -> 02 because "
            "rank creation depends on the tiebreak columns from step 03."
        )
    )
    parser.add_argument(
        "--input",
        default=DEFAULT_INPUT,
        help="Path to the merged tournaments CSV.",
    )
    parser.add_argument(
        "--players-input",
        default=DEFAULT_PLAYERS_INPUT,
        help="Path to the player metadata CSV used by 01_merging_datasets_final.py.",
    )
    parser.add_argument(
        "--regression-output",
        default=DEFAULT_REGRESSION_OUTPUT,
        help="Path to the intermediate regression dataset written by step 01.",
    )
    parser.add_argument(
        "--opponents-output",
        default=None,
        help="Optional path for the intermediate dataset written after step 03.",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Optional path for the final dataset written after step 02.",
    )
    parser.add_argument(
        "--missing-opponent-policy",
        choices=("error", "zero"),
        default="error",
        help="Forwarded to 03_creating_opponents_score.py.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    base_dir = Path(__file__).resolve().parent
    input_path = Path(args.input)
    players_input_path = Path(args.players_input)
    regression_output = Path(args.regression_output)
    opponents_output = (
        Path(args.opponents_output)
        if args.opponents_output
        else default_opponents_output(regression_output)
    )
    final_output = Path(args.output) if args.output else default_final_output(opponents_output)

    if not input_path.exists():
        print(f"Input file not found: {input_path}", file=sys.stderr)
        return 1
    if not players_input_path.exists():
        print(f"Player metadata file not found: {players_input_path}", file=sys.stderr)
        return 1

    run_step(
        [
            sys.executable,
            str(base_dir / STEP_01_SCRIPT),
            "--input",
            str(input_path),
            "--players-input",
            str(players_input_path),
            "--output",
            str(regression_output),
        ]
    )
    run_step(
        [
            sys.executable,
            str(base_dir / STEP_04_SCRIPT),
            "--input",
            str(regression_output),
            "--pairings-input",
            str(input_path),
            "--opponents-output",
            str(opponents_output),
            "--output",
            str(final_output),
            "--missing-opponent-policy",
            args.missing_opponent_policy,
        ]
    )

    print(f"Regression output: {regression_output}")
    print(f"Intermediate output: {opponents_output}")
    print(f"Final output: {final_output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
