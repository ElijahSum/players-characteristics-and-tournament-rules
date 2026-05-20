#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


DEFAULT_INPUT_CANDIDATES = (
    "outputs/new_regression_data_tournaments_2026.csv",
    "outputs/new_regression_data_tournaments_2026_with_opponents_sum_score_with_rank.csv",
)
OPPONENTS_SCORE_SCRIPT = "03_creating_opponents_score.py"
RANK_SCRIPT = "02_creating_rank_variable.py"


def default_opponents_output(input_path: Path) -> Path:
    return input_path.with_name(f"{input_path.stem}_with_opponents_sum_score{input_path.suffix}")


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
            "Run the tiebreak pipeline: calculate Buchholz Cut 1, Buchholz, and Sonneborn-Berger, "
            "then rank / rank_end_round."
        )
    )
    parser.add_argument(
        "--input",
        default=DEFAULT_INPUT_CANDIDATES[0],
        help="Path to the regression dataset.",
    )
    parser.add_argument(
        "--pairings-input",
        default="data/merged_tournaments_1_150_added_missed_links_2.csv",
        help="Path to the original pairings CSV used to recover opponent_name if needed.",
    )
    parser.add_argument(
        "--opponents-output",
        default=None,
        help="Optional path for the intermediate dataset with tiebreak columns.",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Optional path for the final dataset with rank columns.",
    )
    parser.add_argument(
        "--missing-opponent-policy",
        choices=("error", "zero"),
        default="error",
        help="Forwarded to creating_opponents_score.py.",
    )
    return parser.parse_args()


def resolve_input_path(raw_input: str) -> Path:
    input_path = Path(raw_input)
    if input_path.exists():
        return input_path

    if raw_input == DEFAULT_INPUT_CANDIDATES[0]:
        for candidate in DEFAULT_INPUT_CANDIDATES[1:]:
            candidate_path = Path(candidate)
            if candidate_path.exists():
                print(
                    "Default input not found, falling back to:",
                    candidate_path,
                    file=sys.stderr,
                )
                return candidate_path

    return input_path


def main() -> int:
    args = parse_args()
    base_dir = Path(__file__).resolve().parent
    input_path = resolve_input_path(args.input)
    pairings_path = Path(args.pairings_input)
    opponents_output = (
        Path(args.opponents_output)
        if args.opponents_output
        else default_opponents_output(input_path)
    )
    final_output = Path(args.output) if args.output else default_final_output(opponents_output)

    if not input_path.exists():
        print(f"Input file not found: {input_path}", file=sys.stderr)
        return 1
    if not pairings_path.exists():
        print(f"Pairings file not found: {pairings_path}", file=sys.stderr)
        return 1

    run_step(
        [
            sys.executable,
            str(base_dir / OPPONENTS_SCORE_SCRIPT),
            "--input",
            str(input_path),
            "--output",
            str(opponents_output),
            "--pairings-input",
            str(pairings_path),
            "--missing-opponent-policy",
            args.missing_opponent_policy,
        ]
    )
    run_step(
        [
            sys.executable,
            str(base_dir / RANK_SCRIPT),
            "--input",
            str(opponents_output),
            "--output",
            str(final_output),
        ]
    )

    print(f"Intermediate output: {opponents_output}")
    print(f"Final output: {final_output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
