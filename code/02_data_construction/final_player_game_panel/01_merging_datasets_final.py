#!/usr/bin/env python3
from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd


DEFAULT_INPUT = Path("data/merged_tournaments_1_150_added_missed_links_2.csv")
DEFAULT_PLAYERS_INPUT = Path("data/players_final_data.csv")
DEFAULT_OUTPUT = Path("outputs/new_regression_data_tournaments_2026.csv")
PLAYER_METADATA_COLUMNS = [
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
NUMERIC_METADATA_COLUMNS = [
    "classic_rating",
    "rapid_rating",
    "blitz_rating",
    "gdp_per_capita_ppp",
    "gdp_per_capita_ppp_logged",
    "birthday",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build the player-level regression dataset from the merged tournaments CSV."
    )
    parser.add_argument(
        "--input",
        default=str(DEFAULT_INPUT),
        help="Path to the merged tournaments CSV.",
    )
    parser.add_argument(
        "--players-input",
        default=str(DEFAULT_PLAYERS_INPUT),
        help="Path to the player metadata CSV.",
    )
    parser.add_argument(
        "--output",
        default=str(DEFAULT_OUTPUT),
        help="Path to the regression output CSV.",
    )
    return parser.parse_args()


def load_tournaments(input_path: Path) -> pd.DataFrame:
    tournaments = pd.read_csv(input_path)
    if len(tournaments.columns) > 0 and str(tournaments.columns[0]).startswith("Unnamed:"):
        tournaments = tournaments.iloc[:, 1:]
    return tournaments


def load_player_metadata(players_input_path: Path) -> pd.DataFrame:
    player_metadata = pd.read_csv(
        players_input_path,
        usecols=PLAYER_METADATA_COLUMNS,
    ).copy()

    for column in NUMERIC_METADATA_COLUMNS:
        player_metadata[column] = pd.to_numeric(player_metadata[column], errors="coerce")

    return player_metadata.drop_duplicates(subset=["player_name"], keep="first")


def build_regression_dataset(
    tournaments: pd.DataFrame,
    player_metadata: pd.DataFrame,
) -> pd.DataFrame:
    white_games = tournaments[
        [
            "white_name",
            "white_rating",
            "white_title",
            "accuracy_white",
            "round",
            "date",
            "black_rating",
            "result_white",
            "black_name",
        ]
    ].rename(
        columns={
            "white_name": "player_name",
            "white_rating": "player_rating",
            "white_title": "player_title",
            "accuracy_white": "player_accuracy",
            "white_female": "is_female",
            "black_rating": "opponent_rating",
            "result_white": "player_result",
            "black_name": "opponent_name",
        }
    )
    white_games["is_white"] = 1

    black_games = tournaments[
        [
            "black_name",
            "black_rating",
            "black_title",
            "accuracy_black",
            "round",
            "date",
            "white_rating",
            "result_black",
            "white_name",
        ]
    ].rename(
        columns={
            "black_name": "player_name",
            "black_rating": "player_rating",
            "black_title": "player_title",
            "accuracy_black": "player_accuracy",
            "black_female": "is_female",
            "white_rating": "opponent_rating",
            "result_black": "player_result",
            "white_name": "opponent_name",
        }
    )
    black_games["is_white"] = 0

    players_regression_data = pd.concat([white_games, black_games], ignore_index=True)
    players_regression_data["date"] = pd.to_datetime(players_regression_data["date"])
    players_regression_data["_original_order"] = np.arange(len(players_regression_data))
    players_regression_data["_round_numeric"] = pd.to_numeric(players_regression_data["round"])
    players_regression_data["player_result"] = pd.to_numeric(
        players_regression_data["player_result"]
    )

    players_regression_data = players_regression_data.sort_values(
        ["date", "player_name", "_round_numeric"]
    )
    players_regression_data["final_score"] = players_regression_data.groupby(
        ["date", "player_name"]
    )["player_result"].cumsum()
    players_regression_data["final_score_pregame"] = (
        players_regression_data["final_score"] - players_regression_data["player_result"]
    )
    players_regression_data = (
        players_regression_data.sort_values("_original_order")
        .drop(columns=["_original_order", "_round_numeric"])
        .reset_index(drop=True)
    )
    players_regression_data["round_10"] = (players_regression_data["round"] == 10).astype(int)
    players_regression_data["round_11"] = (players_regression_data["round"] == 11).astype(int)
    players_regression_data = players_regression_data.merge(
        player_metadata,
        on="player_name",
        how="left",
        validate="m:1",
    )
    return players_regression_data


def main() -> int:
    args = parse_args()
    input_path = Path(args.input)
    players_input_path = Path(args.players_input)
    output_path = Path(args.output)

    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")
    if not players_input_path.exists():
        raise FileNotFoundError(f"Player metadata file not found: {players_input_path}")

    print(f"Reading tournaments: {input_path}")
    tournaments = load_tournaments(input_path)
    print(f"Reading player metadata: {players_input_path}")
    player_metadata = load_player_metadata(players_input_path)

    print("Building regression dataset...")
    players_regression_data = build_regression_dataset(tournaments, player_metadata)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    players_regression_data.to_csv(output_path, index=False)
    print(f"Wrote {len(players_regression_data)} rows to: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
