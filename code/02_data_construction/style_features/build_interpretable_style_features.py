#!/usr/bin/env python3
"""Build interpretable pre-change player style features.

Inputs:
- centipawn-loss move CSV produced by calculate_centipawn_loss.py
- selected-game PGN file, used for game metadata and result/date validation

The script is streaming/chunked and avoids storing move-level data in memory.
It writes player-level features suitable for clustering playing styles.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
import sys
import time
from collections import defaultdict
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Iterable, Sequence

import chess.pgn


ROOT = Path(__file__).resolve().parents[3]
PROJECT_ROOT = ROOT
DEFAULT_METADATA = PROJECT_ROOT / "data" / "final_regression_data_tournaments_2022_2026.csv"
DEFAULT_DATASET_DIR = ROOT / "outputs" / "whole_dataset_2022_2026"
DEFAULT_CENTIPAWNS = [
    ROOT
    / "outputs"
    / "whole_dataset_2022_2024"
    / "centipawn_loss_nodes2000_watch"
    / "centipawn_loss_watch.csv",
    ROOT
    / "outputs"
    / "whole_dataset_2024_2026"
    / "centipawn_loss_nodes2000_watch"
    / "centipawn_loss_watch.csv",
]
DEFAULT_COMBINED_PGN = DEFAULT_DATASET_DIR / "whole_dataset_2022_2026.pgn"
DEFAULT_SOURCE_PGNS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "whole_dataset_2022_2024.pgn",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "whole_dataset_2024_2026.pgn",
]
DEFAULT_PGNS = [DEFAULT_COMBINED_PGN] if DEFAULT_COMBINED_PGN.exists() else DEFAULT_SOURCE_PGNS
DEFAULT_OUTPUT_DIR = DEFAULT_DATASET_DIR / "style_features"
RULE_CHANGE_DATE = datetime(2025, 9, 1)


@dataclass
class RunningStats:
    n: int = 0
    mean: float = 0.0
    m2: float = 0.0

    def add(self, value: float) -> None:
        self.n += 1
        delta = value - self.mean
        self.mean += delta / self.n
        self.m2 += delta * (value - self.mean)

    @property
    def variance(self) -> float:
        if self.n <= 1:
            return 0.0
        return self.m2 / (self.n - 1)

    @property
    def sd(self) -> float:
        return math.sqrt(max(self.variance, 0.0))


@dataclass
class PlayerAgg:
    player: str
    move_count: int = 0
    game_ids: set[str] = field(default_factory=set)
    result_sum: float = 0.0
    decisive_games: int = 0
    draws: int = 0
    game_lengths_sum: int = 0
    long_games: int = 0

    capture_moves: int = 0
    check_moves: int = 0
    blunders: int = 0
    mistakes: int = 0
    inaccuracies: int = 0
    low_cp_loss_moves: int = 0
    eval_swings: int = 0

    opening_stats: RunningStats = field(default_factory=RunningStats)
    middlegame_stats: RunningStats = field(default_factory=RunningStats)
    endgame_stats: RunningStats = field(default_factory=RunningStats)
    last10_stats: RunningStats = field(default_factory=RunningStats)
    cp_loss_stats: RunningStats = field(default_factory=RunningStats)
    cp_loss_values: list[float] = field(default_factory=list)
    first_blunder_moves: list[int] = field(default_factory=list)

    next_opp_moves: int = 0
    next_opp_blunders: int = 0
    next_opp_mistakes: int = 0

    winning_reached: int = 0
    winning_converted: int = 0
    losing_reached: int = 0
    losing_escaped: int = 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--metadata-csv", type=Path, default=DEFAULT_METADATA)
    parser.add_argument(
        "--centipawn-csv",
        dest="centipawn_csv",
        type=Path,
        action="append",
        default=None,
        help="Centipawn-loss CSV. Can be repeated. Defaults to the two 2022-2026 source windows.",
    )
    parser.add_argument(
        "--centipawn-csvs",
        default="",
        help="Comma-separated centipawn-loss CSVs; appended to --centipawn-csv values.",
    )
    parser.add_argument(
        "--pgn",
        dest="pgn",
        type=Path,
        action="append",
        default=None,
        help="PGN file. Can be repeated. Defaults to the two 2022-2026 source windows.",
    )
    parser.add_argument(
        "--pgns",
        default="",
        help="Comma-separated PGN files; appended to --pgn values.",
    )
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--rule-change-date", default="2025-09-01")
    parser.add_argument(
        "--min-total-games",
        type=int,
        default=100,
        help="Eligible players must have at least this many total player-game rows.",
    )
    parser.add_argument(
        "--min-games",
        type=int,
        default=1,
        help="Minimum pre-change games with usable Stockfish rows required to write style metrics.",
    )
    parser.add_argument("--exclude-first-fullmoves", type=int, default=10)
    parser.add_argument("--exclude-last-plies", type=int, default=10)
    parser.add_argument("--blunder-cp", type=float, default=200)
    parser.add_argument("--mistake-cp", type=float, default=100)
    parser.add_argument("--inaccuracy-cp", type=float, default=50)
    parser.add_argument("--low-cp", type=float, default=50)
    parser.add_argument("--eval-swing-cp", type=float, default=200)
    parser.add_argument("--winning-cp", type=float, default=200)
    parser.add_argument("--cp-loss-cap", type=float, default=1000)
    parser.add_argument("--progress-every", type=int, default=500000)
    parser.add_argument("--no-progress", action="store_true")
    args = parser.parse_args()
    args.centipawn_csvs = parse_path_list(args.centipawn_csv, args.centipawn_csvs, DEFAULT_CENTIPAWNS)
    args.pgns = parse_path_list(args.pgn, args.pgns, DEFAULT_PGNS)
    return args


def parse_path_list(
    repeated_paths: Sequence[Path] | None,
    comma_separated_paths: str,
    defaults: Sequence[Path],
) -> list[Path]:
    paths = [Path(path) for path in repeated_paths or []]
    for item in comma_separated_paths.split(","):
        item = item.strip()
        if item:
            paths.append(Path(item))
    return paths or list(defaults)


def game_id_from_text(value: str) -> str:
    match = re.search(r"/(?:game|analysis/game)/live/(\d+)", value or "")
    if match:
        return match.group(1)
    match = re.search(r"(\d{8,})", value or "")
    return match.group(1) if match else ""


def game_id_from_headers(headers: chess.pgn.Headers) -> str:
    for key in ("Link", "Site", "URL"):
        game_id = game_id_from_text(headers.get(key, ""))
        if game_id:
            return game_id
    return ""


def parse_pgn_date(headers: chess.pgn.Headers) -> datetime | None:
    utc_date = headers.get("UTCDate") or headers.get("Date")
    if not utc_date or "?" in utc_date:
        return None
    try:
        return datetime.strptime(utc_date, "%Y.%m.%d")
    except ValueError:
        return None


def parse_result_for_player(result: str, color: str) -> float | None:
    if result == "1-0":
        return 1.0 if color == "white" else 0.0
    if result == "0-1":
        return 0.0 if color == "white" else 1.0
    if result == "1/2-1/2":
        return 0.5
    return None


def parse_metadata_date(value: str) -> datetime:
    return datetime.fromisoformat(value.strip())


def load_eligible_players(
    metadata_csv: Path,
    rule_change_date: datetime,
    min_total_games: int,
) -> dict[str, dict[str, int]]:
    stats: dict[str, dict[str, int]] = defaultdict(lambda: {"total": 0, "pre": 0, "post": 0})
    with metadata_csv.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            player = row.get("player_name", "").strip()
            if not player:
                continue
            dt = parse_metadata_date(row["date"])
            stats[player]["total"] += 1
            if dt < rule_change_date:
                stats[player]["pre"] += 1
            else:
                stats[player]["post"] += 1
    return {
        player: values
        for player, values in stats.items()
        if values["total"] >= min_total_games and values["pre"] > 0 and values["post"] > 0
    }


def load_prechange_pgn_metadata(
    pgn_paths: Sequence[Path],
    rule_change_date: datetime,
) -> tuple[dict[str, dict[str, object]], dict[str, object]]:
    metadata: dict[str, dict[str, object]] = {}
    start = time.perf_counter()
    total_games = 0
    duplicate_game_ids = 0
    file_summaries = []
    for pgn_path in pgn_paths:
        games = 0
        prechange = 0
        missing_id_or_date = 0
        duplicates = 0
        with pgn_path.open("r", encoding="utf-8", errors="replace") as f:
            while True:
                game = chess.pgn.read_game(f)
                if game is None:
                    break
                games += 1
                total_games += 1
                game_id = game_id_from_headers(game.headers)
                game_date = parse_pgn_date(game.headers)
                if not game_id or game_date is None:
                    missing_id_or_date += 1
                    continue
                if game_date >= rule_change_date:
                    continue
                if game_id in metadata:
                    duplicates += 1
                    duplicate_game_ids += 1
                    continue
                prechange += 1
                ply_count = sum(1 for _ in game.mainline_moves())
                result = game.headers.get("Result", "")
                white = game.headers.get("White", "")
                black = game.headers.get("Black", "")
                metadata[game_id] = {
                    "date": game_date.strftime("%Y-%m-%d"),
                    "white": white,
                    "black": black,
                    "result": result,
                    "ply_count": ply_count,
                    "white_result": parse_result_for_player(result, "white"),
                    "black_result": parse_result_for_player(result, "black"),
                }
                if total_games % 50000 == 0:
                    elapsed = time.perf_counter() - start
                    print(
                        f"PGN metadata: scanned={total_games:,} unique_prechange={len(metadata):,} "
                        f"elapsed={elapsed:,.1f}s",
                        file=sys.stderr,
                    )
        file_summaries.append(
            {
                "pgn": str(pgn_path),
                "games_scanned": games,
                "unique_prechange_games_added": prechange,
                "duplicate_prechange_game_ids": duplicates,
                "missing_id_or_date": missing_id_or_date,
            }
        )
    return metadata, {
        "pgn_files": file_summaries,
        "total_games_scanned": total_games,
        "duplicate_prechange_game_ids": duplicate_game_ids,
    }


def get_agg(players: dict[str, PlayerAgg], player: str) -> PlayerAgg:
    agg = players.get(player)
    if agg is None:
        agg = PlayerAgg(player=player)
        players[player] = agg
    return agg


def safe_float(value: str | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def safe_int(value: str | None) -> int | None:
    if value is None or value == "":
        return None
    try:
        return int(float(value))
    except ValueError:
        return None


def move_phase(fullmove: int) -> str:
    if fullmove <= 10:
        return "opening"
    if fullmove <= 35:
        return "middlegame"
    return "endgame"


def initialize_game_state(meta: dict[str, object]) -> dict[str, object]:
    return {
        "ply_count": int(meta["ply_count"]),
        "white": meta["white"],
        "black": meta["black"],
        "white_result": meta["white_result"],
        "black_result": meta["black_result"],
        "white_moves": 0,
        "black_moves": 0,
        "white_first_blunder": None,
        "black_first_blunder": None,
        "white_reached_winning": False,
        "black_reached_winning": False,
        "white_reached_losing": False,
        "black_reached_losing": False,
    }


def finalize_game_state(
    players: dict[str, PlayerAgg],
    game_id: str,
    state: dict[str, object],
    eligible_players: set[str],
) -> None:
    for color in ("white", "black"):
        player = str(state[color])
        if not player or player not in eligible_players:
            continue
        agg = get_agg(players, player)
        agg.game_ids.add(game_id)
        result = state[f"{color}_result"]
        if result is not None:
            agg.result_sum += float(result)
            if float(result) == 0.5:
                agg.draws += 1
            else:
                agg.decisive_games += 1
        agg.game_lengths_sum += int(state["ply_count"])
        if int(state["ply_count"]) >= 80:
            agg.long_games += 1
        first_blunder = state[f"{color}_first_blunder"]
        if first_blunder is not None:
            agg.first_blunder_moves.append(int(first_blunder))
        if state[f"{color}_reached_winning"]:
            agg.winning_reached += 1
            if result == 1.0:
                agg.winning_converted += 1
        if state[f"{color}_reached_losing"]:
            agg.losing_reached += 1
            if result is not None and float(result) > 0:
                agg.losing_escaped += 1


def percentile(values: list[float], p: float) -> float | None:
    if not values:
        return None
    values = sorted(values)
    if len(values) == 1:
        return values[0]
    idx = (len(values) - 1) * p
    lo = math.floor(idx)
    hi = math.ceil(idx)
    if lo == hi:
        return values[lo]
    return values[lo] + (values[hi] - values[lo]) * (idx - lo)


def ratio(num: float, den: float) -> float | None:
    return num / den if den else None


def build_features(args: argparse.Namespace) -> tuple[list[dict[str, object]], dict[str, object]]:
    rule_change_date = datetime.strptime(args.rule_change_date, "%Y-%m-%d")
    eligible = load_eligible_players(args.metadata_csv, rule_change_date, args.min_total_games)
    eligible_players = set(eligible)
    print(
        f"Eligible players from metadata: {len(eligible_players):,} "
        f"(min_total_games={args.min_total_games}, pre>0, post>0)",
        file=sys.stderr,
    )
    print(
        "Loading pre-change PGN metadata from "
        + ", ".join(str(path) for path in args.pgns),
        file=sys.stderr,
    )
    pgn_meta, pgn_summary = load_prechange_pgn_metadata(args.pgns, rule_change_date)
    print(f"Loaded metadata for {len(pgn_meta):,} pre-change games", file=sys.stderr)

    players: dict[str, PlayerAgg] = {}
    current_game_id: str | None = None
    current_state: dict[str, object] | None = None
    previous_move: dict[str, object] | None = None
    rows = 0
    used_rows = 0
    skipped_post_or_unknown = 0
    skipped_no_eligible_player = 0
    skipped_duplicate_game_rows = 0
    completed_game_ids: set[str] = set()
    centipawn_file_summaries = []
    start = time.perf_counter()

    def finalize_current_game() -> None:
        nonlocal current_game_id, current_state, previous_move
        if current_game_id is not None and current_state is not None:
            finalize_game_state(players, current_game_id, current_state, eligible_players)
            completed_game_ids.add(current_game_id)
        current_game_id = None
        current_state = None
        previous_move = None

    for centipawn_csv in args.centipawn_csvs:
        file_rows_start = rows
        file_used_start = used_rows
        with centipawn_csv.open("r", encoding="utf-8", newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                rows += 1
                game_id = row.get("game_id", "")
                if game_id in completed_game_ids:
                    skipped_duplicate_game_rows += 1
                    continue
                meta = pgn_meta.get(game_id)
                if meta is None:
                    skipped_post_or_unknown += 1
                    continue
                if str(meta["white"]) not in eligible_players and str(meta["black"]) not in eligible_players:
                    skipped_no_eligible_player += 1
                    continue

                if current_game_id != game_id:
                    finalize_current_game()
                    current_game_id = game_id
                    current_state = initialize_game_state(meta)
                    previous_move = None

                assert current_state is not None
                ply = safe_int(row.get("ply"))
                fullmove = safe_int(row.get("fullmove_number"))
                cp_loss = safe_float(row.get("cp_loss"))
                eval_before = safe_float(row.get("eval_before_mover_cp"))
                eval_after = safe_float(row.get("eval_after_mover_cp"))
                color = row.get("mover_color", "")
                player = row.get("mover", "")
                if (
                    ply is None
                    or fullmove is None
                    or cp_loss is None
                    or eval_before is None
                    or eval_after is None
                    or color not in ("white", "black")
                    or not player
                ):
                    continue

                if (
                    previous_move is not None
                    and previous_move["color"] != color
                    and str(previous_move["player"]) in eligible_players
                ):
                    prev_player = str(previous_move["player"])
                    prev_agg = get_agg(players, prev_player)
                    prev_agg.next_opp_moves += 1
                    if cp_loss >= args.blunder_cp:
                        prev_agg.next_opp_blunders += 1
                    if cp_loss >= args.mistake_cp:
                        prev_agg.next_opp_mistakes += 1

                if ply <= current_state["ply_count"] - args.exclude_last_plies and eval_after >= args.winning_cp:
                    current_state[f"{color}_reached_winning"] = True
                if ply <= current_state["ply_count"] - args.exclude_last_plies and eval_after <= -args.winning_cp:
                    current_state[f"{color}_reached_losing"] = True

                if cp_loss >= args.blunder_cp and current_state[f"{color}_first_blunder"] is None:
                    current_state[f"{color}_first_blunder"] = fullmove

                previous_move = {"player": player, "color": color}

                if player not in eligible_players:
                    continue

                if fullmove <= args.exclude_first_fullmoves:
                    continue
                if ply > current_state["ply_count"] - args.exclude_last_plies:
                    continue

                used_rows += 1
                agg = get_agg(players, player)
                loss = min(cp_loss, args.cp_loss_cap)
                agg.move_count += 1
                agg.cp_loss_stats.add(loss)
                agg.cp_loss_values.append(loss)
                if int(float(row.get("is_capture") or 0)) == 1:
                    agg.capture_moves += 1
                if int(float(row.get("gives_check") or 0)) == 1:
                    agg.check_moves += 1
                if cp_loss >= args.blunder_cp:
                    agg.blunders += 1
                if cp_loss >= args.mistake_cp:
                    agg.mistakes += 1
                if cp_loss >= args.inaccuracy_cp:
                    agg.inaccuracies += 1
                if cp_loss < args.low_cp:
                    agg.low_cp_loss_moves += 1
                if abs(eval_after - eval_before) >= args.eval_swing_cp:
                    agg.eval_swings += 1

                phase = move_phase(fullmove)
                if phase == "opening":
                    agg.opening_stats.add(loss)
                elif phase == "middlegame":
                    agg.middlegame_stats.add(loss)
                else:
                    agg.endgame_stats.add(loss)
                if ply > current_state["ply_count"] - 20:
                    agg.last10_stats.add(loss)

                if args.progress_every and not args.no_progress and rows % args.progress_every == 0:
                    elapsed = time.perf_counter() - start
                    print(
                        f"Moves processed: rows={rows:,} used={used_rows:,} players={len(players):,} "
                        f"elapsed={elapsed:,.1f}s",
                        file=sys.stderr,
                    )
        finalize_current_game()
        centipawn_file_summaries.append(
            {
                "centipawn_csv": str(centipawn_csv),
                "rows_read": rows - file_rows_start,
                "rows_used_for_style_moves": used_rows - file_used_start,
            }
        )

    feature_rows: list[dict[str, object]] = []
    for player, agg in players.items():
        games = len(agg.game_ids)
        if games < args.min_games or agg.move_count == 0:
            continue
        feature_rows.append(
            {
                "player": player,
                "prechange_games": games,
                "style_move_count": agg.move_count,
                "avg_game_length_ply": ratio(agg.game_lengths_sum, games),
                "long_game_share": ratio(agg.long_games, games),
                "draw_rate": ratio(agg.draws, games),
                "decisive_game_rate": ratio(agg.decisive_games, games),
                "prechange_result_mean": ratio(agg.result_sum, games),
                "capture_rate": ratio(agg.capture_moves, agg.move_count),
                "check_rate": ratio(agg.check_moves, agg.move_count),
                "own_blunder_rate": ratio(agg.blunders, agg.move_count),
                "own_mistake_rate": ratio(agg.mistakes, agg.move_count),
                "own_inaccuracy_rate": ratio(agg.inaccuracies, agg.move_count),
                "low_cp_loss_rate": ratio(agg.low_cp_loss_moves, agg.move_count),
                "mean_cp_loss": agg.cp_loss_stats.mean,
                "sd_cp_loss": agg.cp_loss_stats.sd,
                "p90_cp_loss": percentile(agg.cp_loss_values, 0.90),
                "opening_cp_loss": agg.opening_stats.mean if agg.opening_stats.n else None,
                "middlegame_cp_loss": agg.middlegame_stats.mean if agg.middlegame_stats.n else None,
                "endgame_cp_loss": agg.endgame_stats.mean if agg.endgame_stats.n else None,
                "last10_cp_loss": agg.last10_stats.mean if agg.last10_stats.n else None,
                "no_blunder_game_rate": None,  # filled below from first blunder count
                "first_blunder_move_mean": ratio(sum(agg.first_blunder_moves), len(agg.first_blunder_moves)),
                "conversion_rate_from_plus_2": ratio(agg.winning_converted, agg.winning_reached),
                "winning_position_games": agg.winning_reached,
                "escape_rate_from_minus_2": ratio(agg.losing_escaped, agg.losing_reached),
                "losing_position_games": agg.losing_reached,
                "opponent_next_blunder_rate": ratio(agg.next_opp_blunders, agg.next_opp_moves),
                "opponent_next_mistake_rate": ratio(agg.next_opp_mistakes, agg.next_opp_moves),
                "eval_swing_rate": ratio(agg.eval_swings, agg.move_count),
            }
        )
        feature_rows[-1]["no_blunder_game_rate"] = 1 - ratio(len(agg.first_blunder_moves), games)

    summary = {
        "centipawn_csvs": [str(path) for path in args.centipawn_csvs],
        "pgns": [str(path) for path in args.pgns],
        "metadata_csv": str(args.metadata_csv),
        "rule_change_date": args.rule_change_date,
        "min_total_games": args.min_total_games,
        "eligible_players_metadata": len(eligible_players),
        "rows_read": rows,
        "rows_used_for_style_moves": used_rows,
        "rows_skipped_post_or_unknown_game": skipped_post_or_unknown,
        "rows_skipped_no_eligible_player_in_game": skipped_no_eligible_player,
        "rows_skipped_duplicate_game": skipped_duplicate_game_rows,
        "prechange_games_in_pgn": len(pgn_meta),
        "players_seen": len(players),
        "players_written_min_games": len(feature_rows),
        "min_games": args.min_games,
        "exclude_first_fullmoves": args.exclude_first_fullmoves,
        "exclude_last_plies": args.exclude_last_plies,
        "pgn_summary": pgn_summary,
        "centipawn_file_summaries": centipawn_file_summaries,
    }
    return feature_rows, summary


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    if not rows:
        path.write_text("", encoding="utf-8")
        return
    fieldnames = list(rows[0].keys())
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    start = time.perf_counter()
    feature_rows, summary = build_features(args)
    summary["seconds"] = time.perf_counter() - start
    feature_path = args.output_dir / "prechange_player_style_features.csv"
    summary_path = args.output_dir / "prechange_player_style_features_summary.json"
    write_csv(feature_path, feature_rows)
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps({**summary, "feature_csv": str(feature_path)}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
