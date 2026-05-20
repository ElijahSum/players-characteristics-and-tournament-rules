#!/usr/bin/env python3
"""Build compact PGN-derived features for the remaining move mechanisms.

This intentionally does not write move-level board states. Disk is tight, so it
streams each PGN together with its matching centipawn CSV and writes only:
- game-level opening/book-depth features;
- player-game complexity/simplification/opening outcomes;
- compact phase-level first-blunder hazard rows.
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
from dataclasses import dataclass, field
from pathlib import Path

import chess
import chess.pgn


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_PGNS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "whole_dataset_2022_2024.pgn",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "whole_dataset_2024_2026.pgn",
]
DEFAULT_CPS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "centipawn_loss_nodes2000_watch" / "centipawn_loss_watch.csv",
]
DEFAULT_PLAYER_GAME_OUTCOMES = (
    ROOT / "analysis_outputs"
    / "stockfish_move_mechanisms_full_2022_2026"
    / "player_game_move_outcomes.csv"
)
DEFAULT_OUTPUT_DIR = ROOT / "analysis_outputs" / "missing_move_mechanisms_features"

PIECE_VALUES = {
    chess.PAWN: 1,
    chess.KNIGHT: 3,
    chess.BISHOP: 3,
    chess.ROOK: 5,
    chess.QUEEN: 9,
    chess.KING: 0,
}
PHASES = [
    ("opening_1_10", 1, 10),
    ("early_middlegame_11_20", 11, 20),
    ("late_middlegame_21_35", 21, 35),
    ("endgame_36_plus", 36, 10_000),
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pgn", action="append", type=Path, default=[])
    parser.add_argument("--centipawn-csv", action="append", type=Path, default=[])
    parser.add_argument("--player-game-outcomes", type=Path, default=DEFAULT_PLAYER_GAME_OUTCOMES)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--cp-loss-cap", type=float, default=1000.0)
    parser.add_argument("--high-legal-threshold", type=int, default=35)
    parser.add_argument("--high-complexity-eval-cp", type=float, default=300.0)
    parser.add_argument("--progress-every", type=int, default=25000)
    parser.add_argument("--limit-games", type=int, default=0)
    return parser.parse_args()


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


def safe_float(value: str | None, default: float = math.nan) -> float:
    try:
        return float(value) if value not in (None, "") else default
    except ValueError:
        return default


def safe_int(value: str | None, default: int = 0) -> int:
    try:
        return int(float(value)) if value not in (None, "") else default
    except ValueError:
        return default


def load_keep_player_games(path: Path) -> tuple[set[str], set[tuple[str, str]]]:
    game_ids: set[str] = set()
    player_games: set[tuple[str, str]] = set()
    with path.open("r", encoding="utf-8", newline="") as f:
        for row in csv.DictReader(f):
            game_id = row.get("game_id", "")
            color = row.get("mover_color", "")
            if game_id and color:
                game_ids.add(game_id)
                player_games.add((game_id, color))
    return game_ids, player_games


def cp_game_groups(path: Path):
    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        current_id = None
        rows: list[dict[str, str]] = []
        for row in reader:
            game_id = row.get("game_id", "")
            if current_id is None:
                current_id = game_id
            if game_id != current_id:
                yield current_id, rows
                current_id = game_id
                rows = []
            rows.append(row)
        if current_id is not None:
            yield current_id, rows


def material_white(board: chess.Board) -> int:
    total = 0
    for piece_type, value in PIECE_VALUES.items():
        if piece_type == chess.KING:
            continue
        total += value * len(board.pieces(piece_type, chess.WHITE))
        total -= value * len(board.pieces(piece_type, chess.BLACK))
    return total


def total_material(board: chess.Board) -> int:
    total = 0
    for piece_type, value in PIECE_VALUES.items():
        if piece_type == chess.KING:
            continue
        total += value * (
            len(board.pieces(piece_type, chess.WHITE)) + len(board.pieces(piece_type, chess.BLACK))
        )
    return total


def pieces_count(board: chess.Board) -> int:
    return len(board.piece_map())


def queens_on(board: chess.Board) -> int:
    return int(bool(board.pieces(chess.QUEEN, chess.WHITE)) or bool(board.pieces(chess.QUEEN, chess.BLACK)))


def parse_opening_name(eco_url: str) -> str:
    if not eco_url:
        return ""
    slug = eco_url.rstrip("/").split("/")[-1]
    slug = re.sub(r"\.\.\..*", "", slug)
    return slug.replace("-", " ")


def book_depth_from_eco_url(eco_url: str) -> int:
    if not eco_url:
        return 0
    slug = eco_url.rstrip("/").split("/")[-1]
    nums = [int(x) for x in re.findall(r"(?:^|[-.])(\d{1,2})\.", slug)]
    if not nums:
        nums = [int(x) for x in re.findall(r"(\d{1,2})\.", slug)]
    return max(nums) if nums else 0


def board_ply_features(game: chess.pgn.Game) -> dict[int, dict[str, int]]:
    board = game.board()
    out: dict[int, dict[str, int]] = {}
    for ply, move in enumerate(game.mainline_moves(), start=1):
        fullmove = board.fullmove_number
        legal_before = board.legal_moves.count()
        pieces_before = pieces_count(board)
        material_before = total_material(board)
        material_white_before = material_white(board)
        queens_before = queens_on(board)
        mover = "white" if board.turn == chess.WHITE else "black"
        board.push(move)
        pieces_after = pieces_count(board)
        out[ply] = {
            "fullmove_number": fullmove,
            "mover_color": mover,
            "legal_moves_before": legal_before,
            "pieces_before": pieces_before,
            "pieces_after": pieces_after,
            "piece_drop": max(0, pieces_before - pieces_after),
            "total_material_before": material_before,
            "total_material_after": total_material(board),
            "material_white_before": material_white_before,
            "material_white_after": material_white(board),
            "queens_before": queens_before,
            "queens_after": queens_on(board),
        }
    return out


def phase_for_move(fullmove: int) -> str:
    for name, lo, hi in PHASES:
        if lo <= fullmove <= hi:
            return name
    return "endgame_36_plus"


@dataclass
class Agg:
    moves: int = 0
    cp_sum: float = 0.0
    legal_sum: float = 0.0
    pieces_sum: float = 0.0
    material_sum: float = 0.0
    abs_material_balance_sum: float = 0.0
    eval_vol_sum: float = 0.0
    high_legal_moves: int = 0
    high_legal_cp_sum: float = 0.0
    low_legal_moves: int = 0
    low_legal_cp_sum: float = 0.0
    complex_moves: int = 0
    complex_cp_sum: float = 0.0
    complex_blunders: int = 0
    quiet_moves: int = 0
    quiet_cp_sum: float = 0.0
    post_book_moves: int = 0
    post_book_cp_sum: float = 0.0
    trades_by_move20: int = 0
    captures_by_move20: int = 0
    first_blunder_move: int | None = None
    first_mistake_move: int | None = None
    max_fullmove: int = 0
    phase_blunder: dict[str, int] = field(default_factory=lambda: defaultdict(int))

    def add(self, row: dict[str, str], feat: dict[str, int], book_exit_move: int, high_legal: int, equal_cp: float, cap: float) -> None:
        fullmove = safe_int(row.get("fullmove_number"))
        cp = min(safe_float(row.get("cp_loss"), 0.0), cap)
        eval_before = safe_float(row.get("eval_before_mover_cp"), 0.0)
        eval_after = safe_float(row.get("eval_after_mover_cp"), eval_before)
        blunder = int(safe_float(row.get("cp_loss"), 0.0) >= 200.0)
        mistake = int(safe_float(row.get("cp_loss"), 0.0) >= 100.0)

        self.moves += 1
        self.cp_sum += cp
        self.legal_sum += feat["legal_moves_before"]
        self.pieces_sum += feat["pieces_before"]
        self.material_sum += feat["total_material_before"]
        self.abs_material_balance_sum += abs(feat["material_white_before"])
        self.eval_vol_sum += abs(eval_after - eval_before)
        self.max_fullmove = max(self.max_fullmove, fullmove)

        if feat["legal_moves_before"] >= high_legal:
            self.high_legal_moves += 1
            self.high_legal_cp_sum += cp
        else:
            self.low_legal_moves += 1
            self.low_legal_cp_sum += cp

        complex_position = (
            feat["legal_moves_before"] >= high_legal
            and feat["pieces_before"] >= 18
            and abs(eval_before) <= equal_cp
        )
        if complex_position:
            self.complex_moves += 1
            self.complex_cp_sum += cp
            self.complex_blunders += blunder
        else:
            self.quiet_moves += 1
            self.quiet_cp_sum += cp

        if book_exit_move > 0 and book_exit_move < fullmove <= book_exit_move + 10:
            self.post_book_moves += 1
            self.post_book_cp_sum += cp

        if fullmove <= 20:
            self.trades_by_move20 += int(feat["piece_drop"] > 0)
            self.captures_by_move20 += safe_int(row.get("is_capture"))

        if blunder and self.first_blunder_move is None:
            self.first_blunder_move = fullmove
        if mistake and self.first_mistake_move is None:
            self.first_mistake_move = fullmove
        if blunder:
            self.phase_blunder[phase_for_move(fullmove)] = 1


def mean(total: float, count: int) -> str:
    return "" if count <= 0 else f"{total / count:.8g}"


def signed_player_eval(eval_white: float, color: str) -> float:
    return eval_white if color == "white" else -eval_white


def game_state_features(ply_features: dict[int, dict[str, int]]) -> dict[str, object]:
    if not ply_features:
        return {
            "ply_count": 0,
            "pieces_remaining_move20": "",
            "pieces_remaining_move30": "",
            "material_remaining_move30": "",
            "queens_off_by_move20": "",
            "queens_off_by_move30": "",
            "queen_trade_move": "",
        }
    last_ply = max(ply_features)
    def at_or_before(target_ply: int) -> dict[str, int]:
        chosen = max((ply for ply in ply_features if ply <= target_ply), default=last_ply)
        return ply_features[chosen]

    f20 = at_or_before(40)
    f30 = at_or_before(60)
    queen_trade_move = ""
    for ply in sorted(ply_features):
        if ply_features[ply]["queens_after"] == 0:
            queen_trade_move = math.ceil(ply / 2)
            break
    return {
        "ply_count": last_ply,
        "pieces_remaining_move20": f20["pieces_after"],
        "pieces_remaining_move30": f30["pieces_after"],
        "material_remaining_move30": f30["total_material_after"],
        "queens_off_by_move20": int(f20["queens_after"] == 0),
        "queens_off_by_move30": int(f30["queens_after"] == 0),
        "queen_trade_move": queen_trade_move,
    }


def process_game(
    game: chess.pgn.Game,
    cp_rows: list[dict[str, str]],
    keep_player_games: set[tuple[str, str]],
    opening_writer: csv.DictWriter,
    player_writer: csv.DictWriter,
    hazard_writer: csv.DictWriter,
    args: argparse.Namespace,
) -> bool:
    game_id = game_id_from_headers(game.headers)
    if not game_id:
        return False
    ply_features = board_ply_features(game)
    state = game_state_features(ply_features)
    eco = game.headers.get("ECO", "")
    eco_url = game.headers.get("ECOUrl", "")
    book_exit_move = book_depth_from_eco_url(eco_url)
    book_exit_ply = min(state["ply_count"], book_exit_move * 2) if book_exit_move else 0
    book_eval_white = ""
    if book_exit_ply:
        candidates = [r for r in cp_rows if safe_int(r.get("ply")) <= book_exit_ply]
        if candidates:
            book_eval_white = safe_float(candidates[-1].get("eval_after_white_cp"))

    opening_writer.writerow(
        {
            "game_id": game_id,
            "date": game.headers.get("UTCDate") or game.headers.get("Date", ""),
            "white": game.headers.get("White", ""),
            "black": game.headers.get("Black", ""),
            "result": game.headers.get("Result", ""),
            "eco": eco,
            "eco_family": eco[:1],
            "opening_name": parse_opening_name(eco_url),
            "eco_url": eco_url,
            "book_exit_move": book_exit_move,
            "book_exit_ply": book_exit_ply,
            "book_eval_white_cp": book_eval_white,
            **state,
        }
    )

    aggs = {"white": Agg(), "black": Agg()}
    for row in cp_rows:
        ply = safe_int(row.get("ply"))
        color = row.get("mover_color", "")
        if color not in aggs or (game_id, color) not in keep_player_games:
            continue
        feat = ply_features.get(ply)
        if feat is None:
            continue
        aggs[color].add(
            row,
            feat,
            book_exit_move,
            args.high_legal_threshold,
            args.high_complexity_eval_cp,
            args.cp_loss_cap,
        )

    for color, agg in aggs.items():
        if (game_id, color) not in keep_player_games or agg.moves == 0:
            continue
        high_minus_low = ""
        if agg.high_legal_moves > 0 and agg.low_legal_moves > 0:
            high_minus_low = f"{agg.high_legal_cp_sum / agg.high_legal_moves - agg.low_legal_cp_sum / agg.low_legal_moves:.8g}"
        complex_minus_quiet = ""
        if agg.complex_moves > 0 and agg.quiet_moves > 0:
            complex_minus_quiet = f"{agg.complex_cp_sum / agg.complex_moves - agg.quiet_cp_sum / agg.quiet_moves:.8g}"
        book_eval_player = ""
        if book_eval_white != "":
            book_eval_player = f"{signed_player_eval(float(book_eval_white), color):.8g}"
        player_writer.writerow(
            {
                "game_id": game_id,
                "mover_color": color,
                "book_exit_move": book_exit_move,
                "book_exit_ply": book_exit_ply,
                "book_eval_player_cp": book_eval_player,
                "moves_with_board_features": agg.moves,
                "mean_cp_loss_cap": mean(agg.cp_sum, agg.moves),
                "mean_legal_moves": mean(agg.legal_sum, agg.moves),
                "mean_pieces_remaining": mean(agg.pieces_sum, agg.moves),
                "mean_material_remaining": mean(agg.material_sum, agg.moves),
                "mean_abs_material_balance": mean(agg.abs_material_balance_sum, agg.moves),
                "mean_eval_volatility": mean(agg.eval_vol_sum, agg.moves),
                "high_legal_share": mean(agg.high_legal_moves, agg.moves),
                "high_legal_cp_loss": mean(agg.high_legal_cp_sum, agg.high_legal_moves),
                "low_legal_cp_loss": mean(agg.low_legal_cp_sum, agg.low_legal_moves),
                "high_minus_low_legal_cp_loss": high_minus_low,
                "complex_share": mean(agg.complex_moves, agg.moves),
                "complex_cp_loss": mean(agg.complex_cp_sum, agg.complex_moves),
                "quiet_cp_loss": mean(agg.quiet_cp_sum, agg.quiet_moves),
                "complex_minus_quiet_cp_loss": complex_minus_quiet,
                "complex_blunder_rate": mean(agg.complex_blunders, agg.complex_moves),
                "post_book_moves": agg.post_book_moves,
                "post_book_cp_loss": mean(agg.post_book_cp_sum, agg.post_book_moves),
                "trades_by_move20": agg.trades_by_move20,
                "captures_by_move20": agg.captures_by_move20,
                "first_blunder_move_pgn": agg.first_blunder_move or "",
                "first_mistake_move_pgn": agg.first_mistake_move or "",
                **state,
            }
        )

        first_blunder = agg.first_blunder_move
        for phase, lo, hi in PHASES:
            if agg.max_fullmove < lo:
                continue
            if first_blunder is not None and first_blunder < lo:
                continue
            hazard_writer.writerow(
                {
                    "game_id": game_id,
                    "mover_color": color,
                    "phase_group": phase,
                    "phase_start": lo,
                    "phase_end": min(hi, agg.max_fullmove),
                    "event_blunder": int(first_blunder is not None and lo <= first_blunder <= hi),
                }
            )
    return True


def main() -> int:
    args = parse_args()
    pgns = args.pgn or DEFAULT_PGNS
    cps = args.centipawn_csv or DEFAULT_CPS
    if len(pgns) != len(cps):
        raise ValueError("Provide the same number of --pgn and --centipawn-csv paths.")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    keep_game_ids, keep_player_games = load_keep_player_games(args.player_game_outcomes)
    start = time.perf_counter()
    summary = {
        "player_game_outcomes": str(args.player_game_outcomes),
        "keep_games": len(keep_game_ids),
        "keep_player_games": len(keep_player_games),
        "pgns": [str(p) for p in pgns],
        "centipawn_csvs": [str(p) for p in cps],
        "games_scanned": 0,
        "games_written": 0,
        "cp_groups_consumed": 0,
    }

    opening_fields = [
        "game_id", "date", "white", "black", "result", "eco", "eco_family", "opening_name",
        "eco_url", "book_exit_move", "book_exit_ply", "book_eval_white_cp", "ply_count",
        "pieces_remaining_move20", "pieces_remaining_move30", "material_remaining_move30",
        "queens_off_by_move20", "queens_off_by_move30", "queen_trade_move",
    ]
    player_fields = [
        "game_id", "mover_color", "book_exit_move", "book_exit_ply", "book_eval_player_cp",
        "moves_with_board_features", "mean_cp_loss_cap", "mean_legal_moves",
        "mean_pieces_remaining", "mean_material_remaining", "mean_abs_material_balance",
        "mean_eval_volatility", "high_legal_share", "high_legal_cp_loss", "low_legal_cp_loss",
        "high_minus_low_legal_cp_loss", "complex_share", "complex_cp_loss", "quiet_cp_loss",
        "complex_minus_quiet_cp_loss", "complex_blunder_rate", "post_book_moves",
        "post_book_cp_loss", "trades_by_move20", "captures_by_move20",
        "first_blunder_move_pgn", "first_mistake_move_pgn", "ply_count",
        "pieces_remaining_move20", "pieces_remaining_move30", "material_remaining_move30",
        "queens_off_by_move20", "queens_off_by_move30", "queen_trade_move",
    ]
    hazard_fields = ["game_id", "mover_color", "phase_group", "phase_start", "phase_end", "event_blunder"]

    with (
        (args.output_dir / "opening_game_features.csv").open("w", encoding="utf-8", newline="") as opening_f,
        (args.output_dir / "player_game_pgn_mechanism_features.csv").open("w", encoding="utf-8", newline="") as player_f,
        (args.output_dir / "phase_blunder_hazard.csv").open("w", encoding="utf-8", newline="") as hazard_f,
    ):
        opening_writer = csv.DictWriter(opening_f, fieldnames=opening_fields)
        player_writer = csv.DictWriter(player_f, fieldnames=player_fields)
        hazard_writer = csv.DictWriter(hazard_f, fieldnames=hazard_fields)
        opening_writer.writeheader()
        player_writer.writeheader()
        hazard_writer.writeheader()

        for pgn_path, cp_path in zip(pgns, cps):
            cp_iter = cp_game_groups(cp_path)
            try:
                cp_id, cp_rows = next(cp_iter)
            except StopIteration:
                continue
            with pgn_path.open("r", encoding="utf-8", errors="replace") as pgn_f:
                while True:
                    game = chess.pgn.read_game(pgn_f)
                    if game is None:
                        break
                    summary["games_scanned"] += 1
                    if args.limit_games and summary["games_scanned"] > args.limit_games:
                        break
                    game_id = game_id_from_headers(game.headers)
                    if game_id not in keep_game_ids:
                        continue
                    while cp_id != game_id:
                        try:
                            cp_id, cp_rows = next(cp_iter)
                            summary["cp_groups_consumed"] += 1
                        except StopIteration:
                            cp_id, cp_rows = "", []
                            break
                    if cp_id != game_id:
                        continue
                    if process_game(
                        game,
                        cp_rows,
                        keep_player_games,
                        opening_writer,
                        player_writer,
                        hazard_writer,
                        args,
                    ):
                        summary["games_written"] += 1
                    try:
                        cp_id, cp_rows = next(cp_iter)
                        summary["cp_groups_consumed"] += 1
                    except StopIteration:
                        cp_id, cp_rows = "", []
                    if args.progress_every and summary["games_scanned"] % args.progress_every == 0:
                        elapsed = time.perf_counter() - start
                        print(
                            f"PGN features: scanned={summary['games_scanned']:,} "
                            f"written={summary['games_written']:,} elapsed={elapsed:,.1f}s",
                            file=sys.stderr,
                        )
            if args.limit_games and summary["games_scanned"] > args.limit_games:
                break

    summary["seconds"] = time.perf_counter() - start
    (args.output_dir / "missing_mechanism_feature_summary.json").write_text(
        json.dumps(summary, indent=2), encoding="utf-8"
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
