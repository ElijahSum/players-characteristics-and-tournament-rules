#!/usr/bin/env python3
"""Build a SQLite opening explorer for Titled Tuesday games.

The database stores positions from the opening phase, their next-move
frequencies, mover ratings, game results, and common Chess.com opening labels.
By default it indexes the first 10 full moves (20 plies) of the combined
2022-2026 PGN.
"""

from __future__ import annotations

import argparse
import json
import sqlite3
import sys
import time
import urllib.parse
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

import chess
import chess.pgn


PACKAGE_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_PGN = PACKAGE_ROOT / "outputs" / "whole_dataset_2022_2026" / "whole_dataset_2022_2026.pgn"
DEFAULT_OUTPUT = PACKAGE_ROOT / "outputs" / "opening_explorer" / "titled_tuesday_openings.sqlite"


@dataclass
class MoveAgg:
    count: int = 0
    rating_sum: float = 0.0
    rating_count: int = 0
    white_wins: int = 0
    draws: int = 0
    black_wins: int = 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pgn", type=Path, default=DEFAULT_PGN)
    parser.add_argument("--output-db", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--max-plies", type=int, default=20, help="20 means first 10 full moves.")
    parser.add_argument("--flush-games", type=int, default=25000)
    parser.add_argument("--max-games", type=int, default=0, help="Optional smoke-test cap.")
    parser.add_argument("--progress-every", type=int, default=50000)
    parser.add_argument("--overwrite", action="store_true")
    return parser.parse_args()


def position_key(board: chess.Board) -> str:
    """Canonical key ignoring halfmove and fullmove counters."""
    return " ".join(board.fen().split()[:4])


def result_code(result: str) -> tuple[int, int, int]:
    if result == "1-0":
        return 1, 0, 0
    if result == "1/2-1/2":
        return 0, 1, 0
    if result == "0-1":
        return 0, 0, 1
    return 0, 0, 0


def safe_rating(value: str | None) -> int | None:
    if not value or value == "?":
        return None
    try:
        return int(float(value))
    except ValueError:
        return None


def clean_opening_name(headers: chess.pgn.Headers) -> tuple[str, str]:
    eco = headers.get("ECO", "").strip()
    eco_url = headers.get("ECOUrl", "").strip()
    if eco_url and "/openings/" in eco_url:
        slug = eco_url.rsplit("/openings/", 1)[1].strip("/")
        name = urllib.parse.unquote(slug)
        name = name.replace("-", " ")
        name = " ".join(name.split())
        return name, eco
    return "", eco


def connect_db(path: Path, overwrite: bool) -> sqlite3.Connection:
    if path.exists() and overwrite:
        path.unlink()
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA temp_store=MEMORY")
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS meta (
            key TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS move_stats (
            position_key TEXT NOT NULL,
            move_uci TEXT NOT NULL,
            move_san TEXT NOT NULL,
            count INTEGER NOT NULL,
            rating_sum REAL NOT NULL,
            rating_count INTEGER NOT NULL,
            white_wins INTEGER NOT NULL,
            draws INTEGER NOT NULL,
            black_wins INTEGER NOT NULL,
            PRIMARY KEY (position_key, move_uci)
        );
        CREATE TABLE IF NOT EXISTS opening_stats (
            position_key TEXT NOT NULL,
            opening_name TEXT NOT NULL,
            eco TEXT NOT NULL,
            count INTEGER NOT NULL,
            PRIMARY KEY (position_key, opening_name, eco)
        );
        CREATE INDEX IF NOT EXISTS idx_move_stats_position_count
            ON move_stats(position_key, count DESC);
        CREATE INDEX IF NOT EXISTS idx_opening_stats_position_count
            ON opening_stats(position_key, count DESC);
        """
    )
    return conn


def upsert_meta(conn: sqlite3.Connection, values: dict[str, object]) -> None:
    conn.executemany(
        "INSERT INTO meta(key, value) VALUES(?, ?) "
        "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
        [(key, json.dumps(value) if isinstance(value, (dict, list)) else str(value)) for key, value in values.items()],
    )


def flush(
    conn: sqlite3.Connection,
    move_aggs: dict[tuple[str, str, str], MoveAgg],
    opening_aggs: dict[tuple[str, str, str], int],
) -> tuple[int, int]:
    move_rows = [
        (
            position,
            uci,
            san,
            agg.count,
            agg.rating_sum,
            agg.rating_count,
            agg.white_wins,
            agg.draws,
            agg.black_wins,
        )
        for (position, uci, san), agg in move_aggs.items()
    ]
    opening_rows = [
        (position, opening_name, eco, count)
        for (position, opening_name, eco), count in opening_aggs.items()
        if opening_name or eco
    ]
    with conn:
        conn.executemany(
            """
            INSERT INTO move_stats(
                position_key, move_uci, move_san, count, rating_sum, rating_count,
                white_wins, draws, black_wins
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(position_key, move_uci) DO UPDATE SET
                move_san=excluded.move_san,
                count=move_stats.count + excluded.count,
                rating_sum=move_stats.rating_sum + excluded.rating_sum,
                rating_count=move_stats.rating_count + excluded.rating_count,
                white_wins=move_stats.white_wins + excluded.white_wins,
                draws=move_stats.draws + excluded.draws,
                black_wins=move_stats.black_wins + excluded.black_wins
            """,
            move_rows,
        )
        conn.executemany(
            """
            INSERT INTO opening_stats(position_key, opening_name, eco, count)
            VALUES (?, ?, ?, ?)
            ON CONFLICT(position_key, opening_name, eco) DO UPDATE SET
                count=opening_stats.count + excluded.count
            """,
            opening_rows,
        )
    move_count = len(move_rows)
    opening_count = len(opening_rows)
    move_aggs.clear()
    opening_aggs.clear()
    return move_count, opening_count


def add_opening(
    opening_aggs: dict[tuple[str, str, str], int],
    board: chess.Board,
    opening_name: str,
    eco: str,
) -> None:
    if opening_name or eco:
        opening_aggs[(position_key(board), opening_name, eco)] += 1


def build_db(args: argparse.Namespace) -> dict[str, object]:
    if not args.pgn.exists():
        raise FileNotFoundError(args.pgn)
    conn = connect_db(args.output_db, args.overwrite)
    move_aggs: dict[tuple[str, str, str], MoveAgg] = {}
    opening_aggs: dict[tuple[str, str, str], int] = defaultdict(int)

    start = time.perf_counter()
    games = 0
    games_with_moves = 0
    moves_indexed = 0
    failed_games = 0
    flushed_move_rows = 0
    flushed_opening_rows = 0

    with args.pgn.open("r", encoding="utf-8", errors="replace") as pgn_f:
        while True:
            game = chess.pgn.read_game(pgn_f)
            if game is None:
                break
            games += 1
            try:
                board = game.board()
                opening_name, eco = clean_opening_name(game.headers)
                result = game.headers.get("Result", "")
                white_wins, draws, black_wins = result_code(result)
                white_rating = safe_rating(game.headers.get("WhiteElo"))
                black_rating = safe_rating(game.headers.get("BlackElo"))

                add_opening(opening_aggs, board, opening_name, eco)
                plies = 0
                for move in game.mainline_moves():
                    if plies >= args.max_plies:
                        break
                    key = position_key(board)
                    san = board.san(move)
                    mover_rating = white_rating if board.turn == chess.WHITE else black_rating
                    agg = move_aggs.setdefault((key, move.uci(), san), MoveAgg())
                    agg.count += 1
                    if mover_rating is not None:
                        agg.rating_sum += mover_rating
                        agg.rating_count += 1
                    agg.white_wins += white_wins
                    agg.draws += draws
                    agg.black_wins += black_wins
                    board.push(move)
                    add_opening(opening_aggs, board, opening_name, eco)
                    moves_indexed += 1
                    plies += 1
                if plies:
                    games_with_moves += 1
            except Exception as exc:
                failed_games += 1
                if failed_games <= 10:
                    print(f"Skipping malformed game {games}: {exc}", file=sys.stderr)

            if args.flush_games and games % args.flush_games == 0:
                move_rows, opening_rows = flush(conn, move_aggs, opening_aggs)
                flushed_move_rows += move_rows
                flushed_opening_rows += opening_rows

            if args.progress_every and games % args.progress_every == 0:
                elapsed = time.perf_counter() - start
                print(
                    f"Opening DB: games={games:,} moves={moves_indexed:,} "
                    f"pending_move_rows={len(move_aggs):,} elapsed={elapsed:,.1f}s",
                    file=sys.stderr,
                )

            if args.max_games and games >= args.max_games:
                break

    move_rows, opening_rows = flush(conn, move_aggs, opening_aggs)
    flushed_move_rows += move_rows
    flushed_opening_rows += opening_rows
    with conn:
        upsert_meta(
            conn,
            {
                "source_pgn": str(args.pgn),
                "max_plies": args.max_plies,
                "games_read": games,
                "games_with_moves": games_with_moves,
                "moves_indexed": moves_indexed,
                "failed_games": failed_games,
                "built_at_unix": int(time.time()),
            },
        )
    conn.execute("ANALYZE")
    conn.close()
    summary = {
        "output_db": str(args.output_db),
        "source_pgn": str(args.pgn),
        "max_plies": args.max_plies,
        "games_read": games,
        "games_with_moves": games_with_moves,
        "moves_indexed": moves_indexed,
        "failed_games": failed_games,
        "flushed_move_rows": flushed_move_rows,
        "flushed_opening_rows": flushed_opening_rows,
        "seconds": time.perf_counter() - start,
    }
    summary_path = args.output_db.with_suffix(".summary.json")
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    return summary


def main() -> int:
    args = parse_args()
    if args.output_db.exists() and not args.overwrite:
        raise FileExistsError(f"{args.output_db} already exists. Pass --overwrite to rebuild.")
    summary = build_db(args)
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
