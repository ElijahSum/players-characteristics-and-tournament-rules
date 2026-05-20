#!/usr/bin/env python3
"""Add top-rated example games per opening position to the explorer database."""

from __future__ import annotations

import argparse
import heapq
import json
import sqlite3
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import chess
import chess.pgn

from build_titled_tuesday_opening_db import (
    DEFAULT_OUTPUT,
    DEFAULT_PGN,
    clean_opening_name,
    position_key,
    safe_rating,
)


@dataclass(frozen=True)
class GameMeta:
    game_id: str
    link: str
    date: str
    white: str
    black: str
    white_elo: int | None
    black_elo: int | None
    avg_elo: float
    result: str
    tournament: str
    opening_name: str
    eco: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pgn", type=Path, default=DEFAULT_PGN)
    parser.add_argument("--db", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--max-plies", type=int, default=20)
    parser.add_argument("--min-position-plies", type=int, default=2)
    parser.add_argument("--top-n", type=int, default=5)
    parser.add_argument("--max-games", type=int, default=0, help="Optional smoke-test cap.")
    parser.add_argument("--progress-every", type=int, default=50000)
    parser.add_argument("--batch-size", type=int, default=50000)
    return parser.parse_args()


def game_id_from_link(link: str, fallback: int) -> str:
    if link:
        return link.rstrip("/").rsplit("/", 1)[-1]
    return f"game_{fallback}"


def avg_rating(*ratings: int | None) -> float | None:
    present = [rating for rating in ratings if rating is not None]
    if not present:
        return None
    return sum(present) / len(present)


def game_meta(game: chess.pgn.Game, fallback_id: int) -> GameMeta | None:
    headers = game.headers
    white_elo = safe_rating(headers.get("WhiteElo"))
    black_elo = safe_rating(headers.get("BlackElo"))
    avg_elo = avg_rating(white_elo, black_elo)
    if avg_elo is None:
        return None
    opening_name, eco = clean_opening_name(headers)
    link = headers.get("Link", "").strip()
    return GameMeta(
        game_id=game_id_from_link(link, fallback_id),
        link=link,
        date=headers.get("Date", "").strip(),
        white=headers.get("White", "").strip(),
        black=headers.get("Black", "").strip(),
        white_elo=white_elo,
        black_elo=black_elo,
        avg_elo=avg_elo,
        result=headers.get("Result", "").strip(),
        tournament=headers.get("Tournament", "").strip(),
        opening_name=opening_name,
        eco=eco,
    )


def create_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS meta (
            key TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );
        DROP TABLE IF EXISTS position_top_games;
        CREATE TABLE position_top_games (
            position_key TEXT NOT NULL,
            rank INTEGER NOT NULL,
            game_id TEXT NOT NULL,
            link TEXT NOT NULL,
            date TEXT NOT NULL,
            white TEXT NOT NULL,
            black TEXT NOT NULL,
            white_elo INTEGER,
            black_elo INTEGER,
            avg_elo REAL NOT NULL,
            result TEXT NOT NULL,
            tournament TEXT NOT NULL,
            opening_name TEXT NOT NULL,
            eco TEXT NOT NULL,
            reached_ply INTEGER NOT NULL,
            PRIMARY KEY (position_key, rank)
        );
        CREATE INDEX IF NOT EXISTS idx_position_top_games_position_rank
            ON position_top_games(position_key, rank);
        """
    )


def upsert_meta(conn: sqlite3.Connection, values: dict[str, object]) -> None:
    conn.executemany(
        "INSERT INTO meta(key, value) VALUES(?, ?) "
        "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
        [(key, json.dumps(value) if isinstance(value, (dict, list)) else str(value)) for key, value in values.items()],
    )


def build_top_games(args: argparse.Namespace) -> dict[str, object]:
    if not args.pgn.exists():
        raise FileNotFoundError(args.pgn)

    start = time.perf_counter()
    games_read = 0
    games_used = 0
    positions_seen = 0
    failed_games = 0
    heaps: dict[str, list[tuple[float, int, str, int]]] = {}
    metas: dict[str, GameMeta] = {}

    with args.pgn.open("r", encoding="utf-8", errors="replace") as pgn_f:
        while True:
            game = chess.pgn.read_game(pgn_f)
            if game is None:
                break
            games_read += 1
            try:
                meta = game_meta(game, games_read)
                if meta is None:
                    continue
                metas[meta.game_id] = meta
                board = game.board()
                seen_in_game: set[str] = set()
                plies = 0
                used_game = False
                for move in game.mainline_moves():
                    if plies >= args.max_plies:
                        break
                    board.push(move)
                    plies += 1
                    if plies < args.min_position_plies:
                        continue
                    key = position_key(board)
                    if key in seen_in_game:
                        continue
                    seen_in_game.add(key)
                    record = (meta.avg_elo, max(meta.white_elo or 0, meta.black_elo or 0), meta.game_id, plies)
                    heap = heaps.setdefault(key, [])
                    if len(heap) < args.top_n:
                        heapq.heappush(heap, record)
                    elif record > heap[0]:
                        heapq.heapreplace(heap, record)
                    positions_seen += 1
                    used_game = True
                if used_game:
                    games_used += 1
            except Exception as exc:
                failed_games += 1
                if failed_games <= 10:
                    print(f"Skipping malformed game {games_read}: {exc}", file=sys.stderr)

            if args.progress_every and games_read % args.progress_every == 0:
                elapsed = time.perf_counter() - start
                print(
                    f"Top games: games={games_read:,} positions={positions_seen:,} "
                    f"unique_positions={len(heaps):,} elapsed={elapsed:,.1f}s",
                    file=sys.stderr,
                )
            if args.max_games and games_read >= args.max_games:
                break

    args.db.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(args.db)
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA temp_store=MEMORY")
    create_schema(conn)

    rows = []
    inserted = 0
    with conn:
        for key, heap in heaps.items():
            ranked = sorted(heap, reverse=True)
            for rank, (avg_elo, _max_elo, game_id, reached_ply) in enumerate(ranked, start=1):
                meta = metas[game_id]
                rows.append(
                    (
                        key,
                        rank,
                        meta.game_id,
                        meta.link,
                        meta.date,
                        meta.white,
                        meta.black,
                        meta.white_elo,
                        meta.black_elo,
                        meta.avg_elo,
                        meta.result,
                        meta.tournament,
                        meta.opening_name,
                        meta.eco,
                        reached_ply,
                    )
                )
                if len(rows) >= args.batch_size:
                    conn.executemany(
                        """
                        INSERT INTO position_top_games(
                            position_key, rank, game_id, link, date, white, black,
                            white_elo, black_elo, avg_elo, result, tournament,
                            opening_name, eco, reached_ply
                        )
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                        """,
                        rows,
                    )
                    inserted += len(rows)
                    rows.clear()
        if rows:
            conn.executemany(
                """
                INSERT INTO position_top_games(
                    position_key, rank, game_id, link, date, white, black,
                    white_elo, black_elo, avg_elo, result, tournament,
                    opening_name, eco, reached_ply
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                rows,
            )
            inserted += len(rows)
            rows.clear()
        upsert_meta(
            conn,
            {
                "top_games_source_pgn": str(args.pgn),
                "top_games_built_at_unix": int(time.time()),
                "top_games_max_plies": args.max_plies,
                "top_games_min_position_plies": args.min_position_plies,
                "top_games_top_n": args.top_n,
                "top_games_rows": inserted,
            },
        )
    conn.execute("ANALYZE")
    conn.close()

    summary = {
        "db": str(args.db),
        "source_pgn": str(args.pgn),
        "max_plies": args.max_plies,
        "min_position_plies": args.min_position_plies,
        "top_n": args.top_n,
        "games_read": games_read,
        "games_used": games_used,
        "positions_seen": positions_seen,
        "unique_positions": len(heaps),
        "top_game_rows": inserted,
        "failed_games": failed_games,
        "seconds": time.perf_counter() - start,
    }
    summary_path = args.db.with_name(args.db.stem + ".top_games_summary.json")
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    return summary


def main() -> int:
    args = parse_args()
    summary = build_top_games(args)
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
