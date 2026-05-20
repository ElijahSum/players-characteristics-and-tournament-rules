#!/usr/bin/env python3
"""Add next-move continuations for top-rated opening explorer games."""

from __future__ import annotations

import argparse
import json
import sqlite3
import sys
import time
from collections import defaultdict
from pathlib import Path

import chess
import chess.pgn

from build_titled_tuesday_opening_db import DEFAULT_OUTPUT
from build_titled_tuesday_top_games import game_id_from_link


PACKAGE_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_PGNS = [
    PACKAGE_ROOT / "outputs" / "whole_dataset_2024_2026" / "whole_dataset_2024_2026.pgn",
    PACKAGE_ROOT / "outputs" / "whole_dataset_2022_2024" / "whole_dataset_2022_2024.pgn",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--pgn", type=Path, action="append", dest="pgns")
    parser.add_argument("--next-moves", type=int, default=5)
    parser.add_argument("--batch-size", type=int, default=50000)
    parser.add_argument("--progress-every", type=int, default=50000)
    return parser.parse_args()


def create_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS top_game_continuations (
            game_id TEXT NOT NULL,
            reached_ply INTEGER NOT NULL,
            continuation_san TEXT NOT NULL,
            PRIMARY KEY (game_id, reached_ply)
        ) WITHOUT ROWID;
        """
    )


def upsert_meta(conn: sqlite3.Connection, values: dict[str, object]) -> None:
    conn.executemany(
        "INSERT INTO meta(key, value) VALUES(?, ?) "
        "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
        [(key, json.dumps(value) if isinstance(value, (dict, list)) else str(value)) for key, value in values.items()],
    )


def needed_pairs(conn: sqlite3.Connection) -> dict[str, set[int]]:
    out: dict[str, set[int]] = defaultdict(set)
    for game_id, reached_ply in conn.execute(
        "SELECT DISTINCT game_id, reached_ply FROM position_top_games"
    ):
        out[str(game_id)].add(int(reached_ply))
    return out


def san_prefix(game: chess.pgn.Game, plies: int) -> list[str]:
    board = game.board()
    out = []
    for move in game.mainline_moves():
        if len(out) >= plies:
            break
        out.append(board.san(move))
        board.push(move)
    return out


def flush_rows(conn: sqlite3.Connection, rows: list[tuple[str, int, str]]) -> int:
    if not rows:
        return 0
    with conn:
        conn.executemany(
            """
            INSERT INTO top_game_continuations(game_id, reached_ply, continuation_san)
            VALUES (?, ?, ?)
            ON CONFLICT(game_id, reached_ply) DO UPDATE SET
                continuation_san = excluded.continuation_san
            """,
            rows,
        )
    count = len(rows)
    rows.clear()
    return count


def build_continuations(args: argparse.Namespace) -> dict[str, object]:
    pgns = args.pgns or DEFAULT_PGNS
    for pgn in pgns:
        if not pgn.exists():
            raise FileNotFoundError(pgn)

    start = time.perf_counter()
    conn = sqlite3.connect(args.db)
    conn.execute("PRAGMA journal_mode=DELETE")
    conn.execute("PRAGMA synchronous=NORMAL")
    conn.execute("PRAGMA temp_store=MEMORY")
    create_schema(conn)
    with conn:
        conn.execute("DELETE FROM top_game_continuations")

    needed = needed_pairs(conn)
    max_needed_ply = max((max(plies) for plies in needed.values()), default=0)
    max_san_plies = max_needed_ply + args.next_moves

    games_read = 0
    games_matched = 0
    inserted = 0
    rows: list[tuple[str, int, str]] = []

    for pgn in pgns:
        with pgn.open("r", encoding="utf-8", errors="replace") as pgn_f:
            while True:
                game = chess.pgn.read_game(pgn_f)
                if game is None:
                    break
                games_read += 1
                link = game.headers.get("Link", "").strip()
                game_id = game_id_from_link(link, games_read)
                plies = needed.get(game_id)
                if plies:
                    try:
                        sans = san_prefix(game, max_san_plies)
                        for reached_ply in plies:
                            continuation = " ".join(sans[reached_ply : reached_ply + args.next_moves])
                            rows.append((game_id, reached_ply, continuation))
                        games_matched += 1
                    except Exception as exc:
                        print(f"Skipping continuation for game {game_id}: {exc}", file=sys.stderr)
                if len(rows) >= args.batch_size:
                    inserted += flush_rows(conn, rows)
                if args.progress_every and games_read % args.progress_every == 0:
                    elapsed = time.perf_counter() - start
                    print(
                        f"Continuations: games={games_read:,} matched={games_matched:,} "
                        f"rows={inserted + len(rows):,} elapsed={elapsed:,.1f}s",
                        file=sys.stderr,
                    )

    inserted += flush_rows(conn, rows)
    with conn:
        upsert_meta(
            conn,
            {
                "top_game_continuations_built_at_unix": int(time.time()),
                "top_game_continuations_rows": inserted,
                "top_game_continuations_next_moves": args.next_moves,
                "top_game_continuations_source_pgns": [str(pgn) for pgn in pgns],
            },
        )
    conn.execute("ANALYZE top_game_continuations")
    conn.close()

    summary = {
        "db": str(args.db),
        "source_pgns": [str(pgn) for pgn in pgns],
        "needed_game_ids": len(needed),
        "needed_pairs": sum(len(plies) for plies in needed.values()),
        "next_moves": args.next_moves,
        "games_read": games_read,
        "games_matched": games_matched,
        "continuation_rows": inserted,
        "seconds": time.perf_counter() - start,
    }
    summary_path = args.db.with_name(args.db.stem + ".continuations_summary.json")
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return summary


def main() -> int:
    args = parse_args()
    build_continuations(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
