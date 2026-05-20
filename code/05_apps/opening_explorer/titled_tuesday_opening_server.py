#!/usr/bin/env python3
"""Serve the local Titled Tuesday opening explorer."""

from __future__ import annotations

import argparse
import json
import mimetypes
import sqlite3
import urllib.parse
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import chess


APP_ROOT = Path(__file__).resolve().parent
PACKAGE_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_DB = PACKAGE_ROOT / "outputs" / "opening_explorer" / "titled_tuesday_openings.sqlite"
STATIC_DIR = APP_ROOT / "web"
NO_POSITION_MESSAGE = "There were no positions like this on Titled Tuesday in the first 10 moves."
TOP_GAMES_MIN_PLY = 3


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=Path, default=DEFAULT_DB)
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8765)
    return parser.parse_args()


def position_key(board: chess.Board) -> str:
    return " ".join(board.fen().split()[:4])


def board_from_value(value: str | None) -> chess.Board:
    if not value or value == "startpos":
        return chess.Board()
    return chess.Board(value)


def legal_moves_payload(board: chess.Board) -> list[dict[str, str]]:
    moves = []
    for move in board.legal_moves:
        moves.append(
            {
                "uci": move.uci(),
                "from": chess.square_name(move.from_square),
                "to": chess.square_name(move.to_square),
                "san": board.san(move),
                "promotion": chess.piece_symbol(move.promotion) if move.promotion else "",
            }
        )
    return moves


def connect(db_path: Path) -> sqlite3.Connection:
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    return conn


def table_exists(conn: sqlite3.Connection, table_name: str) -> bool:
    row = conn.execute(
        "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?",
        (table_name,),
    ).fetchone()
    return row is not None


def query_position(db_path: Path, board: chess.Board) -> dict[str, object]:
    key = position_key(board)
    legal_moves = legal_moves_payload(board)
    with connect(db_path) as conn:
        move_rows = conn.execute(
            """
            SELECT move_uci, move_san, count, rating_sum, rating_count,
                   white_wins, draws, black_wins
            FROM move_stats
            WHERE position_key = ?
            ORDER BY count DESC, move_san
            LIMIT 10
            """,
            (key,),
        ).fetchall()
        total_count_row = conn.execute(
            "SELECT COALESCE(SUM(count), 0) AS total_games FROM move_stats WHERE position_key = ?",
            (key,),
        ).fetchone()
        opening_rows = conn.execute(
            """
            SELECT opening_name, eco, count
            FROM opening_stats
            WHERE position_key = ?
            ORDER BY count DESC
            LIMIT 5
            """,
            (key,),
        ).fetchall()
        top_game_rows = []
        has_top_games_table = table_exists(conn, "position_top_games")
        has_continuations_table = table_exists(conn, "top_game_continuations")
        if has_top_games_table and board.ply() >= TOP_GAMES_MIN_PLY:
            if has_continuations_table:
                top_game_rows = conn.execute(
                    """
                    SELECT g.rank, g.game_id, g.link, g.date, g.white, g.black,
                           g.white_elo, g.black_elo, g.avg_elo, g.result,
                           g.tournament, g.opening_name, g.eco, g.reached_ply,
                           COALESCE(c.continuation_san, '') AS continuation_san
                    FROM position_top_games AS g
                    LEFT JOIN top_game_continuations AS c
                      ON c.game_id = g.game_id
                     AND c.reached_ply = g.reached_ply
                    WHERE g.position_key = ?
                    ORDER BY g.rank
                    LIMIT 5
                    """,
                    (key,),
                ).fetchall()
            else:
                top_game_rows = conn.execute(
                    """
                    SELECT rank, game_id, link, date, white, black, white_elo, black_elo,
                           avg_elo, result, tournament, opening_name, eco, reached_ply,
                           '' AS continuation_san
                    FROM position_top_games
                    WHERE position_key = ?
                    ORDER BY rank
                    LIMIT 5
                    """,
                    (key,),
                ).fetchall()
        meta_rows = conn.execute("SELECT key, value FROM meta").fetchall()

    top_moves = []
    total_games = int(total_count_row["total_games"]) if total_count_row else 0
    for row in move_rows:
        count = int(row["count"])
        white_wins = int(row["white_wins"])
        draws = int(row["draws"])
        black_wins = int(row["black_wins"])
        decided_total = max(white_wins + draws + black_wins, 1)
        avg_rating = (
            float(row["rating_sum"]) / int(row["rating_count"])
            if int(row["rating_count"])
            else None
        )
        top_moves.append(
            {
                "uci": row["move_uci"],
                "san": row["move_san"],
                "count": count,
                "avg_rating": avg_rating,
                "white_wins": white_wins,
                "draws": draws,
                "black_wins": black_wins,
                "white_pct": white_wins / decided_total,
                "draw_pct": draws / decided_total,
                "black_pct": black_wins / decided_total,
            }
        )
    return {
        "fen": board.fen(),
        "position_key": key,
        "turn": "white" if board.turn == chess.WHITE else "black",
        "legal_moves": legal_moves,
        "top_moves": top_moves,
        "opening_names": [
            {
                "opening_name": row["opening_name"],
                "eco": row["eco"],
                "count": int(row["count"]),
            }
            for row in opening_rows
        ],
        "top_games": [
            {
                "rank": int(row["rank"]),
                "game_id": row["game_id"],
                "link": row["link"],
                "date": row["date"],
                "white": row["white"],
                "black": row["black"],
                "white_elo": row["white_elo"],
                "black_elo": row["black_elo"],
                "avg_elo": row["avg_elo"],
                "result": row["result"],
                "tournament": row["tournament"],
                "opening_name": row["opening_name"],
                "eco": row["eco"],
                "reached_ply": int(row["reached_ply"]),
                "continuation_san": row["continuation_san"],
            }
            for row in top_game_rows
        ],
        "top_games_message": top_games_message(board, has_top_games_table, top_game_rows),
        "total_games": total_games,
        "ply": board.ply(),
        "in_database": bool(move_rows),
        "message": "" if move_rows else NO_POSITION_MESSAGE,
        "meta": {row["key"]: row["value"] for row in meta_rows},
    }


def top_games_message(
    board: chess.Board,
    has_top_games_table: bool,
    rows: list[sqlite3.Row],
) -> str:
    if board.ply() < TOP_GAMES_MIN_PLY:
        return "Top rated games appear after the first move pair."
    if not has_top_games_table:
        return "Top rated game index has not been built yet."
    if not rows:
        return "No top rated game examples found for this position."
    return ""


class OpeningExplorerHandler(BaseHTTPRequestHandler):
    db_path: Path

    def log_message(self, fmt: str, *args: object) -> None:
        print(f"{self.address_string()} - {fmt % args}")

    def send_json(self, data: object, status: HTTPStatus = HTTPStatus.OK) -> None:
        body = json.dumps(data, separators=(",", ":")).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def send_error_json(self, message: str, status: HTTPStatus = HTTPStatus.BAD_REQUEST) -> None:
        self.send_json({"error": message}, status)

    def do_GET(self) -> None:
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path == "/api/position":
            params = urllib.parse.parse_qs(parsed.query)
            try:
                board = board_from_value(params.get("fen", ["startpos"])[0])
            except Exception as exc:
                self.send_error_json(f"Invalid FEN: {exc}")
                return
            self.send_json(query_position(self.db_path, board))
            return
        self.serve_static(parsed.path)

    def do_POST(self) -> None:
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path != "/api/move":
            self.send_error_json("Unknown endpoint", HTTPStatus.NOT_FOUND)
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            payload = json.loads(self.rfile.read(length).decode("utf-8"))
            board = board_from_value(payload.get("fen"))
            move = chess.Move.from_uci(str(payload.get("move", "")))
            if move not in board.legal_moves:
                self.send_error_json("Illegal move for this position.")
                return
            board.push(move)
            self.send_json(query_position(self.db_path, board))
        except json.JSONDecodeError:
            self.send_error_json("Invalid JSON payload.")
        except Exception as exc:
            self.send_error_json(str(exc))

    def serve_static(self, url_path: str) -> None:
        if url_path in ("", "/"):
            rel_path = "index.html"
        else:
            rel_path = urllib.parse.unquote(url_path.lstrip("/"))
        static_root = STATIC_DIR.resolve()
        file_path = (static_root / rel_path).resolve()
        if not str(file_path).startswith(str(static_root)) or not file_path.is_file():
            self.send_response(HTTPStatus.NOT_FOUND)
            self.end_headers()
            return
        content_type = mimetypes.guess_type(str(file_path))[0] or "application/octet-stream"
        body = file_path.read_bytes()
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


def main() -> int:
    args = parse_args()
    if not args.db.exists():
        raise FileNotFoundError(f"Opening database not found: {args.db}")
    OpeningExplorerHandler.db_path = args.db
    server = ThreadingHTTPServer((args.host, args.port), OpeningExplorerHandler)
    print(f"Serving Titled Tuesday opening explorer at http://{args.host}:{args.port}")
    print(f"Database: {args.db}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nStopping server.")
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
