#!/usr/bin/env python3
"""Calculate per-move centipawn loss for fetched PGNs using Stockfish.

The implementation is optimized for a throughput test:
- process-level parallelism by PGN chunks;
- one Stockfish process per worker, reused across many games;
- fixed node limit for predictable runtime;
- one engine evaluation per position, then derive each move's loss from
  consecutive position evaluations.
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import math
import os
import re
import sys
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import asdict, dataclass
from pathlib import Path

import chess
import chess.engine
import chess.pgn


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_PGN = ROOT / "outputs" / "first_1000_games.pgn"
DEFAULT_OUTPUT_DIR = ROOT / "outputs" / "centipawn_loss"
DEFAULT_STOCKFISH = ROOT / "stockfish" / "stockfish-macos-m1-apple-silicon"
MATE_SCORE = 100_000


@dataclass
class WorkerSummary:
    worker_id: int
    games: int
    moves: int
    engine_evals: int
    errors: int
    seconds: float
    output_csv: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pgn", type=Path, default=DEFAULT_PGN)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--stockfish", type=Path, default=DEFAULT_STOCKFISH)
    parser.add_argument("--workers", type=int, default=max(1, min((os.cpu_count() or 2) - 1, 8)))
    parser.add_argument("--nodes", type=int, default=2000)
    parser.add_argument("--hash-mb", type=int, default=64)
    parser.add_argument("--limit-games", type=int, default=0)
    parser.add_argument("--include-fen", action="store_true")
    parser.add_argument("--no-progress", action="store_true", help="Disable progress display.")
    parser.add_argument(
        "--watch",
        action="store_true",
        help="Keep watching a growing PGN and append centipawn rows for new games.",
    )
    parser.add_argument(
        "--poll-seconds",
        type=float,
        default=120.0,
        help="Seconds to wait between watch-mode polls when no new games are found.",
    )
    parser.add_argument(
        "--watch-batch-games",
        type=int,
        default=2000,
        help="Maximum new games to analyze per watch-mode batch.",
    )
    parser.add_argument(
        "--idle-exit-polls",
        type=int,
        default=0,
        help="In watch mode, exit after this many idle polls. 0 means run until interrupted.",
    )
    return parser.parse_args()


def print_progress(
    label: str,
    completed: int,
    total: int,
    *,
    games_done: int,
    moves_done: int,
    errors: int,
    start_time: float,
    final: bool = False,
) -> None:
    if total <= 0:
        return
    width = 28
    frac = min(max(completed / total, 0.0), 1.0)
    filled = int(width * frac)
    bar = "#" * filled + "-" * (width - filled)
    elapsed = max(time.perf_counter() - start_time, 1e-9)
    rate = completed / elapsed
    remaining = (total - completed) / rate if rate > 0 else 0.0
    mps = moves_done / elapsed if elapsed > 0 else 0.0
    message = (
        f"\r{label} [{bar}] {completed}/{total} chunks ({frac * 100:5.1f}%) "
        f"games={games_done} moves={moves_done:,} errors={errors} "
        f"{mps:,.0f} moves/s elapsed={elapsed:,.1f}s eta={remaining:,.1f}s"
    )
    sys.stderr.write(message)
    if final:
        sys.stderr.write("\n")
    sys.stderr.flush()


def game_id_from_headers(headers: chess.pgn.Headers) -> str:
    for key in ("Link", "Site", "URL"):
        value = headers.get(key, "")
        match = re.search(r"/(?:game|analysis/game)/live/(\d+)", value)
        if match:
            return match.group(1)
        match = re.search(r"(\d{8,})", value)
        if match:
            return match.group(1)
    return ""


def read_pgn_strings(path: Path, limit_games: int = 0) -> list[str]:
    games: list[str] = []
    with path.open("r", encoding="utf-8", errors="replace") as f:
        while True:
            game = chess.pgn.read_game(f)
            if game is None:
                break
            exporter = chess.pgn.StringExporter(headers=True, variations=False, comments=False)
            games.append(game.accept(exporter))
            if limit_games and len(games) >= limit_games:
                break
    return games


def read_processed_ids(path: Path) -> set[str]:
    if not path.exists():
        return set()
    with path.open("r", encoding="utf-8") as f:
        return {line.strip() for line in f if line.strip()}


def append_processed_ids(path: Path, game_ids: list[str]) -> None:
    if not game_ids:
        return
    with path.open("a", encoding="utf-8") as f:
        for game_id in game_ids:
            f.write(game_id)
            f.write("\n")


def read_new_pgn_strings(
    path: Path,
    processed_ids: set[str],
    limit_games: int,
) -> list[tuple[str, str]]:
    """Read complete unprocessed games from a PGN file snapshot.

    The fetcher appends a blank line after every complete game. Truncating the
    snapshot at the last double newline avoids analyzing a game while it is
    still being written.
    """
    if not path.exists() or path.stat().st_size == 0:
        return []
    text = path.read_text(encoding="utf-8", errors="replace")
    last_complete = text.rfind("\n\n")
    if last_complete == -1:
        return []
    text = text[: last_complete + 2]

    games: list[tuple[str, str]] = []
    handle = io.StringIO(text)
    while True:
        game = chess.pgn.read_game(handle)
        if game is None:
            break
        game_id = game_id_from_headers(game.headers)
        if not game_id or game_id in processed_ids:
            continue
        exporter = chess.pgn.StringExporter(headers=True, variations=False, comments=False)
        games.append((game_id, game.accept(exporter)))
        if limit_games and len(games) >= limit_games:
            break
    return games


def chunks(items: list[str], n_chunks: int) -> list[list[str]]:
    if n_chunks <= 1:
        return [items]
    size = math.ceil(len(items) / n_chunks)
    return [items[i : i + size] for i in range(0, len(items), size)]


def score_to_white_cp(score: chess.engine.PovScore) -> int | None:
    cp = score.white().score(mate_score=MATE_SCORE)
    if cp is None:
        return None
    return int(cp)


def evaluate_white_cp(
    engine: chess.engine.SimpleEngine,
    board: chess.Board,
    limit: chess.engine.Limit,
    cache: dict[str, int | None],
) -> int | None:
    key = board.transposition_key() if hasattr(board, "transposition_key") else board.fen()
    if key in cache:
        return cache[key]
    info = engine.analyse(board, limit)
    score = info.get("score")
    cp = score_to_white_cp(score) if score is not None else None
    cache[key] = cp
    return cp


def phase_from_fullmove(fullmove_number: int) -> str:
    if fullmove_number <= 10:
        return "opening_1_10"
    if fullmove_number <= 20:
        return "early_middlegame_11_20"
    if fullmove_number <= 35:
        return "late_middlegame_21_35"
    return "endgame_36_plus"


def analyse_game(
    engine: chess.engine.SimpleEngine,
    game: chess.pgn.Game,
    limit: chess.engine.Limit,
    cache: dict[str, int | None],
    include_fen: bool,
) -> tuple[list[dict[str, object]], int]:
    board = game.board()
    game_id = game_id_from_headers(game.headers)
    white = game.headers.get("White", "")
    black = game.headers.get("Black", "")
    result = game.headers.get("Result", "")
    event = game.headers.get("Event", "")
    site = game.headers.get("Site", "")
    date = game.headers.get("Date", "")

    position_evals: list[int | None] = [evaluate_white_cp(engine, board, limit, cache)]
    move_records: list[tuple[int, int, chess.Color, str, str, str, bool, bool, str | None]] = []

    for ply, move in enumerate(game.mainline_moves(), start=1):
        fullmove_number = board.fullmove_number
        mover = board.turn
        san = board.san(move)
        uci = move.uci()
        is_capture = board.is_capture(move)
        gives_check = board.gives_check(move)
        fen_before = board.fen() if include_fen else None
        board.push(move)
        position_evals.append(evaluate_white_cp(engine, board, limit, cache))
        move_records.append((
            ply,
            fullmove_number,
            mover,
            san,
            uci,
            phase_from_fullmove(fullmove_number),
            is_capture,
            gives_check,
            fen_before,
        ))

    rows: list[dict[str, object]] = []
    for index, record in enumerate(move_records):
        ply, fullmove_number, mover, san, uci, phase, is_capture, gives_check, fen_before = record
        before_white_cp = position_evals[index]
        after_white_cp = position_evals[index + 1]
        if before_white_cp is None or after_white_cp is None:
            mover_before_cp = None
            mover_after_cp = None
            cp_loss = None
        elif mover == chess.WHITE:
            mover_before_cp = before_white_cp
            mover_after_cp = after_white_cp
            cp_loss = max(0, mover_before_cp - mover_after_cp)
        else:
            mover_before_cp = -before_white_cp
            mover_after_cp = -after_white_cp
            cp_loss = max(0, mover_before_cp - mover_after_cp)

        row = {
            "game_id": game_id,
            "event": event,
            "site": site,
            "date": date,
            "result": result,
            "white": white,
            "black": black,
            "ply": ply,
            "fullmove_number": fullmove_number,
            "phase": phase,
            "mover_color": "white" if mover == chess.WHITE else "black",
            "mover": white if mover == chess.WHITE else black,
            "san": san,
            "uci": uci,
            "is_capture": int(is_capture),
            "gives_check": int(gives_check),
            "eval_before_white_cp": before_white_cp,
            "eval_after_white_cp": after_white_cp,
            "eval_before_mover_cp": mover_before_cp,
            "eval_after_mover_cp": mover_after_cp,
            "cp_loss": cp_loss,
        }
        if include_fen:
            row["fen_before"] = fen_before
        rows.append(row)

    return rows, len(position_evals)


def worker_analyse(
    worker_id: int,
    pgn_strings: list[str],
    output_dir: str,
    stockfish_path: str,
    nodes: int,
    hash_mb: int,
    include_fen: bool,
) -> WorkerSummary:
    start = time.perf_counter()
    output_path = Path(output_dir) / f"centipawn_loss_worker_{worker_id}.csv"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    limit = chess.engine.Limit(nodes=nodes)
    cache: dict[str, int | None] = {}
    games = 0
    moves = 0
    evals = 0
    errors = 0
    fieldnames = [
        "game_id",
        "event",
        "site",
        "date",
        "result",
        "white",
        "black",
        "ply",
        "fullmove_number",
        "phase",
        "mover_color",
        "mover",
        "san",
        "uci",
        "is_capture",
        "gives_check",
        "eval_before_white_cp",
        "eval_after_white_cp",
        "eval_before_mover_cp",
        "eval_after_mover_cp",
        "cp_loss",
    ]
    if include_fen:
        fieldnames.append("fen_before")

    engine = chess.engine.SimpleEngine.popen_uci(stockfish_path)
    try:
        engine.configure({"Threads": 1, "Hash": hash_mb})
        with output_path.open("w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            for pgn_text in pgn_strings:
                try:
                    game = chess.pgn.read_game(io.StringIO(pgn_text))
                    if game is None:
                        errors += 1
                        continue
                    rows, game_evals = analyse_game(engine, game, limit, cache, include_fen)
                    writer.writerows(rows)
                    games += 1
                    moves += len(rows)
                    evals += game_evals
                except Exception:
                    errors += 1
    finally:
        engine.quit()

    return WorkerSummary(
        worker_id=worker_id,
        games=games,
        moves=moves,
        engine_evals=evals,
        errors=errors,
        seconds=time.perf_counter() - start,
        output_csv=str(output_path),
    )


def combine_worker_csvs(worker_summaries: list[WorkerSummary], output_path: Path) -> int:
    rows_written = 0
    wrote_header = False
    with output_path.open("w", encoding="utf-8", newline="") as out_f:
        for summary in sorted(worker_summaries, key=lambda s: s.worker_id):
            path = Path(summary.output_csv)
            with path.open("r", encoding="utf-8", newline="") as in_f:
                header = in_f.readline()
                if not wrote_header:
                    out_f.write(header)
                    wrote_header = True
                for line in in_f:
                    out_f.write(line)
                    rows_written += 1
    return rows_written


def append_worker_csvs(worker_summaries: list[WorkerSummary], output_path: Path) -> int:
    rows_written = 0
    wrote_header = output_path.exists() and output_path.stat().st_size > 0
    with output_path.open("a", encoding="utf-8", newline="") as out_f:
        for summary in sorted(worker_summaries, key=lambda s: s.worker_id):
            path = Path(summary.output_csv)
            with path.open("r", encoding="utf-8", newline="") as in_f:
                header = in_f.readline()
                if not wrote_header:
                    out_f.write(header)
                    wrote_header = True
                for line in in_f:
                    out_f.write(line)
                    rows_written += 1
    return rows_written


def delete_worker_csvs(worker_summaries: list[WorkerSummary]) -> int:
    removed = 0
    for summary in worker_summaries:
        path = Path(summary.output_csv)
        try:
            path.unlink()
            removed += 1
        except FileNotFoundError:
            pass
    return removed


def analyze_pgn_batch(
    pgn_strings: list[str],
    args: argparse.Namespace,
    batch_id: int = 0,
) -> tuple[list[WorkerSummary], int]:
    n_workers = max(1, min(args.workers, len(pgn_strings)))
    game_chunks = chunks(pgn_strings, n_workers)
    summaries: list[WorkerSummary] = []
    progress_start = time.perf_counter()
    progress_label = "Stockfish chunks" if batch_id == 0 else f"Stockfish batch {batch_id}"
    if not args.no_progress:
        print_progress(
            progress_label,
            0,
            len(game_chunks),
            games_done=0,
            moves_done=0,
            errors=0,
            start_time=progress_start,
        )
    with ProcessPoolExecutor(max_workers=n_workers) as pool:
        futures = [
            pool.submit(
                worker_analyse,
                batch_id * 100_000 + worker_id,
                chunk,
                str(args.output_dir),
                str(args.stockfish),
                args.nodes,
                args.hash_mb,
                args.include_fen,
            )
            for worker_id, chunk in enumerate(game_chunks)
        ]
        for future in as_completed(futures):
            summaries.append(future.result())
            if not args.no_progress:
                print_progress(
                    progress_label,
                    len(summaries),
                    len(game_chunks),
                    games_done=sum(s.games for s in summaries),
                    moves_done=sum(s.moves for s in summaries),
                    errors=sum(s.errors for s in summaries),
                    start_time=progress_start,
                    final=len(summaries) == len(game_chunks),
                )
    return summaries, n_workers


def run_watch_mode(args: argparse.Namespace) -> int:
    args.output_dir.mkdir(parents=True, exist_ok=True)
    combined_path = args.output_dir / "centipawn_loss_watch.csv"
    processed_path = args.output_dir / "processed_game_ids.txt"
    summary_path = args.output_dir / "centipawn_loss_watch_summary.json"
    processed_ids = read_processed_ids(processed_path)
    total_start = time.perf_counter()
    batch_id = 0
    idle_polls = 0
    total_games = 0
    total_moves = 0
    total_evals = 0
    total_errors = 0
    total_rows = 0
    last_workers = 0

    print(
        json.dumps(
            {
                "mode": "watch",
                "pgn": str(args.pgn),
                "output_csv": str(combined_path),
                "processed_state": str(processed_path),
                "already_processed_games": len(processed_ids),
                "poll_seconds": args.poll_seconds,
                "watch_batch_games": args.watch_batch_games,
            },
            indent=2,
        )
    )

    while True:
        new_games = read_new_pgn_strings(args.pgn, processed_ids, args.watch_batch_games)
        if not new_games:
            idle_polls += 1
            if args.idle_exit_polls and idle_polls >= args.idle_exit_polls:
                break
            if not args.no_progress:
                sys.stderr.write(
                    f"\nNo new complete PGNs. Sleeping {args.poll_seconds:.0f}s "
                    f"(processed={len(processed_ids)})...\n"
                )
                sys.stderr.flush()
            time.sleep(args.poll_seconds)
            continue

        idle_polls = 0
        batch_id += 1
        batch_game_ids = [game_id for game_id, _ in new_games]
        batch_pgns = [pgn for _, pgn in new_games]
        summaries, last_workers = analyze_pgn_batch(batch_pgns, args, batch_id=batch_id)
        rows_written = append_worker_csvs(summaries, combined_path)
        worker_files_removed = delete_worker_csvs(summaries)
        successful_games = sum(s.games for s in summaries)
        if successful_games == len(batch_game_ids):
            append_processed_ids(processed_path, batch_game_ids)
            processed_ids.update(batch_game_ids)
        else:
            # If a worker failed to analyze a PGN, leave the whole batch
            # unmarked so it can be retried after inspection.
            sys.stderr.write(
                f"\nWarning: batch {batch_id} processed {successful_games}/"
                f"{len(batch_game_ids)} games; not marking this batch complete.\n"
            )
            sys.stderr.flush()

        total_games += successful_games
        total_moves += sum(s.moves for s in summaries)
        total_evals += sum(s.engine_evals for s in summaries)
        total_errors += sum(s.errors for s in summaries)
        total_rows += rows_written
        summary = {
            "mode": "watch",
            "pgn": str(args.pgn),
            "stockfish": str(args.stockfish),
            "nodes": args.nodes,
            "hash_mb_per_worker": args.hash_mb,
            "workers": last_workers,
            "batches_completed_this_run": batch_id,
            "games_processed_this_run": total_games,
            "moves_processed_this_run": total_moves,
            "engine_evals_this_run": total_evals,
            "errors_this_run": total_errors,
            "rows_written_this_run": total_rows,
            "worker_files_removed_last_batch": worker_files_removed,
            "processed_games_total": len(processed_ids),
            "combined_csv": str(combined_path),
            "processed_state": str(processed_path),
            "seconds_this_run": time.perf_counter() - total_start,
        }
        summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
        print(json.dumps(summary, indent=2))

    final_summary = {
        "mode": "watch",
        "pgn": str(args.pgn),
        "stockfish": str(args.stockfish),
        "nodes": args.nodes,
        "hash_mb_per_worker": args.hash_mb,
        "workers": last_workers,
        "batches_completed_this_run": batch_id,
        "games_processed_this_run": total_games,
        "moves_processed_this_run": total_moves,
        "engine_evals_this_run": total_evals,
        "errors_this_run": total_errors,
        "rows_written_this_run": total_rows,
        "worker_files_removed_last_batch": 0,
        "processed_games_total": len(processed_ids),
        "combined_csv": str(combined_path),
        "processed_state": str(processed_path),
        "seconds_this_run": time.perf_counter() - total_start,
    }
    summary_path.write_text(json.dumps(final_summary, indent=2), encoding="utf-8")
    print(json.dumps(final_summary, indent=2))
    return 0


def main() -> int:
    args = parse_args()
    start = time.perf_counter()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    if args.watch:
        return run_watch_mode(args)
    games = read_pgn_strings(args.pgn, args.limit_games)
    if not games:
        raise SystemExit(f"No PGNs found in {args.pgn}")
    summaries, n_workers = analyze_pgn_batch(games, args)

    combined_path = args.output_dir / "centipawn_loss_first_1000.csv"
    rows_written = combine_worker_csvs(summaries, combined_path)
    seconds = time.perf_counter() - start
    summary = {
        "pgn": str(args.pgn),
        "stockfish": str(args.stockfish),
        "nodes": args.nodes,
        "hash_mb_per_worker": args.hash_mb,
        "workers": n_workers,
        "games": sum(s.games for s in summaries),
        "moves": sum(s.moves for s in summaries),
        "engine_evals": sum(s.engine_evals for s in summaries),
        "errors": sum(s.errors for s in summaries),
        "rows_written": rows_written,
        "combined_csv": str(combined_path),
        "seconds": seconds,
        "moves_per_second": rows_written / seconds if seconds > 0 else None,
        "evals_per_second": sum(s.engine_evals for s in summaries) / seconds if seconds > 0 else None,
        "worker_summaries": [asdict(s) for s in sorted(summaries, key=lambda s: s.worker_id)],
    }
    (args.output_dir / "centipawn_loss_summary.json").write_text(
        json.dumps(summary, indent=2),
        encoding="utf-8",
    )
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
