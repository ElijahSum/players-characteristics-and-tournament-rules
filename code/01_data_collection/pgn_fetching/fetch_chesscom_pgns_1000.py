#!/usr/bin/env python3
"""Fetch Chess.com PGNs for linked games in the tournament CSV.

The Chess.com PubAPI exposes monthly PGN archives by player:
https://api.chess.com/pub/player/{username}/games/{YYYY}/{MM}/pgn

This script reads game links, fetches the needed monthly archives,
extracts only the requested games by game id, and writes a compact selected PGN
plus a manifest. By default it does not persist full monthly archives, which is
important because the full tournament dataset can require tens of GB if every
player-month archive is cached.
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable
from threading import Lock

import chess.pgn


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_INPUT = ROOT / "data" / "tournaments_1_261_final_v6.csv"
DEFAULT_OUTPUT_DIR = ROOT / "outputs"
USER_AGENT = (
    "Mozilla/5.0 chess-research-pgn-fetch/0.1 "
    "(contact: local thesis research; respects Chess.com PubAPI cache)"
)


@dataclass(frozen=True)
class RequestedGame:
    source_row: int
    game_id: str
    game_link: str
    date: str
    year: str
    month: str
    white_name: str
    black_name: str
    white_username: str
    black_username: str


@dataclass
class FetchResult:
    username: str
    year: str
    month: str
    cache_path: str
    ok: bool
    from_cache: bool
    status: str
    bytes: int
    games_extracted: int
    seconds: float


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument(
        "--limit",
        type=int,
        default=0,
        help="Maximum unique game links to request. Use 0 for the whole input CSV.",
    )
    parser.add_argument("--workers", type=int, default=8)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--force-refresh", action="store_true")
    parser.add_argument(
        "--keep-monthly-cache",
        action="store_true",
        help="Persist full monthly PGN archives. Off by default to save disk.",
    )
    parser.add_argument(
        "--output-pgn-name",
        default="selected_games.pgn",
        help="Selected-game PGN output filename inside output-dir.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Append missing games to an existing output PGN instead of overwriting it.",
    )
    parser.add_argument("--no-progress", action="store_true", help="Disable progress display.")
    return parser.parse_args()


def print_progress(
    label: str,
    completed: int,
    total: int,
    *,
    found_games: int = 0,
    ok: int = 0,
    failed: int = 0,
    downloaded_mb: float = 0.0,
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
    message = (
        f"\r{label} [{bar}] {completed}/{total} ({frac * 100:5.1f}%) "
        f"found={found_games} ok={ok} fail={failed} read={downloaded_mb:,.1f}MB "
        f"elapsed={elapsed:,.1f}s eta={remaining:,.1f}s"
    )
    sys.stderr.write(message)
    if final:
        sys.stderr.write("\n")
    sys.stderr.flush()


def username_from_link(link: str, fallback: str) -> str:
    if link:
        parsed = urllib.parse.urlparse(link)
        parts = [p for p in parsed.path.split("/") if p]
        if parts:
            return urllib.parse.unquote(parts[-1]).strip()
    return fallback.strip()


def game_id_from_link(link: str) -> str:
    match = re.search(r"/(?:game|analysis/game)/live/(\d+)", link or "")
    if match:
        return match.group(1)
    match = re.search(r"(\d{8,})", link or "")
    if match:
        return match.group(1)
    return ""


def parse_date_parts(value: str) -> tuple[str, str]:
    text = value.strip()
    try:
        dt = datetime.fromisoformat(text)
    except ValueError:
        for fmt in (
            "%b %d, %Y, %I:%M %p",
            "%B %d, %Y, %I:%M %p",
            "%b %d, %Y",
            "%B %d, %Y",
        ):
            try:
                dt = datetime.strptime(text, fmt)
                break
            except ValueError:
                pass
        else:
            raise ValueError(f"Unsupported date format: {value!r}") from None
    return f"{dt.year:04d}", f"{dt.month:02d}"


def read_requested_games(path: Path, limit: int) -> list[RequestedGame]:
    games: list[RequestedGame] = []
    seen: set[str] = set()
    date_parts_cache: dict[str, tuple[str, str]] = {}
    with path.open("r", newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row_number, row in enumerate(reader, start=2):
            game_link = (row.get("game_link") or "").strip()
            game_id = game_id_from_link(game_link)
            if not game_id or game_id in seen:
                continue
            row_date = row["date"]
            if row_date not in date_parts_cache:
                date_parts_cache[row_date] = parse_date_parts(row_date)
            year, month = date_parts_cache[row_date]
            games.append(
                RequestedGame(
                    source_row=row_number,
                    game_id=game_id,
                    game_link=game_link,
                    date=row_date,
                    year=year,
                    month=month,
                    white_name=(row.get("white_name") or "").strip(),
                    black_name=(row.get("black_name") or "").strip(),
                    white_username=username_from_link(row.get("white_link") or "", row.get("white_name") or ""),
                    black_username=username_from_link(row.get("black_link") or "", row.get("black_name") or ""),
                )
            )
            seen.add(game_id)
            if limit > 0 and len(games) >= limit:
                break
    return games


def cache_name(username: str, year: str, month: str) -> str:
    safe = re.sub(r"[^A-Za-z0-9_.-]+", "_", username.lower())
    return f"{safe}_{year}_{month}.pgn"


def fetch_monthly_pgn(
    username: str,
    year: str,
    month: str,
    cache_dir: Path,
    timeout: float,
    force_refresh: bool,
    keep_monthly_cache: bool,
) -> FetchResult:
    start = time.perf_counter()
    cache_path = cache_dir / cache_name(username, year, month)
    if keep_monthly_cache and cache_path.exists() and not force_refresh and cache_path.stat().st_size > 0:
        return FetchResult(
            username=username,
            year=year,
            month=month,
            cache_path=str(cache_path),
            ok=True,
            from_cache=True,
            status="cache",
            bytes=cache_path.stat().st_size,
            games_extracted=0,
            seconds=time.perf_counter() - start,
        )

    url = f"https://api.chess.com/pub/player/{urllib.parse.quote(username)}/games/{year}/{month}/pgn"
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            payload = response.read()
        if keep_monthly_cache:
            cache_path.write_bytes(payload)
        return FetchResult(
            username=username,
            year=year,
            month=month,
            cache_path=str(cache_path) if keep_monthly_cache else "",
            ok=True,
            from_cache=False,
            status="downloaded",
            bytes=len(payload),
            games_extracted=0,
            seconds=time.perf_counter() - start,
        )
    except urllib.error.HTTPError as exc:
        return FetchResult(
            username=username,
            year=year,
            month=month,
            cache_path=str(cache_path),
            ok=False,
            from_cache=False,
            status=f"http_{exc.code}",
            bytes=0,
            games_extracted=0,
            seconds=time.perf_counter() - start,
        )
    except Exception as exc:  # network failures should not abort the whole batch
        return FetchResult(
            username=username,
            year=year,
            month=month,
            cache_path=str(cache_path),
            ok=False,
            from_cache=False,
            status=f"error:{type(exc).__name__}:{exc}",
            bytes=0,
            games_extracted=0,
            seconds=time.perf_counter() - start,
        )


def fetch_all(
    tasks: Iterable[tuple[str, str, str]],
    cache_dir: Path,
    workers: int,
    timeout: float,
    force_refresh: bool,
    keep_monthly_cache: bool,
) -> list[FetchResult]:
    unique_tasks = sorted(set(tasks))
    results: list[FetchResult] = []
    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [
            pool.submit(
                fetch_monthly_pgn,
                username,
                year,
                month,
                cache_dir,
                timeout,
                force_refresh,
                keep_monthly_cache,
            )
            for username, year, month in unique_tasks
        ]
        for future in as_completed(futures):
            results.append(future.result())
    return results


def extract_games_from_pgn_text(
    pgn_text: str,
    needed: set[str],
    found: dict[str, str],
) -> int:
    extracted = 0
    handle = io.StringIO(pgn_text)
    while True:
        start_pos = handle.tell()
        game = chess.pgn.read_game(handle)
        if game is None:
            break
        game_id = game_header_id(game)
        if game_id in needed and game_id not in found:
            end_pos = handle.tell()
            found[game_id] = pgn_text[start_pos:end_pos].strip() + "\n"
            extracted += 1
    return extracted


def fetch_and_extract_monthly_pgn(
    username: str,
    year: str,
    month: str,
    needed: set[str],
    cache_dir: Path,
    timeout: float,
    force_refresh: bool,
    keep_monthly_cache: bool,
) -> tuple[FetchResult, dict[str, str]]:
    start = time.perf_counter()
    cache_path = cache_dir / cache_name(username, year, month)
    from_cache = False
    payload: bytes
    if keep_monthly_cache and cache_path.exists() and not force_refresh and cache_path.stat().st_size > 0:
        payload = cache_path.read_bytes()
        from_cache = True
        status = "cache"
    else:
        url = f"https://api.chess.com/pub/player/{urllib.parse.quote(username)}/games/{year}/{month}/pgn"
        request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                payload = response.read()
            status = "downloaded"
            if keep_monthly_cache:
                cache_path.write_bytes(payload)
        except urllib.error.HTTPError as exc:
            result = FetchResult(
                username=username,
                year=year,
                month=month,
                cache_path=str(cache_path) if keep_monthly_cache else "",
                ok=False,
                from_cache=False,
                status=f"http_{exc.code}",
                bytes=0,
                games_extracted=0,
                seconds=time.perf_counter() - start,
            )
            return result, {}
        except Exception as exc:
            result = FetchResult(
                username=username,
                year=year,
                month=month,
                cache_path=str(cache_path) if keep_monthly_cache else "",
                ok=False,
                from_cache=False,
                status=f"error:{type(exc).__name__}:{exc}",
                bytes=0,
                games_extracted=0,
                seconds=time.perf_counter() - start,
            )
            return result, {}

    local_found: dict[str, str] = {}
    extracted = extract_games_from_pgn_text(
        payload.decode("utf-8", errors="replace"),
        needed,
        local_found,
    )
    result = FetchResult(
        username=username,
        year=year,
        month=month,
        cache_path=str(cache_path) if keep_monthly_cache else "",
        ok=True,
        from_cache=from_cache,
        status=status,
        bytes=len(payload),
        games_extracted=extracted,
        seconds=time.perf_counter() - start,
    )
    return result, local_found


def fetch_extract_all(
    tasks: Iterable[tuple[str, str, str]],
    needed: set[str],
    cache_dir: Path,
    workers: int,
    timeout: float,
    force_refresh: bool,
    keep_monthly_cache: bool,
    label: str,
    show_progress: bool,
) -> tuple[list[FetchResult], dict[str, str]]:
    unique_tasks = sorted(set(tasks))
    results: list[FetchResult] = []
    found: dict[str, str] = {}
    progress_start = time.perf_counter()
    ok_count = 0
    failed_count = 0
    bytes_read = 0
    total = len(unique_tasks)
    if show_progress:
        print_progress(label, 0, total, start_time=progress_start)
    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [
            pool.submit(
                fetch_and_extract_monthly_pgn,
                username,
                year,
                month,
                needed,
                cache_dir,
                timeout,
                force_refresh,
                keep_monthly_cache,
            )
            for username, year, month in unique_tasks
        ]
        for future in as_completed(futures):
            result, local_found = future.result()
            results.append(result)
            if result.ok:
                ok_count += 1
            else:
                failed_count += 1
            bytes_read += result.bytes
            for game_id, pgn in local_found.items():
                found.setdefault(game_id, pgn)
            if show_progress:
                print_progress(
                    label,
                    len(results),
                    total,
                    found_games=len(found),
                    ok=ok_count,
                    failed=failed_count,
                    downloaded_mb=bytes_read / 1024 / 1024,
                    start_time=progress_start,
                    final=len(results) == total,
                )
    return results, found


def fetch_extract_write_all(
    tasks: Iterable[tuple[str, str, str]],
    needed: set[str],
    already_found: set[str],
    output_handle,
    write_lock: Lock,
    cache_dir: Path,
    workers: int,
    timeout: float,
    force_refresh: bool,
    keep_monthly_cache: bool,
    label: str,
    show_progress: bool,
) -> tuple[list[FetchResult], set[str]]:
    unique_tasks = sorted(set(tasks))
    results: list[FetchResult] = []
    newly_found: set[str] = set()
    progress_start = time.perf_counter()
    ok_count = 0
    failed_count = 0
    bytes_read = 0
    total = len(unique_tasks)
    if show_progress:
        print_progress(label, 0, total, start_time=progress_start)

    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [
            pool.submit(
                fetch_and_extract_monthly_pgn,
                username,
                year,
                month,
                needed,
                cache_dir,
                timeout,
                force_refresh,
                keep_monthly_cache,
            )
            for username, year, month in unique_tasks
        ]
        for future in as_completed(futures):
            result, local_found = future.result()
            written = 0
            if local_found:
                with write_lock:
                    for game_id, pgn in local_found.items():
                        if game_id in already_found:
                            continue
                        output_handle.write(pgn.rstrip())
                        output_handle.write("\n\n")
                        already_found.add(game_id)
                        newly_found.add(game_id)
                        written += 1
                    output_handle.flush()
            result.games_extracted = written
            results.append(result)
            if result.ok:
                ok_count += 1
            else:
                failed_count += 1
            bytes_read += result.bytes
            if show_progress:
                print_progress(
                    label,
                    len(results),
                    total,
                    found_games=len(already_found),
                    ok=ok_count,
                    failed=failed_count,
                    downloaded_mb=bytes_read / 1024 / 1024,
                    start_time=progress_start,
                    final=len(results) == total,
                )

    return results, newly_found


def game_header_id(game: chess.pgn.Game) -> str:
    candidates = [
        game.headers.get("Link", ""),
        game.headers.get("Site", ""),
        game.headers.get("URL", ""),
    ]
    for value in candidates:
        game_id = game_id_from_link(value)
        if game_id:
            return game_id
    return ""


def read_existing_pgn_ids(path: Path) -> set[str]:
    found: set[str] = set()
    if not path.exists() or path.stat().st_size == 0:
        return found
    with path.open("r", encoding="utf-8", errors="replace") as f:
        while True:
            game = chess.pgn.read_game(f)
            if game is None:
                break
            game_id = game_header_id(game)
            if game_id:
                found.add(game_id)
    return found


def extract_games_from_archives(
    requested: list[RequestedGame],
    archive_paths: Iterable[Path],
) -> dict[str, str]:
    needed = {g.game_id for g in requested}
    found: dict[str, str] = {}
    for archive_path in archive_paths:
        if not archive_path.exists() or archive_path.stat().st_size == 0:
            continue
        text = archive_path.read_text(encoding="utf-8", errors="replace")
        handle = io.StringIO(text)
        while len(found) < len(needed):
            start_pos = handle.tell()
            game = chess.pgn.read_game(handle)
            if game is None:
                break
            game_id = game_header_id(game)
            if game_id in needed and game_id not in found:
                end_pos = handle.tell()
                found[game_id] = text[start_pos:end_pos].strip() + "\n"
    return found


def write_manifest(path: Path, requested: list[RequestedGame], found: dict[str, str]) -> None:
    fieldnames = [
        "source_row",
        "game_id",
        "found_pgn",
        "date",
        "year",
        "month",
        "white_name",
        "black_name",
        "white_username",
        "black_username",
        "game_link",
    ]
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for game in requested:
            row = asdict(game)
            row["found_pgn"] = int(game.game_id in found)
            writer.writerow(row)


def write_manifest_from_ids(path: Path, requested: list[RequestedGame], found_ids: set[str]) -> None:
    fieldnames = [
        "source_row",
        "game_id",
        "found_pgn",
        "date",
        "year",
        "month",
        "white_name",
        "black_name",
        "white_username",
        "black_username",
        "game_link",
    ]
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for game in requested:
            row = asdict(game)
            row["found_pgn"] = int(game.game_id in found_ids)
            writer.writerow(row)


def main() -> int:
    args = parse_args()
    start = time.perf_counter()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    cache_dir = args.output_dir / "monthly_pgn_cache"
    if args.keep_monthly_cache:
        cache_dir.mkdir(parents=True, exist_ok=True)

    requested = read_requested_games(args.input, args.limit)
    if not requested:
        raise SystemExit("No game links found.")

    needed = {g.game_id for g in requested}
    selected_pgn_path = args.output_dir / args.output_pgn_name
    output_stem = selected_pgn_path.stem
    manifest_path = args.output_dir / f"{output_stem}_manifest.csv"
    summary_path = args.output_dir / f"{output_stem}_fetch_summary.json"
    found_ids = read_existing_pgn_ids(selected_pgn_path) if args.resume else set()
    requested_to_fetch = [g for g in requested if g.game_id not in found_ids]
    write_lock = Lock()
    white_tasks = [(g.white_username, g.year, g.month) for g in requested_to_fetch]
    black_results: list[FetchResult] = []
    output_mode = "a" if args.resume and selected_pgn_path.exists() else "w"
    with selected_pgn_path.open(output_mode, encoding="utf-8") as selected_pgn:
        if output_mode == "a" and requested_to_fetch:
            selected_pgn.write("\n\n")
        white_results, _ = fetch_extract_write_all(
            white_tasks,
            needed,
            found_ids,
            selected_pgn,
            write_lock,
            cache_dir,
            args.workers,
            args.timeout,
            args.force_refresh,
            args.keep_monthly_cache,
            "White archives",
            not args.no_progress,
        )

        missing_after_white = [g for g in requested_to_fetch if g.game_id not in found_ids]
        if missing_after_white:
            black_tasks = [(g.black_username, g.year, g.month) for g in missing_after_white]
            missing_needed = {g.game_id for g in missing_after_white}
            black_results, _ = fetch_extract_write_all(
                black_tasks,
                missing_needed,
                found_ids,
                selected_pgn,
                write_lock,
                cache_dir,
                args.workers,
                args.timeout,
                args.force_refresh,
                args.keep_monthly_cache,
                "Black fallback archives",
                not args.no_progress,
            )

    write_manifest_from_ids(manifest_path, requested, found_ids)

    fetch_results = white_results + black_results
    summary = {
        "input": str(args.input),
        "limit": args.limit,
        "requested_games": len(requested),
        "already_found_games_at_start": len(requested) - len(requested_to_fetch),
        "found_games": len(found_ids),
        "missing_games": len(requested) - len(found_ids),
        "monthly_fetch_tasks": len(fetch_results),
        "downloaded_archives": sum(1 for r in fetch_results if r.ok and not r.from_cache),
        "cached_archives": sum(1 for r in fetch_results if r.ok and r.from_cache),
        "failed_archives": sum(1 for r in fetch_results if not r.ok),
        "monthly_cache_kept": args.keep_monthly_cache,
        "archive_bytes_read": sum(r.bytes for r in fetch_results),
        "games_extracted_from_archives": sum(r.games_extracted for r in fetch_results),
        "selected_pgn_path": str(selected_pgn_path),
        "manifest_path": str(manifest_path),
        "summary_path": str(summary_path),
        "seconds": time.perf_counter() - start,
    }
    summary_path.write_text(
        json.dumps(summary, indent=2),
        encoding="utf-8",
    )
    with (args.output_dir / "monthly_pgn_fetch_results.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(asdict(fetch_results[0]).keys()) if fetch_results else [])
        if fetch_results:
            writer.writeheader()
            writer.writerows(asdict(r) for r in fetch_results)

    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
