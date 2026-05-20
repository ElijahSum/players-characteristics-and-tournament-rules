#!/usr/bin/env python3
"""Remove large intermediate files produced by the moves pipeline.

This keeps final selected PGNs, manifests, summaries, and combined centipawn
CSV files, but removes full monthly archive caches and per-worker shards that
are not needed after successful combination.
"""

from __future__ import annotations

import argparse
import shutil
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_OUTPUT_DIR = ROOT / "outputs"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--remove-selected-pgn",
        action="store_true",
        help="Also remove selected PGN files after Stockfish output is done.",
    )
    return parser.parse_args()


def remove_path(path: Path, dry_run: bool) -> int:
    if not path.exists():
        return 0
    size = 0
    if path.is_dir():
        size = sum(p.stat().st_size for p in path.rglob("*") if p.is_file())
        if not dry_run:
            shutil.rmtree(path)
    else:
        size = path.stat().st_size
        if not dry_run:
            path.unlink()
    action = "would remove" if dry_run else "removed"
    print(f"{action}: {path} ({size / 1024 / 1024:.1f} MB)")
    return size


def main() -> int:
    args = parse_args()
    total = 0
    total += remove_path(args.output_dir / "monthly_pgn_cache", args.dry_run)

    for path in args.output_dir.rglob("centipawn_loss_worker_*.csv"):
        total += remove_path(path, args.dry_run)

    if args.remove_selected_pgn:
        for path in args.output_dir.glob("*games.pgn"):
            total += remove_path(path, args.dry_run)
        for path in args.output_dir.rglob("*games.pgn"):
            total += remove_path(path, args.dry_run)

    action = "would free" if args.dry_run else "freed"
    print(f"{action}: {total / 1024 / 1024:.1f} MB")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
