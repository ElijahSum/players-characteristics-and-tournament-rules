#!/usr/bin/env python3
"""Stream-concatenate the 2022-2026 Titled Tuesday PGN windows.

This utility deliberately does not parse or load the PGNs. It copies bytes in
large chunks, adds a blank-line separator between input files, and writes a
small JSON summary next to the combined PGN.
"""

from __future__ import annotations

import argparse
import json
import shutil
import sys
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_INPUTS = [
    ROOT / "outputs" / "whole_dataset_2022_2024" / "whole_dataset_2022_2024.pgn",
    ROOT / "outputs" / "whole_dataset_2024_2026" / "whole_dataset_2024_2026.pgn",
]
DEFAULT_OUTPUT = ROOT / "outputs" / "whole_dataset_2022_2026" / "whole_dataset_2022_2026.pgn"
BUFFER_SIZE = 16 * 1024 * 1024


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input-pgn",
        type=Path,
        action="append",
        default=None,
        help="Input PGN. Can be repeated. Defaults to the two source windows.",
    )
    parser.add_argument("--output-pgn", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--summary-json", type=Path, default=None)
    parser.add_argument("--overwrite", action="store_true")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate paths and report required disk without writing the output.",
    )
    parser.add_argument(
        "--skip-free-space-check",
        action="store_true",
        help="Write even when free disk appears lower than the expected output size.",
    )
    return parser.parse_args()


def check_inputs(paths: list[Path]) -> list[dict[str, object]]:
    summaries = []
    for path in paths:
        if not path.exists():
            raise FileNotFoundError(path)
        if not path.is_file():
            raise ValueError(f"Input is not a file: {path}")
        summaries.append({"path": str(path), "bytes": path.stat().st_size})
    return summaries


def ensure_writable_output(path: Path, overwrite: bool) -> None:
    if path.exists() and not overwrite:
        raise FileExistsError(f"Output already exists: {path}. Pass --overwrite to replace it.")
    path.parent.mkdir(parents=True, exist_ok=True)


def free_bytes_for(path: Path) -> int:
    path.parent.mkdir(parents=True, exist_ok=True)
    return shutil.disk_usage(path.parent).free


def concatenate(inputs: list[Path], output: Path) -> int:
    bytes_written = 0
    with output.open("wb") as out_f:
        for idx, input_path in enumerate(inputs):
            with input_path.open("rb") as in_f:
                while True:
                    chunk = in_f.read(BUFFER_SIZE)
                    if not chunk:
                        break
                    out_f.write(chunk)
                    bytes_written += len(chunk)
            if idx != len(inputs) - 1:
                out_f.write(b"\n\n")
                bytes_written += 2
    return bytes_written


def main() -> int:
    args = parse_args()
    inputs = args.input_pgn or DEFAULT_INPUTS
    input_summaries = check_inputs(inputs)
    expected_bytes = sum(int(item["bytes"]) for item in input_summaries) + max(len(inputs) - 1, 0) * 2
    summary_path = args.summary_json or args.output_pgn.with_suffix(".summary.json")
    free_bytes = free_bytes_for(args.output_pgn)

    summary = {
        "input_pgns": input_summaries,
        "output_pgn": str(args.output_pgn),
        "expected_output_bytes": expected_bytes,
        "free_bytes": free_bytes,
        "dry_run": bool(args.dry_run),
    }
    if args.dry_run:
        print(json.dumps(summary, indent=2))
        return 0

    ensure_writable_output(args.output_pgn, args.overwrite)
    if not args.skip_free_space_check and free_bytes < expected_bytes:
        raise OSError(
            f"Not enough free disk for {args.output_pgn}: need about {expected_bytes:,} bytes, "
            f"available {free_bytes:,}. Free space or pass --skip-free-space-check."
        )

    start = time.perf_counter()
    bytes_written = concatenate(inputs, args.output_pgn)
    summary.update(
        {
            "dry_run": False,
            "bytes_written": bytes_written,
            "seconds": time.perf_counter() - start,
            "summary_json": str(summary_path),
        }
    )
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
