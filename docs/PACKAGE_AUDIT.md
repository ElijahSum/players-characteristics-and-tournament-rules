# Package Audit

Prepared on 2026-05-20 after pulling the latest thesis repository.

## Completion Checks

- Latest thesis repository was pulled with `git pull --ff-only`; it was already up to date.
- Thesis text was reread through the data, methodology, results, null-results, and appendix sections.
- Scripts were mapped to every empirical component described in the thesis.
- The package was created at `github_master_thesis/`.
- The package was reorganized into a replication workflow under `code/01_data_collection/`, `code/02_data_construction/`, `code/03_analysis/`, `code/04_reporting/`, and `code/05_apps/`.
- Heavy raw/derived data files were excluded.
- No files with these extensions remain in the package: `.csv`, `.rds`, `.RDS`, `.pgn`, `.sqlite`, `.pt`, `.xlsx`, `.xls`, `.zip`, `.jsonl`, `.txt`.
- Generated markdown outputs under the old `analysis_outputs/` workflow were removed; source scripts used to generate results, figures, and tables remain.
- No file larger than 5 MB remains in the package.

## File Counts by Top-Level Folder

- `code/01_data_collection/`: 24 files, accuracy collection, player metadata enrichment, and PGN fetching.
- `code/02_data_construction/`: 23 files, final panel construction, Stockfish/clock construction, move mechanisms, and style features.
- `code/03_analysis/`: 34 files, game-level, tournament-level, move-level, and clock-time econometric analyses.
- `code/04_reporting/`: 5 files, thesis table and figure generation scripts.
- `code/05_apps/`: 19 files, optional opening-explorer database builders, server, and static frontend.
- `data/`: 1 file, data requirements.
- `docs/`: 3 files, run order, mapping, and this audit.
- `external/`: 83 files, third-party Stockfish source reference only.
- `paper/`: 37 files, LaTeX source, generated thesis figures, and generated tables.

Total package size after excluding data: about 20 MB. The package contains 48 Python scripts and 36 R scripts.

## Notes

The package is intended for GitHub release and methodological inspection. A full end-to-end rerun still requires restoring the data inputs listed in `data/DATA_REQUIREMENTS.md` and installing a local Stockfish binary.
