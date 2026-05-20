# Master Thesis Replication Package

Code-only replication package for:

**Player Characteristics and Adaptability to Temporal Constraints: Evidence from Titled Tuesday Chess Tournaments**

The package is organized as a reproducible workflow rather than as the original local research folders. Heavy raw data, derived CSVs, PGNs, SQLite databases, neural checkpoints, RDS files, and scrape caches are intentionally excluded.

## Repository Layout

- `code/01_data_collection/`: scripts for Chess.com accuracy collection, player metadata enrichment, and PGN fetching.
- `code/02_data_construction/`: scripts that build the final player-game panel, Stockfish centipawn-loss data, clock/move mechanism features, and style features.
- `code/03_analysis/`: game-level, tournament-level, move-level, and clock-time econometric analyses.
- `code/04_reporting/`: scripts that regenerate thesis tables and figures from analysis outputs.
- `code/05_apps/`: optional opening-explorer service and static frontend used for interactive inspection.
- `paper/`: LaTeX source, bibliography, generated tables, and generated figures included in the thesis.
- `docs/`: run order, thesis-to-script mapping, and package audit.
- `data/`: documentation for required external inputs. The actual data are not included.
- `external/`: third-party reference code kept separate from thesis code. A Stockfish binary still needs to be supplied separately.

## What Is Excluded

The package omits files that are large, generated, private, or environment-specific:

- raw and constructed data files: `.csv`, `.rds`, `.RDS`, `.xlsx`, `.xls`;
- move archives and PGNs: `.pgn`, `.sqlite`, `.db`;
- trained model artifacts: `.pt`, `.pth`, `.pkl`;
- local virtual environments, browser profiles, scrape caches, and temporary outputs;
- generated markdown result notes under the old `analysis_outputs/` workflow.

See `data/DATA_REQUIREMENTS.md` for the required input files and expected output locations for a full rerun.

## Reproduction Order

1. Restore or reconstruct the required data inputs listed in `data/DATA_REQUIREMENTS.md`.
2. Build the player-game panel with `code/02_data_construction/final_player_game_panel/`.
3. Enrich player metadata with `code/01_data_collection/player_metadata/`.
4. Collect Chess.com accuracy with `code/01_data_collection/accuracy_chesscom/`.
5. Fetch and concatenate PGNs with `code/01_data_collection/pgn_fetching/`.
6. Run Stockfish and attach clock annotations with `code/02_data_construction/stockfish_centipawn_loss/`.
7. Build move, clock, and style features with `code/02_data_construction/`.
8. Run the R analyses in `code/03_analysis/`.
9. Regenerate tables and figures with `code/04_reporting/`.
10. Compile the thesis in `paper/`.

Concrete commands are listed in `docs/RUN_ORDER.md`. The mapping from thesis sections to scripts is in `docs/THESIS_TO_SCRIPTS_MAPPING.md`.

## Dependencies

Python dependencies are listed in `pyproject.toml`; R dependencies are listed in `requirements.R`. The original workflow used Python 3.12/3.13, R, Stockfish, LaTeX, and browser automation for Chess.com accuracy collection.

For Stockfish-based replication, download a compatible binary separately and pass its path to `code/02_data_construction/stockfish_centipawn_loss/calculate_centipawn_loss.py`.
