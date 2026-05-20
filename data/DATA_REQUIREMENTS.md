# Data Requirements

This replication package does not include raw or derived data. To run the full workflow, restore the required files under a local data/output layout and adjust command-line arguments when a script exposes them.

## Core Inputs

- Tournament metadata with game links, rounds, scores, ratings, colors, results, event identifiers, and dates.
- Player-level metadata with Chess.com username, real name where available, FIDE identifier where available, title, gender, federation, birth year, and country variables.
- Country-level economic variables used for GDP and federation-country mappings.
- Chess.com game accuracy outputs when reproducing accuracy-based models.
- PGN files for Titled Tuesday games, including clock annotations where available.
- Stockfish centipawn-loss outputs created from the PGNs.

## Main Derived Files Expected by the Analysis

- Final player-game panel used by game-level regressions.
- PGN waves under `outputs/whole_dataset_2022_2024/`, `outputs/whole_dataset_2024_2026/`, and the combined file under `outputs/whole_dataset_2022_2026/`.
- Combined centipawn-loss CSVs under the corresponding `outputs/whole_dataset_*` folders.
- Move-clock feature tables.
- Missing-mechanism and deeper-mechanism feature tables.
- Interpretable pre-change player style features.
- Neural style dataset, player embeddings, cluster assignments, and quality-check outputs.
- Analysis output CSVs consumed by the reporting scripts.

## Notes

The package keeps generated data out of version control. A practical rerun can use an `outputs/` directory for PGNs, Stockfish results, intermediate feature tables, and analysis outputs. Scripts that still contain hard-coded defaults from the original research environment should be run from the package root with explicit input/output arguments when available.
