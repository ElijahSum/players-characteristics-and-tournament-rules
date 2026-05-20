# Player Characteristics and Adaptability to Temporal Constraints

Master thesis repository, HSE University, Faculty of Computer Science, Master of Data Science, 2026.

Topic: **Player Characteristics and Adaptability to Temporal Constraints: Evidence from Titled Tuesday Chess Tournaments**

**Student:** Ilia Sumernikov

**Academic supervisor:** Dmitry Dagaev

**Format:** individual

## Abstract

This project studies how a real rule change in Chess.com Titled Tuesday affected elite online chess performance, participation, and decision quality. In September 2025, Chess.com changed Titled Tuesday from two weekly 3+1 increment events to one weekly 5+0 no-increment event. The reform changed both the chess time-control technology and the tournament schedule.

The thesis combines game-level econometrics, tournament participation models, Chess.com accuracy data, PGN scraping, Stockfish move evaluation, per-move clock annotations, interpretable player-style features, and neural contrastive style embeddings. The core result is that the new format increased returns to relative skill: stronger players and players with higher realized move quality converted advantages into points more effectively under 5+0. The mechanism is not simply "more time pressure." Ordinary low-clock exposure fell because players started with more time, but severe time trouble became much more costly because the increment no longer insured recovery.

## Thesis, Results, and Artifacts

| Link | Contents |
|---|---|
| [`main.pdf`](main.pdf) | Full thesis PDF. |
| [`paper/main.tex`](paper/main.tex) | LaTeX source for the thesis. |
| [`paper/figures/`](paper/figures/) | Thesis figures, including event-study, clock, Stockfish, and style visualizations. |
| [`paper/tables/`](paper/tables/) | Generated LaTeX result tables used in the thesis. |
| [`docs/RUN_ORDER.md`](docs/RUN_ORDER.md) | Suggested order for reconstructing the empirical pipeline. |
| [`docs/THESIS_TO_SCRIPTS_MAPPING.md`](docs/THESIS_TO_SCRIPTS_MAPPING.md) | Mapping between thesis sections and replication scripts. |
| [`data/DATA_REQUIREMENTS.md`](data/DATA_REQUIREMENTS.md) | Required external data inputs and expected derived files. |
| [Interactive style explorer](https://eli-sumernikov-master-thesis-styles.netlify.app) | Searchable visualization of the style-feature and neural-cluster analysis. |

Raw data, constructed CSV/RDS files, PGNs, SQLite databases, neural checkpoints, scrape caches, and other heavy generated outputs are intentionally excluded from this repository.

## Main Results

| Area | Finding | Interpretation |
|---|---|---|
| Relative skill | A 100-point rating advantage became worth about 1.2 additional percentage points of score share after the reform. | The 5+0 format increased returns to strength rather than increasing underdog randomness. |
| Accuracy-to-points conversion | A 10-point Chess.com accuracy advantage converted more strongly into score after the reform. | The result became more tightly linked to realized move quality. |
| Online-platform capital | Players with higher Chess.com ratings relative to classical ratings gained more. | Platform-specific skill, interface fluency, premoves, and no-increment practice became more valuable. |
| Age heterogeneity | Younger players gained relative to older players, with an estimate of about -1.45 percentage points per additional 10 years. | Adaptation to 5+0 was heterogeneous, although placebo evidence calls for cautious causal language. |
| Time-slot displacement | Pre-reform late-slot regulars became less likely to appear and completed fewer tournaments. | The schedule change mainly affected participation and completion, not per-game accuracy conditional on playing. |
| Field composition | The field became smaller and more positively selected after the reform. | The post-change tournament changed both performance incentives and participant selection. |
| Late-game move quality | Late phases and final-ten-ply positions became more costly in capped centipawn loss. | The no-increment format shifted errors toward positions where conversion and clock handling matter. |
| Critical positions | Blunder probability in near-equal critical positions rose by about 0.20 percentage points. | Small evaluation mistakes became more expensive in tactically sensitive positions. |
| Clock mechanism | Moves started with 10 seconds or less fell from about 10.0% to 3.8%, but the post-change penalty below 10 seconds rose sharply. | 5+0 reduced ordinary low-clock exposure while making severe time trouble much more dangerous. |
| Conversion and recovery | Rating favorites converted winning positions faster, lost fewer advantages, and recovered more from losing positions. | The main mechanism is conversion under a different clock technology. |
| Playing style | Historically chaotic pre-change styles did not receive a relative score premium under 5+0. | The reform rewarded clean conversion and defensive stability more than practical chaos. |
| Neural style clusters | All five learned style clusters improved raw average accuracy after the reform. | Extra initial time improved average move quality broadly, not only for one narrow style type. |

The thesis also reports several unsupported mechanisms. The data do not show robust evidence that the reform mainly operated through White advantage, GDP-linked resource differences, country local-time exposure, opening preparation, simplification strategy, repeated-opponent familiarity, broad gender effects, or post-blunder error cascades.

## Data Scope

The main player-game panel covers Chess.com Titled Tuesday games from February 2022 through April 2026. It combines tournament metadata, player names, ratings, titles, colors, rounds, event identifiers, outcomes, accuracy measures, and available demographic or country-linked variables.

The move-level archive uses Chess.com PGNs, clock annotations, and Stockfish evaluations from two local output folders:

| Source folder | Stockfish move rows | PGN game records | Unique PGN game IDs | Players in PGN headers | Tournament URLs |
|---|---:|---:|---:|---:|---:|
| `whole_dataset_2022_2024` | 42,433,430 | 484,197 | 484,197 | 5,802 | 261 |
| `whole_dataset_2024_2026` | 28,435,053 | 321,059 | 321,059 | 6,103 | 148 |
| Combined | 70,868,483 | 805,256 | 799,056 | 8,053 | 406 |

The combined PGNs contain 6,200 duplicate game IDs across the overlapping folders, so the summed PGN game-record count is 805,256 while the unique game-ID count is 799,056. In the Stockfish CSVs, 794,912 distinct game IDs have move rows, producing 1,586,088 observed player-games with at least one move and 70,325,599 unique `(game_id, ply)` rows after de-duplicating repeated games.

The neural style model is trained on pre-change move sequences for eligible repeated players and produces player-level style embeddings and clusters.

## Repository Structure

```text
players-characteristics-and-tournament-rules/
|-- README.md
|-- main.pdf
|-- pyproject.toml
|-- requirements.R
|-- code/
|   |-- 01_data_collection/
|   |   |-- accuracy_chesscom/
|   |   |-- pgn_fetching/
|   |   `-- player_metadata/
|   |-- 02_data_construction/
|   |   |-- final_player_game_panel/
|   |   |-- move_clock_features/
|   |   |-- stockfish_centipawn_loss/
|   |   `-- style_features/
|   |-- 03_analysis/
|   |   |-- game_level/
|   |   `-- move_clock_level/
|   |-- 04_reporting/
|   |   |-- figure_scripts/
|   |   `-- table_scripts/
|   `-- 05_apps/
|       `-- opening_explorer/
|-- data/
|   `-- DATA_REQUIREMENTS.md
|-- docs/
|   |-- PACKAGE_AUDIT.md
|   |-- RUN_ORDER.md
|   `-- THESIS_TO_SCRIPTS_MAPPING.md
|-- external/
|   `-- stockfish_source_reference/
`-- paper/
    |-- main.tex
    |-- references.bib
    |-- sections/
    |-- figures/
    `-- tables/
```

## Pipeline Overview

Scripts are grouped by stage. The full command sequence is listed in [`docs/RUN_ORDER.md`](docs/RUN_ORDER.md).

| Stage | Code | Purpose |
|---|---|---|
| Player-game panel | `code/02_data_construction/final_player_game_panel/` | Merge tournament records, games, player rows, ratings, scores, ranks, and event-state variables. |
| Player metadata | `code/01_data_collection/player_metadata/` | Enrich Chess.com players with real names, FIDE records, gender, federation, age, and country variables where available. |
| Chess.com accuracy | `code/01_data_collection/accuracy_chesscom/` | Collect and parse game-level Chess.com accuracy measures used in production-function models. |
| PGN fetching | `code/01_data_collection/pgn_fetching/` | Fetch, clean, and concatenate Titled Tuesday PGNs. |
| Stockfish evaluation | `code/02_data_construction/stockfish_centipawn_loss/` | Run Stockfish on PGNs and create move-level centipawn-loss data. |
| Clock and move features | `code/02_data_construction/move_clock_features/` | Build low-clock, phase, critical-position, conversion, recovery, and mechanism variables. |
| Style features | `code/02_data_construction/style_features/` | Build interpretable pre-change style indexes and neural contrastive style embeddings. |
| Game-level analysis | `code/03_analysis/game_level/` | Estimate rating, accuracy, age, online-capital, participation, rank, and null-result models. |
| Move-clock analysis | `code/03_analysis/move_clock_level/` | Estimate Stockfish, low-clock, conversion, defensive recovery, and move-level mechanism models. |
| Reporting | `code/04_reporting/` | Regenerate thesis tables and figures from analysis outputs. |
| Optional app | `code/05_apps/opening_explorer/` | Build and serve an opening explorer for interactive inspection. |

## System Requirements

The original workflow used:

- Python 3.12 or later;
- R with the packages installed by [`requirements.R`](requirements.R);
- Stockfish, supplied separately as a local binary;
- LaTeX for compiling the thesis;
- browser automation and Node-related tooling for parts of the Chess.com accuracy workflow;
- enough disk space for large PGN, Stockfish, CSV, parquet, and model-output files outside version control.

Python dependencies are listed in [`pyproject.toml`](pyproject.toml). A basic setup is:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install .
Rscript requirements.R
```

For Stockfish-based replication, download a compatible Stockfish binary separately and pass its path to `code/02_data_construction/stockfish_centipawn_loss/calculate_centipawn_loss.py`.

## Data

The raw and derived data are excluded because they are large. You can write me in Telegram and I will provide you with the link to my Dropbox folder with all the data.

A full rerun requires:

- Titled Tuesday tournament metadata with game links, rounds, scores, ratings, colors, results, event identifiers, and dates;
- player metadata with Chess.com usernames, real names where available, FIDE identifiers where available, titles, gender, federations, birth years, and country variables;
- country-level economic variables used for GDP and federation-country mappings;
- Chess.com game accuracy outputs;
- PGNs for Titled Tuesday games, including clock annotations where available;
- Stockfish centipawn-loss outputs created from the PGNs;
- intermediate move-clock, style, neural-embedding, and analysis-output tables.

Expected input and output locations are documented in [`data/DATA_REQUIREMENTS.md`](data/DATA_REQUIREMENTS.md).

