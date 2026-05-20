# Suggested Run Order

The original research process was exploratory, but this package is arranged as a staged replication workflow. All commands assume the working directory is the root of this replication package.

## 0. Prepare Data and Dependencies

Restore the files listed in `data/DATA_REQUIREMENTS.md`. Then install the software dependencies:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install .
Rscript requirements.R
```

Download a Stockfish binary separately and keep its path available as `/path/to/stockfish`.

## 1. Build the Player-Game Panel

```bash
.venv/bin/python code/02_data_construction/final_player_game_panel/04_run_buchholz_cut1_pipeline.py
.venv/bin/python code/02_data_construction/final_player_game_panel/05_run_full_pipeline.py
.venv/bin/python code/02_data_construction/final_player_game_panel/update_final_regression_player_columns.py
```

The numbered component scripts in `code/02_data_construction/final_player_game_panel/` can be run directly if intermediate files need to be inspected.

## 2. Enrich Player Metadata

Use the scripts in `code/01_data_collection/player_metadata/` to collect and merge real names, FIDE records, gender, birthday, federation, and country variables.

Network-dependent scripts should be run cautiously because Chess.com, search engines, FIDE, and Wikidata can rate-limit requests.

## 3. Collect Chess.com Accuracy

```bash
Rscript code/01_data_collection/accuracy_chesscom/accuracy.R
.venv/bin/python code/01_data_collection/accuracy_chesscom/accuracy_fast_parsing.py
```

The browser/proxy setup is local-environment dependent. The thesis uses the resulting accuracy columns in the final player-game panel.

## 4. Fetch PGNs

```bash
.venv/bin/python code/01_data_collection/pgn_fetching/fetch_chesscom_pgns_1000.py \
  --workers 8 \
  --output-dir outputs/whole_dataset_2024_2026 \
  --output-pgn-name whole_dataset_2024_2026.pgn
```

For multi-wave scraping, repeat for each wave and then combine PGN files:

```bash
.venv/bin/python code/01_data_collection/pgn_fetching/concatenate_pgn_datasets.py
```

## 5. Run Stockfish and Attach Clock Data

```bash
.venv/bin/python code/02_data_construction/stockfish_centipawn_loss/calculate_centipawn_loss.py \
  --pgn outputs/whole_dataset_2024_2026/whole_dataset_2024_2026.pgn \
  --workers 8 \
  --nodes 2000 \
  --stockfish /path/to/stockfish \
  --output-dir outputs/whole_dataset_2024_2026/centipawn_loss_nodes2000
```

The Stockfish script also supports watch mode while PGNs are still being scraped.

## 6. Build Move, Clock, and Style Features

```bash
.venv/bin/python code/02_data_construction/move_clock_features/build_clock_time_features.py
.venv/bin/python code/02_data_construction/move_clock_features/build_deep_clock_mechanism_features.py
.venv/bin/python code/02_data_construction/move_clock_features/build_exact_clock_mechanism_cells.py
.venv/bin/python code/02_data_construction/move_clock_features/build_interaction_clock_features.py
.venv/bin/python code/02_data_construction/move_clock_features/build_missing_mechanism_features.py
.venv/bin/python code/02_data_construction/style_features/build_interpretable_style_features.py
.venv/bin/python code/02_data_construction/style_features/build_contrastive_style_dataset.py
.venv/bin/python code/02_data_construction/style_features/train_contrastive_style_encoder.py
.venv/bin/python code/02_data_construction/style_features/run_style_quality_checks.py
.venv/bin/python code/02_data_construction/style_features/create_style_cluster_plot.py
```

Some scripts have command-line options for input and output paths. Use `--help` where available.

## 7. Run Game-Level Analyses

```bash
Rscript code/03_analysis/game_level/rule_change_mechanism_validation.R
Rscript code/03_analysis/game_level/rule_change_production_function_tests.R
Rscript code/03_analysis/game_level/rule_change_time_slot_displacement_tests.R
Rscript code/03_analysis/game_level/rule_change_age_hypotheses.R
Rscript code/03_analysis/game_level/rule_change_age_matchup_result_tests.R
Rscript code/03_analysis/game_level/rule_change_metadata_econometric_novelty.R
Rscript code/03_analysis/game_level/rule_change_country_time_hypotheses.R
Rscript code/03_analysis/game_level/rule_change_economic_hypotheses.R
Rscript code/03_analysis/game_level/rule_change_rank_hypotheses.R
Rscript code/03_analysis/game_level/rule_change_lagged_upset_tests.R
Rscript code/03_analysis/game_level/rule_change_risk_return_style.R
```

Additional exploratory and null-result scripts are also included in `code/03_analysis/game_level/`.

## 8. Run Move-Level and Clock Analyses

```bash
Rscript code/03_analysis/move_clock_level/analyze_stockfish_move_mechanisms.R
Rscript code/03_analysis/move_clock_level/analyze_missing_move_mechanisms.R
Rscript code/03_analysis/move_clock_level/analyze_deeper_research_ideas.R
Rscript code/03_analysis/move_clock_level/analyze_clock_time_mechanisms.R
Rscript code/03_analysis/move_clock_level/analyze_deep_clock_mechanisms.R
Rscript code/03_analysis/move_clock_level/analyze_exact_clock_requested_specs.R
Rscript code/03_analysis/move_clock_level/analyze_interaction_clock_mechanisms.R
```

## 9. Regenerate Thesis Tables and Figures

```bash
Rscript code/04_reporting/table_scripts/create_master_thesis_tables.R
.venv/bin/python code/04_reporting/figure_scripts/create_master_thesis_figures.py
.venv/bin/python code/04_reporting/figure_scripts/create_advanced_thesis_visuals.py
.venv/bin/python code/04_reporting/figure_scripts/create_clock_mechanism_figures.py
.venv/bin/python code/04_reporting/figure_scripts/create_style_pca_figure.py
```

The generated LaTeX tables should be placed in `paper/tables/`, and figures in `paper/figures/`.

## 10. Compile the Thesis

```bash
cd paper
latexmk -pdf -interaction=nonstopmode main.tex
```
