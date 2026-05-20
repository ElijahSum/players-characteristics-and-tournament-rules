# Thesis-to-Scripts Mapping

This file maps the thesis content to the replication-package workflow. Paths refer to the reorganized package layout.

## 1. Tournament and Game Metadata

Thesis locations:

- Section 3.2, tournament and game metadata scraping.
- Section 3.3, final player-game dataset and tournament-state variables.
- Section 4.2, baseline player-game design.

Scripts:

- `code/02_data_construction/final_player_game_panel/01_merging_datasets_final.py`
- `code/02_data_construction/final_player_game_panel/02_creating_rank_variable.py`
- `code/02_data_construction/final_player_game_panel/03_creating_opponents_score.py`
- `code/02_data_construction/final_player_game_panel/04_run_buchholz_cut1_pipeline.py`
- `code/02_data_construction/final_player_game_panel/05_run_full_pipeline.py`
- `code/02_data_construction/final_player_game_panel/06_merging_data.py`
- `code/02_data_construction/final_player_game_panel/update_final_regression_player_columns.py`

Purpose:

- Reshape tournament games into player-game observations.
- Construct score before and after each round.
- Reconstruct Buchholz Cut 1, Buchholz, Sonneborn-Berger, start/end ranks, prize states, bubble states, and leader states.
- Build the final regression panel used in the game-level analysis.

## 2. Player Metadata, FIDE, Gender, Federation, and GDP

Thesis locations:

- Section 3.4, external player and country data.
- Player-level variable coverage table.
- Heterogeneity results using age, gender, country, GDP, and online capital.

Scripts:

- `code/01_data_collection/player_metadata/07_find_player_metadata_from_fide.py`
- `code/01_data_collection/player_metadata/08_merge_player_metadata_into_players_final_data.py`
- `code/01_data_collection/player_metadata/fill_fide_birthyears.py`
- `code/02_data_construction/final_player_game_panel/find_players_missing_from_metadata.py`
- `code/01_data_collection/player_metadata/fetch_chesscom_names.py`
- `code/01_data_collection/player_metadata/fetch_chesscom_html_names.py`
- `code/01_data_collection/player_metadata/search_ddg_names.py`
- `code/01_data_collection/player_metadata/search_bing_names.py`
- `code/01_data_collection/player_metadata/match_missing_names_fide.py`
- `code/01_data_collection/player_metadata/match_missing_names_fide_fast.py`
- `code/01_data_collection/player_metadata/add_fide_info_to_players.py`
- `code/01_data_collection/player_metadata/add_gender_to_players.py`
- `code/01_data_collection/player_metadata/lookup_missing_birthdays.py`
- `code/01_data_collection/player_metadata/lookup_missing_birthdays_wikidata.py`

Purpose:

- Match Chess.com usernames to real names and FIDE records.
- Attach FIDE ratings, federation, gender, title, and birth-year variables.
- Attach country-level GDP per capita PPP through federation/country mappings.

## 3. Chess.com Accuracy Collection

Thesis locations:

- Section 3.5, accuracy variable creation.
- Section 4.3, accuracy-to-outcome production function.
- Section 5.2, accuracy advantages and score conversion.

Scripts:

- `code/01_data_collection/accuracy_chesscom/accuracy.R`
- `code/01_data_collection/accuracy_chesscom/accuracy_fast_parsing.py`
- `code/01_data_collection/accuracy_chesscom/find_short_games.py`
- `code/01_data_collection/accuracy_chesscom/chess_games_scraper.ipynb`
- `code/01_data_collection/accuracy_chesscom/generating_missing_links_data.ipynb`

Purpose:

- Open Chess.com game pages, trigger or collect game accuracy when available, and merge accuracy into the player-game panel.
- Identify short-game and missing-link edge cases.

## 4. PGN Fetching and Raw Move Data

Thesis locations:

- Section 3.2, PGN collection through the Chess.com Published Data API.
- Section 3.6, PGN and Stockfish data.

Scripts:

- `code/01_data_collection/pgn_fetching/fetch_chesscom_pgns_1000.py`
- `code/01_data_collection/pgn_fetching/concatenate_pgn_datasets.py`
- `code/01_data_collection/pgn_fetching/cleanup_intermediate_outputs.py`

Purpose:

- Query monthly Chess.com player archives.
- Fetch PGNs for games whose links match Titled Tuesday metadata.
- Avoid storing unused full monthly archive data.
- Concatenate scraping waves into a combined PGN file.

## 5. Stockfish Centipawn Loss and Clock Parsing

Thesis locations:

- Section 3.6, Stockfish move evaluation and clock data.
- Section 3.7, move-level variables.
- Section 5.6-5.10, move-level and clock mechanisms.

Scripts:

- `code/02_data_construction/stockfish_centipawn_loss/calculate_centipawn_loss.py`
- `code/02_data_construction/stockfish_centipawn_loss/stockfish.py`
- `code/02_data_construction/stockfish_centipawn_loss/add_clock_times_to_centipawn_loss.py`

Purpose:

- Parse PGNs and reconstruct legal board states.
- Evaluate pre-move and post-move positions using Stockfish.
- Compute centipawn loss, inaccuracy, mistake, and blunder indicators.
- Parse PGN clock annotations into time-before, time-after, and time-spent variables.

## 6. Move-Level Mechanism Features

Thesis locations:

- Section 3.7, phase, critical position, conversion, complexity, and simplification variables.
- Section 4.4, move-level Stockfish models.
- Section 4.6, conversion and defensive recovery.
- Section 5.6-5.9, move-level results.
- Section 6.2, move-level null results.

Scripts:

- `code/02_data_construction/move_clock_features/build_missing_mechanism_features.py`
- `code/02_data_construction/move_clock_features/build_advanced_player_game_dynamics.py`
- `code/03_analysis/move_clock_level/analyze_missing_move_mechanisms.R`
- `code/03_analysis/move_clock_level/analyze_stockfish_move_mechanisms.R`
- `code/03_analysis/move_clock_level/analyze_deeper_research_ideas.R`

Purpose:

- Construct phase bins, last-ten-ply indicators, first-blunder timing, opening features, complexity proxies, simplification variables, conversion variables, defensive escape variables, repeated-opponent features, and error-cascade features.
- Estimate move-mechanism regressions and null tests.

## 7. Clock-Time Mechanisms

Thesis locations:

- Section 3.6-3.7, clock variables.
- Section 4.5, clock-time mechanism models.
- Section 5.8, robust clock-time mechanisms.

Scripts:

- `code/02_data_construction/move_clock_features/build_clock_time_features.py`
- `code/02_data_construction/move_clock_features/build_deep_clock_mechanism_features.py`
- `code/02_data_construction/move_clock_features/build_exact_clock_mechanism_cells.py`
- `code/02_data_construction/move_clock_features/build_interaction_clock_features.py`
- `code/03_analysis/move_clock_level/analyze_clock_time_mechanisms.R`
- `code/03_analysis/move_clock_level/analyze_deep_clock_mechanisms.R`
- `code/03_analysis/move_clock_level/analyze_exact_clock_requested_specs.R`
- `code/03_analysis/move_clock_level/analyze_interaction_clock_mechanisms.R`

Purpose:

- Build player-game and move-level clock features.
- Estimate low-clock exposure, low-clock cost, recovery after low-clock entry, panic-threshold bins, event studies around first low-clock crossing, shadow price of time, and clock-conversion interactions.

## 8. Interpretable Style Features

Thesis locations:

- Section 3.8, style features.
- Section 4.7, style and pre-change traits.
- Section 5.11, playing style.

Scripts:

- `code/02_data_construction/style_features/build_interpretable_style_features.py`
- `code/03_analysis/game_level/rule_change_risk_return_style.R`
- `code/04_reporting/figure_scripts/create_style_pca_figure.py`

Purpose:

- Use pre-change games to construct clean style, chaos creation, self-risk, practical chaos, defensive skill, complexity tolerance, conversion skill, and opening-specialization indexes.
- Test whether predetermined style profiles had different post-change score returns.

## 9. Neural Style Embeddings

Thesis locations:

- Section 3.8, neural style data.
- Section 5.11, neural style-embedding model and clusters.

Scripts:

- `code/02_data_construction/style_features/build_contrastive_style_dataset.py`
- `code/02_data_construction/style_features/train_contrastive_style_encoder.py`
- `code/02_data_construction/style_features/run_style_quality_checks.py`
- `code/02_data_construction/style_features/create_style_cluster_plot.py`
- `code/04_reporting/figure_scripts/create_style_pca_figure.py`

Purpose:

- Select eligible players with enough total games and both pre/post observations.
- Build pre-change move-token sequences excluding the first 10 full moves and last 10 plies.
- Train a supervised contrastive bidirectional-GRU style encoder.
- Average game embeddings into player embeddings, run k-means clustering, and compare neural clusters to interpretable style features.

## 10. Game-Level Main Results and Robustness

Thesis locations:

- Section 5.1-5.5.
- Section 6.1.

Scripts:

- `code/03_analysis/game_level/rule_change_mechanism_validation.R`
- `code/03_analysis/game_level/rule_change_production_function_tests.R`
- `code/03_analysis/game_level/rule_change_age_hypotheses.R`
- `code/03_analysis/game_level/rule_change_age_matchup_result_tests.R`
- `code/03_analysis/game_level/rule_change_time_slot_displacement_tests.R`
- `code/03_analysis/game_level/rule_change_metadata_econometric_novelty.R`
- `code/03_analysis/game_level/rule_change_country_time_hypotheses.R`
- `code/03_analysis/game_level/rule_change_economic_hypotheses.R`
- `code/03_analysis/game_level/rule_change_metadata_heterogeneity_questions.R`
- `code/03_analysis/game_level/rule_change_rank_hypotheses.R`
- `code/03_analysis/game_level/rule_change_lagged_upset_tests.R`
- `code/03_analysis/game_level/rule_change_fatigue_iteration.R`
- `code/03_analysis/game_level/rule_change_second_iteration_new_ideas.R`
- `code/03_analysis/game_level/rule_change_third_iteration_final_outcomes.R`
- `code/03_analysis/game_level/female_matchup_accuracy_tests.R`

Purpose:

- Estimate rating-return, accuracy-production-function, online-capital, age, time-slot, participation, rank movement, field composition, country, GDP, gender, tilt, path difficulty, learning, color, and null-result models.

## 11. Thesis Tables and Figures

Thesis locations:

- Section 5 figures and tables.
- Appendix reproducibility notes.

Scripts:

- `code/04_reporting/table_scripts/create_master_thesis_tables.R`
- `code/04_reporting/figure_scripts/create_master_thesis_figures.py`
- `code/04_reporting/figure_scripts/create_advanced_thesis_visuals.py`
- `code/04_reporting/figure_scripts/create_clock_mechanism_figures.py`
- `code/04_reporting/figure_scripts/create_style_pca_figure.py`

Purpose:

- Convert result CSVs into stargazer LaTeX tables.
- Generate rating, age, rank, phase, conversion, style, clock, and evidence-map figures.

## 12. Optional Opening Explorer

Thesis location:

- Appendix and supplementary inspection workflow.

Scripts:

- `code/05_apps/opening_explorer/build_titled_tuesday_opening_db.py`
- `code/05_apps/opening_explorer/build_titled_tuesday_game_continuations.py`
- `code/05_apps/opening_explorer/build_titled_tuesday_top_games.py`
- `code/05_apps/opening_explorer/titled_tuesday_opening_server.py`
- `code/05_apps/opening_explorer/web/`

Purpose:

- Build an optional local database and browser interface for inspecting opening choices, continuations, and example games.
