#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stargazer)
})

out_root <- file.path("analysis_outputs", "master_thesis_results_assets")
tables_dir <- file.path(out_root, "tables")
figdata_dir <- file.path(out_root, "figure_data")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figdata_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(x))

fmt_num <- function(x, digits = 4) {
  x <- num(x)
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}

fmt_p <- function(x) {
  x <- num(x)
  ifelse(
    is.na(x), "",
    ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format = "f"))
  )
}

stars <- function(p) {
  p <- num(p)
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))
}

clean_table <- function(dt) {
  dt <- as.data.table(dt)
  for (col in intersect(names(dt), c("Estimate", "SE", "CI Low", "CI High", "Q", "R2"))) {
    dt[[col]] <- fmt_num(dt[[col]], 4)
  }
  if ("P" %in% names(dt)) dt[["P"]] <- fmt_p(dt[["P"]])
  if ("N" %in% names(dt)) dt[["N"]] <- ifelse(is.na(num(dt[["N"]])), "", formatC(num(dt[["N"]]), format = "d", big.mark = ","))
  dt[]
}

write_stargazer_table <- function(dt, name, title, notes = NULL) {
  display <- clean_table(dt)
  csv_path <- file.path(tables_dir, paste0(name, ".csv"))
  html_path <- file.path(tables_dir, paste0(name, ".html"))
  tex_path <- file.path(tables_dir, paste0(name, ".tex"))
  txt_path <- file.path(tables_dir, paste0(name, ".txt"))

  fwrite(display, csv_path)
  capture.output(
    stargazer(display, type = "html", summary = FALSE, rownames = FALSE, title = title, notes = notes),
    file = html_path
  )
  capture.output(
    stargazer(display, type = "latex", summary = FALSE, rownames = FALSE, title = title, notes = notes),
    file = tex_path
  )
  capture.output(
    stargazer(display, type = "text", summary = FALSE, rownames = FALSE, title = title, notes = notes),
    file = txt_path
  )
}

pick1 <- function(dt, expr) {
  out <- dt[eval(substitute(expr))]
  if (nrow(out) == 0) return(NULL)
  out[1]
}

coef_row <- function(label, outcome, source, row, q_col = NULL, spec = NULL, unit = NULL) {
  if (is.null(row) || nrow(row) == 0) {
    return(data.table(
      Result = label, Outcome = outcome, Term = "", Estimate = NA_real_, SE = NA_real_,
      P = NA_real_, Q = NA_real_, N = NA_real_, Spec = spec %||% "", Unit = unit %||% "", Source = source
    ))
  }
  q <- if (!is.null(q_col) && q_col %in% names(row)) num(row[[q_col]]) else NA_real_
  term_value <- if ("term" %in% names(row)) row$term[1] else if ("target_term" %in% names(row)) row$target_term[1] else ""
  data.table(
    Result = label,
    Outcome = outcome,
    Term = as.character(term_value),
    Estimate = num(row$estimate[1]),
    SE = num(row$std.error[1]),
    P = num(row$p.value[1]),
    Q = q,
    N = if ("nobs" %in% names(row)) num(row$nobs[1]) else NA_real_,
    Spec = spec %||% if ("specification" %in% names(row)) as.character(row$specification[1]) else "",
    Unit = unit %||% "",
    Source = source
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Table 1: Baseline game-level treatment effects
# ---------------------------------------------------------------------------
main_robust <- fread("analysis_outputs/rule_change_mechanism_validation/main_robustness_coefficients.csv")
prod <- fread("analysis_outputs/rule_change_production_function_tests/player_production_function_tests.csv")
age <- fread("analysis_outputs/rule_change_age_hypotheses/hypothesis_target_coefficients.csv")
age_n <- num(fread("analysis_outputs/rule_change_age_hypotheses/sample_summary.csv")[statistic == "rows", value][1])

t1 <- rbindlist(list(
  coef_row(
    "Relative skill return",
    "Player result",
    "mechanism_validation",
    pick1(main_robust, variable == "rating_diff100" & outcome == "player_result" & specification == "event_fe_all"),
    "p_bh_by_spec_outcome",
    "Event FE",
    "Score-share change per +100 rating advantage"
  ),
  coef_row(
    "Accuracy-to-points conversion",
    "Player result",
    "production_function",
    pick1(prod, hypothesis == "PF01_accuracy_conversion" & outcome == "player_result" & specification == "event_fe"),
    "p_bh_by_spec",
    "Event FE",
    "Score-share change per +10 accuracy points"
  ),
  coef_row(
    "Online-platform capital",
    "Player result",
    "mechanism_validation",
    pick1(main_robust, variable == "online_classic_gap100" & outcome == "player_result" & specification == "event_fe_all"),
    "p_bh_by_spec_outcome",
    "Event FE",
    "Score-share change per +100 Chess.com-over-classical rating"
  ),
  coef_row(
    "Age heterogeneity",
    "Player result",
    "age_hypotheses",
    pick1(age, hypothesis == "H1_age_post" & outcome == "player_result"),
    "p_bh_within_outcome",
    "Player/date FE",
    "Score-share change per +10 years of age"
  )
), fill = TRUE)
t1[Result == "Age heterogeneity", N := age_n]
t1[, Sig := stars(P)]
write_stargazer_table(
  t1[, .(Result, Outcome, Term, Estimate, SE, P, Q, Sig, N, Unit, Spec)],
  "table1_baseline_game_level_effects",
  "Baseline Game-Level Treatment Effects",
  "All estimates are post-change interaction terms."
)

# ---------------------------------------------------------------------------
# Table 2: Rating-advantage robustness and placebo comparison
# ---------------------------------------------------------------------------
spec_labels <- c(
  event_fe_all = "Event FE, all rounds > 1",
  event_fe_balanced_players = "Balanced players",
  event_fe_near_window_pm12m = "+/- 12 month window",
  event_fe_round_gt2 = "Exclude rounds 1 and 2",
  paired_game_fe = "Paired-game FE"
)
rating_robust <- main_robust[
  variable == "rating_diff100" &
    outcome == "player_result" &
    specification %in% names(spec_labels)
]
rating_robust[, Spec := spec_labels[specification]]
t2 <- rating_robust[, .(
  Spec,
  Term = term,
  Estimate = num(estimate),
  SE = num(std.error),
  P = num(p.value),
  Q = num(p_bh_by_spec_outcome),
  N = num(nobs)
)]

placebo <- fread("analysis_outputs/rule_change_mechanism_validation/placebo_grid_result_summary.csv")
pl <- placebo[variable == "rating_diff100"][1]
t2 <- rbind(
  t2,
  data.table(
    Spec = "Placebo grid mean",
    Term = "fake cutoff x rating_diff100",
    Estimate = num(pl$placebo_mean),
    SE = num(pl$placebo_sd),
    P = num(pl$empirical_two_sided_p),
    Q = NA_real_,
    N = num(pl$n_placebos)
  ),
  fill = TRUE
)
t2[, Sig := stars(P)]
write_stargazer_table(
  t2[, .(Spec, Term, Estimate, SE, P, Q, Sig, N)],
  "table2_rating_advantage_robustness",
  "Rating-Advantage Robustness and Placebo Estimates",
  "The placebo row reports the mean and SD across fake pre-rule cutoffs."
)

# ---------------------------------------------------------------------------
# Table 3: Time-slot displacement
# ---------------------------------------------------------------------------
slot_game <- fread("analysis_outputs/rule_change_time_slot_displacement_tests/game_level_displacement_coefficients.csv")
slot_event <- fread("analysis_outputs/rule_change_time_slot_displacement_tests/event_level_displacement_coefficients.csv")
slot_part <- fread("analysis_outputs/rule_change_time_slot_displacement_tests/participation_selection_coefficients.csv")

slot_rows <- rbindlist(list(
  coef_row("Any post event", "Participation", "time_slot_participation", pick1(slot_part, outcome == "has_post"), NULL, "Player-level LPM", "Pure late-slot vs pure early-slot exposure"),
  coef_row("Number of post events", "Participation", "time_slot_participation", pick1(slot_part, outcome == "post_events"), NULL, "Player-level OLS", "Pure late-slot vs pure early-slot exposure"),
  coef_row("Tournament score after R1", "Tournament", "time_slot_event", pick1(slot_event, treatment == "late_regular_vs_early" & outcome == "score_after_r1_pct"), "p_bh", "Player/event FE", "Late regular vs early regular"),
  coef_row("Final rank percentile", "Tournament", "time_slot_event", pick1(slot_event, treatment == "late_regular_vs_early" & outcome == "rank_percentile_high_good"), "p_bh", "Player/event FE", "Late regular vs early regular"),
  coef_row("Games completed after R1", "Completion", "time_slot_event", pick1(slot_event, treatment == "late_regular_vs_early" & outcome == "games_after_r1_pct"), "p_bh", "Player/event FE", "Late regular vs early regular"),
  coef_row("Completed after R1", "Completion", "time_slot_event", pick1(slot_event, treatment == "late_regular_vs_early" & outcome == "completed_after_r1"), "p_bh", "Player/event FE", "Late regular vs early regular"),
  coef_row("Per-game accuracy", "Per-game null", "time_slot_game", pick1(slot_game, treatment == "late_regular_vs_early" & outcome == "player_accuracy"), "p_bh", "Player/event FE", "Late regular vs early regular"),
  coef_row("Per-game result", "Per-game null", "time_slot_game", pick1(slot_game, treatment == "late_regular_vs_early" & outcome == "player_result"), "p_bh", "Player/event FE", "Late regular vs early regular")
), fill = TRUE)
slot_rows[, Sig := stars(P)]
write_stargazer_table(
  slot_rows[, .(Result, Outcome, Estimate, SE, P, Q, Sig, N, Unit, Spec)],
  "table3_time_slot_displacement",
  "Time-Slot Displacement: Participation, Completion, and Per-Game Nulls",
  "Negative tournament and completion estimates mean old late-slot regulars did worse relative to old early-slot regulars."
)

# ---------------------------------------------------------------------------
# Table 4: Move mechanisms
# ---------------------------------------------------------------------------
sf_head <- fread("analysis_outputs/stockfish_move_mechanisms_full_2022_2026/headline_format_interactions.csv")
sf_pg <- fread("analysis_outputs/stockfish_move_mechanisms_full_2022_2026/player_game_model_coefficients.csv")
deep <- fread("analysis_outputs/deeper_research_ideas_full_2022_2026/conversion_deep_model_coefficients.csv")

move_rows <- rbindlist(list(
  coef_row("Late phase error", "Capped centipawn loss", "stockfish_move", pick1(sf_head, model == "late_phase_error_age" & term == "format_5_0:late_phase"), "q.value", "Move-level FE", "Endgame phase, fullmove 36+"),
  coef_row("Last-10-ply error", "Capped centipawn loss", "stockfish_move", pick1(sf_head, model == "last10_error_age" & term == "format_5_0:last_10_ply"), "q.value", "Move-level FE", "Final 10 plies of the game"),
  coef_row("Critical/equal blunder", "Blunder probability", "stockfish_move", pick1(sf_head, model == "critical_equal_blunder_age" & term == "format_5_0:critical_equal"), "q.value", "Move-level FE", "Pre-move abs(eval) < 100 cp"),
  coef_row("Critical/equal CPL", "Capped centipawn loss", "stockfish_move", pick1(sf_head, model == "critical_equal_error_age" & term == "format_5_0:critical_equal"), "q.value", "Move-level FE", "Pre-move abs(eval) < 100 cp"),
  coef_row("Winning conversion", "Converted winning position", "stockfish_player_game", pick1(sf_pg, model == "conversion_from_winning_position" & term == "format_5_0:rating_diff100"), "q.value", "Player-game FE", "Post-change slope per +100 rating advantage"),
  coef_row("Defensive escape", "Escaped losing position", "stockfish_player_game", pick1(sf_pg, model == "defensive_escape_from_losing_position" & term == "format_5_0:rating_diff100"), "q.value", "Player-game FE", "Post-change slope per +100 rating advantage"),
  coef_row("Conversion speed after +2", "Plies to conversion", "deeper_conversion", pick1(deep, model == "conversion_speed_after_plus2" & term == "format_5_0:rating_diff100"), "q.value", "Player-game FE", "Post-change slope per +100 rating advantage"),
  coef_row("Recovery after -2", "Defensive recovery probability", "deeper_conversion", pick1(deep, model == "defensive_recovery_after_minus2" & term == "format_5_0:rating_diff100"), "q.value", "Player-game FE", "Post-change slope per +100 rating advantage"),
  coef_row("First blunder timing by age", "First blunder move", "stockfish_player_game", pick1(sf_head, model == "first_blunder_move_conditional" & term == "format_5_0:age10_c"), "q.value", "Player-game FE", "Post-change slope per +10 years of age"),
  coef_row("First blunder timing by female status", "First blunder move", "stockfish_player_game", pick1(sf_head, model == "first_blunder_move_conditional" & term == "format_5_0:female"), "q.value", "Player-game FE", "Female post-change shift")
), fill = TRUE)
move_rows[, Sig := stars(P)]
write_stargazer_table(
  move_rows[, .(Result, Outcome, Term, Estimate, SE, P, Q, Sig, N, Unit)],
  "table4_move_mechanisms",
  "Move-Level and Stockfish Mechanism Estimates",
  "Move-level models use player/tournament/move controls; player-game models use player-game outcomes derived from Stockfish evaluations."
)

# ---------------------------------------------------------------------------
# Table 5: Style indexes
# ---------------------------------------------------------------------------
style <- fread("analysis_outputs/rule_change_risk_return_style/headline_risk_return_coefficients.csv")
style_sel <- style[q.value <= 0.10 & grepl("chaos_creator|practical_chaos|clean_style|self_risk", term)]
style_sel <- style_sel[order(q.value, -abs_t)][1:min(.N, 10)]
style_tab <- style_sel[, .(
  Model = model,
  Term = term,
  Estimate = num(estimate),
  SE = num(std.error),
  P = num(p.value),
  Q = num(q.value),
  N = num(nobs)
)]
style_tab[, Sig := stars(P)]
write_stargazer_table(
  style_tab[, .(Model, Term, Estimate, SE, P, Q, Sig, N)],
  "table5_style_index_interactions",
  "Pre-Change Interpretable Style Indexes and Post-Change Payoffs",
  "Style indexes are computed only from pre-change moves; coefficients are post-change interactions."
)

# ---------------------------------------------------------------------------
# Table 6: Null or weak results
# ---------------------------------------------------------------------------
meta <- fread("analysis_outputs/rule_change_metadata_heterogeneity_questions/research_question_results.csv")
econ <- fread("analysis_outputs/rule_change_economic_hypotheses/economic_hypothesis_coefficients.csv")
country_time <- fread("analysis_outputs/rule_change_country_time_hypotheses/game_level_country_time_coefficients.csv")
opening <- fread("analysis_outputs/missing_move_mechanisms_full_2022_2026/opening_model_coefficients.csv")
simplification <- fread("analysis_outputs/missing_move_mechanisms_full_2022_2026/simplification_model_coefficients.csv")
cascade <- fread("analysis_outputs/deeper_research_ideas_full_2022_2026/error_cascade_model_coefficients.csv")

null_rows <- rbindlist(list(
  coef_row("GDP and score", "Player result", "metadata", pick1(meta, question_id == "RQ4"), "q_value", "Player/tournament FE", "Direct resource-performance channel"),
  coef_row("White advantage", "Player result", "economic_hypotheses", pick1(econ, hypothesis == "E05_white_advantage" & outcome == "player_result"), "p_bh_within_outcome", "Player/date FE", "Post-change first-move advantage"),
  coef_row("Country local-time burden", "Player result", "country_time", pick1(country_time, test_id == "G02_result_post_first_penalty"), "p_bh", "Player/event FE", "Remaining slot inconvenience"),
  coef_row("Opening/book-exit age", "Book exit move", "opening_features", pick1(opening, model == "book_exit_move_metadata" & term == "format_5_0:age10_c"), "q.value", "Player-game FE", "Opening preparation proxy"),
  coef_row("Simplification strategy", "Player result", "simplification", pick1(simplification, model == "player_result_simplified_age" & term == "format_5_0:simplified_move30"), "q.value", "Player-game FE", "Simplified by move 30"),
  coef_row("Error cascade after blunder", "Next-5 blunder probability", "error_cascade", pick1(cascade, model == "cascade_blunder_next5" & term == "format_5_0:age10_c"), "q.value", "Player-game FE", "Post-blunder cascade by age")
), fill = TRUE)
null_rows[, Support := ifelse(!is.na(Q) & Q < 0.05, "Statistically clear", "Null/weak after correction")]
write_stargazer_table(
  null_rows[, .(Result, Outcome, Term, Estimate, SE, P, Q, N, Unit, Support)],
  "table6_null_and_weak_results",
  "Null and Weak Results Across Player/Game and Move-Level Mechanisms",
  "Rows summarize representative tests from mechanisms that were explored but not supported as robust headline results."
)

# ---------------------------------------------------------------------------
# Small figure-data summaries for Python plotting
# ---------------------------------------------------------------------------
message("Creating figure data summaries...")

rank_file <- "data/final_regression_data_tournaments_2022_2026.csv"
rank_dt <- fread(
  rank_file,
  select = c("date", "round", "player_rating", "opponent_rating", "rank", "rank_end_round")
)
rank_dt <- rank_dt[
  round > 1 &
    !is.na(player_rating) & !is.na(opponent_rating) &
    !is.na(rank) & !is.na(rank_end_round)
]
rank_dt[, event_id := date]
rank_dt[, format_5_0 := as.integer(substr(date, 1, 10) >= "2025-09-01")]
rank_dt[, event_size := max(rank, rank_end_round, na.rm = TRUE), by = event_id]
rank_dt <- rank_dt[event_size > 1]
rank_dt[, pre_rank_pct := (rank - 1) / (event_size - 1)]
rank_dt[, end_rank_pct := (rank_end_round - 1) / (event_size - 1)]
rank_dt[, rank_improvement_pct := pre_rank_pct - end_rank_pct]
rank_dt[, rating_diff100 := (player_rating - opponent_rating) / 100]
rank_dt[, rating_bin := cut(
  rating_diff100,
  breaks = c(-Inf, -3, -1, 1, 3, Inf),
  labels = c("Underdog >300", "Underdog 100-300", "Close +/-100", "Favorite 100-300", "Favorite >300"),
  right = FALSE
)]
rank_summary <- rank_dt[, .(
  n = .N,
  mean_rank_improvement = mean(rank_improvement_pct, na.rm = TRUE),
  se_rank_improvement = sd(rank_improvement_pct, na.rm = TRUE) / sqrt(.N)
), by = .(format_5_0, rating_bin)]
rank_summary[, period := ifelse(format_5_0 == 1, "5+0", "3+1")]
fwrite(rank_summary[order(format_5_0, rating_bin)], file.path(figdata_dir, "rank_movement_by_rating_bin.csv"))
rm(rank_dt)
gc()

pg <- fread(
  "analysis_outputs/stockfish_move_mechanisms_full_2022_2026/player_game_move_outcomes.csv",
  select = c(
    "format_5_0", "rating_diff100", "reached_winning_position",
    "converted_winning_position", "reached_losing_position",
    "escaped_losing_position", "last10_cp_loss"
  )
)
pg[, rating_bin := cut(
  rating_diff100,
  breaks = c(-Inf, -3, -1, 1, 3, Inf),
  labels = c("Underdog >300", "Underdog 100-300", "Close +/-100", "Favorite 100-300", "Favorite >300"),
  right = FALSE
)]
conv <- pg[, .(
  winning_n = sum(reached_winning_position == 1, na.rm = TRUE),
  conversion_rate = mean(converted_winning_position[reached_winning_position == 1], na.rm = TRUE),
  losing_n = sum(reached_losing_position == 1, na.rm = TRUE),
  escape_rate = mean(escaped_losing_position[reached_losing_position == 1], na.rm = TRUE)
), by = .(format_5_0, rating_bin)]
conv[, period := ifelse(format_5_0 == 1, "5+0", "3+1")]
fwrite(conv[order(format_5_0, rating_bin)], file.path(figdata_dir, "conversion_escape_by_rating_bin.csv"))

last10 <- pg[, .(
  player_games = .N,
  mean_last10_cp_loss = mean(last10_cp_loss, na.rm = TRUE)
), by = format_5_0]
last10[, period := ifelse(format_5_0 == 1, "5+0", "3+1")]
fwrite(last10[order(format_5_0)], file.path(figdata_dir, "last10_cp_loss_by_format.csv"))
rm(pg)
gc()

manifest <- file.path(out_root, "README.md")
writeLines(c(
  "# Master Thesis Results Assets",
  "",
  "Generated by `analysis_outputs/create_master_thesis_tables.R` and `analysis_outputs/create_master_thesis_figures.py`.",
  "",
  "## Tables",
  "",
  "- `tables/table1_baseline_game_level_effects.{csv,html,tex,txt}`",
  "- `tables/table2_rating_advantage_robustness.{csv,html,tex,txt}`",
  "- `tables/table3_time_slot_displacement.{csv,html,tex,txt}`",
  "- `tables/table4_move_mechanisms.{csv,html,tex,txt}`",
  "- `tables/table5_style_index_interactions.{csv,html,tex,txt}`",
  "- `tables/table6_null_and_weak_results.{csv,html,tex,txt}`",
  "",
  "## Figure Data",
  "",
  "- `figure_data/rank_movement_by_rating_bin.csv`",
  "- `figure_data/conversion_escape_by_rating_bin.csv`",
  "- `figure_data/last10_cp_loss_by_format.csv`"
), manifest)

message("Done. Tables written to: ", tables_dir)
message("Figure data written to: ", figdata_dir)
