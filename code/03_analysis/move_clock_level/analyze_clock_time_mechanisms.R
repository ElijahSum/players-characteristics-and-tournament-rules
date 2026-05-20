suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

setFixest_notes(FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "code/03_analysis/move_clock_level/analyze_clock_time_mechanisms.R"
ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
if (!dir.exists(file.path(ROOT, "analysis_outputs"))) ROOT <- getwd()

out_dir <- file.path(ROOT, "analysis_outputs", "clock_time_mechanisms_2022_2026")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clock_features_path <- file.path(out_dir, "player_game_clock_features.csv")
metadata_path <- file.path(ROOT, "data", "final_regression_data_tournaments_2022_2026.csv")
bridge_paths <- c(
  file.path(ROOT, "data", "tournaments_1_261_final_v6.csv"),
  file.path(ROOT, "data", "merged_tournaments_1_150_added_missed_links_3.csv")
)

required_files <- c(clock_features_path, metadata_path, bridge_paths)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

normalize_event_datetime <- function(x) {
  x <- as.character(x)
  parsed_iso <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  parsed_mdy <- as.POSIXct(x, format = "%b %d, %Y, %I:%M %p", tz = "UTC")
  parsed <- parsed_iso
  parsed[is.na(parsed)] <- parsed_mdy[is.na(parsed)]
  format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

extract_game_id <- function(x) {
  x <- as.character(x)
  out <- sub(".*live/([0-9]+).*", "\\1", x)
  out[!grepl("^[0-9]+$", out)] <- NA_character_
  out
}

read_bridge <- function(path) {
  raw <- fread(
    path,
    select = c(
      "white_name", "black_name", "white_rating", "black_rating",
      "date", "round", "game_link"
    ),
    colClasses = list(character = c("date", "game_link"))
  )
  raw[, date_key := normalize_event_datetime(date)]
  raw[, game_id := extract_game_id(game_link)]
  raw <- raw[!is.na(game_id) & !is.na(date_key)]
  raw[, round := as.integer(round)]

  white <- raw[, .(
    date_key,
    round,
    game_id,
    player_name = white_name,
    opponent_name = black_name,
    is_white = 1L
  )]
  black <- raw[, .(
    date_key,
    round,
    game_id,
    player_name = black_name,
    opponent_name = white_name,
    is_white = 0L
  )]
  rbindlist(list(white, black), use.names = TRUE)
}

clean_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

sig_label <- function(p) {
  fifelse(is.na(p), "",
    fifelse(p < 0.01, "***",
      fifelse(p < 0.05, "**",
        fifelse(p < 0.1, "*", "")
      )
    )
  )
}

tidy_fixest <- function(model, model_id, idea, outcome, sample_n) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
    c("estimate", "std_error", "t_value", "p_value"),
    skip_absent = TRUE
  )
  ct[, `:=`(
    model_id = model_id,
    idea = idea,
    outcome = outcome,
    n_obs = nobs(model),
    sample_n_before_fe = sample_n,
    r2_within = tryCatch(as.numeric(fitstat(model, "wr2")), error = function(e) NA_real_),
    significance = sig_label(p_value)
  )]
  setcolorder(ct, c(
    "model_id", "idea", "outcome", "term", "estimate", "std_error",
    "t_value", "p_value", "significance", "n_obs", "sample_n_before_fe",
    "r2_within"
  ))
  ct[]
}

fit_one <- function(model_id, idea, outcome, formula, data, key_terms) {
  d <- data[complete.cases(data[, all.vars(formula), with = FALSE])]
  sample_n <- nrow(d)
  model <- feols(formula, data = d, cluster = ~player_name)
  coefs <- tidy_fixest(model, model_id, idea, outcome, sample_n)
  headline <- coefs[term %in% key_terms]
  if (nrow(headline) == 0) {
    headline <- coefs[grepl(paste(key_terms, collapse = "|"), term)]
  }
  headline[, is_headline := TRUE]
  list(model = model, coefs = coefs, headline = headline)
}

cat("Reading clock features...\n")
clock_cols <- c(
  "game_id", "player_name", "mean_cp_loss_cap", "blunder_rate",
  "low_before_5_share", "low_before_10_share", "low_before_30_share",
  "low_after_10_share", "low_after_30_share", "first_low_after_10_ply",
  "fast_moves_le_1s_share", "think_moves_ge_10s_share", "think_moves_ge_20s_share",
  "total_time_spent", "mean_time_spent", "sd_time_spent", "final_time_after",
  "time_after_move_10", "time_after_move_20", "time_after_move_30",
  "mean_cp_low_before_10", "mean_cp_nonlow_before_10",
  "blunder_rate_low_before_10", "blunder_rate_nonlow_before_10",
  "critical_share", "mean_time_critical", "mean_cp_critical",
  "blunder_rate_critical", "critical_low_before_10_share",
  "reached_winning_position", "reached_losing_position",
  "first_winning_time_after", "first_losing_time_after",
  "first_winning_low_after_10", "first_losing_low_after_10",
  "converted_winning_position", "escaped_losing_position",
  "first_blunder_ply", "first_blunder_low_before_10",
  "last10_mean_cp", "last10_mean_time_spent", "last10_low_before_10_share",
  "last10_blunder_rate", "opening_1_10_mean_time_spent",
  "opening_1_10_low_before_10_share", "late_middlegame_21_35_low_before_10_share",
  "endgame_36_plus_low_before_10_share", "endgame_36_plus_blunder_rate"
)
clock <- fread(clock_features_path, select = clock_cols, colClasses = list(character = "game_id"))
clock[, game_id := as.character(game_id)]

cat("Reading metadata and game-id bridge...\n")
meta_cols <- c(
  "player_name", "player_rating", "player_accuracy", "round", "date",
  "opponent_rating", "player_result", "opponent_name", "is_white",
  "final_score", "final_score_pregame", "birthday", "female",
  "gdp_per_capita_ppp_logged", "classic_rating", "blitz_rating",
  "rank", "rank_end_round", "leader", "in_prizes", "bubble", "eliminated",
  "played_against_leader", "played_against_prizes", "played_against_bubble",
  "played_against_eliminated"
)
meta <- fread(metadata_path, select = meta_cols, colClasses = list(character = "date"))
meta[, date_key := normalize_event_datetime(date)]
meta[, round := as.integer(round)]
meta[, is_white := as.integer(is_white)]

bridge <- rbindlist(lapply(bridge_paths, read_bridge), use.names = TRUE)
bridge <- unique(bridge)
bridge_key <- c("date_key", "round", "player_name", "opponent_name", "is_white")
bridge_dups <- bridge[, .N, by = bridge_key][N > 1]
if (nrow(bridge_dups) > 0) {
  bridge <- bridge[order(date_key, round, player_name, opponent_name, is_white, game_id)]
  bridge <- bridge[, .SD[1], by = bridge_key]
}

meta_game <- merge(meta, bridge, by = bridge_key, all.x = TRUE, sort = FALSE)
matched_meta_rows <- meta_game[!is.na(game_id), .N]

panel <- merge(
  meta_game[!is.na(game_id)],
  clock,
  by = c("game_id", "player_name"),
  all = FALSE,
  sort = FALSE
)

numeric_cols <- setdiff(names(panel), c("game_id", "player_name", "opponent_name", "date", "date_key"))
for (col in numeric_cols) {
  if (!is.numeric(panel[[col]]) && !is.integer(panel[[col]])) {
    panel[, (col) := clean_numeric(get(col))]
  }
}

panel[, event_date := as.IDate(date_key)]
panel[, format_5_0 := as.integer(event_date >= as.IDate("2025-09-02"))]
panel[, rating_diff100 := (player_rating - opponent_rating) / 100]
panel[, age_at_reform := 2025 - birthday]
panel[, age10_c := age_at_reform / 10 - mean(age_at_reform / 10, na.rm = TRUE)]
panel[, female := as.integer(female)]
panel[, no_blunder_game := as.integer(first_blunder_ply == 0)]
panel[, ever_low_after_10 := as.integer(first_low_after_10_ply > 0)]
panel[, time_pressure_penalty_cp := mean_cp_low_before_10 - mean_cp_nonlow_before_10]
panel[, time_pressure_penalty_blunder := blunder_rate_low_before_10 - blunder_rate_nonlow_before_10]
panel[, first_blunder_low10_cond := fifelse(first_blunder_ply > 0, first_blunder_low_before_10, NA_real_)]
panel[, winning_low10 := fifelse(reached_winning_position == 1, first_winning_low_after_10, NA_real_)]
panel[, losing_low10 := fifelse(reached_losing_position == 1, first_losing_low_after_10, NA_real_)]
panel[, clock_saved_by_move10 := time_after_move_10]
panel[, start_clock_seconds := fifelse(format_5_0 == 1, 300, 180)]
panel[, final_clock_fraction := final_time_after / start_clock_seconds]
panel[, clock_saved_by_move10_fraction := clock_saved_by_move10 / start_clock_seconds]
panel[, time_smoothness := sd_time_spent]

sample_summary <- data.table(
  metric = c(
    "clock_feature_player_games",
    "metadata_player_games",
    "bridge_player_games",
    "duplicate_bridge_keys",
    "metadata_rows_with_game_id",
    "matched_player_games",
    "matched_games",
    "matched_players",
    "matched_events",
    "pre_player_games",
    "post_player_games"
  ),
  value = c(
    nrow(clock),
    nrow(meta),
    nrow(bridge),
    nrow(bridge_dups),
    matched_meta_rows,
    nrow(panel),
    uniqueN(panel$game_id),
    uniqueN(panel$player_name),
    uniqueN(panel$date_key),
    panel[format_5_0 == 0, .N],
    panel[format_5_0 == 1, .N]
  )
)
fwrite(sample_summary, file.path(out_dir, "clock_sample_summary.csv"))

descriptives <- panel[, .(
  player_games = .N,
  games = uniqueN(game_id),
  players = uniqueN(player_name),
  events = uniqueN(date_key),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_cp_loss = mean(mean_cp_loss_cap, na.rm = TRUE),
  mean_blunder_rate = mean(blunder_rate, na.rm = TRUE),
  low_before_10_share = mean(low_before_10_share, na.rm = TRUE),
  low_before_30_share = mean(low_before_30_share, na.rm = TRUE),
  ever_low_after_10 = mean(ever_low_after_10, na.rm = TRUE),
  final_time_after = mean(final_time_after, na.rm = TRUE),
  final_clock_fraction = mean(final_clock_fraction, na.rm = TRUE),
  time_after_move_10 = mean(time_after_move_10, na.rm = TRUE),
  clock_saved_by_move10_fraction = mean(clock_saved_by_move10_fraction, na.rm = TRUE),
  fast_moves_le_1s_share = mean(fast_moves_le_1s_share, na.rm = TRUE),
  think_moves_ge_10s_share = mean(think_moves_ge_10s_share, na.rm = TRUE),
  last10_low_before_10_share = mean(last10_low_before_10_share, na.rm = TRUE),
  last10_blunder_rate = mean(last10_blunder_rate, na.rm = TRUE),
  critical_low_before_10_share = mean(critical_low_before_10_share, na.rm = TRUE),
  converted_winning_position = mean(converted_winning_position[reached_winning_position == 1], na.rm = TRUE),
  escaped_losing_position = mean(escaped_losing_position[reached_losing_position == 1], na.rm = TRUE)
), by = .(format_5_0)]
descriptives[, format := fifelse(format_5_0 == 1, "5+0 post", "3+1 pre")]
setcolorder(descriptives, c("format", "format_5_0"))
fwrite(descriptives, file.path(out_dir, "clock_descriptives_by_format.csv"))

models <- list()
models[["A01"]] <- fit_one(
  "A01", "Low-clock exposure after the reform",
  "Share of moves with <=10 seconds before moving",
  low_before_10_share ~ format_5_0 + rating_diff100 + i(round) | player_name,
  panel,
  c("format_5_0")
)
models[["A02"]] <- fit_one(
  "A02", "Final-clock reserve after the reform",
  "Fraction of starting clock left after player's final move",
  final_clock_fraction ~ format_5_0 + rating_diff100 + i(round) | player_name,
  panel,
  c("format_5_0")
)
models[["A03"]] <- fit_one(
  "A03", "Opening clock conservation",
  "Fraction of starting clock left after move 10",
  clock_saved_by_move10_fraction ~ format_5_0 + rating_diff100 + i(round) | player_name,
  panel[!is.na(clock_saved_by_move10_fraction)],
  c("format_5_0")
)
models[["A04"]] <- fit_one(
  "A04", "Time pressure and centipawn loss",
  "Capped centipawn loss per move at player-game level",
  mean_cp_loss_cap ~ low_before_10_share + low_before_10_share:format_5_0 +
    rating_diff100 + i(round) | player_name + date_key,
  panel,
  c("low_before_10_share", "low_before_10_share:format_5_0")
)
models[["A05"]] <- fit_one(
  "A05", "Time pressure and blunder probability",
  "Player-game blunder rate",
  blunder_rate ~ low_before_10_share + low_before_10_share:format_5_0 +
    rating_diff100 + i(round) | player_name + date_key,
  panel,
  c("low_before_10_share", "low_before_10_share:format_5_0")
)
models[["A06"]] <- fit_one(
  "A06", "Age and clock adaptation",
  "Share of moves with <=10 seconds before moving",
  low_before_10_share ~ format_5_0:age10_c + rating_diff100 + i(round) | player_name,
  panel[!is.na(age10_c)],
  c("format_5_0:age10_c")
)
models[["A07"]] <- fit_one(
  "A07", "Female status and clock adaptation",
  "Share of moves with <=10 seconds before moving",
  low_before_10_share ~ format_5_0:female + rating_diff100 + i(round) | player_name,
  panel[!is.na(female)],
  c("format_5_0:female")
)
models[["A08"]] <- fit_one(
  "A08", "Critical-position clock pressure",
  "Centipawn loss in critical positions",
  mean_cp_critical ~ critical_low_before_10_share +
    critical_low_before_10_share:format_5_0 + rating_diff100 + i(round) |
    player_name + date_key,
  panel[!is.na(mean_cp_critical) & !is.na(critical_low_before_10_share)],
  c("critical_low_before_10_share", "critical_low_before_10_share:format_5_0")
)
models[["A09"]] <- fit_one(
  "A09", "Conversion technology with clocks",
  "Converted after reaching +2 engine position",
  converted_winning_position ~ rating_diff100 + rating_diff100:format_5_0 +
    winning_low10 + winning_low10:format_5_0 + i(round) | player_name + date_key,
  panel[reached_winning_position == 1 & !is.na(winning_low10)],
  c("rating_diff100", "rating_diff100:format_5_0", "winning_low10", "winning_low10:format_5_0")
)
models[["A10"]] <- fit_one(
  "A10", "Defensive recovery with clocks",
  "Escaped after reaching -2 engine position",
  escaped_losing_position ~ rating_diff100 + rating_diff100:format_5_0 +
    losing_low10 + losing_low10:format_5_0 + i(round) | player_name + date_key,
  panel[reached_losing_position == 1 & !is.na(losing_low10)],
  c("rating_diff100", "rating_diff100:format_5_0", "losing_low10", "losing_low10:format_5_0")
)
models[["A11"]] <- fit_one(
  "A11", "Late-game clock pressure",
  "Last-10-move blunder rate",
  last10_blunder_rate ~ last10_low_before_10_share +
    last10_low_before_10_share:format_5_0 + format_5_0:age10_c +
    rating_diff100 + i(round) | player_name + date_key,
  panel[!is.na(last10_blunder_rate) & !is.na(last10_low_before_10_share) & !is.na(age10_c)],
  c("last10_low_before_10_share", "last10_low_before_10_share:format_5_0", "format_5_0:age10_c")
)
models[["A12"]] <- fit_one(
  "A12", "First blunder under low clock",
  "First blunder occurred with <=10 seconds before move",
  first_blunder_low10_cond ~ format_5_0 + rating_diff100 + age10_c + female + i(round) |
    player_name,
  panel[!is.na(first_blunder_low10_cond) & !is.na(age10_c) & !is.na(female)],
  c("format_5_0")
)

all_coefs <- rbindlist(lapply(models, `[[`, "coefs"), fill = TRUE)
headlines <- rbindlist(lapply(models, `[[`, "headline"), fill = TRUE)
fwrite(all_coefs, file.path(out_dir, "clock_model_coefficients.csv"))
fwrite(headlines, file.path(out_dir, "clock_headline_coefficients.csv"))

idea_catalog <- data.table(
  model_id = sprintf("A%02d", 1:12),
  include_in_top10 = c(rep(TRUE, 10), FALSE, FALSE),
  idea = c(
    "Low-clock exposure after the reform",
    "Final-clock reserve after the reform",
    "Opening clock conservation",
    "Time pressure and centipawn loss",
    "Time pressure and blunder probability",
    "Age and clock adaptation",
    "Female status and clock adaptation",
    "Critical-position clock pressure",
    "Conversion technology with clocks",
    "Defensive recovery with clocks",
    "Late-game clock pressure",
    "First blunder under low clock"
  ),
  interpretation = c(
    "Tests whether the no-increment format mechanically pushed players into low-clock states more often.",
    "Tests whether the new format changed the fraction of starting clock players had left at the end of their games.",
    "Tests whether players conserved or spent more of the starting clock during the opening after the reform.",
    "Tests whether low-clock exposure predicts worse engine quality, and whether that penalty changed after 5+0.",
    "Same as A04, but for transparent blunder rates rather than average centipawn loss.",
    "Tests whether older players were disproportionately pushed into low-clock states after the reform.",
    "Tests whether female players had a different post-reform clock-exposure response.",
    "Tests whether low clock is especially costly in engine-critical positions.",
    "Tests whether rating favorites converted winning positions differently after 5+0 and whether low clock at the winning threshold mattered.",
    "Tests whether players in bad positions escaped less often after 5+0 and whether low clock at the losing threshold mattered.",
    "Extension: isolates late-game clock pressure and age heterogeneity in the last ten moves.",
    "Extension: asks whether the first decisive error happens under low clock more often after the reform."
  )
)
fwrite(idea_catalog, file.path(out_dir, "clock_research_ideas_catalog.csv"))

headline_for <- function(model_id_value, term_value) {
  row <- headlines[model_id == model_id_value & term == term_value]
  if (nrow(row) == 0) return("not estimated")
  row <- row[1]
  sprintf("%.4f (se %.4f, p=%.3g)%s", row$estimate, row$std_error, row$p_value, row$significance)
}

format_desc <- copy(descriptives)
format_desc[, `:=`(
  low_before_10_pp = 100 * low_before_10_share,
  low_before_30_pp = 100 * low_before_30_share,
  blunder_pp = 100 * mean_blunder_rate,
  last10_blunder_pp = 100 * last10_blunder_rate,
  final_clock_fraction_pp = 100 * final_clock_fraction,
  move10_clock_fraction_pp = 100 * clock_saved_by_move10_fraction
)]

md <- c(
  "# Clock-Time Mechanism Analyses",
  "",
  "This file summarizes the first-pass empirical tests enabled by recovered per-move clock data. The results are not inserted into the thesis text yet.",
  "",
  "## Sample",
  "",
  paste0("- Clock feature rows: ", format(sample_summary[metric == "clock_feature_player_games", value], big.mark = ","), " player-games."),
  paste0("- Matched econometric panel: ", format(sample_summary[metric == "matched_player_games", value], big.mark = ","), " player-games, ",
         format(sample_summary[metric == "matched_games", value], big.mark = ","), " games, ",
         format(sample_summary[metric == "matched_players", value], big.mark = ","), " players, ",
         format(sample_summary[metric == "matched_events", value], big.mark = ","), " event time slots."),
  paste0("- Pre-change player-games: ", format(sample_summary[metric == "pre_player_games", value], big.mark = ","), "."),
  paste0("- Post-change player-games: ", format(sample_summary[metric == "post_player_games", value], big.mark = ","), "."),
  "",
  "## Descriptive Clock Shift",
  "",
  paste0("- Mean share of moves started with <=10 seconds: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, low_before_10_pp]), "%; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, low_before_10_pp]), "%."),
  paste0("- Mean share of moves started with <=30 seconds: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, low_before_30_pp]), "%; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, low_before_30_pp]), "%."),
  paste0("- Mean final clock after player's last move: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, final_time_after]), " seconds; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, final_time_after]), " seconds."),
  paste0("- Mean final clock as a share of starting clock: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, final_clock_fraction_pp]), "%; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, final_clock_fraction_pp]), "%."),
  paste0("- Mean clock after move 10 as a share of starting clock: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, move10_clock_fraction_pp]), "%; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, move10_clock_fraction_pp]), "%."),
  paste0("- Mean player-game blunder rate: pre ",
         sprintf("%.2f", format_desc[format_5_0 == 0, blunder_pp]), "%; post ",
         sprintf("%.2f", format_desc[format_5_0 == 1, blunder_pp]), "%."),
  "",
  "## Ten Main Research Ideas and Headline Estimates",
  "",
  "1. **Low-clock exposure after 5+0.** Player fixed-effect estimate on post-format indicator for <=10-second move share: ",
  headline_for("A01", "format_5_0"),
  "",
  "2. **Final-clock reserve.** Player fixed-effect estimate on post-format indicator for final clock as a share of starting clock: ",
  headline_for("A02", "format_5_0"),
  "",
  "3. **Opening clock conservation.** Player fixed-effect estimate on post-format indicator for clock after move 10 as a share of starting clock: ",
  headline_for("A03", "format_5_0"),
  "",
  "4. **Time pressure and centipawn loss.** Within-event estimate for <=10-second move share and its post interaction: ",
  paste0("low-clock main ", headline_for("A04", "low_before_10_share"), "; post interaction ",
         headline_for("A04", "low_before_10_share:format_5_0")),
  "",
  "5. **Time pressure and blunder probability.** Within-event estimate for <=10-second move share and its post interaction: ",
  paste0("low-clock main ", headline_for("A05", "low_before_10_share"), "; post interaction ",
         headline_for("A05", "low_before_10_share:format_5_0")),
  "",
  "6. **Age and clock adaptation.** Player fixed-effect post-by-age estimate for <=10-second move share: ",
  headline_for("A06", "format_5_0:age10_c"),
  "",
  "7. **Female status and clock adaptation.** Player fixed-effect post-by-female estimate for <=10-second move share: ",
  headline_for("A07", "format_5_0:female"),
  "",
  "8. **Critical-position clock pressure.** Within-event estimate for critical low-clock share and its post interaction: ",
  paste0("critical low-clock main ", headline_for("A08", "critical_low_before_10_share"), "; post interaction ",
         headline_for("A08", "critical_low_before_10_share:format_5_0")),
  "",
  "9. **Conversion technology with clocks.** Among player-games reaching +2, estimates for rating advantage and low clock at the first winning position: ",
  paste0("rating advantage ", headline_for("A09", "rating_diff100"), "; post rating interaction ",
         headline_for("A09", "rating_diff100:format_5_0"), "; low-clock threshold ",
         headline_for("A09", "winning_low10"), "; post low-clock interaction ",
         headline_for("A09", "winning_low10:format_5_0")),
  "",
  "10. **Defensive recovery with clocks.** Among player-games reaching -2, estimates for rating advantage and low clock at the first losing position: ",
  paste0("rating advantage ", headline_for("A10", "rating_diff100"), "; post rating interaction ",
         headline_for("A10", "rating_diff100:format_5_0"), "; low-clock threshold ",
         headline_for("A10", "losing_low10"), "; post low-clock interaction ",
         headline_for("A10", "losing_low10:format_5_0")),
  "",
  "## Extra Diagnostics",
  "",
  "A11 and A12 are saved as additional diagnostics: late-game low-clock pressure and whether the first blunder occurred under low clock. They are useful robustness checks for the main ten ideas.",
  "",
  "## Files Written",
  "",
  "- `player_game_clock_features.csv`: compact features built from per-move clock and Stockfish data.",
  "- `clock_sample_summary.csv`: coverage and match counts.",
  "- `clock_descriptives_by_format.csv`: pre/post descriptive statistics.",
  "- `clock_model_coefficients.csv`: full coefficient table.",
  "- `clock_headline_coefficients.csv`: selected coefficients for the main mechanisms.",
  "- `clock_research_ideas_catalog.csv`: idea catalog and interpretations."
)
writeLines(md, file.path(out_dir, "clock_time_research_results.md"))

cat("Analysis complete.\n")
print(sample_summary)
print(headlines)
