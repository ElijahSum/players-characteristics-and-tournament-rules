suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

setFixest_notes(FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "code/03_analysis/move_clock_level/analyze_interaction_clock_mechanisms.R"
ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
if (!dir.exists(file.path(ROOT, "analysis_outputs"))) ROOT <- getwd()

out_dir <- file.path(ROOT, "analysis_outputs", "interaction_clock_mechanisms_2022_2026")
clock_dir <- file.path(ROOT, "analysis_outputs", "clock_time_mechanisms_2022_2026")
deep_dir <- file.path(ROOT, "analysis_outputs", "deep_clock_mechanisms_2022_2026")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  pressure = file.path(out_dir, "time_pressure_production_cells.csv"),
  rhythm = file.path(out_dir, "opponent_long_think_disruption_cells.csv"),
  player_game = file.path(clock_dir, "player_game_clock_features.csv"),
  recovery = file.path(deep_dir, "deep_clock_recovery_player_game.csv"),
  snapshots = file.path(deep_dir, "deep_clock_snapshots.csv"),
  metadata = file.path(ROOT, "data", "final_regression_data_tournaments_2022_2026.csv")
)
bridge_paths <- c(
  file.path(ROOT, "data", "tournaments_1_261_final_v6.csv"),
  file.path(ROOT, "data", "merged_tournaments_1_150_added_missed_links_3.csv")
)

missing_files <- c(unlist(paths), bridge_paths)[!file.exists(c(unlist(paths), bridge_paths))]
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
    select = c("white_name", "black_name", "date", "round", "game_link"),
    colClasses = list(character = c("date", "game_link"))
  )
  raw[, date_key := normalize_event_datetime(date)]
  raw[, game_id := extract_game_id(game_link)]
  raw <- raw[!is.na(game_id) & !is.na(date_key)]
  raw[, round := as.integer(round)]
  white <- raw[, .(
    date_key,
    round,
    game_id = as.character(game_id),
    player_name = white_name,
    opponent_name = black_name,
    is_white = 1L
  )]
  black <- raw[, .(
    date_key,
    round,
    game_id = as.character(game_id),
    player_name = black_name,
    opponent_name = white_name,
    is_white = 0L
  )]
  rbindlist(list(white, black), use.names = TRUE)
}

clean_numeric <- function(x) suppressWarnings(as.numeric(x))

sig_label <- function(p) {
  fifelse(is.na(p), "",
    fifelse(p < 0.01, "***",
      fifelse(p < 0.05, "**",
        fifelse(p < 0.1, "*", "")
      )
    )
  )
}

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x * NA_real_)
  (x - mean(x, na.rm = TRUE)) / s
}

tidy_fixest <- function(model, model_id, mechanism, outcome) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
    c("estimate", "std_error", "t_value", "p_value"),
    skip_absent = TRUE
  )
  ct[, `:=`(
    model_id = model_id,
    mechanism = mechanism,
    outcome = outcome,
    n_obs = nobs(model),
    r2_within = tryCatch(as.numeric(fitstat(model, "wr2")), error = function(e) NA_real_),
    significance = sig_label(p_value)
  )]
  setcolorder(ct, c(
    "model_id", "mechanism", "outcome", "term", "estimate", "std_error",
    "t_value", "p_value", "significance", "n_obs", "r2_within"
  ))
  ct[]
}

fit_one <- function(model_id, mechanism, outcome, formula, data, key_patterns = NULL, weights = NULL) {
  cat("Estimating", model_id, "-", mechanism, "\n")
  model <- feols(
    formula,
    data = data,
    cluster = ~player_name,
    weights = weights,
    mem.clean = TRUE
  )
  coefs <- tidy_fixest(model, model_id, mechanism, outcome)
  if (is.null(key_patterns)) {
    headline <- coefs
  } else {
    keep <- Reduce(`|`, lapply(key_patterns, function(pat) grepl(pat, coefs$term)))
    headline <- coefs[keep]
  }
  headline[, is_headline := TRUE]
  list(model = model, coefs = coefs, headline = headline)
}

coef_fmt <- function(coefs, model_id_value, patterns) {
  keep <- coefs$model_id == model_id_value
  for (pat in patterns) {
    row <- coefs[keep & grepl(pat, term)]
    if (nrow(row) > 0) {
      row <- row[1]
      return(sprintf("%.4f (se %.4f, p=%.3g)%s", row$estimate, row$std_error, row$p_value, row$significance))
    }
  }
  "not estimated"
}

cat("Building metadata bridge...\n")
meta_cols <- c(
  "player_name", "player_rating", "round", "date", "opponent_rating",
  "player_result", "opponent_name", "is_white", "final_score_pregame",
  "leader", "in_prizes", "bubble", "eliminated"
)
meta <- fread(paths$metadata, select = meta_cols, colClasses = list(character = "date"))
meta[, date_key := normalize_event_datetime(date)]
meta[, round := as.integer(round)]
meta[, is_white := as.integer(is_white)]

bridge <- unique(rbindlist(lapply(bridge_paths, read_bridge), use.names = TRUE))
bridge_key <- c("date_key", "round", "player_name", "opponent_name", "is_white")
bridge <- bridge[order(date_key, round, player_name, opponent_name, is_white, game_id)]
bridge <- bridge[, .SD[1], by = bridge_key]

meta_game <- merge(meta, bridge, by = bridge_key, all.x = TRUE, sort = FALSE)
meta_small <- meta_game[!is.na(game_id), .(
  game_id = as.character(game_id),
  player_name,
  opponent_name,
  date_key,
  round,
  player_result = as.numeric(player_result),
  player_rating = as.numeric(player_rating),
  opponent_rating = as.numeric(opponent_rating),
  rating_diff100 = (as.numeric(player_rating) - as.numeric(opponent_rating)) / 100,
  final_score_pregame = as.numeric(final_score_pregame),
  leader = as.integer(leader),
  in_prizes = as.integer(in_prizes),
  bubble = as.integer(bubble),
  eliminated = as.integer(eliminated)
)]
meta_small[, event_date := as.IDate(date_key)]
meta_small[, format_5_0 := as.integer(event_date >= as.IDate("2025-09-02"))]
meta_small[, weeks_since_reform := as.numeric(event_date - as.IDate("2025-09-02")) / 7]
meta_small[, score_pregame0 := fifelse(is.na(final_score_pregame), 0, final_score_pregame)]
setkey(meta_small, game_id, player_name)

sample_rows <- list()
sample_rows[["metadata"]] <- data.table(
  dataset = "metadata_bridge",
  rows = nrow(meta_small),
  games = uniqueN(meta_small$game_id),
  players = uniqueN(meta_small$player_name),
  events = uniqueN(meta_small$date_key),
  moves = NA_real_
)

all_coefs <- list()
all_headlines <- list()

cat("Reading time-pressure production cells...\n")
pressure <- fread(paths$pressure)
pressure[, `:=`(
  format_5_0 = as.integer(format_5_0),
  is_capture = as.integer(is_capture),
  gives_check = as.integer(gives_check),
  critical_position = as.integer(critical_position),
  eval_swing_midpoint = as.numeric(eval_swing_midpoint),
  n_moves = as.numeric(n_moves)
)]
pressure[, phase := factor(phase)]
sample_rows[["pressure"]] <- data.table(
  dataset = "time_pressure_production_cells",
  rows = nrow(pressure),
  games = NA_integer_,
  players = uniqueN(pressure$player_name),
  events = NA_integer_,
  moves = sum(pressure$n_moves, na.rm = TRUE)
)

m <- fit_one(
  "I01a",
  "Time-pressure production",
  "Opponent next-move time spent",
  mean_opponent_time_spent_next ~
    (gives_check + is_capture + critical_position + eval_swing_midpoint) * format_5_0 +
    i(phase) |
    player_name + opponent_name,
  pressure,
  c("gives_check", "is_capture", "critical_position", "eval_swing_midpoint"),
  weights = ~n_moves
)
all_coefs[["I01a"]] <- m$coefs
all_headlines[["I01a"]] <- m$headline

m <- fit_one(
  "I01b",
  "Time-pressure production",
  "Opponent is below 10 seconds after next move",
  opponent_low10_after_next_rate ~
    (gives_check + is_capture + critical_position + eval_swing_midpoint) * format_5_0 +
    i(phase) |
    player_name + opponent_name,
  pressure,
  c("gives_check", "is_capture", "critical_position", "eval_swing_midpoint"),
  weights = ~n_moves
)
all_coefs[["I01b"]] <- m$coefs
all_headlines[["I01b"]] <- m$headline
rm(pressure)
gc()

cat("Reading opponent long-think disruption cells...\n")
rhythm <- fread(paths$rhythm)
rhythm[, `:=`(
  format_5_0 = as.integer(format_5_0),
  fullmove_number = as.integer(fullmove_number),
  previous_time_midpoint = as.numeric(previous_time_midpoint),
  n_moves = as.numeric(n_moves)
)]
rhythm[, previous_time_midpoint2 := previous_time_midpoint^2]
sample_rows[["rhythm"]] <- data.table(
  dataset = "opponent_long_think_disruption_cells",
  rows = nrow(rhythm),
  games = NA_integer_,
  players = uniqueN(rhythm$player_name),
  events = NA_integer_,
  moves = sum(rhythm$n_moves, na.rm = TRUE)
)

m <- fit_one(
  "I03a",
  "Opponent long-think disruption",
  "Own next-move capped centipawn loss",
  mean_cp_loss ~
    previous_time_midpoint + previous_time_midpoint2 +
    previous_time_midpoint:format_5_0 + previous_time_midpoint2:format_5_0 |
    player_name + fullmove_number,
  rhythm,
  c("previous_time_midpoint"),
  weights = ~n_moves
)
all_coefs[["I03a"]] <- m$coefs
all_headlines[["I03a"]] <- m$headline

m <- fit_one(
  "I03b",
  "Opponent long-think disruption",
  "Own next-move blunder probability",
  blunder_rate ~
    previous_time_midpoint + previous_time_midpoint2 +
    previous_time_midpoint:format_5_0 + previous_time_midpoint2:format_5_0 |
    player_name + fullmove_number,
  rhythm,
  c("previous_time_midpoint"),
  weights = ~n_moves
)
all_coefs[["I03b"]] <- m$coefs
all_headlines[["I03b"]] <- m$headline
rm(rhythm)
gc()

cat("Reading player-game clock features...\n")
clock_cols <- c(
  "game_id", "player_name", "n_moves", "mean_cp_loss_cap", "blunder_rate",
  "mean_time_spent", "mean_time_critical", "critical_share",
  "low_before_10_share", "opening_1_10_moves", "opening_1_10_mean_time_spent",
  "think_moves_ge_10s_share", "mean_cp_low_before_10", "mean_cp_nonlow_before_10"
)
clock <- fread(paths$player_game, select = clock_cols, colClasses = list(character = "game_id"))
clock[, game_id := as.character(game_id)]
panel <- merge(meta_small, clock, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
num_cols <- setdiff(names(panel), c("game_id", "player_name", "opponent_name", "date_key"))
for (col in num_cols) {
  if (!is.numeric(panel[[col]]) && !is.integer(panel[[col]])) panel[, (col) := clean_numeric(get(col))]
}
panel[, critical_n := pmax(critical_share * n_moves, 1)]
panel[, time_pressure_penalty_cp := mean_cp_low_before_10 - mean_cp_nonlow_before_10]
sample_rows[["panel"]] <- data.table(
  dataset = "player_game_clock_panel",
  rows = nrow(panel),
  games = uniqueN(panel$game_id),
  players = uniqueN(panel$player_name),
  events = uniqueN(panel$date_key),
  moves = sum(panel$n_moves, na.rm = TRUE)
)

m <- fit_one(
  "I05a",
  "Prize pressure and clock use",
  "Mean time spent per move",
  mean_time_spent ~
    leader * format_5_0 + in_prizes * format_5_0 + bubble * format_5_0 +
    rating_diff100 + score_pregame0 + i(round) |
    player_name + date_key,
  panel,
  c("leader:format_5_0", "format_5_0:leader", "in_prizes:format_5_0", "format_5_0:in_prizes", "bubble:format_5_0", "format_5_0:bubble"),
  weights = ~n_moves
)
all_coefs[["I05a"]] <- m$coefs
all_headlines[["I05a"]] <- m$headline

m <- fit_one(
  "I05b",
  "Prize pressure and clock use",
  "Mean time spent in critical positions",
  mean_time_critical ~
    leader * format_5_0 + in_prizes * format_5_0 + bubble * format_5_0 +
    rating_diff100 + score_pregame0 + i(round) |
    player_name + date_key,
  panel,
  c("leader:format_5_0", "format_5_0:leader", "in_prizes:format_5_0", "format_5_0:in_prizes", "bubble:format_5_0", "format_5_0:bubble"),
  weights = ~critical_n
)
all_coefs[["I05b"]] <- m$coefs
all_headlines[["I05b"]] <- m$headline

m <- fit_one(
  "I05c",
  "Prize pressure and clock use",
  "Share of moves beginning below 10 seconds",
  low_before_10_share ~
    leader * format_5_0 + in_prizes * format_5_0 + bubble * format_5_0 +
    rating_diff100 + score_pregame0 + i(round) |
    player_name + date_key,
  panel,
  c("leader:format_5_0", "format_5_0:leader", "in_prizes:format_5_0", "format_5_0:in_prizes", "bubble:format_5_0", "format_5_0:bubble"),
  weights = ~n_moves
)
all_coefs[["I05c"]] <- m$coefs
all_headlines[["I05c"]] <- m$headline

cat("Estimating post-reform learning models...\n")
post_panel <- panel[format_5_0 == 1 & weeks_since_reform >= 0]
m <- fit_one(
  "I07a",
  "Learning after reform",
  "Share of moves beginning below 10 seconds",
  low_before_10_share ~ weeks_since_reform + rating_diff100 + score_pregame0 + i(round) |
    player_name,
  post_panel,
  c("weeks_since_reform"),
  weights = ~n_moves
)
all_coefs[["I07a"]] <- m$coefs
all_headlines[["I07a"]] <- m$headline

m <- fit_one(
  "I07b",
  "Learning after reform",
  "Opening mean time spent",
  opening_1_10_mean_time_spent ~ weeks_since_reform + rating_diff100 + score_pregame0 + i(round) |
    player_name,
  post_panel[opening_1_10_moves > 0],
  c("weeks_since_reform"),
  weights = ~opening_1_10_moves
)
all_coefs[["I07b"]] <- m$coefs
all_headlines[["I07b"]] <- m$headline

cat("Reading recovery data for low-clock conversion learning...\n")
rec <- fread(paths$recovery, colClasses = list(character = c("game_id", "player_name")))
rec[, game_id := as.character(game_id)]
rec <- merge(rec, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
rec_post_low_win <- rec[
  format_5_0 == 1 &
    weeks_since_reform >= 0 &
    board_reached_winning == 1 &
    first_winning_own_low10 == 1
]
sample_rows[["recovery_low_win"]] <- data.table(
  dataset = "recovery_low_clock_winning_panel",
  rows = nrow(rec_post_low_win),
  games = uniqueN(rec_post_low_win$game_id),
  players = uniqueN(rec_post_low_win$player_name),
  events = uniqueN(rec_post_low_win$date_key),
  moves = NA_real_
)
if (nrow(rec_post_low_win) > 0 && uniqueN(rec_post_low_win$player_name) > 1) {
  m <- fit_one(
    "I07c",
    "Learning after reform",
    "Conversion from winning positions first reached under low clock",
    board_converted_winning ~ weeks_since_reform + rating_diff100 + score_pregame0 + i(round) |
      player_name,
    rec_post_low_win,
    c("weeks_since_reform")
  )
  all_coefs[["I07c"]] <- m$coefs
  all_headlines[["I07c"]] <- m$headline
}

cat("Estimating clock-resilience models...\n")
trait <- panel[
  format_5_0 == 0 & is.finite(time_pressure_penalty_cp),
  .(
    pre_games = .N,
    pre_time_pressure_penalty_cp = weighted.mean(time_pressure_penalty_cp, w = n_moves, na.rm = TRUE),
    pre_low10_share = weighted.mean(low_before_10_share, w = n_moves, na.rm = TRUE)
  ),
  by = player_name
][pre_games >= 20]
trait[, pre_clock_resilience_z := -zscore(pre_time_pressure_penalty_cp)]
trait[, pre_low10_share_z := zscore(pre_low10_share)]
fwrite(trait, file.path(out_dir, "pre_change_clock_resilience_traits.csv"))

panel_res <- merge(panel, trait, by = "player_name", all = FALSE, sort = FALSE)
sample_rows[["resilience_panel"]] <- data.table(
  dataset = "clock_resilience_player_game_panel",
  rows = nrow(panel_res),
  games = uniqueN(panel_res$game_id),
  players = uniqueN(panel_res$player_name),
  events = uniqueN(panel_res$date_key),
  moves = sum(panel_res$n_moves, na.rm = TRUE)
)

m <- fit_one(
  "I08a",
  "Clock resilience",
  "Player result",
  player_result ~ pre_clock_resilience_z:format_5_0 + pre_low10_share_z:format_5_0 +
    rating_diff100 + score_pregame0 + i(round) |
    player_name + date_key,
  panel_res,
  c("pre_clock_resilience_z:format_5_0", "format_5_0:pre_clock_resilience_z"),
  weights = ~n_moves
)
all_coefs[["I08a"]] <- m$coefs
all_headlines[["I08a"]] <- m$headline

m <- fit_one(
  "I08b",
  "Clock resilience",
  "Capped centipawn loss",
  mean_cp_loss_cap ~ pre_clock_resilience_z:format_5_0 + pre_low10_share_z:format_5_0 +
    rating_diff100 + score_pregame0 + i(round) |
    player_name + date_key,
  panel_res,
  c("pre_clock_resilience_z:format_5_0", "format_5_0:pre_clock_resilience_z"),
  weights = ~n_moves
)
all_coefs[["I08b"]] <- m$coefs
all_headlines[["I08b"]] <- m$headline
rm(panel_res, trait, post_panel)
gc()

cat("Reading snapshots for board-clock substitution...\n")
snap <- fread(paths$snapshots, colClasses = list(character = c("game_id", "player_name")))
snap[, game_id := as.character(game_id)]
snap <- merge(snap, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
snap[, eval_adv100 := pmax(pmin(as.numeric(eval_advantage_cp), 1000), -1000) / 100]
snap[, clock_adv_min := as.numeric(clock_advantage_seconds) / 60]
sample_rows[["snapshots"]] <- data.table(
  dataset = "clock_eval_snapshots",
  rows = nrow(snap),
  games = uniqueN(snap$game_id),
  players = uniqueN(snap$player_name),
  events = uniqueN(snap$date_key),
  moves = NA_real_
)

m <- fit_one(
  "I09a",
  "Board advantage versus clock advantage substitution",
  "Player result at moves 10/20/30",
  player_result ~ eval_adv100 * clock_adv_min * format_5_0 +
    rating_diff100 + score_pregame0 + i(snapshot_move) |
    player_name + date_key,
  snap,
  c("eval_adv100", "clock_adv_min", "eval_adv100:clock_adv_min", "eval_adv100:clock_adv_min:format_5_0", "format_5_0:eval_adv100:clock_adv_min"),
  weights = NULL
)
all_coefs[["I09a"]] <- m$coefs
all_headlines[["I09a"]] <- m$headline

snap20 <- snap[snapshot_move == 20]
m <- fit_one(
  "I09b",
  "Board advantage versus clock advantage substitution",
  "Player result at move 20",
  player_result ~ eval_adv100 * clock_adv_min * format_5_0 +
    rating_diff100 + score_pregame0 |
    player_name + date_key,
  snap20,
  c("eval_adv100", "clock_adv_min", "eval_adv100:clock_adv_min", "eval_adv100:clock_adv_min:format_5_0", "format_5_0:eval_adv100:clock_adv_min"),
  weights = NULL
)
all_coefs[["I09b"]] <- m$coefs
all_headlines[["I09b"]] <- m$headline
rm(snap, snap20, panel, rec, rec_post_low_win)
gc()

all_coefs_dt <- rbindlist(all_coefs, use.names = TRUE, fill = TRUE)
all_headlines_dt <- rbindlist(all_headlines, use.names = TRUE, fill = TRUE)
sample_summary <- rbindlist(sample_rows, use.names = TRUE, fill = TRUE)

fwrite(all_coefs_dt, file.path(out_dir, "interaction_clock_model_coefficients.csv"))
fwrite(all_headlines_dt, file.path(out_dir, "interaction_clock_headline_coefficients.csv"))
fwrite(sample_summary, file.path(out_dir, "interaction_clock_sample_summary.csv"))

md <- c(
  "# Interaction Clock Mechanisms, 2022-2026",
  "",
  "This file reports the six additional clock-based mechanism tests requested after the first clock analysis. All models use the scraped PGN clock annotations now attached to the move-level Stockfish data. Coefficients from player-game regressions are weighted by the relevant number of moves; cell-level move regressions are weighted by cell move counts.",
  "",
  "## Tested Mechanisms",
  "",
  "1. **Time-pressure production.** Whether checks, captures, critical positions, or larger evaluation swings force the opponent to spend more time or leave the opponent below 10 seconds after the reply.",
  "2. **Opponent long-think disruption.** Whether the player's next move is worse after the opponent has just spent more time.",
  "3. **Prize pressure and clock use.** Whether leaders, players in prize positions, and bubble players change their time use differently after the switch to 5+0.",
  "4. **Learning after reform.** Whether post-change weeks are associated with lower low-clock exposure, different opening time allocation, or better low-clock conversion.",
  "5. **Clock resilience.** Whether players whose pre-change move quality deteriorated less under low time performed better after 5+0.",
  "6. **Board-clock substitution.** Whether clock advantage substitutes for, or complements, board advantage in predicting results.",
  "",
  "## Headline Coefficients",
  "",
  paste0("- Time-pressure production, check on opponent next time: ", coef_fmt(all_coefs_dt, "I01a", c("gives_check$"))),
  paste0("- Time-pressure production, post-change check interaction: ", coef_fmt(all_coefs_dt, "I01a", c("gives_check:format_5_0", "format_5_0:gives_check"))),
  paste0("- Time-pressure production, critical-position post interaction for opponent low-clock rate: ", coef_fmt(all_coefs_dt, "I01b", c("critical_position:format_5_0", "format_5_0:critical_position"))),
  paste0("- Opponent long-think disruption, previous-time slope: ", coef_fmt(all_coefs_dt, "I03a", c("^previous_time_midpoint$"))),
  paste0("- Opponent long-think disruption, post-change previous-time slope: ", coef_fmt(all_coefs_dt, "I03a", c("previous_time_midpoint:format_5_0", "format_5_0:previous_time_midpoint"))),
  paste0("- Prize pressure, leader x post on mean time spent: ", coef_fmt(all_coefs_dt, "I05a", c("leader:format_5_0", "format_5_0:leader"))),
  paste0("- Prize pressure, bubble x post on mean time spent: ", coef_fmt(all_coefs_dt, "I05a", c("bubble:format_5_0", "format_5_0:bubble"))),
  paste0("- Learning after reform, weeks on low-clock share: ", coef_fmt(all_coefs_dt, "I07a", c("^weeks_since_reform$"))),
  paste0("- Learning after reform, weeks on opening time: ", coef_fmt(all_coefs_dt, "I07b", c("^weeks_since_reform$"))),
  paste0("- Learning after reform, weeks on low-clock winning conversion: ", coef_fmt(all_coefs_dt, "I07c", c("^weeks_since_reform$"))),
  paste0("- Clock resilience, pre-resilience x post on result: ", coef_fmt(all_coefs_dt, "I08a", c("pre_clock_resilience_z:format_5_0", "format_5_0:pre_clock_resilience_z"))),
  paste0("- Clock resilience, pre-resilience x post on centipawn loss: ", coef_fmt(all_coefs_dt, "I08b", c("pre_clock_resilience_z:format_5_0", "format_5_0:pre_clock_resilience_z"))),
  paste0("- Board-clock substitution, eval x clock: ", coef_fmt(all_coefs_dt, "I09a", c("eval_adv100:clock_adv_min$"))),
  paste0("- Board-clock substitution, eval x clock x post: ", coef_fmt(all_coefs_dt, "I09a", c("eval_adv100:clock_adv_min:format_5_0", "format_5_0:eval_adv100:clock_adv_min"))),
  "",
  "## Output Files",
  "",
  "- `interaction_clock_model_coefficients.csv`: all coefficient estimates.",
  "- `interaction_clock_headline_coefficients.csv`: mechanism-specific headline terms.",
  "- `interaction_clock_sample_summary.csv`: samples used by each input panel.",
  "- `pre_change_clock_resilience_traits.csv`: player-level pre-change clock-resilience traits."
)
writeLines(md, file.path(out_dir, "interaction_clock_results.md"))

capture.output(sessionInfo(), file = file.path(out_dir, "session_info.txt"))
cat("Wrote interaction clock analysis to", out_dir, "\n")
