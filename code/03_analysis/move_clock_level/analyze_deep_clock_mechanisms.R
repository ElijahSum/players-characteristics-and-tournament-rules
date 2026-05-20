suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

setFixest_notes(FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "code/03_analysis/move_clock_level/analyze_deep_clock_mechanisms.R"
ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
if (!dir.exists(file.path(ROOT, "analysis_outputs"))) ROOT <- getwd()

out_dir <- file.path(ROOT, "analysis_outputs", "deep_clock_mechanisms_2022_2026")
first_pass_dir <- file.path(ROOT, "analysis_outputs", "clock_time_mechanisms_2022_2026")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

metadata_path <- file.path(ROOT, "data", "final_regression_data_tournaments_2022_2026.csv")
bridge_paths <- c(
  file.path(ROOT, "data", "tournaments_1_261_final_v6.csv"),
  file.path(ROOT, "data", "merged_tournaments_1_150_added_missed_links_3.csv")
)

paths <- list(
  snapshots = file.path(out_dir, "deep_clock_snapshots.csv"),
  clock_bins = file.path(out_dir, "deep_clock_bins_player_game.csv"),
  time_bins = file.path(out_dir, "deep_time_spent_bins_player_game.csv"),
  recovery = file.path(out_dir, "deep_clock_recovery_player_game.csv"),
  low_event = file.path(out_dir, "deep_low_clock_event_player_game.csv"),
  player_game = file.path(first_pass_dir, "player_game_clock_features.csv")
)
required_files <- c(metadata_path, bridge_paths, unlist(paths))
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
    select = c("white_name", "black_name", "date", "round", "game_link"),
    colClasses = list(character = c("date", "game_link"))
  )
  raw[, date_key := normalize_event_datetime(date)]
  raw[, game_id := extract_game_id(game_link)]
  raw <- raw[!is.na(game_id) & !is.na(date_key)]
  raw[, round := as.integer(round)]
  white <- raw[, .(
    date_key, round, game_id,
    player_name = white_name, opponent_name = black_name, is_white = 1L
  )]
  black <- raw[, .(
    date_key, round, game_id,
    player_name = black_name, opponent_name = white_name, is_white = 0L
  )]
  rbindlist(list(white, black), use.names = TRUE)
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
  model <- feols(formula, data = data, cluster = ~player_name, weights = weights)
  coefs <- tidy_fixest(model, model_id, mechanism, outcome)
  if (is.null(key_patterns)) {
    headline <- coefs
  } else {
    keep <- Reduce(`|`, lapply(key_patterns, function(pat) grepl(pat, coefs$term, fixed = FALSE)))
    headline <- coefs[keep]
  }
  headline[, is_headline := TRUE]
  list(model = model, coefs = coefs, headline = headline)
}

cat("Building metadata bridge...\n")
meta_cols <- c(
  "player_name", "player_rating", "player_accuracy", "round", "date",
  "opponent_rating", "player_result", "opponent_name", "is_white",
  "birthday", "female", "classic_rating", "blitz_rating"
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
meta_small <- meta_game[!is.na(game_id), .(
  game_id = as.character(game_id),
  player_name,
  date_key,
  round,
  player_result = as.numeric(player_result),
  player_rating = as.numeric(player_rating),
  opponent_rating = as.numeric(opponent_rating),
  rating_diff100 = (as.numeric(player_rating) - as.numeric(opponent_rating)) / 100,
  age10_c = (2025 - as.numeric(birthday)) / 10,
  female = as.integer(female),
  online_over_classical100 = (as.numeric(player_rating) - as.numeric(classic_rating)) / 100,
  online_over_blitz100 = (as.numeric(player_rating) - as.numeric(blitz_rating)) / 100
)]
meta_small[, age10_c := age10_c - mean(age10_c, na.rm = TRUE)]
meta_small[, event_date := as.IDate(date_key)]
meta_small[, format_5_0 := as.integer(event_date >= as.IDate("2025-09-02"))]
setkey(meta_small, game_id, player_name)

all_coefs <- list()
all_headlines <- list()
sample_rows <- list()

sample_rows[["meta"]] <- data.table(
  dataset = "metadata_bridge",
  rows = nrow(meta_small),
  games = uniqueN(meta_small$game_id),
  players = uniqueN(meta_small$player_name),
  events = uniqueN(meta_small$date_key)
)

cat("Reading snapshot data...\n")
snap <- fread(paths$snapshots, colClasses = list(character = c("game_id", "player_name")))
snap <- merge(snap, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
snap[, eval_adv100 := pmax(pmin(as.numeric(eval_advantage_cp), 1000), -1000) / 100]
snap[, clock_adv_min := as.numeric(clock_advantage_seconds) / 60]
snap[, game_snapshot_id := paste(game_id, snapshot_move, sep = "_")]
sample_rows[["snapshots"]] <- data.table(
  dataset = "snapshots",
  rows = nrow(snap), games = uniqueN(snap$game_id),
  players = uniqueN(snap$player_name), events = uniqueN(snap$date_key)
)

m <- fit_one(
  "D01", "Shadow price of clock time",
  "Player result at move 10/20/30 snapshots",
  player_result ~ eval_adv100 + eval_adv100:format_5_0 +
    clock_adv_min + clock_adv_min:format_5_0 | game_snapshot_id,
  snap,
  c("eval_adv100", "clock_adv_min")
)
all_coefs[["D01"]] <- m$coefs
all_headlines[["D01"]] <- m$headline

m <- fit_one(
  "D02", "Skill requires clock capital",
  "Player result at move 10/20/30 snapshots",
  player_result ~ eval_adv100 + eval_adv100:format_5_0 +
    clock_adv_min + clock_adv_min:format_5_0 +
    rating_diff100 + rating_diff100:format_5_0 +
    rating_diff100:clock_adv_min +
    rating_diff100:clock_adv_min:format_5_0 | player_name + date_key + snapshot_move,
  snap,
  c("rating_diff100", "clock_adv_min")
)
all_coefs[["D02"]] <- m$coefs
all_headlines[["D02"]] <- m$headline

shadow_rows <- list()
coef_get <- function(b, names) {
  for (nm in names) {
    if (nm %in% names(b)) return(unname(b[nm]))
  }
  0
}
for (mv in sort(unique(snap$snapshot_move))) {
  sm <- snap[snapshot_move == mv]
  mod <- feols(
    player_result ~ eval_adv100 + eval_adv100:format_5_0 +
      clock_adv_min + clock_adv_min:format_5_0 | game_snapshot_id,
    data = sm,
    cluster = ~player_name
  )
  b <- coef(mod)
  beta_eval_pre <- coef_get(b, "eval_adv100")
  beta_eval_post <- beta_eval_pre + coef_get(b, c("eval_adv100:format_5_0", "format_5_0:eval_adv100"))
  beta_clock_pre <- coef_get(b, "clock_adv_min")
  beta_clock_post <- beta_clock_pre + coef_get(b, c("clock_adv_min:format_5_0", "format_5_0:clock_adv_min"))
  shadow_rows[[as.character(mv)]] <- data.table(
    snapshot_move = mv,
    n_obs = nobs(mod),
    beta_eval100_pre = beta_eval_pre,
    beta_eval100_post = beta_eval_post,
    beta_clock_min_pre = beta_clock_pre,
    beta_clock_min_post = beta_clock_post,
    clock_min_equiv_cp_pre = 100 * beta_clock_pre / beta_eval_pre,
    clock_min_equiv_cp_post = 100 * beta_clock_post / beta_eval_post
  )
}
shadow_price <- rbindlist(shadow_rows)
fwrite(shadow_price, file.path(out_dir, "deep_shadow_price_summary.csv"))
rm(snap)
gc()

cat("Reading recovery and threshold data...\n")
rec <- fread(paths$recovery, colClasses = list(character = c("game_id", "player_name")))
rec <- merge(rec, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
sample_rows[["recovery"]] <- data.table(
  dataset = "recovery",
  rows = nrow(rec), games = uniqueN(rec$game_id),
  players = uniqueN(rec$player_name), events = uniqueN(rec$date_key)
)

m <- fit_one(
  "D03", "Increment insurance",
  "Recovery above 10 seconds after entering <=10 seconds",
  recovered_low10 ~ format_5_0 + rating_diff100 + i(round) | player_name,
  rec[reached_low10 == 1],
  c("format_5_0")
)
all_coefs[["D03"]] <- m$coefs
all_headlines[["D03"]] <- m$headline

m <- fit_one(
  "D04", "Age and increment insurance",
  "Recovery above 10 seconds after entering <=10 seconds",
  recovered_low10 ~ format_5_0 + format_5_0:age10_c + rating_diff100 + i(round) | player_name,
  rec[reached_low10 == 1 & !is.na(age10_c)],
  c("format_5_0", "format_5_0:age10_c")
)
all_coefs[["D04"]] <- m$coefs
all_headlines[["D04"]] <- m$headline

m <- fit_one(
  "D05", "Mechanical increment accumulation",
  "Share of own moves where clock after move exceeds clock before move",
  clock_gain_move_share ~ format_5_0 + rating_diff100 + i(round) | player_name,
  rec,
  c("format_5_0")
)
all_coefs[["D05"]] <- m$coefs
all_headlines[["D05"]] <- m$headline

m <- fit_one(
  "D06", "Practical flagging conversion",
  "Conversion after first reaching +2",
  board_converted_winning ~ first_winning_opp_low10 + first_winning_opp_low10:format_5_0 +
    first_winning_own_low10 + first_winning_own_low10:format_5_0 +
    rating_diff100 + i(round) | player_name + date_key,
  rec[board_reached_winning == 1],
  c("first_winning_opp_low10", "first_winning_own_low10")
)
all_coefs[["D06"]] <- m$coefs
all_headlines[["D06"]] <- m$headline

m <- fit_one(
  "D07", "Dirty survival from bad positions",
  "Escape after first reaching -2",
  board_escaped_losing ~ first_losing_opp_low10 + first_losing_opp_low10:format_5_0 +
    first_losing_own_low10 + first_losing_own_low10:format_5_0 +
    rating_diff100 + i(round) | player_name + date_key,
  rec[board_reached_losing == 1],
  c("first_losing_opp_low10", "first_losing_own_low10")
)
all_coefs[["D07"]] <- m$coefs
all_headlines[["D07"]] <- m$headline

cat("Reading player-game clock features for early-saving and style models...\n")
pg_cols <- c(
  "game_id", "player_name", "mean_cp_loss_cap", "player_result_pgn",
  "low_before_10_share", "mean_cp_low_before_10", "mean_cp_nonlow_before_10",
  "time_after_move_10", "last10_blunder_rate", "last10_mean_cp",
  "blunder_rate", "final_time_after"
)
pg <- fread(paths$player_game, select = pg_cols, colClasses = list(character = c("game_id", "player_name")))
pg <- merge(pg, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
pg[, start_clock_seconds := fifelse(format_5_0 == 1, 300, 180)]
pg[, move10_clock_fraction := as.numeric(time_after_move_10) / start_clock_seconds]
pg[, panic_penalty_cp := as.numeric(mean_cp_low_before_10) - as.numeric(mean_cp_nonlow_before_10)]
pg_rec <- rec[, .(game_id, player_name, recovered_low10, reached_low10, clock_gain_move_share)]
setkey(pg_rec, game_id, player_name)
pg <- merge(pg, pg_rec, by = c("game_id", "player_name"), all.x = TRUE, sort = FALSE)
sample_rows[["player_game"]] <- data.table(
  dataset = "player_game",
  rows = nrow(pg), games = uniqueN(pg$game_id),
  players = uniqueN(pg$player_name), events = uniqueN(pg$date_key)
)

m <- fit_one(
  "D08", "Opening time as late-game insurance",
  "Last-10-move blunder rate",
  last10_blunder_rate ~ move10_clock_fraction + move10_clock_fraction:format_5_0 +
    rating_diff100 + i(round) | player_name + date_key,
  pg[!is.na(move10_clock_fraction) & !is.na(last10_blunder_rate)],
  c("move10_clock_fraction")
)
all_coefs[["D08"]] <- m$coefs
all_headlines[["D08"]] <- m$headline

pre_traits <- pg[format_5_0 == 0, .(
  pre_games = .N,
  pre_low10_share = mean(as.numeric(low_before_10_share), na.rm = TRUE),
  pre_recovery_low10 = mean(as.numeric(recovered_low10)[reached_low10 == 1], na.rm = TRUE),
  pre_panic_penalty_cp = mean(panic_penalty_cp, na.rm = TRUE),
  pre_move10_clock_fraction = mean(move10_clock_fraction, na.rm = TRUE),
  pre_clock_gain_share = mean(as.numeric(clock_gain_move_share), na.rm = TRUE)
), by = player_name]
pre_traits <- pre_traits[pre_games >= 20]
for (v in c("pre_low10_share", "pre_recovery_low10", "pre_panic_penalty_cp", "pre_move10_clock_fraction", "pre_clock_gain_share")) {
  pre_traits[, paste0(v, "_z") := zscore(get(v))]
}
fwrite(pre_traits, file.path(out_dir, "deep_pre_change_clock_style_traits.csv"))
pg <- merge(pg, pre_traits, by = "player_name", all.x = TRUE, sort = FALSE)

m <- fit_one(
  "D09", "Predetermined clock style and adaptation",
  "Player result",
  player_result ~ format_5_0:pre_low10_share_z +
    format_5_0:pre_recovery_low10_z +
    format_5_0:pre_panic_penalty_cp_z +
    format_5_0:pre_move10_clock_fraction_z +
    rating_diff100 + i(round) | player_name + date_key,
  pg[!is.na(pre_low10_share_z)],
  c("format_5_0:pre_")
)
all_coefs[["D09"]] <- m$coefs
all_headlines[["D09"]] <- m$headline

cat("Reading low-clock event data...\n")
low_event <- fread(paths$low_event, colClasses = list(character = c("game_id", "player_name")))
low_event <- merge(low_event, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
sample_rows[["low_event"]] <- data.table(
  dataset = "low_event",
  rows = nrow(low_event), games = uniqueN(low_event$game_id),
  players = uniqueN(low_event$player_name), events = uniqueN(low_event$date_key)
)

m <- fit_one(
  "D10", "Error jump after first low-clock entry",
  "Delta cp loss after first <=30 second move",
  delta_post5_cp ~ format_5_0 + format_5_0:age10_c + rating_diff100 + i(round) | player_name,
  low_event[threshold == 30 & pre5_n >= 3 & post5_n >= 3 & !is.na(age10_c)],
  c("format_5_0")
)
all_coefs[["D10"]] <- m$coefs
all_headlines[["D10"]] <- m$headline

rm(low_event)
gc()

cat("Reading clock-bin data for panic-threshold models...\n")
clock_bins <- fread(paths$clock_bins, colClasses = list(character = c("game_id", "player_name", "clock_bin")))
clock_bins <- merge(clock_bins, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
clock_bins[, clock_bin_f := factor(clock_bin, levels = c("gt120", "60_120", "30_60", "10_30", "05_10", "00_05"))]
sample_rows[["clock_bins"]] <- data.table(
  dataset = "clock_bins",
  rows = nrow(clock_bins), games = uniqueN(clock_bins$game_id),
  players = uniqueN(clock_bins$player_name), events = uniqueN(clock_bins$date_key)
)

m <- fit_one(
  "D11", "Panic threshold shift",
  "Capped centipawn loss by clock bin",
  mean_cp_loss ~ i(clock_bin_f, ref = "gt120") +
    i(clock_bin_f, format_5_0, ref = "gt120") +
    rating_diff100 + i(round) | player_name + date_key,
  clock_bins,
  c("clock_bin_f::"),
  weights = ~n_moves
)
all_coefs[["D11"]] <- m$coefs
all_headlines[["D11"]] <- m$headline

m <- fit_one(
  "D12", "Panic threshold shift in blunders",
  "Blunder rate by clock bin",
  blunder_rate ~ i(clock_bin_f, ref = "gt120") +
    i(clock_bin_f, format_5_0, ref = "gt120") +
    rating_diff100 + i(round) | player_name + date_key,
  clock_bins,
  c("clock_bin_f::"),
  weights = ~n_moves
)
all_coefs[["D12"]] <- m$coefs
all_headlines[["D12"]] <- m$headline
rm(clock_bins)
gc()

cat("Reading time-spent-bin data for thinking-time model...\n")
time_bins <- fread(
  paths$time_bins,
  colClasses = list(character = c("game_id", "player_name", "time_spent_bin"))
)
time_bins <- time_bins[critical == 1]
time_bins <- merge(time_bins, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
time_bins[, time_spent_bin_f := factor(time_spent_bin, levels = c("le1", "01_03", "03_05", "05_10", "10_20", "gt20"))]
sample_rows[["time_bins_critical"]] <- data.table(
  dataset = "time_bins_critical",
  rows = nrow(time_bins), games = uniqueN(time_bins$game_id),
  players = uniqueN(time_bins$player_name), events = uniqueN(time_bins$date_key)
)

m <- fit_one(
  "D13", "Marginal product of thinking time in critical positions",
  "Critical-position cp loss by thinking-time bin",
  mean_cp_loss ~ i(time_spent_bin_f, ref = "le1") +
    i(time_spent_bin_f, format_5_0, ref = "le1") +
    rating_diff100 + i(round) | player_name + date_key,
  time_bins,
  c("time_spent_bin_f::"),
  weights = ~n_moves
)
all_coefs[["D13"]] <- m$coefs
all_headlines[["D13"]] <- m$headline
rm(time_bins)
gc()

coef_table <- rbindlist(all_coefs, fill = TRUE)
headline_table <- rbindlist(all_headlines, fill = TRUE)
sample_summary <- rbindlist(sample_rows, fill = TRUE)

fwrite(coef_table, file.path(out_dir, "deep_clock_model_coefficients.csv"))
fwrite(headline_table, file.path(out_dir, "deep_clock_headline_coefficients.csv"))
fwrite(sample_summary, file.path(out_dir, "deep_clock_sample_summary.csv"))

descriptives <- rbindlist(list(
  rec[, .(
    metric = "recovered_low10_given_reached",
    pre = mean(recovered_low10[format_5_0 == 0 & reached_low10 == 1], na.rm = TRUE),
    post = mean(recovered_low10[format_5_0 == 1 & reached_low10 == 1], na.rm = TRUE)
  )],
  rec[, .(
    metric = "clock_gain_move_share",
    pre = mean(clock_gain_move_share[format_5_0 == 0], na.rm = TRUE),
    post = mean(clock_gain_move_share[format_5_0 == 1], na.rm = TRUE)
  )],
  rec[, .(
    metric = "opp_low10_when_player_first_winning",
    pre = mean(first_winning_opp_low10[format_5_0 == 0 & board_reached_winning == 1], na.rm = TRUE),
    post = mean(first_winning_opp_low10[format_5_0 == 1 & board_reached_winning == 1], na.rm = TRUE)
  )],
  rec[, .(
    metric = "opp_low10_when_player_first_losing",
    pre = mean(first_losing_opp_low10[format_5_0 == 0 & board_reached_losing == 1], na.rm = TRUE),
    post = mean(first_losing_opp_low10[format_5_0 == 1 & board_reached_losing == 1], na.rm = TRUE)
  )],
  pg[, .(
    metric = "move10_clock_fraction",
    pre = mean(move10_clock_fraction[format_5_0 == 0], na.rm = TRUE),
    post = mean(move10_clock_fraction[format_5_0 == 1], na.rm = TRUE)
  )]
))
descriptives[, diff := post - pre]
fwrite(descriptives, file.path(out_dir, "deep_clock_descriptives.csv"))

term_value <- function(model_id_value, pattern) {
  row <- headline_table[model_id == model_id_value & grepl(pattern, term)]
  if (nrow(row) == 0) return("not estimated")
  row <- row[1]
  sprintf("%.4f (se %.4f, p=%.3g)%s", row$estimate, row$std_error, row$p_value, row$significance)
}

shadow_line <- function(mv) {
  row <- shadow_price[snapshot_move == mv]
  if (nrow(row) == 0) return("")
  sprintf(
    "move %d: pre %.1f cp/min, post %.1f cp/min",
    mv, row$clock_min_equiv_cp_pre, row$clock_min_equiv_cp_post
  )
}

md <- c(
  "# Deeper Clock Mechanism Results",
  "",
  "These analyses treat clock time as a strategic state variable rather than as a simple low-time dummy. They are not inserted into the thesis text.",
  "",
  "## Sample",
  "",
  paste0("- Metadata bridge: ", format(sample_summary[dataset == "metadata_bridge", rows], big.mark = ","), " player-games."),
  paste0("- Snapshot sample: ", format(sample_summary[dataset == "snapshots", rows], big.mark = ","), " player-snapshot rows."),
  paste0("- Recovery sample: ", format(sample_summary[dataset == "recovery", rows], big.mark = ","), " player-games."),
  paste0("- Clock-bin sample: ", format(sample_summary[dataset == "clock_bins", rows], big.mark = ","), " player-game clock-bin rows."),
  "",
  "## Main Mechanisms",
  "",
  "1. **Shadow price of clock time.** Clock advantage is priced in expected score after controlling for board evaluation within the same game-snapshot. Estimated centipawn equivalents of one minute of clock advantage:",
  paste0("   - ", shadow_line(10)),
  paste0("   - ", shadow_line(20)),
  paste0("   - ", shadow_line(30)),
  "",
  paste0("2. **Skill requires clock capital.** Triple interaction between rating advantage, clock advantage, and 5+0: ", term_value("D02", "rating_diff100.*clock_adv_min.*format_5_0|format_5_0.*clock_adv_min.*rating_diff100|clock_adv_min.*rating_diff100.*format_5_0"), "."),
  "",
  paste0("3. **Increment insurance.** Post-format effect on recovery above 10 seconds after entering <=10 seconds: ", term_value("D03", "^format_5_0$"), "."),
  paste0("   Descriptively, recovery probability moved from ", sprintf("%.2f", 100 * descriptives[metric == "recovered_low10_given_reached", pre]), "% pre to ", sprintf("%.2f", 100 * descriptives[metric == "recovered_low10_given_reached", post]), "% post."),
  "",
  paste0("4. **Age and increment insurance.** Post-by-age effect on low-clock recovery: ", term_value("D04", "format_5_0:age10_c"), "."),
  "",
  paste0("5. **Mechanical increment accumulation.** Post-format effect on the share of moves where the clock increased after moving: ", term_value("D05", "^format_5_0$"), "."),
  "",
  paste0("6. **Practical flagging conversion.** If the opponent is already <=10 seconds when the player first reaches +2, conversion changes by ", term_value("D06", "^first_winning_opp_low10$"), "; the post interaction is ", term_value("D06", "first_winning_opp_low10:format_5_0"), "."),
  "",
  paste0("7. **Dirty survival from bad positions.** If the opponent is <=10 seconds when the player first reaches -2, escape changes by ", term_value("D07", "^first_losing_opp_low10$"), "; the post interaction is ", term_value("D07", "first_losing_opp_low10:format_5_0"), "."),
  "",
  paste0("8. **Opening time as late-game insurance.** Move-10 clock reserve effect on last-10 blunder rate: ", term_value("D08", "^move10_clock_fraction$"), "; post interaction ", term_value("D08", "move10_clock_fraction:format_5_0"), "."),
  "",
  paste0("9. **Predetermined clock style.** Post interactions with pre-change clock traits are saved in `D09`; the cleanest trait is pre-change low-clock exposure: ", term_value("D09", "format_5_0:pre_low10_share_z"), "."),
  "",
  paste0("10. **First low-clock entry event.** Post-format effect on the jump in cp loss after first entering <=30 seconds: ", term_value("D10", "^format_5_0$"), "; post-by-age ", term_value("D10", "format_5_0:age10_c"), "."),
  "",
  "## Additional Deep Diagnostics",
  "",
  "- `D11` and `D12` estimate panic-threshold curves for centipawn loss and blunder rates by clock bin.",
  "- `D13` estimates the relation between thinking-time bins and move quality in critical positions.",
  "",
  "## Files Written",
  "",
  "- `deep_clock_model_coefficients.csv`: all coefficients.",
  "- `deep_clock_headline_coefficients.csv`: selected coefficients.",
  "- `deep_shadow_price_summary.csv`: centipawn-equivalent value of one minute of clock by snapshot.",
  "- `deep_clock_descriptives.csv`: descriptive mechanism shifts.",
  "- `deep_pre_change_clock_style_traits.csv`: predetermined player clock-style traits.",
  "- `deep_clock_sample_summary.csv`: sample sizes."
)
writeLines(md, file.path(out_dir, "deep_clock_mechanism_results.md"))

cat("Deep clock mechanism analysis complete.\n")
print(sample_summary)
print(shadow_price)
print(headline_table)
