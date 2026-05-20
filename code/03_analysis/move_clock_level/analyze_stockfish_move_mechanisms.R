suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit == length(args)) return(default)
  args[[hit + 1L]]
}

arg_values <- function(flag) {
  hit <- which(args == flag)
  hit <- hit[hit < length(args)]
  if (length(hit) == 0) return(character())
  args[hit + 1L]
}

split_paths <- function(x) {
  if (length(x) == 0) return(character())
  y <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  trimws(y[nzchar(trimws(y))])
}

CP_PATH <- arg_value(
  "--centipawn-csv",
  "outputs/whole_dataset_2024_2026/centipawn_loss_nodes2000_watch/centipawn_loss_watch.csv"
)
CP_PATHS <- unique(split_paths(c(arg_values("--centipawn-csv"), arg_value("--centipawn-csvs", ""))))
if (length(CP_PATHS) == 0) CP_PATHS <- CP_PATH
TOURNAMENT_PATH <- arg_value(
  "--tournament-csv",
  "data/merged_tournaments_1_150_added_missed_links_3.csv"
)
METADATA_PATH <- arg_value(
  "--metadata-csv",
  "data/final_regression_data_tournaments_2022_2026.csv"
)
OUT_DIR <- arg_value(
  "--output-dir",
  "analysis_outputs/stockfish_move_mechanisms"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RULE_CHANGE_DATE <- as.IDate("2025-09-01")
CP_LOSS_CAP <- 1000
WINNING_CP <- 200
EQUAL_CP <- 100
MIN_PLIES_BEFORE_END <- 10

clean_term <- function(x) gsub("`", "", x, fixed = TRUE)

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))
}

write_md_table <- function(dt, cols) {
  d <- as.data.table(dt)[, ..cols]
  lines <- c(
    paste0("| ", paste(cols, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  )
  for (i in seq_len(nrow(d))) {
    vals <- vapply(d[i], as.character, character(1))
    vals <- gsub("\\|", "/", vals)
    lines <- c(lines, paste0("| ", paste(vals, collapse = " | "), " |"))
  }
  lines
}

extract_game_id <- function(x) sub(".*live/([0-9]+).*", "\\1", x)

safe_feols <- function(name, formula, data, cluster = ~ player_name + tournament_id) {
  tryCatch(
    list(name = name, model = feols(formula, data = data, cluster = cluster), error = NA_character_),
    error = function(e) list(name = name, model = NULL, error = conditionMessage(e))
  )
}

tidy_model <- function(model_obj, family) {
  if (is.null(model_obj$model)) {
    return(data.table(
      family = family,
      model = model_obj$name,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      nobs = NA_integer_,
      r2_within = NA_real_,
      error = model_obj$error
    ))
  }
  out <- as.data.table(broom::tidy(model_obj$model, conf.int = TRUE))
  out[, term := clean_term(term)]
  out[, `:=`(
    family = family,
    model = model_obj$name,
    nobs = nobs(model_obj$model),
    r2_within = as.numeric(fitstat(model_obj$model, "wr2")[[1]]),
    error = NA_character_
  )]
  setcolorder(out, c(
    "family", "model", "term", "estimate", "std.error", "conf.low",
    "conf.high", "p.value", "nobs", "r2_within", "error"
  ))
  out
}

estimate_and_tidy <- function(name, formula, data, family, cluster = ~ player_name + tournament_id) {
  message("  - ", name)
  model_obj <- safe_feols(name, formula, data, cluster = cluster)
  out <- tidy_model(model_obj, family)
  rm(model_obj)
  invisible(gc(verbose = FALSE))
  out
}

message("Reading centipawn-loss data: ", paste(CP_PATHS, collapse = ", "))
move_cols <- c(
  "game_id", "date", "result", "white", "black", "ply", "fullmove_number",
  "phase", "mover_color", "mover", "is_capture", "gives_check",
  "eval_before_white_cp", "eval_after_white_cp", "eval_before_mover_cp",
  "eval_after_mover_cp", "cp_loss"
)
read_move_file <- function(path) {
  dt <- fread(path, select = move_cols, showProgress = TRUE)
  dt[, source_centipawn_csv := path]
  dt[]
}
moves <- rbindlist(lapply(CP_PATHS, read_move_file), use.names = TRUE, fill = TRUE)
CP_TOTAL_ROWS <- nrow(moves)
moves[, game_id := as.character(game_id)]
moves <- moves[!is.na(game_id) & !is.na(cp_loss)]
moves <- unique(moves, by = c("game_id", "ply"))
moves[, `:=`(
  cp_loss = as.numeric(cp_loss),
  cp_loss_cap = pmin(as.numeric(cp_loss), CP_LOSS_CAP),
  fullmove_number = as.integer(fullmove_number),
  ply = as.integer(ply),
  is_capture = as.integer(is_capture),
  gives_check = as.integer(gives_check),
  eval_before_mover_cp = as.numeric(eval_before_mover_cp),
  eval_after_mover_cp = as.numeric(eval_after_mover_cp),
  eval_after_white_cp = as.numeric(eval_after_white_cp)
)]

message("Reading tournament mapping: ", TOURNAMENT_PATH)
games <- fread(
  TOURNAMENT_PATH,
  select = c(
    "white_name", "black_name", "white_rating", "black_rating",
    "result_white", "result_black", "date", "round", "game_link",
    "accuracy_white", "accuracy_black"
  ),
  showProgress = TRUE
)
games[, game_id := as.character(extract_game_id(game_link))]
games <- games[game_id != "" & !is.na(game_id)]

player_games <- rbindlist(list(
  games[, .(
    game_id,
    mover_color = "white",
    player_name = white_name,
    opponent_name = black_name,
    player_rating = as.numeric(white_rating),
    opponent_rating = as.numeric(black_rating),
    player_result = as.numeric(result_white),
    player_accuracy = as.numeric(accuracy_white),
    tournament_id = date,
    round = as.integer(round)
  )],
  games[, .(
    game_id,
    mover_color = "black",
    player_name = black_name,
    opponent_name = white_name,
    player_rating = as.numeric(black_rating),
    opponent_rating = as.numeric(white_rating),
    player_result = as.numeric(result_black),
    player_accuracy = as.numeric(accuracy_black),
    tournament_id = date,
    round = as.integer(round)
  )]
), use.names = TRUE)
player_games <- unique(player_games, by = c("game_id", "mover_color"))

message("Reading player metadata: ", METADATA_PATH)
metadata <- fread(
  METADATA_PATH,
  select = c(
    "player_name", "opponent_name", "date", "round", "female",
    "country_name", "gdp_per_capita_ppp_logged", "birthday"
  ),
  showProgress = TRUE
)
metadata <- unique(metadata, by = c("player_name", "opponent_name", "date", "round"))
setnames(metadata, "date", "tournament_id")

player_games <- merge(
  player_games,
  metadata,
  by = c("player_name", "opponent_name", "tournament_id", "round"),
  all.x = TRUE,
  sort = FALSE
)
player_games[, `:=`(
  event_date = as.IDate(substr(tournament_id, 1, 10)),
  format_5_0 = as.integer(as.IDate(substr(tournament_id, 1, 10)) >= RULE_CHANGE_DATE),
  tournament_year = as.integer(substr(tournament_id, 1, 4)),
  rating_diff100 = (player_rating - opponent_rating) / 100,
  female = as.integer(female),
  gdp_log = as.numeric(gdp_per_capita_ppp_logged),
  birthday = as.numeric(birthday)
)]
player_games[, age := tournament_year - birthday]
player_games <- player_games[
  !is.na(player_name) &
    !is.na(format_5_0) &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    !is.na(female) &
    !is.na(gdp_log) &
    !is.na(age) & age >= 10 & age <= 90
]
player_games[, `:=`(
  age10_c = (age - mean(age, na.rm = TRUE)) / 10,
  gdp_log_c = gdp_log - mean(gdp_log, na.rm = TRUE)
)]

message("Joining move rows to player-game metadata")
moves <- merge(
  moves,
  player_games,
  by = c("game_id", "mover_color"),
  all.x = FALSE,
  all.y = FALSE,
  sort = FALSE
)
moves <- moves[
  !is.na(cp_loss_cap) &
    !is.na(fullmove_number) &
    !is.na(player_name) &
    !is.na(tournament_id)
]

moves[, max_ply := max(ply, na.rm = TRUE), by = game_id]
moves[, `:=`(
  late_phase = as.integer(fullmove_number >= 36),
  last_10_ply = as.integer(ply > max_ply - 10),
  blunder = as.integer(cp_loss >= 200),
  mistake = as.integer(cp_loss >= 100),
  inaccuracy = as.integer(cp_loss >= 50),
  critical_equal = as.integer(abs(eval_before_mover_cp) <= EQUAL_CP),
  phase_group = factor(
    fifelse(fullmove_number <= 10, "opening_1_10",
      fifelse(fullmove_number <= 20, "early_middlegame_11_20",
        fifelse(fullmove_number <= 35, "late_middlegame_21_35", "endgame_36_plus")
      )
    ),
    levels = c("opening_1_10", "early_middlegame_11_20", "late_middlegame_21_35", "endgame_36_plus")
  ),
  player_game_id = paste(game_id, mover_color, sep = "|")
)]

sample_summary <- data.table(
  metric = c(
    "centipawn_rows_read", "usable_move_rows", "unique_games", "unique_player_games",
    "players", "tournaments", "pre_move_rows", "post_move_rows",
    "mean_cp_loss", "mean_cp_loss_capped", "blunder_rate", "mistake_rate",
    "inaccuracy_rate", "metadata_missing_dropped"
  ),
  value = c(
    CP_TOTAL_ROWS,
    nrow(moves),
    uniqueN(moves$game_id),
    uniqueN(moves$player_game_id),
    uniqueN(moves$player_name),
    uniqueN(moves$tournament_id),
    sum(moves$format_5_0 == 0),
    sum(moves$format_5_0 == 1),
    round(mean(moves$cp_loss, na.rm = TRUE), 4),
    round(mean(moves$cp_loss_cap, na.rm = TRUE), 4),
    round(mean(moves$blunder, na.rm = TRUE), 5),
    round(mean(moves$mistake, na.rm = TRUE), 5),
    round(mean(moves$inaccuracy, na.rm = TRUE), 5),
    CP_TOTAL_ROWS - nrow(moves)
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

phase_descriptives <- moves[, .(
  move_rows = .N,
  games = uniqueN(game_id),
  mean_cp_loss = mean(cp_loss, na.rm = TRUE),
  mean_cp_loss_cap = mean(cp_loss_cap, na.rm = TRUE),
  blunder_rate = mean(blunder, na.rm = TRUE),
  mistake_rate = mean(mistake, na.rm = TRUE),
  inaccuracy_rate = mean(inaccuracy, na.rm = TRUE)
), by = .(format_5_0, phase_group)][order(format_5_0, phase_group)]
fwrite(phase_descriptives, file.path(OUT_DIR, "phase_descriptives.csv"))

message("Building player-game outcomes")
position_summary <- moves[, .(
  max_ply = max(ply, na.rm = TRUE),
  first_white_win_ply = as.numeric(suppressWarnings(min(ply[eval_after_white_cp >= WINNING_CP], na.rm = TRUE))),
  first_black_win_ply = as.numeric(suppressWarnings(min(ply[eval_after_white_cp <= -WINNING_CP], na.rm = TRUE)))
), by = game_id]
position_summary[is.infinite(first_white_win_ply), first_white_win_ply := NA_real_]
position_summary[is.infinite(first_black_win_ply), first_black_win_ply := NA_real_]

move_outcomes <- moves[, {
  n <- .N
  x <- as.numeric(fullmove_number)
  y <- as.numeric(cp_loss_cap)
  denom <- n * sum(x^2, na.rm = TRUE) - sum(x, na.rm = TRUE)^2
  slope <- if (is.na(denom) || denom <= 0) NA_real_ else {
    (n * sum(x * y, na.rm = TRUE) - sum(x, na.rm = TRUE) * sum(y, na.rm = TRUE)) / denom
  }
  .(
    move_rows = n,
    mean_cp_loss = mean(cp_loss, na.rm = TRUE),
    mean_cp_loss_cap = mean(cp_loss_cap, na.rm = TRUE),
    opening_cp_loss = mean(cp_loss_cap[fullmove_number <= 10], na.rm = TRUE),
    endgame_cp_loss = mean(cp_loss_cap[fullmove_number >= 36], na.rm = TRUE),
    last10_cp_loss = mean(cp_loss_cap[last_10_ply == 1], na.rm = TRUE),
    cp_loss_slope = slope,
    blunder_rate = mean(blunder, na.rm = TRUE),
    mistake_rate = mean(mistake, na.rm = TRUE),
    inaccuracy_rate = mean(inaccuracy, na.rm = TRUE),
    first_blunder_move = as.numeric(suppressWarnings(min(fullmove_number[blunder == 1], na.rm = TRUE))),
    first_mistake_move = as.numeric(suppressWarnings(min(fullmove_number[mistake == 1], na.rm = TRUE))),
    no_blunder_game = as.integer(sum(blunder, na.rm = TRUE) == 0),
    no_mistake_game = as.integer(sum(mistake, na.rm = TRUE) == 0)
  )
}, by = .(player_game_id, game_id, mover_color)]
move_outcomes[is.infinite(first_blunder_move), first_blunder_move := NA_real_]
move_outcomes[is.infinite(first_mistake_move), first_mistake_move := NA_real_]

player_game_outcomes <- merge(
  unique(player_games, by = c("game_id", "mover_color")),
  move_outcomes,
  by = c("game_id", "mover_color"),
  all.y = TRUE,
  sort = FALSE
)
player_game_outcomes <- merge(
  player_game_outcomes,
  position_summary,
  by = "game_id",
  all.x = TRUE,
  sort = FALSE
)
player_game_outcomes[, `:=`(
  first_player_win_ply = fifelse(mover_color == "white", first_white_win_ply, first_black_win_ply),
  first_player_loss_ply = fifelse(mover_color == "white", first_black_win_ply, first_white_win_ply)
)]
player_game_outcomes[, `:=`(
  reached_winning_position = as.integer(!is.na(first_player_win_ply) & first_player_win_ply <= max_ply - MIN_PLIES_BEFORE_END),
  reached_losing_position = as.integer(!is.na(first_player_loss_ply) & first_player_loss_ply <= max_ply - MIN_PLIES_BEFORE_END),
  converted_winning_position = as.integer(player_result == 1),
  escaped_losing_position = as.integer(player_result > 0)
)]

fwrite(
  player_game_outcomes,
  file.path(OUT_DIR, "player_game_move_outcomes.csv")
)

player_game_descriptives <- player_game_outcomes[, .(
  player_games = .N,
  reached_winning_rate = mean(reached_winning_position, na.rm = TRUE),
  conversion_rate_if_winning = mean(converted_winning_position[reached_winning_position == 1], na.rm = TRUE),
  reached_losing_rate = mean(reached_losing_position, na.rm = TRUE),
  escape_rate_if_losing = mean(escaped_losing_position[reached_losing_position == 1], na.rm = TRUE),
  mean_slope = mean(cp_loss_slope, na.rm = TRUE),
  no_blunder_rate = mean(no_blunder_game, na.rm = TRUE),
  mean_first_blunder_move = mean(first_blunder_move, na.rm = TRUE)
), by = format_5_0][order(format_5_0)]
fwrite(player_game_descriptives, file.path(OUT_DIR, "player_game_descriptives.csv"))

message("Estimating move-level models")
move_coefs <- rbindlist(list(
  estimate_and_tidy(
    "late_phase_error_age",
    cp_loss_cap ~ format_5_0 * age10_c * late_phase +
      rating_diff100 + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "last10_error_age",
    cp_loss_cap ~ format_5_0 * age10_c * last_10_ply +
      rating_diff100 + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "blunder_probability_metadata",
    blunder ~ format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c +
      rating_diff100 + is_capture + gives_check + i(phase_group) |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "mistake_probability_metadata",
    mistake ~ format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c +
      rating_diff100 + is_capture + gives_check + i(phase_group) |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "inaccuracy_probability_metadata",
    inaccuracy ~ format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c +
      rating_diff100 + is_capture + gives_check + i(phase_group) |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "critical_equal_error_age",
    cp_loss_cap ~ format_5_0 * age10_c * critical_equal +
      rating_diff100 + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "critical_equal_blunder_age",
    blunder ~ format_5_0 * age10_c * critical_equal +
      rating_diff100 + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "late_phase_error_female",
    cp_loss_cap ~ format_5_0 * female * late_phase +
      rating_diff100 + age10_c + gdp_log_c + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "last10_error_female",
    cp_loss_cap ~ format_5_0 * female * last_10_ply +
      rating_diff100 + age10_c + gdp_log_c + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "critical_equal_error_female",
    cp_loss_cap ~ format_5_0 * female * critical_equal +
      rating_diff100 + age10_c + gdp_log_c + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  ),
  estimate_and_tidy(
    "critical_equal_blunder_female",
    blunder ~ format_5_0 * female * critical_equal +
      rating_diff100 + age10_c + gdp_log_c + is_capture + gives_check |
      player_name + tournament_id + fullmove_number,
    moves,
    "move_level"
  )
), fill = TRUE)
move_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(move_coefs, file.path(OUT_DIR, "move_level_model_coefficients.csv"))

message("Estimating player-game models")
pg_coefs <- rbindlist(list(
  estimate_and_tidy(
    "conversion_from_winning_position",
    converted_winning_position ~ format_5_0 * rating_diff100 +
      format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    player_game_outcomes[reached_winning_position == 1],
    "player_game"
  ),
  estimate_and_tidy(
    "defensive_escape_from_losing_position",
    escaped_losing_position ~ format_5_0 * rating_diff100 +
      format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    player_game_outcomes[reached_losing_position == 1],
    "player_game"
  ),
  estimate_and_tidy(
    "within_game_cp_loss_decay_slope",
    cp_loss_slope ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    player_game_outcomes[!is.na(cp_loss_slope) & move_rows >= 10],
    "player_game"
  ),
  estimate_and_tidy(
    "no_blunder_game_probability",
    no_blunder_game ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    player_game_outcomes,
    "player_game"
  ),
  estimate_and_tidy(
    "first_blunder_move_conditional",
    first_blunder_move ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    player_game_outcomes[!is.na(first_blunder_move)],
    "player_game"
  )
), fill = TRUE)
pg_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(pg_coefs, file.path(OUT_DIR, "player_game_model_coefficients.csv"))

all_coefs <- rbindlist(list(move_coefs, pg_coefs), fill = TRUE)
fwrite(all_coefs, file.path(OUT_DIR, "all_model_coefficients.csv"))

headline_terms <- all_coefs[
  !is.na(term) &
    grepl("format_5_0", term) &
    !grepl("^format_5_0$", term)
]
headline_terms[, abs_t := abs(estimate / std.error)]
setorder(headline_terms, -abs_t)
headline <- headline_terms[1:min(.N, 30)]
fwrite(headline, file.path(OUT_DIR, "headline_format_interactions.csv"))

report_headline <- copy(headline)
report_headline[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value),
  r2_within = fmt(r2_within, 4)
)]

report_sample <- copy(sample_summary)
setnames(report_sample, c("Metric", "Value"))

report_phase <- copy(phase_descriptives)
report_phase[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  mean_cp_loss = fmt(mean_cp_loss, 2),
  mean_cp_loss_cap = fmt(mean_cp_loss_cap, 2),
  blunder_rate = fmt(blunder_rate, 4),
  mistake_rate = fmt(mistake_rate, 4),
  inaccuracy_rate = fmt(inaccuracy_rate, 4)
)]

report_pg_desc <- copy(player_game_descriptives)
report_pg_desc[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  reached_winning_rate = fmt(reached_winning_rate, 4),
  conversion_rate_if_winning = fmt(conversion_rate_if_winning, 4),
  reached_losing_rate = fmt(reached_losing_rate, 4),
  escape_rate_if_losing = fmt(escape_rate_if_losing, 4),
  mean_slope = fmt(mean_slope, 4),
  no_blunder_rate = fmt(no_blunder_rate, 4),
  mean_first_blunder_move = fmt(mean_first_blunder_move, 2)
)]

model_inventory <- data.table(
  hypothesis = c(
    "Late-game error accumulation",
    "Blunder/mistake/inaccuracy probability",
    "Critical/equal-position performance",
    "Conversion from winning positions",
    "Defensive survival from losing positions",
    "Within-game accuracy decay",
    "First major mistake timing",
    "Opening preparation/book exit",
    "Legal-move/piece complexity",
    "Simplification strategy"
  ),
  status = c(
    "estimated",
    "estimated",
    "estimated with equal-position proxy",
    "estimated using +2 position at least 10 plies before game end",
    "estimated using -2 position at least 10 plies before game end",
    "estimated as player-game slope of capped cp loss on move number",
    "estimated as no-blunder and first-blunder-move outcomes",
    "not estimated from current centipawn CSV; requires PGN ECO/book parsing",
    "not estimated from current centipawn CSV; requires FEN/legal move features or PGN re-parse",
    "not estimated from current centipawn CSV; requires pieces/queen/material features"
  )
)

md <- c(
  "# Stockfish Move-Mechanism Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Inputs",
  "",
  paste0("- Centipawn CSV: `", paste(CP_PATHS, collapse = "`, `"), "`"),
  paste0("- Tournament CSV: `", TOURNAMENT_PATH, "`"),
  paste0("- Metadata CSV: `", METADATA_PATH, "`"),
  "",
  "The analysis uses the currently available Stockfish results. If the watcher is still running, rerun this script later to refresh the estimates on the larger sample.",
  "",
  "## Sample",
  "",
  write_md_table(report_sample, c("Metric", "Value")),
  "",
  "## Hypothesis Coverage",
  "",
  write_md_table(model_inventory, c("hypothesis", "status")),
  "",
  "## Descriptives by Phase and Format",
  "",
  write_md_table(
    report_phase,
    c("period", "phase_group", "move_rows", "games", "mean_cp_loss", "mean_cp_loss_cap", "blunder_rate", "mistake_rate", "inaccuracy_rate")
  ),
  "",
  "## Player-Game Descriptives",
  "",
  write_md_table(
    report_pg_desc,
    c("period", "player_games", "reached_winning_rate", "conversion_rate_if_winning", "reached_losing_rate", "escape_rate_if_losing", "mean_slope", "no_blunder_rate", "mean_first_blunder_move")
  ),
  "",
  "## Strongest Format-Interaction Coefficients",
  "",
  "These rows rank all estimated format-interaction terms by absolute t-statistic. Interpret signs in the units of the model outcome.",
  "",
  write_md_table(
    report_headline,
    c("family", "model", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within")
  ),
  "",
  "## Model Notes",
  "",
  "- Move-level models use player fixed effects, tournament fixed effects, and fullmove-number fixed effects, with two-way clustered standard errors by player and tournament.",
  "- `cp_loss_cap` caps centipawn loss at 1000 to prevent mate-score outliers from dominating means and linear models.",
  "- `late_phase` is fullmove number 36 or later. `last_10_ply` marks the final 10 plies of the game.",
  "- `critical_equal` is a practical proxy for critical positions: absolute engine evaluation before the move is within 100 centipawns from the mover's perspective.",
  "- Conversion uses positions where a player reached at least +200 centipawns at least 10 plies before the end; escape uses positions where a player reached at most -200 centipawns at least 10 plies before the end.",
  "- Opening/book-exit and board-complexity mechanisms need additional features not present in the current centipawn CSV.",
  "",
  "## Output Files",
  "",
  "- `sample_summary.csv`",
  "- `phase_descriptives.csv`",
  "- `player_game_descriptives.csv`",
  "- `player_game_move_outcomes.csv`",
  "- `move_level_model_coefficients.csv`",
  "- `player_game_model_coefficients.csv`",
  "- `all_model_coefficients.csv`",
  "- `headline_format_interactions.csv`",
  "- `move_mechanisms_report.md`",
  "- `session_info.txt`"
)

writeLines(md, file.path(OUT_DIR, "move_mechanisms_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
