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
  if (length(hit) == 0L || hit == length(args)) return(default)
  args[[hit + 1L]]
}

REGRESSION_PATH <- arg_value(
  "--regression-csv",
  "data/final_regression_data_tournaments_2022_2026.csv"
)
STYLE_PATH <- arg_value(
  "--style-csv",
  "outputs/whole_dataset_2022_2026/style_features/prechange_player_style_features.csv"
)
PLAYER_GAME_MOVE_PATH <- arg_value(
  "--player-game-move-csv",
  "analysis_outputs/stockfish_move_mechanisms/player_game_move_outcomes.csv"
)
OUT_DIR <- arg_value(
  "--output-dir",
  "analysis_outputs/rule_change_risk_return_style"
)
MIN_STYLE_GAMES <- as.integer(arg_value("--min-style-games", "30"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RULE_CHANGE_DATE <- as.IDate("2025-09-01")

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

winsorize <- function(x, probs = c(0.01, 0.99)) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (sum(ok) < 5L) return(x)
  qs <- as.numeric(quantile(x[ok], probs = probs, na.rm = TRUE, names = FALSE))
  pmin(pmax(x, qs[[1L]]), qs[[2L]])
}

zscore <- function(x) {
  x <- winsorize(x)
  sx <- sd(x, na.rm = TRUE)
  if (is.na(sx) || sx == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sx
}

row_mean_na <- function(dt, cols) {
  cols <- cols[cols %in% names(dt)]
  if (length(cols) == 0L) return(rep(NA_real_, nrow(dt)))
  out <- rowMeans(as.matrix(dt[, ..cols]), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

ntile <- function(x, n = 4L) {
  r <- frank(x, ties.method = "average", na.last = "keep")
  nn <- sum(!is.na(x))
  as.integer(pmin(n, ceiling(r / nn * n)))
}

safe_feols <- function(name, formula, data, cluster = ~ player_name + event_id) {
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
    "family", "model", "term", "estimate", "std.error",
    "conf.low", "conf.high", "p.value", "nobs", "r2_within", "error"
  ))
  out
}

message("Reading pre-change style features: ", STYLE_PATH)
style <- fread(STYLE_PATH, showProgress = TRUE)
if (!"player" %in% names(style)) {
  stop("Style file must contain a `player` column.")
}
style[, player_key := tolower(player)]
style <- style[prechange_games >= MIN_STYLE_GAMES]

numeric_style_cols <- setdiff(names(style), c("player", "player_key"))
for (col in numeric_style_cols) {
  style[, (col) := suppressWarnings(as.numeric(get(col)))]
}

clean_components <- list(
  no_blunder_game_rate = 1,
  low_own_blunder_rate = -1,
  low_own_mistake_rate = -1,
  low_own_inaccuracy_rate = -1,
  low_mean_cp_loss = -1,
  low_p90_cp_loss = -1,
  low_sd_cp_loss = -1
)
clean_cols <- character()
for (nm in names(clean_components)) {
  source_col <- sub("^low_", "", nm)
  if (source_col %in% names(style)) {
    z_col <- paste0(nm, "_z")
    style[, (z_col) := clean_components[[nm]] * zscore(get(source_col))]
    clean_cols <- c(clean_cols, z_col)
  }
}

chaos_components <- c(
  "eval_swing_rate",
  "capture_rate",
  "check_rate",
  "opponent_next_blunder_rate",
  "opponent_next_mistake_rate",
  "sd_cp_loss",
  "decisive_game_rate"
)
chaos_cols <- character()
for (col in chaos_components) {
  if (col %in% names(style)) {
    z_col <- paste0(col, "_chaos_z")
    style[, (z_col) := zscore(get(col))]
    chaos_cols <- c(chaos_cols, z_col)
  }
}

self_risk_components <- c(
  "own_blunder_rate",
  "own_mistake_rate",
  "p90_cp_loss",
  "sd_cp_loss"
)
self_risk_cols <- character()
for (col in self_risk_components) {
  if (col %in% names(style)) {
    z_col <- paste0(col, "_self_risk_z")
    style[, (z_col) := zscore(get(col))]
    self_risk_cols <- c(self_risk_cols, z_col)
  }
}

style[, `:=`(
  clean_style_index = row_mean_na(.SD, clean_cols),
  chaos_creator_index = row_mean_na(.SD, chaos_cols),
  self_risk_index = row_mean_na(.SD, self_risk_cols)
)]
style[, practical_chaos_index := chaos_creator_index - self_risk_index]
for (col in c("clean_style_index", "chaos_creator_index", "self_risk_index", "practical_chaos_index")) {
  style[, (paste0(col, "_z")) := zscore(get(col))]
}

style[, `:=`(
  clean_quartile = ntile(clean_style_index_z, 4L),
  chaos_quartile = ntile(chaos_creator_index_z, 4L),
  practical_chaos_quartile = ntile(practical_chaos_index_z, 4L)
)]
style[, style_archetype := fcase(
  clean_quartile == 4L & chaos_quartile == 4L, "sharp_clean",
  chaos_quartile == 4L & clean_quartile < 4L, "chaos_creator",
  clean_quartile == 4L & chaos_quartile < 4L, "clean_technician",
  clean_quartile == 1L & chaos_quartile == 1L, "quiet_leaky",
  default = "middle"
)]

style_scores <- style[, .(
  player,
  player_key,
  prechange_games,
  clean_style_index,
  chaos_creator_index,
  self_risk_index,
  practical_chaos_index,
  clean_style_index_z,
  chaos_creator_index_z,
  self_risk_index_z,
  practical_chaos_index_z,
  clean_quartile,
  chaos_quartile,
  practical_chaos_quartile,
  style_archetype,
  capture_rate,
  check_rate,
  eval_swing_rate,
  opponent_next_blunder_rate,
  opponent_next_mistake_rate,
  no_blunder_game_rate,
  own_blunder_rate,
  own_mistake_rate,
  mean_cp_loss,
  sd_cp_loss,
  p90_cp_loss,
  decisive_game_rate,
  draw_rate
)]
fwrite(style_scores, file.path(OUT_DIR, "style_scores.csv"))

style_score_cols <- c(
  "player_key", "prechange_games", "clean_style_index_z",
  "chaos_creator_index_z", "self_risk_index_z", "practical_chaos_index_z",
  "clean_quartile", "chaos_quartile", "practical_chaos_quartile",
  "style_archetype"
)
style_small <- style_scores[, ..style_score_cols]

style_descriptives <- style_scores[, .(
  players = .N,
  mean_prechange_games = mean(prechange_games, na.rm = TRUE),
  mean_clean_index = mean(clean_style_index_z, na.rm = TRUE),
  mean_chaos_index = mean(chaos_creator_index_z, na.rm = TRUE),
  mean_self_risk_index = mean(self_risk_index_z, na.rm = TRUE),
  mean_practical_chaos_index = mean(practical_chaos_index_z, na.rm = TRUE),
  mean_capture_rate = mean(capture_rate, na.rm = TRUE),
  mean_check_rate = mean(check_rate, na.rm = TRUE),
  mean_eval_swing_rate = mean(eval_swing_rate, na.rm = TRUE),
  mean_opponent_next_blunder_rate = mean(opponent_next_blunder_rate, na.rm = TRUE),
  mean_own_blunder_rate = mean(own_blunder_rate, na.rm = TRUE),
  mean_no_blunder_game_rate = mean(no_blunder_game_rate, na.rm = TRUE)
), by = style_archetype][order(style_archetype)]
fwrite(style_descriptives, file.path(OUT_DIR, "style_archetype_descriptives.csv"))

message("Reading regression panel: ", REGRESSION_PATH)
needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "birthday", "classic_rating", "blitz_rating", "rapid_rating",
  "final_score_pregame", "rank", "in_prizes", "bubble", "eliminated", "leader"
)
df <- fread(REGRESSION_PATH, select = needed_cols, showProgress = TRUE)
df[, `:=`(
  player_key = tolower(player_name),
  opponent_key = tolower(opponent_name),
  event_id = as.character(date),
  event_date = as.IDate(substr(as.character(date), 1, 10)),
  round = as.integer(round),
  player_rating = as.numeric(player_rating),
  opponent_rating = as.numeric(opponent_rating),
  player_result = as.numeric(player_result),
  player_accuracy = as.numeric(player_accuracy),
  is_white = as.integer(is_white),
  birthday = suppressWarnings(as.numeric(birthday)),
  classic_rating = as.numeric(classic_rating),
  blitz_rating = as.numeric(blitz_rating),
  rapid_rating = as.numeric(rapid_rating)
)]
df[, format_5_0 := as.integer(event_date >= RULE_CHANGE_DATE)]
df[, event_month_rel := (
  as.integer(format(event_date, "%Y")) * 12L + as.integer(format(event_date, "%m"))
) - (
  as.integer(format(RULE_CHANGE_DATE, "%Y")) * 12L + as.integer(format(RULE_CHANGE_DATE, "%m"))
)]
df[, tournament_year := as.integer(format(event_date, "%Y"))]
df[, age := tournament_year - birthday]

main <- merge(df, style_small, by = "player_key", all.x = FALSE, sort = FALSE)
opponent_style <- copy(style_small)
setnames(
  opponent_style,
  old = setdiff(names(opponent_style), "player_key"),
  new = paste0("opponent_", setdiff(names(opponent_style), "player_key"))
)
setnames(opponent_style, "player_key", "opponent_key")
main <- merge(main, opponent_style, by = "opponent_key", all.x = TRUE, sort = FALSE)

main <- main[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_name) &
    !is.na(opponent_name) &
    !is.na(player_result) &
    !is.na(player_rating) & player_rating > 0 &
    !is.na(opponent_rating) & opponent_rating > 0 &
    !is.na(format_5_0)
]
main[, `:=`(
  player_rating100 = (player_rating - 2500) / 100,
  opponent_rating100 = (opponent_rating - 2500) / 100,
  rating_diff100 = (player_rating - opponent_rating) / 100,
  expected_score = 1 / (1 + 10^((opponent_rating - player_rating) / 400)),
  win = as.integer(player_result == 1),
  loss = as.integer(player_result == 0),
  draw = as.integer(player_result == 0.5),
  underdog = as.integer(player_rating < opponent_rating),
  heavy_underdog = as.integer(opponent_rating - player_rating >= 200),
  favorite = as.integer(player_rating > opponent_rating),
  bubble_zone = as.integer(bubble == 1),
  prize_zone = as.integer(in_prizes == 1),
  eliminated_zone = as.integer(eliminated == 1),
  leader_zone = as.integer(leader == 1),
  valid_accuracy = !is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100,
  game_id = paste(event_id, round, pmin(player_name, opponent_name), pmax(player_name, opponent_name), sep = "||")
)]
main[, result_over_expected := player_result - expected_score]
main[, age10_c := (age - mean(age, na.rm = TRUE)) / 10]
main[, online_classic_gap100 := (player_rating - classic_rating) / 100]
main[, online_blitz_gap100 := (player_rating - blitz_rating) / 100]

period_style_descriptives <- main[, .(
  player_games = .N,
  players = uniqueN(player_name),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_result_over_expected = mean(result_over_expected, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[valid_accuracy == TRUE], na.rm = TRUE),
  win_rate = mean(win, na.rm = TRUE),
  loss_rate = mean(loss, na.rm = TRUE),
  draw_rate = mean(draw, na.rm = TRUE),
  underdog_share = mean(underdog, na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE),
  mean_rating_diff100 = mean(rating_diff100, na.rm = TRUE)
), by = .(format_5_0, chaos_quartile, clean_quartile, style_archetype)]
setorder(period_style_descriptives, style_archetype, chaos_quartile, clean_quartile, format_5_0)
fwrite(period_style_descriptives, file.path(OUT_DIR, "period_style_descriptives.csv"))

quartile_period <- main[, .(
  player_games = .N,
  players = uniqueN(player_name),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_result_over_expected = mean(result_over_expected, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[valid_accuracy == TRUE], na.rm = TRUE),
  win_rate = mean(win, na.rm = TRUE),
  underdog_win_rate = mean(win[underdog == 1], na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE)
), by = .(format_5_0, chaos_quartile)]
setorder(quartile_period, chaos_quartile, format_5_0)
fwrite(quartile_period, file.path(OUT_DIR, "chaos_quartile_period_descriptives.csv"))

quartile_wide <- dcast(
  quartile_period,
  chaos_quartile ~ format_5_0,
  value.var = c("mean_result", "mean_result_over_expected", "mean_accuracy", "win_rate", "underdog_win_rate")
)
for (stub in c("mean_result", "mean_result_over_expected", "mean_accuracy", "win_rate", "underdog_win_rate")) {
  pre <- paste0(stub, "_0")
  post <- paste0(stub, "_1")
  diff <- paste0(stub, "_post_minus_pre")
  if (all(c(pre, post) %in% names(quartile_wide))) {
    quartile_wide[, (diff) := get(post) - get(pre)]
  }
}
fwrite(quartile_wide, file.path(OUT_DIR, "chaos_quartile_period_contrasts.csv"))

message("Estimating risk-return style regressions")
base_controls <- "+ player_rating100 + opponent_rating100 + is_white + factor(round)"
base_fe <- "| player_name + event_id"

model_data <- main[
  !is.na(clean_style_index_z) &
    !is.na(chaos_creator_index_z) &
    !is.na(self_risk_index_z) &
    !is.na(practical_chaos_index_z)
]

models <- list(
  safe_feols(
    "result_clean_vs_chaos",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "result_practical_chaos",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * practical_chaos_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "result_over_expected_clean_vs_chaos",
    as.formula(paste0(
      "result_over_expected ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "result_over_expected_practical_chaos",
    as.formula(paste0(
      "result_over_expected ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * practical_chaos_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "accuracy_clean_vs_chaos",
    as.formula(paste0(
      "player_accuracy ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data[valid_accuracy == TRUE]
  ),
  safe_feols(
    "win_probability_clean_vs_chaos",
    as.formula(paste0(
      "win ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "loss_probability_clean_vs_chaos",
    as.formula(paste0(
      "loss ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "underdog_result_triple_interaction",
    as.formula(paste0(
      "player_result ~ format_5_0 * underdog * chaos_creator_index_z + ",
      "format_5_0 * underdog * clean_style_index_z + ",
      "format_5_0 * underdog * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "heavy_underdog_result_triple_interaction",
    as.formula(paste0(
      "player_result ~ format_5_0 * heavy_underdog * chaos_creator_index_z + ",
      "format_5_0 * heavy_underdog * clean_style_index_z + ",
      "format_5_0 * heavy_underdog * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "underdog_practical_chaos_triple_interaction",
    as.formula(paste0(
      "player_result ~ format_5_0 * underdog * practical_chaos_index_z + ",
      "format_5_0 * underdog * clean_style_index_z ",
      base_controls, " ", base_fe
    )),
    model_data
  ),
  safe_feols(
    "underdogs_only_result",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data[underdog == 1]
  ),
  safe_feols(
    "underdogs_only_practical_chaos",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * practical_chaos_index_z ",
      base_controls, " ", base_fe
    )),
    model_data[underdog == 1]
  ),
  safe_feols(
    "favorites_only_result",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z ",
      base_controls, " ", base_fe
    )),
    model_data[favorite == 1]
  ),
  safe_feols(
    "result_with_online_capital_controls",
    as.formula(paste0(
      "player_result ~ format_5_0 * clean_style_index_z + ",
      "format_5_0 * chaos_creator_index_z + ",
      "format_5_0 * self_risk_index_z + ",
      "format_5_0 * online_classic_gap100 + format_5_0 * online_blitz_gap100 ",
      base_controls, " ", base_fe
    )),
    model_data[!is.na(online_classic_gap100) & !is.na(online_blitz_gap100)]
  )
)

regression_coefs <- rbindlist(lapply(models, tidy_model, family = "player_game_regression"), fill = TRUE)
regression_coefs[, q.value := p.adjust(p.value, method = "BH")]

move_coefs <- data.table()
move_sample_summary <- data.table()
if (file.exists(PLAYER_GAME_MOVE_PATH)) {
  message("Reading optional player-game move outcomes: ", PLAYER_GAME_MOVE_PATH)
  pg_cols <- c(
    "game_id", "mover_color", "player_name", "opponent_name", "tournament_id",
    "round", "player_rating", "opponent_rating", "player_result",
    "player_accuracy", "event_date", "format_5_0", "rating_diff100",
    "mean_cp_loss_cap", "blunder_rate", "mistake_rate", "inaccuracy_rate",
    "no_blunder_game", "first_blunder_move", "reached_winning_position",
    "reached_losing_position", "converted_winning_position", "escaped_losing_position"
  )
  pg <- fread(PLAYER_GAME_MOVE_PATH, select = pg_cols, showProgress = TRUE)
  pg[, `:=`(
    player_key = tolower(player_name),
    event_id = as.character(tournament_id),
    event_date = as.IDate(event_date),
    format_5_0 = as.integer(format_5_0),
    round = as.integer(round),
    player_rating = as.numeric(player_rating),
    opponent_rating = as.numeric(opponent_rating),
    player_result = as.numeric(player_result),
    player_accuracy = as.numeric(player_accuracy),
    rating_diff100 = as.numeric(rating_diff100),
    mean_cp_loss_cap = as.numeric(mean_cp_loss_cap),
    blunder_rate = as.numeric(blunder_rate),
    mistake_rate = as.numeric(mistake_rate),
    inaccuracy_rate = as.numeric(inaccuracy_rate),
    no_blunder_game = as.integer(no_blunder_game),
    first_blunder_move = as.numeric(first_blunder_move),
    reached_winning_position = as.integer(reached_winning_position),
    reached_losing_position = as.integer(reached_losing_position),
    converted_winning_position = as.integer(converted_winning_position),
    escaped_losing_position = as.integer(escaped_losing_position)
  )]
  pg <- merge(pg, style_small, by = "player_key", all.x = FALSE, sort = FALSE)
  pg <- pg[
    !is.na(clean_style_index_z) &
      !is.na(chaos_creator_index_z) &
      !is.na(self_risk_index_z) &
      !is.na(player_result) &
      !is.na(player_rating) & player_rating > 0 &
      !is.na(opponent_rating) & opponent_rating > 0 &
      round > 1
  ]
  pg[, `:=`(
    player_rating100 = (player_rating - 2500) / 100,
    opponent_rating100 = (opponent_rating - 2500) / 100,
    is_white = as.integer(mover_color == "white"),
    underdog = as.integer(player_rating < opponent_rating)
  )]
  move_sample_summary <- data.table(
    metric = c("move_player_games", "move_players", "move_events", "move_pre_rows", "move_post_rows"),
    value = c(nrow(pg), uniqueN(pg$player_name), uniqueN(pg$event_id), nrow(pg[format_5_0 == 0]), nrow(pg[format_5_0 == 1]))
  )

  move_controls <- "+ player_rating100 + opponent_rating100 + is_white + factor(round)"
  move_fe <- "| player_name + event_id"
  move_models <- list(
    safe_feols(
      "stockfish_blunder_rate_clean_vs_chaos",
      as.formula(paste0(
        "blunder_rate ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg
    ),
    safe_feols(
      "stockfish_no_blunder_game_clean_vs_chaos",
      as.formula(paste0(
        "no_blunder_game ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg
    ),
    safe_feols(
      "stockfish_mean_cpl_clean_vs_chaos",
      as.formula(paste0(
        "mean_cp_loss_cap ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg
    ),
    safe_feols(
      "stockfish_reached_winning_clean_vs_chaos",
      as.formula(paste0(
        "reached_winning_position ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg
    ),
    safe_feols(
      "stockfish_conversion_clean_vs_chaos",
      as.formula(paste0(
        "converted_winning_position ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg[reached_winning_position == 1]
    ),
    safe_feols(
      "stockfish_escape_clean_vs_chaos",
      as.formula(paste0(
        "escaped_losing_position ~ format_5_0 * clean_style_index_z + ",
        "format_5_0 * chaos_creator_index_z + ",
        "format_5_0 * self_risk_index_z ",
        move_controls, " ", move_fe
      )),
      pg[reached_losing_position == 1]
    )
  )
  move_coefs <- rbindlist(lapply(move_models, tidy_model, family = "stockfish_player_game"), fill = TRUE)
  move_coefs[, q.value := p.adjust(p.value, method = "BH")]
}

all_coefs <- rbindlist(list(regression_coefs, move_coefs), fill = TRUE)
fwrite(regression_coefs, file.path(OUT_DIR, "regression_model_coefficients.csv"))
if (nrow(move_coefs) > 0L) fwrite(move_coefs, file.path(OUT_DIR, "stockfish_player_game_coefficients.csv"))
fwrite(all_coefs, file.path(OUT_DIR, "all_model_coefficients.csv"))

headline <- all_coefs[
  !is.na(term) &
    grepl("format_5_0", term) &
    (
      grepl("chaos", term) |
        grepl("clean_style", term) |
        grepl("self_risk", term) |
        grepl("practical_chaos", term)
    )
]
headline[, abs_t := abs(estimate / std.error)]
setorder(headline, -abs_t)
headline <- headline[1:min(.N, 40L)]
fwrite(headline, file.path(OUT_DIR, "headline_risk_return_coefficients.csv"))

sample_summary <- data.table(
  metric = c(
    "style_players_min_games",
    "regression_rows_with_style",
    "regression_players_with_style",
    "regression_events",
    "regression_pre_rows",
    "regression_post_rows",
    "opponent_style_observed_share"
  ),
  value = c(
    nrow(style_scores),
    nrow(model_data),
    uniqueN(model_data$player_name),
    uniqueN(model_data$event_id),
    nrow(model_data[format_5_0 == 0]),
    nrow(model_data[format_5_0 == 1]),
    round(mean(!is.na(main$opponent_chaos_creator_index_z)), 4)
  )
)
sample_summary <- rbindlist(list(sample_summary, move_sample_summary), fill = TRUE)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

report_sample <- copy(sample_summary)
setnames(report_sample, c("Metric", "Value"))

report_styles <- copy(style_descriptives)
num_cols <- setdiff(names(report_styles), "style_archetype")
for (col in num_cols) report_styles[, (col) := fmt(get(col), 4)]

report_quartiles <- copy(quartile_period)
report_quartiles[, period := fifelse(format_5_0 == 1, "5+0", "3+1")]
for (col in c(
  "mean_result", "mean_result_over_expected", "mean_accuracy",
  "win_rate", "underdog_win_rate", "mean_rating"
)) {
  report_quartiles[, (col) := fmt(get(col), ifelse(col == "mean_rating", 1, 4))]
}

report_headline <- copy(headline)
for (col in c("estimate", "std.error", "conf.low", "conf.high", "r2_within")) {
  report_headline[, (col) := fmt(get(col), 4)]
}
report_headline[, `:=`(
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value)
)]

component_inventory <- data.table(
  index = c("clean_style_index", "chaos_creator_index", "self_risk_index", "practical_chaos_index"),
  components = c(
    paste(clean_cols, collapse = ", "),
    paste(chaos_cols, collapse = ", "),
    paste(self_risk_cols, collapse = ", "),
    "chaos_creator_index - self_risk_index"
  ),
  interpretation = c(
    "Higher means historically lower own error rates and more no-blunder games.",
    "Higher means historically more forcing/volatile games and more opponent errors after moves.",
    "Higher means historically more own large errors or volatile CPL.",
    "Higher means forcing chaos net of self-destructive risk."
  )
)

md <- c(
  "# Risk-Return Style: Clean Players vs Chaos Creators",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Inputs",
  "",
  paste0("- Regression CSV: `", REGRESSION_PATH, "`"),
  paste0("- Pre-change style CSV: `", STYLE_PATH, "`"),
  paste0("- Optional Stockfish player-game CSV: `", PLAYER_GAME_MOVE_PATH, "`"),
  paste0("- Minimum pre-change style games: `", MIN_STYLE_GAMES, "`"),
  "",
  "## Design",
  "",
  "This analysis treats pre-change move-style features as predetermined player traits, then asks whether those traits became more or less valuable after the September 1, 2025 switch to 5+0. The main specifications use player and event fixed effects, round/color/rating controls, and two-way clustered standard errors by player and event.",
  "",
  "## Sample",
  "",
  write_md_table(report_sample, c("Metric", "Value")),
  "",
  "## Style Index Construction",
  "",
  write_md_table(component_inventory, c("index", "components", "interpretation")),
  "",
  "All component variables are winsorized at the 1st and 99th percentiles, standardized, and averaged. `clean_style_index`, `chaos_creator_index`, `self_risk_index`, and `practical_chaos_index` are then standardized again across players.",
  "",
  "## Style Archetype Descriptives",
  "",
  write_md_table(
    report_styles,
    c(
      "style_archetype", "players", "mean_prechange_games", "mean_clean_index",
      "mean_chaos_index", "mean_self_risk_index", "mean_practical_chaos_index",
      "mean_capture_rate", "mean_check_rate", "mean_eval_swing_rate",
      "mean_opponent_next_blunder_rate", "mean_own_blunder_rate",
      "mean_no_blunder_game_rate"
    )
  ),
  "",
  "## Raw Outcomes by Chaos Quartile",
  "",
  write_md_table(
    report_quartiles,
    c(
      "period", "chaos_quartile", "player_games", "players",
      "mean_result", "mean_result_over_expected", "mean_accuracy",
      "win_rate", "underdog_win_rate", "mean_rating"
    )
  ),
  "",
  "## Strongest Risk-Return Format Interactions",
  "",
  "These rows are ranked by absolute t-statistic. The key terms are `format_5_0:chaos_creator_index_z`, `format_5_0:clean_style_index_z`, `format_5_0:self_risk_index_z`, and the underdog triple interactions.",
  "",
  write_md_table(
    report_headline,
    c("family", "model", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within")
  ),
  "",
  "## Output Files",
  "",
  "- `style_scores.csv`",
  "- `style_archetype_descriptives.csv`",
  "- `period_style_descriptives.csv`",
  "- `chaos_quartile_period_descriptives.csv`",
  "- `chaos_quartile_period_contrasts.csv`",
  "- `regression_model_coefficients.csv`",
  "- `stockfish_player_game_coefficients.csv` if the optional Stockfish player-game file exists",
  "- `all_model_coefficients.csv`",
  "- `headline_risk_return_coefficients.csv`",
  "- `sample_summary.csv`",
  "- `risk_return_style_report.md`",
  "- `session_info.txt`",
  "",
  "## Interpretation Notes",
  "",
  "- The style indexes are pre-change traits, so the post interactions are less exposed to post-treatment-bias concerns than models using post-change move quality directly.",
  "- `chaos_creator_index` is intended to capture forcing and volatile play that induces opponent errors; `self_risk_index` separates that from simply making more own large errors.",
  "- The models are conditional on participation, so they estimate whether a style paid off more among observed Titled Tuesday entrants, not whether a style made players more likely to participate.",
  "- Stockfish outcomes use the currently materialized move-mechanism file. Rerun `code/03_analysis/move_clock_level/analyze_stockfish_move_mechanisms.R` first if you want the optional move-outcome models refreshed."
)

writeLines(md, file.path(OUT_DIR, "risk_return_style_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
