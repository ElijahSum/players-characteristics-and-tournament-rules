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

FEATURE_DIR <- arg_value("--feature-dir", "analysis_outputs/missing_move_mechanisms_features")
PLAYER_GAME_PATH <- arg_value(
  "--player-game-outcomes",
  "analysis_outputs/stockfish_move_mechanisms_full_2022_2026/player_game_move_outcomes.csv"
)
OUT_DIR <- arg_value("--output-dir", "analysis_outputs/missing_move_mechanisms_full_2022_2026")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))
}

clean_term <- function(x) gsub("`", "", x, fixed = TRUE)

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

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

message("Reading player-game outcomes: ", PLAYER_GAME_PATH)
pg <- fread(
  PLAYER_GAME_PATH,
  select = c(
    "game_id", "mover_color", "player_name", "opponent_name", "tournament_id",
    "round", "player_result", "player_rating", "opponent_rating",
    "format_5_0", "rating_diff100", "age10_c", "female", "gdp_log_c",
    "mean_cp_loss_cap", "blunder_rate", "first_blunder_move", "no_blunder_game",
    "cp_loss_slope"
  ),
  showProgress = TRUE
)
pg[, game_id := as.character(game_id)]
pg <- unique(pg, by = c("game_id", "mover_color"))

message("Reading PGN mechanism features")
pgn_features <- fread(file.path(FEATURE_DIR, "player_game_pgn_mechanism_features.csv"), showProgress = TRUE)
pgn_features[, game_id := as.character(game_id)]
pgn_features <- unique(pgn_features, by = c("game_id", "mover_color"))

opening_features <- fread(file.path(FEATURE_DIR, "opening_game_features.csv"), showProgress = TRUE)
opening_features[, game_id := as.character(game_id)]
opening_features <- unique(opening_features, by = "game_id")

hazard <- fread(file.path(FEATURE_DIR, "phase_blunder_hazard.csv"), showProgress = TRUE)
hazard[, game_id := as.character(game_id)]
hazard <- unique(hazard, by = c("game_id", "mover_color", "phase_group"))

analysis <- merge(pg, pgn_features, by = c("game_id", "mover_color"), all.x = FALSE, all.y = FALSE, sort = FALSE)
analysis <- merge(
  analysis,
  opening_features[, .(game_id, eco, eco_family, opening_name)],
  by = "game_id",
  all.x = TRUE,
  sort = FALSE
)

numeric_cols <- c(
  "book_exit_move", "book_eval_player_cp", "post_book_cp_loss",
  "mean_legal_moves", "mean_pieces_remaining", "mean_material_remaining",
  "mean_abs_material_balance", "mean_eval_volatility", "high_legal_share",
  "high_legal_cp_loss", "low_legal_cp_loss", "high_minus_low_legal_cp_loss",
  "complex_share", "complex_cp_loss", "quiet_cp_loss",
  "complex_minus_quiet_cp_loss", "complex_blunder_rate",
  "trades_by_move20", "captures_by_move20", "pieces_remaining_move20",
  "pieces_remaining_move30", "material_remaining_move30", "queens_off_by_move20",
  "queens_off_by_move30", "queen_trade_move"
)
for (col in numeric_cols) {
  if (col %in% names(analysis)) analysis[, (col) := as.numeric(get(col))]
}

analysis[, `:=`(
  book_exit_move_c = book_exit_move - mean(book_exit_move, na.rm = TRUE),
  book_eval_player_cp_cap = pmax(-1000, pmin(1000, book_eval_player_cp)),
  mean_legal_moves_c = mean_legal_moves - mean(mean_legal_moves, na.rm = TRUE),
  high_legal_share_c = high_legal_share - mean(high_legal_share, na.rm = TRUE),
  complexity_index = zscore(mean_legal_moves) +
    zscore(mean_pieces_remaining) +
    zscore(mean_eval_volatility) -
    zscore(mean_abs_material_balance),
  simplified_move30 = as.integer(queens_off_by_move30 == 1 | pieces_remaining_move30 <= 22),
  low_material_move30 = as.integer(pieces_remaining_move30 <= 22),
  eco_family = fifelse(is.na(eco_family) | eco_family == "", "missing", eco_family)
)]
analysis[, complexity_index_c := complexity_index - mean(complexity_index, na.rm = TRUE)]

sample_summary <- data.table(
  metric = c(
    "player_games_in_stockfish_outcomes", "player_games_with_pgn_features",
    "unique_games_with_pgn_features", "players", "tournaments",
    "mean_book_exit_move", "nonzero_book_exit_share", "mean_legal_moves",
    "mean_high_legal_share", "mean_complex_share", "queen_off_by_move20_rate",
    "queen_off_by_move30_rate", "simplified_move30_rate"
  ),
  value = c(
    nrow(pg),
    nrow(analysis),
    uniqueN(analysis$game_id),
    uniqueN(analysis$player_name),
    uniqueN(analysis$tournament_id),
    round(mean(analysis$book_exit_move, na.rm = TRUE), 4),
    round(mean(analysis$book_exit_move > 0, na.rm = TRUE), 4),
    round(mean(analysis$mean_legal_moves, na.rm = TRUE), 4),
    round(mean(analysis$high_legal_share, na.rm = TRUE), 4),
    round(mean(analysis$complex_share, na.rm = TRUE), 4),
    round(mean(analysis$queens_off_by_move20, na.rm = TRUE), 4),
    round(mean(analysis$queens_off_by_move30, na.rm = TRUE), 4),
    round(mean(analysis$simplified_move30, na.rm = TRUE), 4)
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

opening_desc <- analysis[, .(
  player_games = .N,
  mean_book_exit_move = mean(book_exit_move, na.rm = TRUE),
  nonzero_book_exit_share = mean(book_exit_move > 0, na.rm = TRUE),
  mean_book_eval_player_cp = mean(book_eval_player_cp_cap, na.rm = TRUE),
  mean_post_book_cp_loss = mean(post_book_cp_loss, na.rm = TRUE)
), by = format_5_0][order(format_5_0)]
fwrite(opening_desc, file.path(OUT_DIR, "opening_descriptives.csv"))

complexity_desc <- analysis[, .(
  player_games = .N,
  mean_legal_moves = mean(mean_legal_moves, na.rm = TRUE),
  mean_high_legal_share = mean(high_legal_share, na.rm = TRUE),
  mean_complex_share = mean(complex_share, na.rm = TRUE),
  mean_high_minus_low_cp = mean(high_minus_low_legal_cp_loss, na.rm = TRUE),
  mean_complex_minus_quiet_cp = mean(complex_minus_quiet_cp_loss, na.rm = TRUE),
  mean_complex_blunder_rate = mean(complex_blunder_rate, na.rm = TRUE)
), by = format_5_0][order(format_5_0)]
fwrite(complexity_desc, file.path(OUT_DIR, "complexity_descriptives.csv"))

simplification_desc <- analysis[, .(
  player_games = .N,
  trades_by_move20 = mean(trades_by_move20, na.rm = TRUE),
  captures_by_move20 = mean(captures_by_move20, na.rm = TRUE),
  queens_off_by_move20 = mean(queens_off_by_move20, na.rm = TRUE),
  queens_off_by_move30 = mean(queens_off_by_move30, na.rm = TRUE),
  pieces_remaining_move30 = mean(pieces_remaining_move30, na.rm = TRUE),
  material_remaining_move30 = mean(material_remaining_move30, na.rm = TRUE),
  simplified_move30 = mean(simplified_move30, na.rm = TRUE)
), by = format_5_0][order(format_5_0)]
fwrite(simplification_desc, file.path(OUT_DIR, "simplification_descriptives.csv"))

message("Preparing phase-level blunder hazard data")
hazard <- merge(
  hazard,
  pg[, .(
    game_id, mover_color, player_name, tournament_id, format_5_0,
    rating_diff100, age10_c, female, gdp_log_c
  )],
  by = c("game_id", "mover_color"),
  all.x = FALSE,
  all.y = FALSE,
  sort = FALSE
)
hazard[, phase_group := factor(
  phase_group,
  levels = c("opening_1_10", "early_middlegame_11_20", "late_middlegame_21_35", "endgame_36_plus")
)]

hazard_desc <- hazard[, .(
  at_risk_player_game_phases = .N,
  event_rate = mean(event_blunder, na.rm = TRUE)
), by = .(format_5_0, phase_group)][order(format_5_0, phase_group)]
fwrite(hazard_desc, file.path(OUT_DIR, "hazard_descriptives.csv"))

message("Estimating opening models")
opening_coefs <- rbindlist(list(
  estimate_and_tidy(
    "book_exit_move_metadata",
    book_exit_move ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    analysis[!is.na(book_exit_move)],
    "opening"
  ),
  estimate_and_tidy(
    "book_eval_at_exit_metadata",
    book_eval_player_cp_cap ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 + book_exit_move |
      player_name + tournament_id,
    analysis[!is.na(book_eval_player_cp_cap) & book_exit_move > 0],
    "opening"
  ),
  estimate_and_tidy(
    "post_book_accuracy_metadata",
    post_book_cp_loss ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 + book_exit_move |
      player_name + tournament_id,
    analysis[!is.na(post_book_cp_loss) & post_book_moves >= 3],
    "opening"
  )
), fill = TRUE)
opening_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(opening_coefs, file.path(OUT_DIR, "opening_model_coefficients.csv"))

message("Estimating complexity models")
complexity_coefs <- rbindlist(list(
  estimate_and_tidy(
    "mean_legal_moves_selection",
    mean_legal_moves ~ format_5_0 * age10_c + format_5_0 * rating_diff100 +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(mean_legal_moves)],
    "complexity"
  ),
  estimate_and_tidy(
    "high_legal_cp_loss_penalty",
    high_minus_low_legal_cp_loss ~ format_5_0 * age10_c +
      format_5_0 * rating_diff100 + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(high_minus_low_legal_cp_loss)],
    "complexity"
  ),
  estimate_and_tidy(
    "complex_minus_quiet_cp_loss",
    complex_minus_quiet_cp_loss ~ format_5_0 * age10_c +
      format_5_0 * rating_diff100 + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complex_minus_quiet_cp_loss)],
    "complexity"
  ),
  estimate_and_tidy(
    "complex_blunder_rate",
    complex_blunder_rate ~ format_5_0 * age10_c +
      format_5_0 * rating_diff100 + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complex_blunder_rate) & complex_share > 0.05],
    "complexity"
  ),
  estimate_and_tidy(
    "mean_cp_loss_complexity_moderator",
    mean_cp_loss_cap.x ~ format_5_0 * age10_c * complexity_index_c +
      rating_diff100 + female + gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complexity_index_c) & !is.na(mean_cp_loss_cap.x)],
    "complexity"
  )
), fill = TRUE)
complexity_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(complexity_coefs, file.path(OUT_DIR, "complexity_model_coefficients.csv"))

message("Estimating simplification models")
simplification_coefs <- rbindlist(list(
  estimate_and_tidy(
    "trades_by_move20",
    trades_by_move20 ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    analysis[!is.na(trades_by_move20)],
    "simplification"
  ),
  estimate_and_tidy(
    "queens_off_by_move20",
    queens_off_by_move20 ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    analysis[!is.na(queens_off_by_move20)],
    "simplification"
  ),
  estimate_and_tidy(
    "pieces_remaining_move30",
    pieces_remaining_move30 ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    analysis[!is.na(pieces_remaining_move30)],
    "simplification"
  ),
  estimate_and_tidy(
    "player_result_simplified_age",
    player_result ~ format_5_0 * age10_c * simplified_move30 +
      rating_diff100 + female + gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(simplified_move30)],
    "simplification"
  )
), fill = TRUE)
simplification_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(simplification_coefs, file.path(OUT_DIR, "simplification_model_coefficients.csv"))

message("Estimating phase-level first-blunder hazard models")
hazard_coefs <- rbindlist(list(
  estimate_and_tidy(
    "phase_blunder_hazard_metadata",
    event_blunder ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 + i(phase_group) |
      player_name + tournament_id,
    hazard,
    "hazard"
  ),
  estimate_and_tidy(
    "phase_blunder_hazard_age_by_phase",
    event_blunder ~ format_5_0 * age10_c * phase_group +
      rating_diff100 + female + gdp_log_c |
      player_name + tournament_id,
    hazard,
    "hazard"
  )
), fill = TRUE)
hazard_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(hazard_coefs, file.path(OUT_DIR, "hazard_model_coefficients.csv"))

all_coefs <- rbindlist(
  list(opening_coefs, complexity_coefs, simplification_coefs, hazard_coefs),
  fill = TRUE
)
fwrite(all_coefs, file.path(OUT_DIR, "all_missing_mechanism_coefficients.csv"))

headline <- all_coefs[
  !is.na(term) &
    grepl("format_5_0", term) &
    !grepl("^format_5_0$", term)
]
headline[, abs_t := abs(estimate / std.error)]
setorder(headline, -abs_t)
headline <- headline[1:min(.N, 40)]
fwrite(headline, file.path(OUT_DIR, "headline_missing_mechanism_interactions.csv"))

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

report_opening <- copy(opening_desc)
report_opening[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  mean_book_exit_move = fmt(mean_book_exit_move, 2),
  nonzero_book_exit_share = fmt(nonzero_book_exit_share, 4),
  mean_book_eval_player_cp = fmt(mean_book_eval_player_cp, 2),
  mean_post_book_cp_loss = fmt(mean_post_book_cp_loss, 2)
)]

report_complexity <- copy(complexity_desc)
report_complexity[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  mean_legal_moves = fmt(mean_legal_moves, 2),
  mean_high_legal_share = fmt(mean_high_legal_share, 4),
  mean_complex_share = fmt(mean_complex_share, 4),
  mean_high_minus_low_cp = fmt(mean_high_minus_low_cp, 2),
  mean_complex_minus_quiet_cp = fmt(mean_complex_minus_quiet_cp, 2),
  mean_complex_blunder_rate = fmt(mean_complex_blunder_rate, 4)
)]

report_simplification <- copy(simplification_desc)
report_simplification[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  trades_by_move20 = fmt(trades_by_move20, 3),
  captures_by_move20 = fmt(captures_by_move20, 3),
  queens_off_by_move20 = fmt(queens_off_by_move20, 4),
  queens_off_by_move30 = fmt(queens_off_by_move30, 4),
  pieces_remaining_move30 = fmt(pieces_remaining_move30, 2),
  material_remaining_move30 = fmt(material_remaining_move30, 2),
  simplified_move30 = fmt(simplified_move30, 4)
)]

report_hazard <- copy(hazard_desc)
report_hazard[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  event_rate = fmt(event_rate, 4)
)]

md <- c(
  "# Missing Move-Mechanism Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Inputs",
  "",
  paste0("- Feature directory: `", FEATURE_DIR, "`"),
  paste0("- Player-game outcomes: `", PLAYER_GAME_PATH, "`"),
  "",
  "## Sample",
  "",
  write_md_table(report_sample, c("Metric", "Value")),
  "",
  "## Opening Descriptives",
  "",
  write_md_table(
    report_opening,
    c("period", "player_games", "mean_book_exit_move", "nonzero_book_exit_share", "mean_book_eval_player_cp", "mean_post_book_cp_loss")
  ),
  "",
  "## Complexity Descriptives",
  "",
  write_md_table(
    report_complexity,
    c("period", "player_games", "mean_legal_moves", "mean_high_legal_share", "mean_complex_share", "mean_high_minus_low_cp", "mean_complex_minus_quiet_cp", "mean_complex_blunder_rate")
  ),
  "",
  "## Simplification Descriptives",
  "",
  write_md_table(
    report_simplification,
    c("period", "player_games", "trades_by_move20", "captures_by_move20", "queens_off_by_move20", "queens_off_by_move30", "pieces_remaining_move30", "material_remaining_move30", "simplified_move30")
  ),
  "",
  "## First-Blunder Hazard Descriptives",
  "",
  write_md_table(
    report_hazard,
    c("period", "phase_group", "at_risk_player_game_phases", "event_rate")
  ),
  "",
  "## Strongest Missing-Mechanism Format Interactions",
  "",
  write_md_table(
    report_headline,
    c("family", "model", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within")
  ),
  "",
  "## Notes",
  "",
  "- Opening depth is parsed from Chess.com `ECOUrl` and should be interpreted as Chess.com's recognized opening-line depth, not an external opening-book exit.",
  "- High-legal positions are moves with at least 35 legal moves before the move.",
  "- Complex positions require high legal mobility, at least 18 pieces, and pre-move engine evaluation within 300 centipawns.",
  "- Simplification is measured from reconstructed board states: early trades/captures, queen-off indicators, and pieces/material remaining by move 30.",
  "- The hazard dataset is phase-level: a player-game remains at risk until its first blunder, then exits.",
  "",
  "## Output Files",
  "",
  "- `sample_summary.csv`",
  "- `opening_descriptives.csv`",
  "- `complexity_descriptives.csv`",
  "- `simplification_descriptives.csv`",
  "- `hazard_descriptives.csv`",
  "- `opening_model_coefficients.csv`",
  "- `complexity_model_coefficients.csv`",
  "- `simplification_model_coefficients.csv`",
  "- `hazard_model_coefficients.csv`",
  "- `all_missing_mechanism_coefficients.csv`",
  "- `headline_missing_mechanism_interactions.csv`",
  "- `missing_move_mechanisms_report.md`",
  "- `session_info.txt`"
)

writeLines(md, file.path(OUT_DIR, "missing_move_mechanisms_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
