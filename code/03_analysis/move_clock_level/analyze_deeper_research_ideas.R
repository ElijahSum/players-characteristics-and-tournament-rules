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

PLAYER_GAME_PATH <- arg_value(
  "--player-game-outcomes",
  "analysis_outputs/stockfish_move_mechanisms_full_2022_2026/player_game_move_outcomes.csv"
)
PGN_FEATURE_PATH <- arg_value(
  "--pgn-features",
  "analysis_outputs/missing_move_mechanisms_features/player_game_pgn_mechanism_features.csv"
)
OPENING_FEATURE_PATH <- arg_value(
  "--opening-features",
  "analysis_outputs/missing_move_mechanisms_features/opening_game_features.csv"
)
DYNAMIC_FEATURE_PATH <- arg_value(
  "--dynamic-features",
  "analysis_outputs/advanced_player_game_dynamics/advanced_player_game_dynamics.csv"
)
OUT_DIR <- arg_value("--output-dir", "analysis_outputs/deeper_research_ideas_full_2022_2026")

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

safe_feols <- function(name, formula, data, family, cluster = ~ player_name + tournament_id) {
  message("  - ", name)
  tryCatch(
    {
      model <- feols(formula, data = data, cluster = cluster)
      out <- as.data.table(broom::tidy(model, conf.int = TRUE))
      out[, term := clean_term(term)]
      out[, `:=`(
        family = family,
        model = name,
        nobs = nobs(model),
        r2_within = as.numeric(fitstat(model, "wr2")[[1]]),
        error = NA_character_
      )]
      setcolorder(out, c(
        "family", "model", "term", "estimate", "std.error", "conf.low",
        "conf.high", "p.value", "nobs", "r2_within", "error"
      ))
      rm(model)
      invisible(gc(verbose = FALSE))
      out
    },
    error = function(e) data.table(
      family = family,
      model = name,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      nobs = NA_integer_,
      r2_within = NA_real_,
      error = conditionMessage(e)
    )
  )
}

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

message("Reading base player-game outcomes")
pg <- fread(
  PLAYER_GAME_PATH,
  select = c(
    "game_id", "mover_color", "player_name", "opponent_name", "tournament_id",
    "round", "event_date", "format_5_0", "player_result", "player_rating",
    "opponent_rating", "rating_diff100", "age10_c", "female", "gdp_log_c",
    "mean_cp_loss_cap", "blunder_rate", "mistake_rate", "inaccuracy_rate",
    "cp_loss_slope", "first_blunder_move", "no_blunder_game",
    "reached_winning_position", "converted_winning_position",
    "reached_losing_position", "escaped_losing_position"
  ),
  showProgress = TRUE
)
pg[, game_id := as.character(game_id)]
pg[, event_date := as.IDate(event_date)]
setnames(pg, "mean_cp_loss_cap", "pg_mean_cp_loss_cap")
pg <- unique(pg, by = c("game_id", "mover_color"))

message("Reading PGN and dynamic features")
pgn <- fread(PGN_FEATURE_PATH, showProgress = TRUE)
pgn[, game_id := as.character(game_id)]
pgn <- unique(pgn, by = c("game_id", "mover_color"))
opening <- fread(
  OPENING_FEATURE_PATH,
  select = c("game_id", "eco", "eco_family", "opening_name"),
  showProgress = TRUE
)
opening[, game_id := as.character(game_id)]
opening <- unique(opening, by = "game_id")
dyn <- fread(DYNAMIC_FEATURE_PATH, showProgress = TRUE)
dyn[, game_id := as.character(game_id)]
dyn <- unique(dyn, by = c("game_id", "mover_color"))

analysis <- merge(pg, pgn, by = c("game_id", "mover_color"), all.x = FALSE, all.y = FALSE, sort = FALSE)
analysis <- merge(analysis, opening, by = "game_id", all.x = TRUE, sort = FALSE)
analysis <- merge(analysis, dyn, by = c("game_id", "mover_color"), all.x = FALSE, all.y = FALSE, sort = FALSE)

numeric_cols <- setdiff(names(analysis), c(
  "game_id", "mover_color", "player_name", "opponent_name", "tournament_id",
  "event_date", "eco", "eco_family", "opening_name", "player_game_id"
))
for (col in numeric_cols) {
  if (is.character(analysis[[col]])) {
    suppressWarnings(analysis[, (col) := as.numeric(get(col))])
  }
}

analysis[, `:=`(
  eco = fifelse(is.na(eco) | eco == "", "missing", eco),
  eco_family = fifelse(is.na(eco_family) | eco_family == "", "missing", eco_family),
  mean_legal_moves_c = mean_legal_moves - mean(mean_legal_moves, na.rm = TRUE),
  high_legal_share_c = high_legal_share - mean(high_legal_share, na.rm = TRUE),
  complexity_index = zscore(mean_legal_moves) +
    zscore(mean_pieces_remaining) +
    zscore(mean_eval_volatility) -
    zscore(mean_abs_material_balance),
  simplified_move30 = as.integer(queens_off_by_move30 == 1 | pieces_remaining_move30 <= 22),
  log_book_exit_move = log1p(book_exit_move),
  log_prior_pair_meetings = NA_real_,
  prior_directed_score = NA_real_
)]
analysis[, complexity_index_c := complexity_index - mean(complexity_index, na.rm = TRUE)]

message("Building adaptation and repeated-match variables")
setorder(analysis, player_name, event_date, round, game_id)
analysis[format_5_0 == 1, post_game_number := seq_len(.N), by = player_name]
analysis[, log_post_game_number := log1p(post_game_number - 1)]
analysis[format_5_0 == 1, log_post_game_number_c := log_post_game_number - mean(log_post_game_number, na.rm = TRUE)]

analysis[, pair_id := fifelse(
  player_name <= opponent_name,
  paste(player_name, opponent_name, sep = "___"),
  paste(opponent_name, player_name, sep = "___")
)]
pair_games <- unique(analysis[, .(game_id, pair_id, event_date, round, tournament_id)])
setorder(pair_games, pair_id, event_date, round, game_id)
pair_games[, prior_pair_meetings := seq_len(.N) - 1L, by = pair_id]
analysis <- merge(
  analysis,
  pair_games[, .(game_id, pair_id, prior_pair_meetings)],
  by = c("game_id", "pair_id"),
  all.x = TRUE,
  sort = FALSE
)
analysis[, log_prior_pair_meetings := log1p(prior_pair_meetings)]

setorder(analysis, player_name, opponent_name, event_date, round, game_id)
analysis[, prior_directed_meetings := seq_len(.N) - 1L, by = .(player_name, opponent_name)]
analysis[, prior_score_sum := shift(cumsum(player_result), fill = 0), by = .(player_name, opponent_name)]
analysis[, prior_directed_score := fifelse(
  prior_directed_meetings > 0,
  prior_score_sum / prior_directed_meetings,
  NA_real_
)]
analysis[, prior_directed_score_filled := fifelse(is.na(prior_directed_score), 0.5, prior_directed_score)]

message("Building pre-change player traits")
eco_counts <- analysis[format_5_0 == 0, .N, by = .(player_name, eco)]
opening_traits <- eco_counts[, {
  p <- N / sum(N)
  .(
    pre_opening_hhi = sum(p^2),
    pre_opening_entropy = -sum(p * log(p)),
    pre_top_eco_share = max(p),
    pre_distinct_eco = .N
  )
}, by = player_name]

pre_traits <- analysis[format_5_0 == 0, .(
  pre_games = .N,
  pre_result = mean(player_result, na.rm = TRUE),
  pre_accuracy_skill = -mean(pg_mean_cp_loss_cap, na.rm = TRUE),
  pre_blunder_avoidance = -mean(blunder_rate, na.rm = TRUE),
  pre_decay_resilience = -mean(cp_loss_slope, na.rm = TRUE),
  pre_conversion_skill = mean(converted_winning_position[reached_winning_position == 1], na.rm = TRUE),
  pre_defensive_skill = mean(escaped_losing_position[reached_losing_position == 1], na.rm = TRUE),
  pre_complexity_tolerance = -mean(complex_minus_quiet_cp_loss, na.rm = TRUE),
  pre_complexity_choice = mean(complexity_index, na.rm = TRUE),
  pre_simplification_rate = mean(simplified_move30, na.rm = TRUE),
  pre_cascade_resilience = -mean(cascade_blunder_next5, na.rm = TRUE),
  pre_recovery_after_blunder = mean(recovered_to_equal_next5_after_blunder, na.rm = TRUE),
  pre_mean_book_exit = mean(book_exit_move, na.rm = TRUE),
  pre_post_book_accuracy = -mean(post_book_cp_loss, na.rm = TRUE)
), by = player_name]
pre_traits <- merge(pre_traits, opening_traits, by = "player_name", all.x = TRUE)
pre_traits <- pre_traits[pre_games >= 20]
trait_cols <- setdiff(names(pre_traits), c("player_name", "pre_games"))
for (col in trait_cols) {
  pre_traits[, paste0(col, "_z") := zscore(get(col))]
}
analysis <- merge(analysis, pre_traits, by = "player_name", all.x = TRUE, sort = FALSE)

sample_summary <- data.table(
  metric = c(
    "player_games", "unique_games", "players", "tournaments",
    "post_player_games", "players_with_pre_traits",
    "player_games_with_pre_traits", "repeated_pair_share",
    "first_blunder_player_games", "reached_plus2_player_games",
    "reached_minus2_player_games"
  ),
  value = c(
    nrow(analysis),
    uniqueN(analysis$game_id),
    uniqueN(analysis$player_name),
    uniqueN(analysis$tournament_id),
    sum(analysis$format_5_0 == 1),
    uniqueN(pre_traits$player_name),
    sum(!is.na(analysis$pre_accuracy_skill_z)),
    round(mean(analysis$prior_pair_meetings > 0, na.rm = TRUE), 4),
    sum(!is.na(analysis$first_blunder_move_dynamic)),
    sum(!is.na(analysis$first_plus2_ply)),
    sum(!is.na(analysis$first_minus2_ply))
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

adapt_desc <- analysis[format_5_0 == 1, .(
  player_games = .N,
  mean_post_game_number = mean(post_game_number, na.rm = TRUE),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_cp_loss = mean(pg_mean_cp_loss_cap, na.rm = TRUE),
  mean_blunder_rate = mean(blunder_rate, na.rm = TRUE)
), by = .(post_exposure_bin = cut(
  post_game_number,
  breaks = c(0, 10, 25, 50, 100, Inf),
  labels = c("1-10", "11-25", "26-50", "51-100", "101+")
))]
fwrite(adapt_desc, file.path(OUT_DIR, "adaptation_descriptives.csv"))

repeat_desc <- analysis[, .(
  player_games = .N,
  mean_result = mean(player_result, na.rm = TRUE),
  mean_cp_loss = mean(pg_mean_cp_loss_cap, na.rm = TRUE)
), by = .(
  format_5_0,
  prior_pair_bin = cut(
    prior_pair_meetings,
    breaks = c(-1, 0, 1, 3, 10, Inf),
    labels = c("0", "1", "2-3", "4-10", "11+")
  )
)][order(format_5_0, prior_pair_bin)]
fwrite(repeat_desc, file.path(OUT_DIR, "repeated_match_descriptives.csv"))

message("Estimating adaptation models")
post <- analysis[format_5_0 == 1 & !is.na(log_post_game_number_c)]
adaptation_coefs <- rbindlist(list(
  safe_feols(
    "post_learning_cp_loss",
    pg_mean_cp_loss_cap ~ log_post_game_number_c * age10_c +
      log_post_game_number_c * female + log_post_game_number_c * gdp_log_c +
      rating_diff100 + round |
      player_name + tournament_id,
    post,
    "adaptation"
  ),
  safe_feols(
    "post_learning_result",
    player_result ~ log_post_game_number_c * age10_c +
      log_post_game_number_c * female + log_post_game_number_c * gdp_log_c +
      rating_diff100 + round |
      player_name + tournament_id,
    post,
    "adaptation"
  ),
  safe_feols(
    "post_learning_blunder_rate",
    blunder_rate ~ log_post_game_number_c * age10_c +
      log_post_game_number_c * female + log_post_game_number_c * gdp_log_c +
      rating_diff100 + round |
      player_name + tournament_id,
    post,
    "adaptation"
  )
), fill = TRUE)
adaptation_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(adaptation_coefs, file.path(OUT_DIR, "adaptation_model_coefficients.csv"))

message("Estimating risk-taking models")
risk_coefs <- rbindlist(list(
  safe_feols(
    "risk_choice_complexity_index",
    complexity_index ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complexity_index)],
    "risk_taking"
  ),
  safe_feols(
    "risk_choice_eval_volatility",
    mean_eval_volatility ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(mean_eval_volatility)],
    "risk_taking"
  ),
  safe_feols(
    "risk_choice_complex_share",
    complex_share ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complex_share)],
    "risk_taking"
  ),
  safe_feols(
    "risk_payoff_complexity",
    player_result ~ format_5_0 * rating_diff100 * complexity_index_c +
      age10_c + female + gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(complexity_index_c)],
    "risk_taking"
  )
), fill = TRUE)
risk_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(risk_coefs, file.path(OUT_DIR, "risk_taking_model_coefficients.csv"))

message("Estimating error-cascade models")
cascade_sample <- analysis[!is.na(first_blunder_move_dynamic)]
cascade_coefs <- rbindlist(list(
  safe_feols(
    "cascade_blunder_next5",
    cascade_blunder_next5 ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    cascade_sample[!is.na(cascade_blunder_next5)],
    "error_cascade"
  ),
  safe_feols(
    "cascade_cp_loss_next5",
    mean_cp_loss_next5_after_blunder ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    cascade_sample[!is.na(mean_cp_loss_next5_after_blunder)],
    "error_cascade"
  ),
  safe_feols(
    "recovery_after_blunder_next5",
    recovered_to_equal_next5_after_blunder ~ format_5_0 * age10_c + format_5_0 * female +
      format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    cascade_sample[!is.na(recovered_to_equal_next5_after_blunder)],
    "error_cascade"
  ),
  safe_feols(
    "additional_eval_drop_after_blunder",
    additional_eval_drop_next5_after_blunder_cp ~ format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c + rating_diff100 |
      player_name + tournament_id,
    cascade_sample[!is.na(additional_eval_drop_next5_after_blunder_cp)],
    "error_cascade"
  )
), fill = TRUE)
cascade_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(cascade_coefs, file.path(OUT_DIR, "error_cascade_model_coefficients.csv"))

message("Estimating deeper conversion/recovery models")
conversion_coefs <- rbindlist(list(
  safe_feols(
    "advantage_loss_after_plus2",
    advantage_loss_after_plus2_cp ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(first_plus2_ply) & !is.na(advantage_loss_after_plus2_cp)],
    "conversion_deep"
  ),
  safe_feols(
    "dropped_below_equal_after_plus2",
    dropped_below_equal_after_plus2 ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(first_plus2_ply) & !is.na(dropped_below_equal_after_plus2)],
    "conversion_deep"
  ),
  safe_feols(
    "conversion_speed_after_plus2",
    conversion_speed_plies ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(first_plus2_ply) & !is.na(conversion_speed_plies)],
    "conversion_deep"
  ),
  safe_feols(
    "defensive_recovery_after_minus2",
    recovered_to_equal_after_minus2 ~ format_5_0 * rating_diff100 + format_5_0 * age10_c +
      format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(first_minus2_ply) & !is.na(recovered_to_equal_after_minus2)],
    "conversion_deep"
  ),
  safe_feols(
    "defensive_recovery_cp_after_minus2",
    defensive_recovery_after_minus2_cp ~ format_5_0 * rating_diff100 +
      format_5_0 * age10_c + format_5_0 * female + format_5_0 * gdp_log_c |
      player_name + tournament_id,
    analysis[!is.na(first_minus2_ply) & !is.na(defensive_recovery_after_minus2_cp)],
    "conversion_deep"
  )
), fill = TRUE)
conversion_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(conversion_coefs, file.path(OUT_DIR, "conversion_deep_model_coefficients.csv"))

message("Estimating opening human-capital models")
opening_trait_sample <- analysis[!is.na(pre_opening_hhi_z)]
opening_hc_coefs <- rbindlist(list(
  safe_feols(
    "opening_specialization_result",
    player_result ~ format_5_0:pre_opening_hhi_z + format_5_0:pre_top_eco_share_z +
      format_5_0:pre_mean_book_exit_z + rating_diff100 |
      player_name + tournament_id,
    opening_trait_sample,
    "opening_human_capital"
  ),
  safe_feols(
    "opening_specialization_cp_loss",
    pg_mean_cp_loss_cap ~ format_5_0:pre_opening_hhi_z + format_5_0:pre_top_eco_share_z +
      format_5_0:pre_mean_book_exit_z + rating_diff100 |
      player_name + tournament_id,
    opening_trait_sample,
    "opening_human_capital"
  ),
  safe_feols(
    "opening_specialization_blunder_rate",
    blunder_rate ~ format_5_0:pre_opening_hhi_z + format_5_0:pre_top_eco_share_z +
      format_5_0:pre_mean_book_exit_z + rating_diff100 |
      player_name + tournament_id,
    opening_trait_sample,
    "opening_human_capital"
  )
), fill = TRUE)
opening_hc_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(opening_hc_coefs, file.path(OUT_DIR, "opening_human_capital_model_coefficients.csv"))

message("Estimating repeated-match learning models")
repeated_coefs <- rbindlist(list(
  safe_feols(
    "prior_meetings_result",
    player_result ~ format_5_0 * log_prior_pair_meetings +
      format_5_0 * prior_directed_score_filled + rating_diff100 |
      player_name + opponent_name + tournament_id,
    analysis,
    "repeated_matching"
  ),
  safe_feols(
    "prior_meetings_cp_loss",
    pg_mean_cp_loss_cap ~ format_5_0 * log_prior_pair_meetings +
      format_5_0 * prior_directed_score_filled + rating_diff100 |
      player_name + opponent_name + tournament_id,
    analysis,
    "repeated_matching"
  ),
  safe_feols(
    "prior_meetings_blunder_rate",
    blunder_rate ~ format_5_0 * log_prior_pair_meetings +
      format_5_0 * prior_directed_score_filled + rating_diff100 |
      player_name + opponent_name + tournament_id,
    analysis,
    "repeated_matching"
  )
), fill = TRUE)
repeated_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(repeated_coefs, file.path(OUT_DIR, "repeated_matching_model_coefficients.csv"))

message("Estimating pre-change skill stress-test models")
stress_sample <- analysis[!is.na(pre_accuracy_skill_z)]
stress_coefs <- rbindlist(list(
  safe_feols(
    "pre_traits_result_combined",
    player_result ~ rating_diff100 +
      format_5_0:pre_accuracy_skill_z +
      format_5_0:pre_decay_resilience_z +
      format_5_0:pre_conversion_skill_z +
      format_5_0:pre_defensive_skill_z +
      format_5_0:pre_complexity_tolerance_z +
      format_5_0:pre_cascade_resilience_z +
      format_5_0:pre_opening_hhi_z |
      player_name + tournament_id,
    stress_sample,
    "stress_test_traits"
  ),
  safe_feols(
    "pre_traits_cp_loss_combined",
    pg_mean_cp_loss_cap ~ rating_diff100 +
      format_5_0:pre_accuracy_skill_z +
      format_5_0:pre_decay_resilience_z +
      format_5_0:pre_conversion_skill_z +
      format_5_0:pre_defensive_skill_z +
      format_5_0:pre_complexity_tolerance_z +
      format_5_0:pre_cascade_resilience_z +
      format_5_0:pre_opening_hhi_z |
      player_name + tournament_id,
    stress_sample,
    "stress_test_traits"
  ),
  safe_feols(
    "pre_traits_blunder_rate_combined",
    blunder_rate ~ rating_diff100 +
      format_5_0:pre_accuracy_skill_z +
      format_5_0:pre_decay_resilience_z +
      format_5_0:pre_conversion_skill_z +
      format_5_0:pre_defensive_skill_z +
      format_5_0:pre_complexity_tolerance_z +
      format_5_0:pre_cascade_resilience_z +
      format_5_0:pre_opening_hhi_z |
      player_name + tournament_id,
    stress_sample,
    "stress_test_traits"
  )
), fill = TRUE)
stress_coefs[, q.value := p.adjust(p.value, method = "BH")]
fwrite(stress_coefs, file.path(OUT_DIR, "stress_test_trait_model_coefficients.csv"))

all_coefs <- rbindlist(
  list(
    adaptation_coefs, risk_coefs, cascade_coefs, conversion_coefs,
    opening_hc_coefs, repeated_coefs, stress_coefs
  ),
  fill = TRUE
)
fwrite(all_coefs, file.path(OUT_DIR, "all_deeper_research_coefficients.csv"))

headline <- all_coefs[
  !is.na(term) &
    (
      grepl("format_5_0", term) |
        grepl("log_post_game_number_c", term)
    ) &
    !grepl("^format_5_0$", term)
]
headline[, abs_t := abs(estimate / std.error)]
setorder(headline, -abs_t)
headline <- headline[1:min(.N, 50)]
fwrite(headline, file.path(OUT_DIR, "headline_deeper_research_results.csv"))

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

report_adapt <- copy(adapt_desc)
report_adapt[, `:=`(
  mean_result = fmt(mean_result, 4),
  mean_cp_loss = fmt(mean_cp_loss, 2),
  mean_blunder_rate = fmt(mean_blunder_rate, 4),
  mean_post_game_number = fmt(mean_post_game_number, 1)
)]
report_repeat <- copy(repeat_desc)
report_repeat[, `:=`(
  period = fifelse(format_5_0 == 1, "5+0", "3+1"),
  mean_result = fmt(mean_result, 4),
  mean_cp_loss = fmt(mean_cp_loss, 2)
)]

md <- c(
  "# Deeper Research Ideas Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Inputs",
  "",
  paste0("- Player-game outcomes: `", PLAYER_GAME_PATH, "`"),
  paste0("- PGN features: `", PGN_FEATURE_PATH, "`"),
  paste0("- Opening features: `", OPENING_FEATURE_PATH, "`"),
  paste0("- Dynamic features: `", DYNAMIC_FEATURE_PATH, "`"),
  "",
  "## Sample",
  "",
  write_md_table(report_sample, c("Metric", "Value")),
  "",
  "## Post-Change Adaptation Descriptives",
  "",
  write_md_table(
    report_adapt,
    c("post_exposure_bin", "player_games", "mean_post_game_number", "mean_result", "mean_cp_loss", "mean_blunder_rate")
  ),
  "",
  "## Repeated-Match Descriptives",
  "",
  write_md_table(
    report_repeat,
    c("period", "prior_pair_bin", "player_games", "mean_result", "mean_cp_loss")
  ),
  "",
  "## Strongest Deeper-Research Coefficients",
  "",
  write_md_table(
    report_headline,
    c("family", "model", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within")
  ),
  "",
  "## Analysis Coverage",
  "",
  "- Idea 2: post-change adaptation/learning using player-specific post-change game number.",
  "- Idea 5: strategic risk-taking using complexity, legal-move volume, eval volatility, and complexity payoff.",
  "- Idea 6: error cascades and recovery after first blunder.",
  "- Idea 7: deeper conversion and defensive recovery after +2/-2 positions.",
  "- Idea 8: opening preparation as pre-change human capital using opening repertoire specialization.",
  "- Idea 11: repeated opponent matching and familiarity.",
  "- Idea 12: pre-change skill traits as predictors of treatment gains under 5+0.",
  "",
  "## Output Files",
  "",
  "- `sample_summary.csv`",
  "- `adaptation_descriptives.csv`",
  "- `repeated_match_descriptives.csv`",
  "- `adaptation_model_coefficients.csv`",
  "- `risk_taking_model_coefficients.csv`",
  "- `error_cascade_model_coefficients.csv`",
  "- `conversion_deep_model_coefficients.csv`",
  "- `opening_human_capital_model_coefficients.csv`",
  "- `repeated_matching_model_coefficients.csv`",
  "- `stress_test_trait_model_coefficients.csv`",
  "- `all_deeper_research_coefficients.csv`",
  "- `headline_deeper_research_results.csv`",
  "- `deeper_research_report.md`",
  "- `session_info.txt`"
)

writeLines(md, file.path(OUT_DIR, "deeper_research_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))
message("Wrote outputs to ", OUT_DIR)
