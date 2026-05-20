suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_rank_hypotheses"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "birthday", "classic_rating", "blitz_rating", "rapid_rating",
  "rank", "rank_end_round", "final_score_pregame", "final_score",
  "in_prizes", "bubble", "eliminated", "leader", "buchholz_score",
  "opponents_sum_score"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, event_id := as.character(date)]
df[, date := as.Date(date)]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]
df[, age := 2025L - birthday]

main <- df[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0 &
    !is.na(rank) &
    !is.na(rank_end_round)
]

main[, player_rating100 := (player_rating - 2500) / 100]
main[, opponent_rating100 := (opponent_rating - 2500) / 100]
main[, rating_diff100 := (player_rating - opponent_rating) / 100]
main[, abs_rating_diff100 := abs(player_rating - opponent_rating) / 100]
main[, expected_score := 1 / (1 + 10^((opponent_rating - player_rating) / 400))]
main[, result_over_expected := player_result - expected_score]
main[, online_classic_gap100 := (player_rating - classic_rating) / 100]
main[, online_blitz_gap100 := (player_rating - blitz_rating) / 100]
main[, blitz_classic_gap100 := (blitz_rating - classic_rating) / 100]
main[, age10 := (age - 35) / 10]
main[, bubble_zone := as.integer(bubble == 1)]
main[, prize_zone := as.integer(in_prizes == 1)]
main[, eliminated_zone := as.integer(eliminated == 1)]
main[, leader_zone := as.integer(leader == 1)]
main[, score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, buchholz_c := buchholz_score - mean(buchholz_score, na.rm = TRUE)]
main[, opponents_sum_c := opponents_sum_score - mean(opponents_sum_score, na.rm = TRUE)]

main[, event_month := (
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
) - (
  as.integer(format(rule_change_date, "%Y")) * 12L +
    as.integer(format(rule_change_date, "%m"))
)]

main[, field_size_round := max(.N, rank, rank_end_round, na.rm = TRUE), by = .(event_id, round)]
main[, rank_pct := fifelse(field_size_round > 1, (rank - 1) / (field_size_round - 1), 0)]
main[, end_rank_pct := fifelse(field_size_round > 1, (rank_end_round - 1) / (field_size_round - 1), 0)]
main[, rank_quality := 1 - end_rank_pct]
main[, rank_improvement_pct := rank_pct - end_rank_pct]
main[, rank_improvement_raw := rank - rank_end_round]

main[, game_id := paste(
  event_id,
  round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "||"
)]

opponent_lookup <- main[, .(
  event_id,
  round,
  player_name,
  opponent_accuracy_observed = player_accuracy,
  opponent_rank_pct = rank_pct,
  opponent_end_rank_pct = end_rank_pct
)]
setkey(opponent_lookup, event_id, round, player_name)
main[, `:=`(
  opponent_accuracy_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_accuracy_observed
  ],
  opponent_rank_pct = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_rank_pct
  ],
  opponent_end_rank_pct = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_end_rank_pct
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

main[, accuracy_diff10 := (player_accuracy - opponent_accuracy_observed) / 10]
main[, rank_gap_pct := rank_pct - opponent_rank_pct]

setorder(main, player_name, event_id, round)
lag_cols <- c(
  "round", "player_result", "player_accuracy", "rating_diff100",
  "expected_score", "result_over_expected", "rank_pct", "end_rank_pct",
  "rank_improvement_pct"
)
for (col in lag_cols) {
  main[, paste0("prev_", col) := shift(get(col)), by = .(player_name, event_id)]
}

main[, `:=`(
  prev_loss = as.integer(prev_player_result == 0),
  prev_win = as.integer(prev_player_result == 1),
  prev_draw = as.integer(prev_player_result == 0.5),
  prev_unexpected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 >= 2),
  prev_expected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 <= -2),
  prev_upset_win = as.integer(prev_player_result == 1 & prev_rating_diff100 <= -2)
)]

lagged <- main[
  !is.na(prev_round) &
    prev_round == round - 1 &
    !is.na(prev_player_result) &
    !is.na(prev_player_accuracy) &
    !is.na(prev_rating_diff100)
]

fwrite(
  main[, .(
    rows = .N,
    players = uniqueN(player_name),
    events = uniqueN(event_id),
    games = uniqueN(game_id),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    mean_rank_improvement_pct = mean(rank_improvement_pct),
    sd_rank_improvement_pct = sd(rank_improvement_pct),
    mean_rank_improvement_raw = mean(rank_improvement_raw),
    sd_rank_improvement_raw = sd(rank_improvement_raw)
  )],
  file.path(output_dir, "sample_summary.csv")
)

fwrite(
  main[, .(
    rows = .N,
    mean_rank_quality = mean(rank_quality),
    mean_rank_improvement_pct = mean(rank_improvement_pct),
    mean_rank_improvement_raw = mean(rank_improvement_raw),
    mean_result = mean(player_result),
    mean_rating_diff100 = mean(rating_diff100)
  ), by = .(format_5_0, round)][order(format_5_0, round)],
  file.path(output_dir, "rank_descriptives_by_round.csv")
)

find_term <- function(model, pieces) {
  term_names <- names(coef(model))
  hits <- term_names[
    vapply(
      term_names,
      function(x) all(vapply(pieces, grepl, logical(1), x = x, fixed = TRUE)),
      logical(1)
    )
  ]
  if (length(hits) != 1) {
    stop("Could not uniquely identify term for pieces: ", paste(pieces, collapse = ", "))
  }
  hits
}

extract_target <- function(model, pieces) {
  target_term <- find_term(model, pieces)
  tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
  tt[tt[["term"]] == target_term][, nobs := nobs(model)]
}

ensure_p_value <- function(dt) {
  if (!"p.value" %in% names(dt)) {
    dt[, p.value := NA_real_]
  }
  dt
}

round_controls <- paste(
  "+ rank_pct + player_rating100 + opponent_rating100 + is_white +",
  "score_c + factor(round)"
)

fit_round_model <- function(data, outcome, rhs,
                            fe = "player_name + event_id",
                            cluster = ~ player_name + event_id) {
  fml <- as.formula(paste(outcome, "~", rhs, round_controls, "|", fe))
  feols(fml, data = data, cluster = cluster)
}

round_hypotheses <- list(
  list(
    id = "R01_result_to_rank",
    family = "score_to_rank_conversion",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * player_result",
    target = c("format_5_0", "player_result"),
    sample = "main",
    story = "Does the same game result move players more in the standings after the rule change?"
  ),
  list(
    id = "R02_rating_edge_to_rank",
    family = "relative_skill_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * rating_diff100",
    target = c("format_5_0", "rating_diff100"),
    sample = "main",
    story = "Does a 100-point rating edge generate more rank improvement after the rule change?"
  ),
  list(
    id = "R03_accuracy_edge_to_rank",
    family = "accuracy_to_rank_conversion",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * accuracy_diff10",
    target = c("format_5_0", "accuracy_diff10"),
    sample = "accuracy",
    story = "Does a 10-point accuracy edge generate more rank improvement after the rule change?"
  ),
  list(
    id = "R04_online_capital_to_rank",
    family = "platform_capital_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * online_classic_gap100",
    target = c("format_5_0", "online_classic_gap100"),
    sample = "main",
    story = "Does Chess.com-over-classical strength translate into better rank movement after the rule change?"
  ),
  list(
    id = "R05_bubble_rank_pressure",
    family = "threshold_pressure_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * bubble_zone",
    target = c("format_5_0", "bubble_zone"),
    sample = "main",
    story = "Do bubble players move differently in the standings after the rule change?"
  ),
  list(
    id = "R06_eliminated_rank_pressure",
    family = "low_downside_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "format_5_0 * eliminated_zone",
    target = c("format_5_0", "eliminated_zone"),
    sample = "main",
    story = "Do eliminated players move differently in the standings after the rule change?"
  ),
  list(
    id = "R07_prev_loss_next_rank",
    family = "lagged_loss_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "prev_loss",
    target = c("prev_loss"),
    sample = "lagged",
    story = "Does losing the previous game predict worse next-round rank movement?"
  ),
  list(
    id = "R08_unexpected_loss_next_rank",
    family = "lagged_upset_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_unexpected_loss"),
    sample = "lagged",
    story = "Does an unexpected previous loss predict worse next-round rank movement?"
  ),
  list(
    id = "R09_upset_win_next_rank",
    family = "lagged_upset_to_rank",
    outcome = "rank_improvement_pct",
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_upset_win"),
    sample = "lagged",
    story = "Does an upset previous win predict better next-round rank movement?"
  )
)

round_specs <- list(
  list(name = "event_fe", fe = "player_name + event_id", cluster = ~ player_name + event_id, raw = FALSE),
  list(name = "round_gt2_event_fe", fe = "player_name + event_id", cluster = ~ player_name + event_id, raw = FALSE, round_gt2 = TRUE),
  list(name = "game_fe", fe = "player_name + game_id", cluster = ~ player_name + game_id, raw = FALSE),
  list(name = "raw_rank_event_fe", fe = "player_name + event_id", cluster = ~ player_name + event_id, raw = TRUE)
)

round_rows <- list()
for (spec in round_specs) {
  for (h in round_hypotheses) {
    data_use <- switch(
      h$sample,
      main = main,
      accuracy = main[!is.na(accuracy_diff10)],
      lagged = lagged
    )
    if (isTRUE(spec$round_gt2)) data_use <- data_use[round > 2]
    outcome_use <- if (isTRUE(spec$raw)) "rank_improvement_raw" else h$outcome
    key <- paste(spec$name, h$id, sep = "__")
    round_rows[[key]] <- tryCatch({
      model <- fit_round_model(
        data = data_use,
        outcome = outcome_use,
        rhs = h$rhs,
        fe = spec$fe,
        cluster = spec$cluster
      )
      row <- extract_target(model, h$target)
      row[, `:=`(
        specification = spec$name,
        hypothesis = h$id,
        family = h$family,
        outcome = outcome_use,
        story = h$story
      )]
      row
    }, error = function(e) {
      data.table(
        specification = spec$name,
        hypothesis = h$id,
        family = h$family,
        outcome = outcome_use,
        story = h$story,
        error = e$message
      )
    })
  }
}

round_tests <- rbindlist(round_rows, fill = TRUE)
round_tests <- ensure_p_value(round_tests)
round_tests[, p_bh_by_spec := p.adjust(p.value, method = "BH"), by = specification]
setorder(round_tests, specification, p.value)
fwrite(round_tests, file.path(output_dir, "round_rank_hypothesis_coefficients.csv"))

# Final rank outcomes: one row per player-event, using the player's last observed round.
setorder(main, player_name, event_id, round)
final_rank <- main[, .SD[.N], by = .(player_name, event_id)]
final_rank[, final_field_size := max(rank_end_round, na.rm = TRUE), by = event_id]
final_rank[, final_rank_pct := fifelse(final_field_size > 1, (rank_end_round - 1) / (final_field_size - 1), 0)]
final_rank[, final_rank_quality := 1 - final_rank_pct]
final_rank[, final_rank_raw_neg := -rank_end_round]
final_rank[, completed_final_round := as.integer(round == max(round, na.rm = TRUE)), by = event_id]

final_controls <- "+ player_rating100 + factor(round)"
fit_final_model <- function(data, outcome, rhs) {
  fml <- as.formula(paste(outcome, "~", rhs, final_controls, "| player_name + event_id"))
  feols(fml, data = data, cluster = ~ player_name + event_id)
}

final_hypotheses <- list(
  list(
    id = "F01_rating_final_rank",
    family = "skill_to_final_rank",
    rhs = "format_5_0 * player_rating100",
    target = c("format_5_0", "player_rating100"),
    story = "Does rating translate into better final rank after the rule change?"
  ),
  list(
    id = "F02_online_capital_final_rank",
    family = "platform_capital_to_final_rank",
    rhs = "format_5_0 * online_classic_gap100",
    target = c("format_5_0", "online_classic_gap100"),
    story = "Does Chess.com-over-classical strength translate into better final rank after the rule change?"
  ),
  list(
    id = "F03_age_final_rank",
    family = "age_to_final_rank",
    rhs = "format_5_0 * age10",
    target = c("format_5_0", "age10"),
    story = "Do older players finish worse in the standings after the rule change?"
  ),
  list(
    id = "F04_blitz_capital_final_rank",
    family = "blitz_capital_to_final_rank",
    rhs = "format_5_0 * blitz_classic_gap100",
    target = c("format_5_0", "blitz_classic_gap100"),
    story = "Does FIDE blitz-over-classical strength translate into better final rank after the rule change?"
  )
)

final_rows <- list()
for (outcome in c("final_rank_quality", "final_rank_raw_neg")) {
  for (h in final_hypotheses) {
    key <- paste(outcome, h$id, sep = "__")
    final_rows[[key]] <- tryCatch({
      model <- fit_final_model(final_rank, outcome, h$rhs)
      row <- extract_target(model, h$target)
      row[, `:=`(
        hypothesis = h$id,
        family = h$family,
        outcome = outcome,
        story = h$story
      )]
      row
    }, error = function(e) {
      data.table(hypothesis = h$id, family = h$family, outcome = outcome, story = h$story, error = e$message)
    })
  }
}

final_tests <- rbindlist(final_rows, fill = TRUE)
final_tests <- ensure_p_value(final_tests)
final_tests[, p_bh_by_outcome := p.adjust(p.value, method = "BH"), by = outcome]
setorder(final_tests, outcome, p.value)
fwrite(final_tests, file.path(output_dir, "final_rank_hypothesis_coefficients.csv"))

fake_cutoffs <- as.Date(c(
  "2023-03-01", "2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"
))
placebo_h <- round_hypotheses[1:6]
placebo_rows <- list()
pre_actual <- copy(main[date < rule_change_date])
for (cutoff in fake_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  for (h in placebo_h) {
    data_use <- if (h$sample == "accuracy") pre_actual[!is.na(accuracy_diff10)] else pre_actual
    rhs <- gsub("format_5_0", "placebo_post", h$rhs, fixed = TRUE)
    target <- gsub("format_5_0", "placebo_post", h$target, fixed = TRUE)
    key <- paste(cutoff, h$id, sep = "__")
    placebo_rows[[key]] <- tryCatch({
      model <- fit_round_model(
        data = data_use,
        outcome = h$outcome,
        rhs = rhs,
        fe = "player_name + event_id",
        cluster = ~ player_name + event_id
      )
      row <- extract_target(model, target)
      row[, `:=`(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome)]
      row
    }, error = function(e) {
      data.table(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome, error = e$message)
    })
  }
}

placebos <- rbindlist(placebo_rows, fill = TRUE)
placebos <- ensure_p_value(placebos)
placebos[, p_bh_by_cutoff := p.adjust(p.value, method = "BH"), by = cutoff]
fwrite(placebos, file.path(output_dir, "round_rank_placebo_cutoffs.csv"))

event_rows <- list()
event_sample <- main[event_month >= -18 & event_month <= 6]
event_terms <- list(
  R01_result_to_rank = list(x = "player_result", outcome = "rank_improvement_pct"),
  R02_rating_edge_to_rank = list(x = "rating_diff100", outcome = "rank_improvement_pct"),
  R03_accuracy_edge_to_rank = list(x = "accuracy_diff10", outcome = "rank_improvement_pct"),
  R04_online_capital_to_rank = list(x = "online_classic_gap100", outcome = "rank_improvement_pct")
)
for (id in names(event_terms)) {
  spec <- event_terms[[id]]
  data_use <- if (spec$x == "accuracy_diff10") event_sample[!is.na(accuracy_diff10)] else event_sample
  fml <- as.formula(paste(
    spec$outcome,
    "~ i(event_month,",
    spec$x,
    ", ref = -1)",
    round_controls,
    "| player_name + event_id"
  ))
  event_rows[[id]] <- tryCatch({
    model <- feols(fml, data = data_use, cluster = ~ player_name + event_id)
    tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
    tt <- tt[grepl("event_month::", term, fixed = TRUE)]
    tt[, `:=`(
      hypothesis = id,
      event_month = as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term)),
      nobs = nobs(model)
    )]
    tt
  }, error = function(e) {
    data.table(hypothesis = id, error = e$message)
  })
}

event_study <- rbindlist(event_rows, fill = TRUE)
setorder(event_study, hypothesis, event_month)
fwrite(event_study, file.path(output_dir, "round_rank_event_study_coefficients.csv"))

event_plot <- ggplot(event_study[!is.na(event_month)], aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.35, color = "#2364aa") +
  facet_wrap(~ hypothesis, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Rank-movement interaction coefficient relative to month -1",
    title = "Event-study: rank movement mechanisms"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(size = 8))
ggsave(
  file.path(output_dir, "round_rank_event_study.png"),
  event_plot,
  width = 11,
  height = 7,
  dpi = 200
)

effect_sizes <- rbindlist(list(
  main[, .(
    variable = "rank_improvement_pct",
    mean = mean(rank_improvement_pct, na.rm = TRUE),
    sd = sd(rank_improvement_pct, na.rm = TRUE),
    p25 = quantile(rank_improvement_pct, 0.25, na.rm = TRUE),
    p75 = quantile(rank_improvement_pct, 0.75, na.rm = TRUE),
    iqr = IQR(rank_improvement_pct, na.rm = TRUE)
  )],
  main[, .(
    variable = "rank_improvement_raw",
    mean = mean(rank_improvement_raw, na.rm = TRUE),
    sd = sd(rank_improvement_raw, na.rm = TRUE),
    p25 = quantile(rank_improvement_raw, 0.25, na.rm = TRUE),
    p75 = quantile(rank_improvement_raw, 0.75, na.rm = TRUE),
    iqr = IQR(rank_improvement_raw, na.rm = TRUE)
  )],
  final_rank[, .(
    variable = "final_rank_quality",
    mean = mean(final_rank_quality, na.rm = TRUE),
    sd = sd(final_rank_quality, na.rm = TRUE),
    p25 = quantile(final_rank_quality, 0.25, na.rm = TRUE),
    p75 = quantile(final_rank_quality, 0.75, na.rm = TRUE),
    iqr = IQR(final_rank_quality, na.rm = TRUE)
  )]
), fill = TRUE)
fwrite(effect_sizes, file.path(output_dir, "rank_effect_variable_distributions.csv"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
