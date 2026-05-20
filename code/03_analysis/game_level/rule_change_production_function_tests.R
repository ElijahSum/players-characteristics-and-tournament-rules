suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_production_function_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "birthday", "classic_rating", "blitz_rating", "rapid_rating",
  "rank", "in_prizes", "bubble", "eliminated", "leader",
  "final_score_pregame", "buchholz_score", "opponents_sum_score"
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
    !is.na(player_accuracy) &
    player_accuracy > 0 &
    player_accuracy < 100 &
    !is.na(player_result) &
    !is.na(age) &
    age >= 10 &
    age <= 85 &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0
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
main[, close_game := as.integer(abs(player_rating - opponent_rating) <= 100)]
main[, heavy_favorite := as.integer(player_rating - opponent_rating >= 200)]
main[, underdog := as.integer(player_rating < opponent_rating)]
main[, score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, buchholz_c := buchholz_score - mean(buchholz_score, na.rm = TRUE)]
main[, opponents_sum_c := opponents_sum_score - mean(opponents_sum_score, na.rm = TRUE)]

main[, event_month := (
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
) - (
  as.integer(format(rule_change_date, "%Y")) * 12L +
    as.integer(format(rule_change_date, "%m"))
)]

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
  opp_accuracy = player_accuracy,
  opp_result = player_result,
  opp_is_white = is_white,
  opp_online_classic_gap100 = online_classic_gap100,
  opp_bubble_zone = bubble_zone,
  opp_prize_zone = prize_zone,
  opp_eliminated_zone = eliminated_zone,
  opp_score_c = score_c
)]
setkey(opponent_lookup, event_id, round, player_name)
main[, `:=`(
  opponent_accuracy_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_accuracy
  ],
  opponent_result_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_result
  ],
  opponent_online_classic_gap100 = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_online_classic_gap100
  ],
  opponent_bubble_zone = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_bubble_zone
  ],
  opponent_prize_zone = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_prize_zone
  ],
  opponent_eliminated_zone = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_eliminated_zone
  ],
  opponent_score_c = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opp_score_c
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

paired <- main[!is.na(opponent_accuracy_observed)]
paired[, accuracy_diff10 := (player_accuracy - opponent_accuracy_observed) / 10]
paired[, abs_accuracy_diff10 := abs(player_accuracy - opponent_accuracy_observed) / 10]
paired[, mean_game_accuracy := (player_accuracy + opponent_accuracy_observed) / 2]
paired[, accuracy_over_expected := player_accuracy - mean(player_accuracy, na.rm = TRUE)]
paired[, score_gap_c := score_c - opponent_score_c]
paired[, online_gap_diff100 := online_classic_gap100 - opponent_online_classic_gap100]
paired[, bubble_vs_opponent := bubble_zone - opponent_bubble_zone]
paired[, prize_vs_opponent := prize_zone - opponent_prize_zone]
paired[, eliminated_vs_opponent := eliminated_zone - opponent_eliminated_zone]
paired[, win := as.integer(player_result == 1)]
paired[, loss := as.integer(player_result == 0)]
paired[, draw := as.integer(player_result == 0.5)]

game <- paired[, .SD[which.max(is_white)], by = game_id]
game[, favorite_is_player := player_rating >= opponent_rating]
game[, favorite_result := fifelse(favorite_is_player, player_result, opponent_result_observed)]
game[, favorite_rating := pmax(player_rating, opponent_rating)]
game[, underdog_rating := pmin(player_rating, opponent_rating)]
game[, abs_rating_diff100_game := abs(player_rating - opponent_rating) / 100]
game[, favorite_accuracy := fifelse(favorite_is_player, player_accuracy, opponent_accuracy_observed)]
game[, underdog_accuracy := fifelse(favorite_is_player, opponent_accuracy_observed, player_accuracy)]
game[, favorite_accuracy_adv10 := (favorite_accuracy - underdog_accuracy) / 10]
game[, favorite_win := as.integer(favorite_result == 1)]
game[, favorite_loss := as.integer(favorite_result == 0)]
game[, draw_game := as.integer(favorite_result == 0.5)]
game[, decisive_game := as.integer(favorite_result != 0.5)]
game[, close_game_game := as.integer(abs_rating_diff100_game <= 1)]
game[, mean_game_rating100 := ((player_rating + opponent_rating) / 2 - 2500) / 100]
game[, mean_game_accuracy_c := mean_game_accuracy - mean(mean_game_accuracy, na.rm = TRUE)]

fwrite(
  data.table(
    paired_rows = nrow(paired),
    games = uniqueN(paired$game_id),
    game_rows = nrow(game),
    events = uniqueN(paired$event_id),
    pre_rows = nrow(paired[format_5_0 == 0]),
    post_rows = nrow(paired[format_5_0 == 1]),
    pre_games = nrow(game[format_5_0 == 0]),
    post_games = nrow(game[format_5_0 == 1])
  ),
  file.path(output_dir, "sample_summary.csv")
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

fit_player_model <- function(data, outcome, rhs, target_pieces,
                             fe = "player_name + event_id",
                             cluster = ~ player_name + event_id,
                             controls = "+ player_rating100 + opponent_rating100 + is_white + factor(round)") {
  fml <- as.formula(paste(outcome, "~", rhs, controls, "|", fe))
  model <- feols(fml, data = data, cluster = cluster)
  extract_target(model, target_pieces)
}

fit_game_model <- function(data, outcome, rhs, target_pieces,
                           fe = "event_id",
                           cluster = ~ event_id,
                           controls = "+ mean_game_rating100 + factor(round)") {
  fml <- as.formula(paste(outcome, "~", rhs, controls, "|", fe))
  model <- feols(fml, data = data, cluster = cluster)
  extract_target(model, target_pieces)
}

player_hypotheses <- list(
  list(
    id = "PF01_accuracy_conversion",
    family = "accuracy_to_points",
    outcome = "player_result",
    rhs = "format_5_0 * accuracy_diff10",
    target = c("format_5_0", "accuracy_diff10"),
    story = "Does the new format change how strongly a 10-point accuracy advantage converts into game points?"
  ),
  list(
    id = "PF02_accuracy_conversion_over_expected",
    family = "accuracy_to_rating_adjusted_points",
    outcome = "result_over_expected",
    rhs = "format_5_0 * accuracy_diff10",
    target = c("format_5_0", "accuracy_diff10"),
    story = "Does the new format change accuracy-to-result conversion after subtracting Elo-expected score?"
  ),
  list(
    id = "PF03_accuracy_conversion_favorites",
    family = "conversion_x_relative_skill",
    outcome = "player_result",
    rhs = "format_5_0 * accuracy_diff10 * rating_diff100",
    target = c("format_5_0", "accuracy_diff10", "rating_diff100"),
    story = "Do rating favorites become better at turning the same accuracy edge into points?"
  ),
  list(
    id = "PF04_accuracy_conversion_online_capital",
    family = "conversion_x_platform_capital",
    outcome = "player_result",
    rhs = "format_5_0 * accuracy_diff10 * online_classic_gap100",
    target = c("format_5_0", "accuracy_diff10", "online_classic_gap100"),
    story = "Does online-specific capital increase conversion of accuracy into points under 5+0?"
  ),
  list(
    id = "PF05_accuracy_conversion_bubble",
    family = "conversion_x_threshold_pressure",
    outcome = "player_result",
    rhs = "format_5_0 * accuracy_diff10 * bubble_zone",
    target = c("format_5_0", "accuracy_diff10", "bubble_zone"),
    story = "Do bubble players become less able to convert the same accuracy edge into points?"
  ),
  list(
    id = "PF06_accuracy_conversion_eliminated",
    family = "conversion_x_low_downside",
    outcome = "player_result",
    rhs = "format_5_0 * accuracy_diff10 * eliminated_zone",
    target = c("format_5_0", "accuracy_diff10", "eliminated_zone"),
    story = "Do eliminated players convert accuracy into points differently when downside risk is lower?"
  ),
  list(
    id = "PF07_close_game_accuracy_quality",
    family = "quality_in_even_pairings",
    outcome = "player_accuracy",
    rhs = "format_5_0 * close_game",
    target = c("format_5_0", "close_game"),
    story = "Does 5+0 reduce move quality especially in ex-ante close pairings?"
  ),
  list(
    id = "PF08_high_favorite_accuracy_quality",
    family = "quality_in_lopsided_pairings",
    outcome = "player_accuracy",
    rhs = "format_5_0 * heavy_favorite",
    target = c("format_5_0", "heavy_favorite"),
    story = "Does 5+0 help heavy favorites play more accurately, consistent with easier conversion?"
  )
)

player_rows <- list()
player_specs <- list(
  list(name = "event_fe", fe = "player_name + event_id", cluster = ~ player_name + event_id),
  list(name = "game_fe", fe = "player_name + game_id", cluster = ~ player_name + game_id),
  list(name = "round_gt2_event_fe", fe = "player_name + event_id", cluster = ~ player_name + event_id, filter_round_gt2 = TRUE)
)

for (spec in player_specs) {
  data_spec <- if (isTRUE(spec$filter_round_gt2)) paired[round > 2] else paired
  for (h in player_hypotheses) {
    key <- paste(spec$name, h$id, sep = "__")
    player_rows[[key]] <- tryCatch({
      row <- fit_player_model(
        data = data_spec,
        outcome = h$outcome,
        rhs = h$rhs,
        target_pieces = h$target,
        fe = spec$fe,
        cluster = spec$cluster
      )
      row[, `:=`(
        specification = spec$name,
        hypothesis = h$id,
        family = h$family,
        outcome = h$outcome,
        story = h$story
      )]
      row
    }, error = function(e) {
      data.table(
        specification = spec$name,
        hypothesis = h$id,
        family = h$family,
        outcome = h$outcome,
        story = h$story,
        error = e$message
      )
    })
  }
}

player_tests <- rbindlist(player_rows, fill = TRUE)
player_tests[, p_bh_by_spec := p.adjust(p.value, method = "BH"), by = specification]
setorder(player_tests, specification, p.value)
fwrite(player_tests, file.path(output_dir, "player_production_function_tests.csv"))

game_hypotheses <- list(
  list(
    id = "GF01_favorite_result_rating_gap",
    family = "favorite_conversion",
    outcome = "favorite_result",
    rhs = "format_5_0 * abs_rating_diff100_game",
    target = c("format_5_0", "abs_rating_diff100_game"),
    story = "Does a larger favorite rating edge translate into more score after the rule change?"
  ),
  list(
    id = "GF02_favorite_loss_rating_gap",
    family = "upset_risk",
    outcome = "favorite_loss",
    rhs = "format_5_0 * abs_rating_diff100_game",
    target = c("format_5_0", "abs_rating_diff100_game"),
    story = "Does the favorite's upset-loss risk fall more steeply with rating gap after the rule change?"
  ),
  list(
    id = "GF03_draw_rating_gap",
    family = "decisiveness",
    outcome = "draw_game",
    rhs = "format_5_0 * abs_rating_diff100_game",
    target = c("format_5_0", "abs_rating_diff100_game"),
    story = "Does the relation between rating gaps and draw probability change under 5+0?"
  ),
  list(
    id = "GF04_decisive_close_game",
    family = "decisiveness_in_even_pairings",
    outcome = "decisive_game",
    rhs = "format_5_0 * close_game_game",
    target = c("format_5_0", "close_game_game"),
    story = "Does 5+0 make ex-ante close games more decisive?"
  ),
  list(
    id = "GF05_accuracy_dispersion_close_game",
    family = "quality_dispersion",
    outcome = "abs_accuracy_diff10",
    rhs = "format_5_0 * close_game_game",
    target = c("format_5_0", "close_game_game"),
    story = "Does 5+0 create larger accuracy gaps in ex-ante close games?"
  ),
  list(
    id = "GF06_favorite_accuracy_conversion",
    family = "favorite_accuracy_to_points",
    outcome = "favorite_result",
    rhs = "format_5_0 * favorite_accuracy_adv10",
    target = c("format_5_0", "favorite_accuracy_adv10"),
    story = "Does a favorite's accuracy advantage convert into more score under 5+0?"
  )
)

game_rows <- list()
for (h in game_hypotheses) {
  game_rows[[h$id]] <- tryCatch({
    row <- fit_game_model(
      data = game,
      outcome = h$outcome,
      rhs = h$rhs,
      target_pieces = h$target
    )
    row[, `:=`(
      hypothesis = h$id,
      family = h$family,
      outcome = h$outcome,
      story = h$story
    )]
    row
  }, error = function(e) {
    data.table(hypothesis = h$id, family = h$family, outcome = h$outcome, story = h$story, error = e$message)
  })
}
game_tests <- rbindlist(game_rows, fill = TRUE)
game_tests[, p_bh := p.adjust(p.value, method = "BH")]
setorder(game_tests, p.value)
fwrite(game_tests, file.path(output_dir, "game_level_tests.csv"))

fake_cutoffs <- as.Date(c(
  "2023-03-01", "2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"
))
placebo_player_h <- player_hypotheses[1:6]
placebo_rows <- list()
pre_actual <- copy(paired[date < rule_change_date])
for (cutoff in fake_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  for (h in placebo_player_h) {
    rhs <- gsub("format_5_0", "placebo_post", h$rhs, fixed = TRUE)
    target <- gsub("format_5_0", "placebo_post", h$target, fixed = TRUE)
    key <- paste(cutoff, h$id, sep = "__")
    placebo_rows[[key]] <- tryCatch({
      row <- fit_player_model(
        data = pre_actual,
        outcome = h$outcome,
        rhs = rhs,
        target_pieces = target,
        fe = "player_name + event_id",
        cluster = ~ player_name + event_id
      )
      row[, `:=`(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome)]
      row
    }, error = function(e) {
      data.table(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome, error = e$message)
    })
  }
}
player_placebos <- rbindlist(placebo_rows, fill = TRUE)
player_placebos[, p_bh_by_cutoff := p.adjust(p.value, method = "BH"), by = cutoff]
fwrite(player_placebos, file.path(output_dir, "player_placebo_cutoffs.csv"))

grid_cutoffs <- seq.Date(as.Date("2023-01-01"), as.Date("2025-05-01"), by = "2 months")
grid_rows <- list()
for (cutoff in grid_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  for (h in placebo_player_h[1:4]) {
    rhs <- gsub("format_5_0", "placebo_post", h$rhs, fixed = TRUE)
    target <- gsub("format_5_0", "placebo_post", h$target, fixed = TRUE)
    key <- paste(cutoff, h$id, sep = "__")
    grid_rows[[key]] <- tryCatch({
      row <- fit_player_model(
        data = pre_actual,
        outcome = h$outcome,
        rhs = rhs,
        target_pieces = target,
        fe = "player_name + event_id",
        cluster = ~ player_name + event_id
      )
      row[, `:=`(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome)]
      row
    }, error = function(e) {
      data.table(cutoff = cutoff, hypothesis = h$id, family = h$family, outcome = h$outcome, error = e$message)
    })
  }
}
placebo_grid <- rbindlist(grid_rows, fill = TRUE)
actual_lookup <- player_tests[
  specification == "event_fe" & hypothesis %in% vapply(placebo_player_h[1:4], `[[`, character(1), "id"),
  .(hypothesis, actual_estimate = estimate)
]
placebo_grid <- merge(placebo_grid, actual_lookup, by = "hypothesis", all.x = TRUE)
placebo_grid_summary <- placebo_grid[!is.na(estimate), .(
  n_placebos = .N,
  placebo_mean = mean(estimate),
  placebo_sd = sd(estimate),
  placebo_p10 = quantile(estimate, 0.10),
  placebo_p50 = quantile(estimate, 0.50),
  placebo_p90 = quantile(estimate, 0.90),
  actual_estimate = unique(actual_estimate),
  empirical_two_sided_p = (sum(abs(estimate) >= abs(unique(actual_estimate))) + 1) / (.N + 1)
), by = .(hypothesis, family)]
fwrite(placebo_grid, file.path(output_dir, "player_placebo_grid.csv"))
fwrite(placebo_grid_summary, file.path(output_dir, "player_placebo_grid_summary.csv"))

pretrend_rows <- list()
pretrend_sample <- paired[event_month >= -18 & event_month <= -1]
pretrend_sample[, event_month_pre := event_month + 1L]
for (h in placebo_player_h[1:4]) {
  rhs <- gsub("format_5_0", "event_month_pre", h$rhs, fixed = TRUE)
  target <- gsub("format_5_0", "event_month_pre", h$target, fixed = TRUE)
  pretrend_rows[[h$id]] <- tryCatch({
    row <- fit_player_model(
      data = pretrend_sample,
      outcome = h$outcome,
      rhs = rhs,
      target_pieces = target,
      fe = "player_name + event_id",
      cluster = ~ player_name + event_id
    )
    row[, `:=`(hypothesis = h$id, family = h$family, outcome = h$outcome)]
    row
  }, error = function(e) {
    data.table(hypothesis = h$id, family = h$family, outcome = h$outcome, error = e$message)
  })
}
pretrends <- rbindlist(pretrend_rows, fill = TRUE)
pretrends[, p_bh := p.adjust(p.value, method = "BH")]
fwrite(pretrends, file.path(output_dir, "player_pretrend_tests.csv"))

event_rows <- list()
event_sample <- paired[event_month >= -18 & event_month <= 6]
event_h <- player_hypotheses[1:4]
for (h in event_h) {
  x_term <- switch(
    h$id,
    PF01_accuracy_conversion = "accuracy_diff10",
    PF02_accuracy_conversion_over_expected = "accuracy_diff10",
    PF03_accuracy_conversion_favorites = "I(accuracy_diff10 * rating_diff100)",
    PF04_accuracy_conversion_online_capital = "I(accuracy_diff10 * online_classic_gap100)"
  )
  fml <- as.formula(paste(
    h$outcome,
    "~ i(event_month,",
    x_term,
    ", ref = -1) + player_rating100 + opponent_rating100 + is_white + factor(round)",
    "| player_name + event_id"
  ))
  event_rows[[h$id]] <- tryCatch({
    model <- feols(fml, data = event_sample, cluster = ~ player_name + event_id)
    tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
    tt <- tt[grepl("event_month::", term, fixed = TRUE)]
    tt[, `:=`(
      hypothesis = h$id,
      family = h$family,
      outcome = h$outcome,
      event_month = as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term)),
      nobs = nobs(model)
    )]
    tt
  }, error = function(e) {
    data.table(hypothesis = h$id, family = h$family, outcome = h$outcome, error = e$message)
  })
}
event_study <- rbindlist(event_rows, fill = TRUE)
setorder(event_study, hypothesis, event_month)
fwrite(event_study, file.path(output_dir, "player_event_study_coefficients.csv"))

plot_data <- event_study[!is.na(event_month)]
event_plot <- ggplot(plot_data, aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.35, color = "#2364aa") +
  facet_wrap(~ family, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Interaction coefficient relative to month -1",
    title = "Event-study: production-function mechanisms"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(size = 8))
ggsave(
  file.path(output_dir, "production_function_event_study.png"),
  event_plot,
  width = 11,
  height = 7,
  dpi = 200
)

effect_stats <- rbindlist(list(
  paired[, .(
    variable = "accuracy_diff10",
    mean = mean(accuracy_diff10, na.rm = TRUE),
    sd = sd(accuracy_diff10, na.rm = TRUE),
    p25 = quantile(accuracy_diff10, 0.25, na.rm = TRUE),
    p75 = quantile(accuracy_diff10, 0.75, na.rm = TRUE),
    iqr = IQR(accuracy_diff10, na.rm = TRUE)
  )],
  paired[, .(
    variable = "rating_diff100",
    mean = mean(rating_diff100, na.rm = TRUE),
    sd = sd(rating_diff100, na.rm = TRUE),
    p25 = quantile(rating_diff100, 0.25, na.rm = TRUE),
    p75 = quantile(rating_diff100, 0.75, na.rm = TRUE),
    iqr = IQR(rating_diff100, na.rm = TRUE)
  )],
  paired[, .(
    variable = "online_classic_gap100",
    mean = mean(online_classic_gap100, na.rm = TRUE),
    sd = sd(online_classic_gap100, na.rm = TRUE),
    p25 = quantile(online_classic_gap100, 0.25, na.rm = TRUE),
    p75 = quantile(online_classic_gap100, 0.75, na.rm = TRUE),
    iqr = IQR(online_classic_gap100, na.rm = TRUE)
  )]
), fill = TRUE)
fwrite(effect_stats, file.path(output_dir, "effect_variable_distributions.csv"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
