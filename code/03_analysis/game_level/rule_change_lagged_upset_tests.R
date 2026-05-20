suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_lagged_upset_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "birthday", "final_score_pregame", "rank", "in_prizes",
  "bubble", "eliminated", "leader", "classic_rating", "blitz_rating",
  "rapid_rating"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, event_id := as.character(date)]
df[, date := as.Date(date)]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]
df[, age := 2025L - birthday]

main <- df[
  player_title != "No Title" &
    !is.na(player_accuracy) &
    player_accuracy > 0 &
    player_accuracy < 100 &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0
]

main[, player_rating100 := (player_rating - 2500) / 100]
main[, opponent_rating100 := (opponent_rating - 2500) / 100]
main[, rating_diff100 := (player_rating - opponent_rating) / 100]
main[, expected_score := 1 / (1 + 10^((opponent_rating - player_rating) / 400))]
main[, result_over_expected := player_result - expected_score]
main[, online_classic_gap100 := (player_rating - classic_rating) / 100]
main[, online_blitz_gap100 := (player_rating - blitz_rating) / 100]
main[, age10 := (age - 35) / 10]
main[, score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, bubble_zone := as.integer(bubble == 1)]
main[, prize_zone := as.integer(in_prizes == 1)]
main[, eliminated_zone := as.integer(eliminated == 1)]
main[, leader_zone := as.integer(leader == 1)]
main[, win := as.integer(player_result == 1)]
main[, loss := as.integer(player_result == 0)]
main[, draw := as.integer(player_result == 0.5)]

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

setorder(main, player_name, event_id, round)
lag_cols <- c(
  "round", "player_result", "player_accuracy", "rating_diff100",
  "expected_score", "result_over_expected", "is_white",
  "opponent_rating", "player_rating", "score_c", "bubble_zone",
  "prize_zone", "eliminated_zone", "leader_zone", "game_id"
)
for (col in lag_cols) {
  main[, paste0("prev_", col) := shift(get(col)), by = .(player_name, event_id)]
}

lagged <- main[
  round > 1 &
    prev_round == round - 1 &
    !is.na(prev_player_result) &
    !is.na(prev_player_accuracy) &
    !is.na(prev_rating_diff100) &
    !is.na(prev_expected_score)
]

lagged[, `:=`(
  prev_loss = as.integer(prev_player_result == 0),
  prev_win = as.integer(prev_player_result == 1),
  prev_draw = as.integer(prev_player_result == 0.5),
  prev_unexpected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 >= 2),
  prev_expected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 <= -2),
  prev_even_loss = as.integer(prev_player_result == 0 & abs(prev_rating_diff100) < 2),
  prev_upset_win = as.integer(prev_player_result == 1 & prev_rating_diff100 <= -2),
  prev_expected_win = as.integer(prev_player_result == 1 & prev_rating_diff100 >= 2),
  prev_even_win = as.integer(prev_player_result == 1 & abs(prev_rating_diff100) < 2),
  prev_negative_shock = pmin(prev_result_over_expected, 0),
  prev_positive_shock = pmax(prev_result_over_expected, 0),
  prev_abs_shock = abs(prev_result_over_expected),
  prev_accuracy10 = (prev_player_accuracy - mean(prev_player_accuracy, na.rm = TRUE)) / 10,
  prev_score_c = prev_score_c
)]

lagged[, prev_result_type := fifelse(
  prev_unexpected_loss == 1, "unexpected_loss",
  fifelse(
    prev_expected_loss == 1, "expected_loss",
    fifelse(
      prev_even_loss == 1, "even_loss",
      fifelse(
        prev_draw == 1, "draw",
        fifelse(
          prev_upset_win == 1, "upset_win",
          fifelse(prev_even_win == 1, "even_win", "expected_win")
        )
      )
    )
  )
)]

fwrite(
  lagged[, .(
    rows = .N,
    players = uniqueN(player_name),
    events = uniqueN(event_id),
    current_games = uniqueN(game_id),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    min_round = min(round),
    max_round = max(round)
  )],
  file.path(output_dir, "sample_summary.csv")
)

fwrite(
  lagged[, .(
    rows = .N,
    mean_current_accuracy = mean(player_accuracy),
    mean_current_result = mean(player_result),
    mean_current_win = mean(win),
    mean_current_loss = mean(loss),
    mean_current_draw = mean(draw),
    mean_prev_accuracy = mean(prev_player_accuracy),
    mean_prev_expected_score = mean(prev_expected_score),
    mean_prev_result_over_expected = mean(prev_result_over_expected)
  ), by = prev_result_type][order(prev_result_type)],
  file.path(output_dir, "transition_descriptives_by_prev_result_type.csv")
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

contrast_terms <- function(model, term_a, term_b, label) {
  b <- coef(model)
  v <- vcov(model)
  if (!all(c(term_a, term_b) %in% names(b))) {
    return(data.table(
      contrast = label,
      term_a = term_a,
      term_b = term_b,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }
  est <- unname(b[term_a] - b[term_b])
  se <- sqrt(v[term_a, term_a] + v[term_b, term_b] - 2 * v[term_a, term_b])
  stat <- est / se
  data.table(
    contrast = label,
    term_a = term_a,
    term_b = term_b,
    estimate = est,
    std.error = se,
    statistic = stat,
    p.value = 2 * pnorm(abs(stat), lower.tail = FALSE)
  )
}

base_controls <- paste(
  "+ prev_player_accuracy + prev_expected_score + prev_rating_diff100 +",
  "prev_score_c + player_rating100 + opponent_rating100 + is_white + factor(round)"
)

fit_model <- function(data, outcome, rhs, fe, cluster) {
  fml <- as.formula(paste(outcome, "~", rhs, base_controls, "|", fe))
  feols(fml, data = data, cluster = cluster)
}

specs <- list(
  list(name = "event_fe", data = lagged, fe = "player_name + event_id", cluster = ~ player_name + event_id),
  list(name = "game_fe", data = lagged, fe = "player_name + game_id", cluster = ~ player_name + game_id),
  list(name = "round_gt2_event_fe", data = lagged[round > 2], fe = "player_name + event_id", cluster = ~ player_name + event_id),
  list(name = "near_window_pm12m", data = lagged[event_month >= -12 & event_month <= 6], fe = "player_name + event_id", cluster = ~ player_name + event_id)
)

main_hypotheses <- list(
  list(
    id = "L01_prev_loss_accuracy",
    family = "loss_carryover",
    outcome = "player_accuracy",
    rhs = "prev_loss",
    target = c("prev_loss"),
    story = "Does losing the previous game reduce next-game accuracy?"
  ),
  list(
    id = "L02_prev_loss_result",
    family = "loss_carryover",
    outcome = "player_result",
    rhs = "prev_loss",
    target = c("prev_loss"),
    story = "Does losing the previous game reduce next-game score?"
  ),
  list(
    id = "L03_negative_shock_accuracy",
    family = "result_surprise",
    outcome = "player_accuracy",
    rhs = "prev_negative_shock + prev_positive_shock",
    target = c("prev_negative_shock"),
    story = "Do worse-than-expected previous results reduce next-game accuracy?"
  ),
  list(
    id = "L04_positive_shock_accuracy",
    family = "result_surprise",
    outcome = "player_accuracy",
    rhs = "prev_negative_shock + prev_positive_shock",
    target = c("prev_positive_shock"),
    story = "Do better-than-expected previous results raise next-game accuracy?"
  )
)

main_rows <- list()
for (spec in specs) {
  for (h in main_hypotheses) {
    key <- paste(spec$name, h$id, sep = "__")
    main_rows[[key]] <- tryCatch({
      model <- fit_model(spec$data, h$outcome, h$rhs, spec$fe, spec$cluster)
      row <- extract_target(model, h$target)
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
main_tests <- rbindlist(main_rows, fill = TRUE)
main_tests <- ensure_p_value(main_tests)
main_tests[, p_bh_by_spec := p.adjust(p.value, method = "BH"), by = specification]
setorder(main_tests, specification, p.value)
fwrite(main_tests, file.path(output_dir, "lagged_loss_main_coefficients.csv"))

loss_type_rhs <- paste(
  "prev_expected_loss + prev_even_loss + prev_unexpected_loss +",
  "prev_draw + prev_upset_win + prev_even_win"
)
loss_type_rows <- list()
contrast_rows <- list()
for (spec in specs) {
  for (outcome in c("player_accuracy", "player_result", "win", "loss", "draw")) {
    key <- paste(spec$name, outcome, sep = "__")
    loss_type_rows[[key]] <- tryCatch({
      model <- fit_model(spec$data, outcome, loss_type_rhs, spec$fe, spec$cluster)
      tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
      keep <- c(
        "prev_expected_loss", "prev_even_loss", "prev_unexpected_loss",
        "prev_draw", "prev_upset_win", "prev_even_win"
      )
      rows <- tt[term %in% keep]
      rows[, `:=`(specification = spec$name, outcome = outcome, nobs = nobs(model))]
      contrast_rows[[paste(key, "unexpected_vs_expected_loss", sep = "__")]] <- contrast_terms(
        model,
        "prev_unexpected_loss",
        "prev_expected_loss",
        "unexpected_loss_minus_expected_loss"
      )[, `:=`(specification = spec$name, outcome = outcome, nobs = nobs(model))]
      contrast_rows[[paste(key, "unexpected_vs_even_loss", sep = "__")]] <- contrast_terms(
        model,
        "prev_unexpected_loss",
        "prev_even_loss",
        "unexpected_loss_minus_even_loss"
      )[, `:=`(specification = spec$name, outcome = outcome, nobs = nobs(model))]
      rows
    }, error = function(e) {
      data.table(specification = spec$name, outcome = outcome, error = e$message)
    })
  }
}
loss_type_tests <- rbindlist(loss_type_rows, fill = TRUE)
loss_type_tests <- ensure_p_value(loss_type_tests)
loss_type_tests[, p_bh_by_spec_outcome := p.adjust(p.value, method = "BH"),
                by = .(specification, outcome)]
setorder(loss_type_tests, specification, outcome, p.value)
fwrite(loss_type_tests, file.path(output_dir, "loss_type_coefficients.csv"))

loss_type_contrasts <- rbindlist(contrast_rows, fill = TRUE)
loss_type_contrasts <- ensure_p_value(loss_type_contrasts)
loss_type_contrasts[, p_bh_by_spec_outcome := p.adjust(p.value, method = "BH"),
                    by = .(specification, outcome)]
fwrite(loss_type_contrasts, file.path(output_dir, "loss_type_contrasts.csv"))

rule_rhs <- paste(
  "format_5_0 * prev_loss +",
  "format_5_0 * prev_unexpected_loss +",
  "format_5_0 * prev_expected_loss +",
  "format_5_0 * prev_upset_win"
)
rule_rows <- list()
for (outcome in c("player_accuracy", "player_result")) {
  model <- fit_model(
    lagged,
    outcome,
    rule_rhs,
    "player_name + event_id",
    ~ player_name + event_id
  )
  tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
  tt <- tt[grepl("format_5_0:", term, fixed = TRUE)]
  tt[, `:=`(outcome = outcome, nobs = nobs(model))]
  rule_rows[[outcome]] <- tt
}
rule_change_tests <- rbindlist(rule_rows, fill = TRUE)
rule_change_tests <- ensure_p_value(rule_change_tests)
rule_change_tests[, p_bh_by_outcome := p.adjust(p.value, method = "BH"), by = outcome]
setorder(rule_change_tests, outcome, p.value)
fwrite(rule_change_tests, file.path(output_dir, "rule_change_lag_interactions.csv"))

threshold_rows <- list()
for (threshold in c(100, 200, 300)) {
  tmp <- copy(lagged)
  cutoff <- threshold / 100
  tmp[, `:=`(
    prev_unexpected_loss_t = as.integer(prev_player_result == 0 & prev_rating_diff100 >= cutoff),
    prev_expected_loss_t = as.integer(prev_player_result == 0 & prev_rating_diff100 <= -cutoff),
    prev_upset_win_t = as.integer(prev_player_result == 1 & prev_rating_diff100 <= -cutoff),
    prev_expected_win_t = as.integer(prev_player_result == 1 & prev_rating_diff100 >= cutoff)
  )]
  rhs <- "prev_unexpected_loss_t + prev_expected_loss_t + prev_upset_win_t + prev_expected_win_t"
  for (outcome in c("player_accuracy", "player_result")) {
    key <- paste(threshold, outcome, sep = "__")
    threshold_rows[[key]] <- tryCatch({
      model <- fit_model(tmp, outcome, rhs, "player_name + event_id", ~ player_name + event_id)
      tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
      tt <- tt[term %in% c(
        "prev_unexpected_loss_t", "prev_expected_loss_t",
        "prev_upset_win_t", "prev_expected_win_t"
      )]
      tt[, `:=`(threshold = threshold, outcome = outcome, nobs = nobs(model))]
      tt
    }, error = function(e) {
      data.table(threshold = threshold, outcome = outcome, error = e$message)
    })
  }
}
threshold_sensitivity <- rbindlist(threshold_rows, fill = TRUE)
threshold_sensitivity <- ensure_p_value(threshold_sensitivity)
threshold_sensitivity[, p_bh_by_threshold_outcome := p.adjust(p.value, method = "BH"),
                      by = .(threshold, outcome)]
setorder(threshold_sensitivity, outcome, threshold, term)
fwrite(threshold_sensitivity, file.path(output_dir, "threshold_sensitivity_coefficients.csv"))

fake_cutoffs <- as.Date(c(
  "2023-03-01", "2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"
))
placebo_rows <- list()
pre_actual <- copy(lagged[date < rule_change_date])
for (cutoff in fake_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  rhs <- paste(
    "placebo_post * prev_loss +",
    "placebo_post * prev_unexpected_loss +",
    "placebo_post * prev_expected_loss +",
    "placebo_post * prev_upset_win"
  )
  for (outcome in c("player_accuracy", "player_result")) {
    key <- paste(cutoff, outcome, sep = "__")
    placebo_rows[[key]] <- tryCatch({
      model <- fit_model(pre_actual, outcome, rhs, "player_name + event_id", ~ player_name + event_id)
      tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
      tt <- tt[grepl("placebo_post:", term, fixed = TRUE)]
      tt[, `:=`(cutoff = cutoff, outcome = outcome, nobs = nobs(model))]
      tt
    }, error = function(e) {
      data.table(cutoff = cutoff, outcome = outcome, error = e$message)
    })
  }
}
placebo_tests <- rbindlist(placebo_rows, fill = TRUE)
placebo_tests <- ensure_p_value(placebo_tests)
placebo_tests[, p_bh_by_cutoff_outcome := p.adjust(p.value, method = "BH"),
              by = .(cutoff, outcome)]
fwrite(placebo_tests, file.path(output_dir, "placebo_rule_change_lag_interactions.csv"))

event_rows <- list()
event_sample <- lagged[event_month >= -18 & event_month <= 6]
for (x in c("prev_loss", "prev_unexpected_loss", "prev_expected_loss", "prev_upset_win")) {
  fml <- as.formula(paste(
    "player_accuracy ~ i(event_month,",
    x,
    ", ref = -1) +",
    sub("^\\+ ", "", base_controls),
    "| player_name + event_id"
  ))
  event_rows[[x]] <- tryCatch({
    model <- feols(fml, data = event_sample, cluster = ~ player_name + event_id)
    tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
    tt <- tt[grepl("event_month::", term, fixed = TRUE)]
    tt[, `:=`(
      variable = x,
      event_month = as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term)),
      nobs = nobs(model)
    )]
    tt
  }, error = function(e) {
    data.table(variable = x, error = e$message)
  })
}
event_study <- rbindlist(event_rows, fill = TRUE)
setorder(event_study, variable, event_month)
fwrite(event_study, file.path(output_dir, "event_study_lagged_accuracy_coefficients.csv"))

event_plot <- ggplot(event_study[!is.na(event_month)], aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.35, color = "#2364aa") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Lagged-result effect on current accuracy relative to month -1",
    title = "Event-study: previous result and next-game accuracy"
  ) +
  theme_minimal(base_size = 10)
ggsave(
  file.path(output_dir, "event_study_lagged_accuracy.png"),
  event_plot,
  width = 11,
  height = 7,
  dpi = 200
)

effect_sizes <- rbindlist(list(
  lagged[, .(
    variable = "prev_loss",
    mean = mean(prev_loss),
    sd = sd(prev_loss),
    p25 = quantile(prev_loss, 0.25),
    p75 = quantile(prev_loss, 0.75),
    iqr = IQR(prev_loss)
  )],
  lagged[, .(
    variable = "prev_unexpected_loss",
    mean = mean(prev_unexpected_loss),
    sd = sd(prev_unexpected_loss),
    p25 = quantile(prev_unexpected_loss, 0.25),
    p75 = quantile(prev_unexpected_loss, 0.75),
    iqr = IQR(prev_unexpected_loss)
  )],
  lagged[, .(
    variable = "prev_expected_loss",
    mean = mean(prev_expected_loss),
    sd = sd(prev_expected_loss),
    p25 = quantile(prev_expected_loss, 0.25),
    p75 = quantile(prev_expected_loss, 0.75),
    iqr = IQR(prev_expected_loss)
  )],
  lagged[, .(
    variable = "prev_result_over_expected",
    mean = mean(prev_result_over_expected),
    sd = sd(prev_result_over_expected),
    p25 = quantile(prev_result_over_expected, 0.25),
    p75 = quantile(prev_result_over_expected, 0.75),
    iqr = IQR(prev_result_over_expected)
  )]
), fill = TRUE)
fwrite(effect_sizes, file.path(output_dir, "effect_variable_distributions.csv"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
