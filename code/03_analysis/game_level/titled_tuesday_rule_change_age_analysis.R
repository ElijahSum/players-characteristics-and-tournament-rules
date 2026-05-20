library(dplyr)
library(fixest)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(modelsummary)

fixest::setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/titled_tuesday_rule_change_age"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

clean_term <- function(x) {
  str_replace_all(x, "`", "")
}

model_nobs <- function(model_object) {
  nobs(model_object)
}

extract_term <- function(model, term, hypothesis, outcome, model_name, interpretation) {
  out <- broom::tidy(model, conf.int = TRUE)
  out$term <- clean_term(out$term)
  term_candidates <- term
  hit <- out %>% filter(term %in% term_candidates)
  n_obs <- model_nobs(model)
  within_r2 <- fixest::fitstat(model, "wr2")[[1]]

  if (nrow(hit) == 0) {
    return(tibble(
      hypothesis = hypothesis,
      outcome = outcome,
      model = model_name,
      target_term = paste(term_candidates, collapse = " OR "),
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      nobs = n_obs,
      r2_within = within_r2,
      interpretation = paste0("Target term not estimated or was removed for collinearity. ", interpretation)
    ))
  }

  hit <- hit %>% slice(1)

  hit %>%
    transmute(
      hypothesis = hypothesis,
      outcome = outcome,
      model = model_name,
      target_term = term,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value,
      nobs = n_obs,
      r2_within = within_r2,
      interpretation = interpretation
    )
}

support_label <- function(estimate, p_value) {
  case_when(
    is.na(estimate) | is.na(p_value) ~ "not estimated",
    p_value < 0.01 & estimate > 0 ~ "strong positive",
    p_value < 0.05 & estimate > 0 ~ "positive",
    p_value < 0.10 & estimate > 0 ~ "weak positive",
    p_value < 0.01 & estimate < 0 ~ "strong negative",
    p_value < 0.05 & estimate < 0 ~ "negative",
    p_value < 0.10 & estimate < 0 ~ "weak negative",
    TRUE ~ "not statistically clear"
  )
}

fit_hypothesis <- function(data, outcome, rhs, target_term, hypothesis, model_name, interpretation) {
  fml <- as.formula(paste0(
    outcome,
    " ~ ",
    rhs,
    " + player_rating + opponent_rating + is_white + i(round) | player_name + date"
  ))

  fit <- feols(
    fml,
    data = data,
    cluster = ~ player_name + date
  )

  list(
    model = fit,
    result = extract_term(
      model = fit,
      term = target_term,
      hypothesis = hypothesis,
      outcome = outcome,
      model_name = model_name,
      interpretation = interpretation
    )
  )
}

message("Reading data: ", DATA_PATH)
df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

df_players <- df_raw %>%
  mutate(
    date = as.Date(date),
    format_5_0 = as.integer(date >= as.Date("2025-09-01")),
    birth_year = as.integer(birthday),
    tournament_year = as.integer(format(date, "%Y")),
    age = tournament_year - birth_year,
    age10_c = (age - 35) / 10,
    rating_diff_100 = (player_rating - opponent_rating) / 100,
    player_rating_100c = (player_rating - 2600) / 100,
    opponent_rating_100c = (opponent_rating - 2600) / 100,
    round_c = round - mean(round, na.rm = TRUE),
    final_score_pregame_c = final_score_pregame - mean(final_score_pregame, na.rm = TRUE),
    rank_100c = (rank - mean(rank, na.rm = TRUE)) / 100,
    gdp_log_c = gdp_per_capita_ppp_logged - mean(gdp_per_capita_ppp_logged, na.rm = TRUE),
    is_gm = as.integer(player_title == "GM"),
    is_im = as.integer(player_title == "IM"),
    is_fm = as.integer(player_title == "FM"),
    is_w_title = as.integer(player_title %in% c("WCM", "WFM", "WGM", "WIM", "WNM")),
    high_rating = as.integer(player_rating >= 2800),
    late_round = as.integer(round >= 8),
    top_score_pregame = as.integer(final_score_pregame >= 6),
    date_month = as.integer(format(date, "%Y")) * 12 + as.integer(format(date, "%m")),
    event_month = date_month - (2025 * 12 + 9),
    age_group = case_when(
      age < 25 ~ "under_25",
      age < 35 ~ "25_34",
      age < 45 ~ "35_44",
      TRUE ~ "45_plus"
    )
  ) %>%
  filter(
    player_title != "No Title",
    round > 1,
    !is.na(player_accuracy),
    player_accuracy != 0,
    player_accuracy != 100,
    !is.na(player_result),
    !is.na(age),
    age >= 10,
    age <= 90,
    player_rating > 0,
    opponent_rating > 0,
    !is.na(player_name),
    !is.na(date)
  )

sample_summary <- tibble(
  metric = c(
    "rows",
    "unique_players",
    "unique_tournaments",
    "pre_rows",
    "post_rows",
    "first_date",
    "last_date",
    "round_filter"
  ),
  value = c(
    as.character(nrow(df_players)),
    as.character(n_distinct(df_players$player_name)),
    as.character(n_distinct(df_players$date)),
    as.character(sum(df_players$format_5_0 == 0)),
    as.character(sum(df_players$format_5_0 == 1)),
    as.character(min(df_players$date)),
    as.character(max(df_players$date)),
    "round > 1"
  )
)

write_csv(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

descriptive_means <- df_players %>%
  group_by(format_5_0, age_group) %>%
  summarise(
    rows = n(),
    players = n_distinct(player_name),
    avg_age = mean(age),
    avg_accuracy = mean(player_accuracy),
    avg_result = mean(player_result),
    avg_player_rating = mean(player_rating),
    avg_opponent_rating = mean(opponent_rating),
    avg_rating_diff = mean(player_rating - opponent_rating),
    .groups = "drop"
  ) %>%
  arrange(format_5_0, age_group)

write_csv(descriptive_means, file.path(OUT_DIR, "descriptive_means_by_age_group.csv"))

# A direct felm benchmark in the style of chess_2026.R. The richer hypothesis
# battery below uses fixest because it is faster and handles multi-way clustered
# standard errors cleanly.
model1_felm <- lfe::felm(
  player_accuracy ~ player_rating + is_white + format_5_0 * age + opponent_rating |
    0 | 0 | player_name,
  data = df_players
)

capture.output(
  summary(model1_felm),
  file = file.path(OUT_DIR, "model1_felm_style_summary.txt")
)

base_controls <- "format_5_0 * age10_c"
model_specs <- tribble(
  ~hypothesis, ~model_name, ~rhs, ~target_term, ~interpretation,
  "H1: Older players' accuracy changed differently after the rule switch.",
  "baseline_age_treatment_round2plus",
  "format_5_0 * age10_c",
  "format_5_0:age10_c",
  "Effect of the new rules for players 10 years older, relative to younger players, in accuracy points.",

  "H2: Older players' game results changed differently after the rule switch.",
  "baseline_age_treatment_result_round2plus",
  "format_5_0 * age10_c",
  "format_5_0:age10_c",
  "Effect of the new rules for players 10 years older, relative to younger players, in game-score points.",

  "H3: The age effect is larger in later/fatigue rounds.",
  "age_treatment_late_round",
  "format_5_0 * age10_c * late_round",
  "format_5_0:age10_c:late_round",
  "Additional post-rule age effect in rounds 8-11 versus rounds 2-7.",

  "H4: The age effect changes as rounds progress linearly.",
  "age_treatment_round_slope",
  "format_5_0 * age10_c * round_c",
  "format_5_0:age10_c:round_c",
  "Additional post-rule age effect for each later round.",

  "H5: The age effect depends on color.",
  "age_treatment_white",
  "format_5_0 * age10_c * is_white",
  "format_5_0:age10_c:is_white",
  "Additional post-rule age effect when the player has White.",

  "H6: The age effect depends on rating mismatch.",
  "age_treatment_rating_diff",
  "format_5_0 * age10_c * rating_diff_100",
  "format_5_0:age10_c:rating_diff_100",
  "Additional post-rule age effect for each 100 Elo advantage over the opponent.",

  "H7: The age effect differs for very high-rated online players.",
  "age_treatment_high_rating",
  "format_5_0 * age10_c * high_rating",
  "format_5_0:age10_c:high_rating",
  "Additional post-rule age effect for players rated 2800+ on Chess.com.",

  "H8: The age effect differs for GMs.",
  "age_treatment_gm",
  "format_5_0 * age10_c * is_gm",
  "format_5_0:age10_c:is_gm",
  "Additional post-rule age effect among GMs versus other titled players.",

  "H9: The age effect differs for women's titles.",
  "age_treatment_w_title",
  "format_5_0 * age10_c * is_w_title",
  "format_5_0:age10_c:is_w_title",
  "Additional post-rule age effect among players with WCM/WFM/WGM/WIM/WNM titles.",

  "H10: The age effect is different for players already in prize positions.",
  "age_treatment_in_prizes",
  "format_5_0 * age10_c * in_prizes",
  "format_5_0:age10_c:in_prizes",
  "Additional post-rule age effect when the player entered the game in a prize rank.",

  "H11: The age effect is different for bubble players.",
  "age_treatment_bubble",
  "format_5_0 * age10_c * bubble",
  "format_5_0:age10_c:bubble",
  "Additional post-rule age effect when the player entered the game on the prize bubble.",

  "H12: The age effect is different for tournament leaders.",
  "age_treatment_leader",
  "format_5_0 * age10_c * leader",
  "format_5_0:age10_c:leader",
  "Additional post-rule age effect when the player entered the game as tournament leader.",

  "H13: The age effect is different for eliminated players.",
  "age_treatment_eliminated",
  "format_5_0 * age10_c * eliminated",
  "format_5_0:age10_c:eliminated",
  "Additional post-rule age effect when the player entered the game effectively eliminated.",

  "H14: The age effect depends on pregame score.",
  "age_treatment_pregame_score",
  "format_5_0 * age10_c * final_score_pregame_c",
  "format_5_0:age10_c:final_score_pregame_c",
  "Additional post-rule age effect for each extra point of pregame score.",

  "H15: The age effect depends on current rank.",
  "age_treatment_rank",
  "format_5_0 * age10_c * rank_100c",
  "format_5_0:age10_c:rank_100c",
  "Additional post-rule age effect for each 100 places worse current rank.",

  "H16: The age effect depends on country income.",
  "age_treatment_gdp",
  "format_5_0 * age10_c * gdp_log_c",
  "format_5_0:age10_c:gdp_log_c",
  "Additional post-rule age effect for each log-point higher GDP per capita PPP.",

  "H17: Older players changed more when facing prize-position opponents.",
  "age_treatment_vs_prizes",
  "format_5_0 * age10_c * played_against_prizes",
  "format_5_0:age10_c:played_against_prizes",
  "Additional post-rule age effect when the opponent entered the game in a prize rank.",

  "H18: Older players changed more when facing bubble opponents.",
  "age_treatment_vs_bubble",
  "format_5_0 * age10_c * played_against_bubble",
  "format_5_0:age10_c:played_against_bubble",
  "Additional post-rule age effect when the opponent entered the game on the prize bubble.",

  "H19: Older players changed more when facing eliminated opponents.",
  "age_treatment_vs_eliminated",
  "format_5_0 * age10_c * played_against_eliminated",
  "format_5_0:age10_c:played_against_eliminated",
  "Additional post-rule age effect when the opponent entered the game effectively eliminated.",

  "H20: Older players changed more when facing tournament leaders.",
  "age_treatment_vs_leader",
  "format_5_0 * age10_c * played_against_leader",
  "format_5_0:age10_c:played_against_leader",
  "Additional post-rule age effect when the opponent entered the game as tournament leader."
)

message("Fitting hypothesis regressions on round > 1 sample...")
accuracy_specs <- model_specs %>%
  filter(model_name != "baseline_age_treatment_result_round2plus")

accuracy_fits <- pmap(
  accuracy_specs,
  function(hypothesis, model_name, rhs, target_term, interpretation) {
    fit_hypothesis(
      data = df_players,
      outcome = "player_accuracy",
      rhs = rhs,
      target_term = target_term,
      hypothesis = hypothesis,
      model_name = model_name,
      interpretation = interpretation
    )
  }
)

result_fit <- fit_hypothesis(
  data = df_players,
  outcome = "player_result",
  rhs = "format_5_0 * age10_c",
  target_term = "format_5_0:age10_c",
  hypothesis = "H2: Older players' game results changed differently after the rule switch.",
  model_name = "baseline_age_treatment_result_round2plus",
  interpretation = "Effect of the new rules for players 10 years older, relative to younger players, in game-score points."
)

all_fits <- c(accuracy_fits, list(result_fit))
hypothesis_results_raw <- bind_rows(map(all_fits, "result"))
models <- setNames(map(all_fits, "model"), hypothesis_results_raw$model)
hypothesis_results <- hypothesis_results_raw %>%
  mutate(
    support = support_label(estimate, p.value),
    estimate = round(estimate, 5),
    std.error = round(std.error, 5),
    conf.low = round(conf.low, 5),
    conf.high = round(conf.high, 5),
    p.value = round(p.value, 5),
    r2_within = round(r2_within, 5)
  )

write_csv(hypothesis_results, file.path(OUT_DIR, "hypothesis_tests.csv"))

all_coefficients <- imap_dfr(models, function(model, model_name) {
  n_obs <- model_nobs(model)
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      model = model_name,
      term = clean_term(term),
      nobs = n_obs
    ) %>%
    select(model, term, estimate, std.error, conf.low, conf.high, p.value, nobs)
})

write_csv(all_coefficients, file.path(OUT_DIR, "all_model_coefficients.csv"))

message("Fitting round > 2 sensitivity for the baseline age-treatment model...")
round3_fit <- fit_hypothesis(
  data = df_players %>% filter(round > 2),
  outcome = "player_accuracy",
  rhs = "format_5_0 * age10_c",
  target_term = "format_5_0:age10_c",
  hypothesis = "Sensitivity: H1 excluding rounds 1 and 2.",
  model_name = "baseline_age_treatment_round3plus",
  interpretation = "Round > 2 sensitivity for the post-rule age effect."
)

write_csv(round3_fit$result, file.path(OUT_DIR, "round_gt2_baseline_sensitivity.csv"))

message("Fitting event-month age-gradient model...")
event_df <- df_players %>%
  filter(event_month >= -18, event_month <= 6)

event_model <- feols(
  player_accuracy ~ i(event_month, age10_c, ref = -1) +
    player_rating + opponent_rating + is_white + i(round) |
    player_name + date,
  data = event_df,
  cluster = ~ player_name + date
)

event_coefficients <- broom::tidy(event_model, conf.int = TRUE) %>%
  mutate(
    term = clean_term(term),
    event_month = as.integer(str_match(term, "event_month::(-?[0-9]+):age10_c")[, 2])
  ) %>%
  filter(!is.na(event_month)) %>%
  arrange(event_month)

write_csv(event_coefficients, file.path(OUT_DIR, "event_month_age_coefficients.csv"))

pretrend_model <- feols(
  player_accuracy ~ event_month:age10_c +
    player_rating + opponent_rating + is_white + i(round) |
    player_name + date,
  data = event_df %>% filter(event_month < 0),
  cluster = ~ player_name + date
)

pretrend_result <- extract_term(
  model = pretrend_model,
  term = c("age10_c:event_month", "event_month:age10_c"),
  hypothesis = "Pre-trend check: older-player accuracy gradient before September 2025.",
  outcome = "player_accuracy",
  model_name = "pretrend_age_gradient",
  interpretation = "Linear monthly change in the older-player accuracy gradient before the rule switch."
) %>%
  mutate(
    support = support_label(estimate, p.value),
    estimate = round(estimate, 5),
    std.error = round(std.error, 5),
    conf.low = round(conf.low, 5),
    conf.high = round(conf.high, 5),
    p.value = round(p.value, 5),
    r2_within = round(r2_within, 5)
  )

write_csv(pretrend_result, file.path(OUT_DIR, "pretrend_check.csv"))

message("Fitting age-trend adjusted robustness models...")
local_baseline_model <- feols(
  player_accuracy ~ format_5_0 * age10_c +
    player_rating + opponent_rating + is_white + i(round) |
    player_name + date,
  data = event_df,
  cluster = ~ player_name + date
)

local_trend_adjusted_model <- feols(
  player_accuracy ~ format_5_0 * age10_c + event_month:age10_c +
    player_rating + opponent_rating + is_white + i(round) |
    player_name + date,
  data = event_df,
  cluster = ~ player_name + date
)

full_trend_adjusted_model <- feols(
  player_accuracy ~ format_5_0 * age10_c + event_month:age10_c +
    player_rating + opponent_rating + is_white + i(round) |
    player_name + date,
  data = df_players,
  cluster = ~ player_name + date
)

trend_robustness <- bind_rows(
  extract_term(
    model = local_baseline_model,
    term = "format_5_0:age10_c",
    hypothesis = "Robustness: baseline age-treatment effect in the [-18, +6] event-month window.",
    outcome = "player_accuracy",
    model_name = "local_window_baseline_no_age_trend",
    interpretation = "Baseline post-rule age effect in the local event window without an age-specific linear time trend."
  ),
  extract_term(
    model = local_trend_adjusted_model,
    term = "format_5_0:age10_c",
    hypothesis = "Robustness: local age-treatment effect after controlling a linear age-gradient trend.",
    outcome = "player_accuracy",
    model_name = "local_window_linear_age_trend",
    interpretation = "Post-rule age-gradient break in the local event window after controlling `event_month:age10_c`."
  ),
  extract_term(
    model = full_trend_adjusted_model,
    term = "format_5_0:age10_c",
    hypothesis = "Robustness: full-sample age-treatment effect after controlling a linear age-gradient trend.",
    outcome = "player_accuracy",
    model_name = "full_sample_linear_age_trend",
    interpretation = "Post-rule age-gradient break in the full sample after controlling `event_month:age10_c`."
  )
) %>%
  mutate(
    support = support_label(estimate, p.value),
    estimate = round(estimate, 5),
    std.error = round(std.error, 5),
    conf.low = round(conf.low, 5),
    conf.high = round(conf.high, 5),
    p.value = round(p.value, 5),
    r2_within = round(r2_within, 5)
  )

write_csv(trend_robustness, file.path(OUT_DIR, "trend_adjusted_robustness.csv"))

etable(
  models[1:min(length(models), 8)],
  file = file.path(OUT_DIR, "selected_models_etable.txt"),
  replace = TRUE
)

modelsummary(
  models[1:min(length(models), 8)],
  output = file.path(OUT_DIR, "selected_models.html"),
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "IC|Log|RMSE"
)

top_findings <- hypothesis_results %>%
  filter(!is.na(p.value)) %>%
  arrange(p.value) %>%
  slice_head(n = 10) %>%
  mutate(
    line = paste0(
      "- ", model, ": ", support,
      ", estimate = ", estimate,
      ", SE = ", std.error,
      ", p = ", p.value,
      ", term = `", target_term, "`."
    )
  ) %>%
  pull(line)

baseline_row <- hypothesis_results %>%
  filter(model == "baseline_age_treatment_round2plus") %>%
  slice(1)

sensitivity_row <- round3_fit$result %>%
  mutate(
    support = support_label(estimate, p.value),
    estimate = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value = round(p.value, 5)
  )

local_baseline_row <- trend_robustness %>%
  filter(model == "local_window_baseline_no_age_trend") %>%
  slice(1)

local_trend_row <- trend_robustness %>%
  filter(model == "local_window_linear_age_trend") %>%
  slice(1)

full_trend_row <- trend_robustness %>%
  filter(model == "full_sample_linear_age_trend") %>%
  slice(1)

findings <- c(
  "# Titled Tuesday Rule Change: Age-Interaction Regressions",
  "",
  paste0("- Data: `", DATA_PATH, "`."),
  "- Treatment: `format_5_0 = 1` for tournaments on or after 2025-09-01.",
  "- Sample: titled players only, valid accuracy only, ratings > 0, non-missing age, and `round > 1`.",
  paste0("- Estimation sample rows: ", sample_summary$value[sample_summary$metric == "rows"], "."),
  "- Main specification: `player_accuracy ~ format_5_0 * age10_c * variable + player_rating + opponent_rating + is_white + i(round) | player_name + date`, clustered by player and date.",
  "- `age10_c` is age centered at 35 and scaled by 10, so age interactions are per 10 years older.",
  "",
  "## Main Baseline",
  paste0(
    "- Baseline post-rule age coefficient: ",
    baseline_row$estimate,
    " accuracy points per 10 years older (SE ",
    baseline_row$std.error,
    ", p ",
    baseline_row$p.value,
    ")."
  ),
  paste0(
    "- Round > 2 sensitivity: ",
    round(sensitivity_row$estimate, 5),
    " accuracy points per 10 years older (SE ",
    round(sensitivity_row$std.error, 5),
    ", p ",
    round(sensitivity_row$p.value, 5),
    ")."
  ),
  paste0(
    "- Local event-window baseline [-18,+6 months]: ",
    local_baseline_row$estimate,
    " accuracy points per 10 years older (SE ",
    local_baseline_row$std.error,
    ", p ",
    local_baseline_row$p.value,
    ")."
  ),
  "",
  "## Strongest Targeted Hypothesis Tests",
  top_findings,
  "",
  "## Pre-Trend Check",
  paste0(
    "- Pre-rule monthly age-gradient trend: ",
    pretrend_result$estimate,
    " accuracy points per 10 years older per month (SE ",
    pretrend_result$std.error,
    ", p ",
    pretrend_result$p.value,
    ")."
  ),
  paste0(
    "- After adding a local linear age-gradient trend, the post-rule break is ",
    local_trend_row$estimate,
    " (SE ",
    local_trend_row$std.error,
    ", p ",
    local_trend_row$p.value,
    ")."
  ),
  paste0(
    "- In the full sample with the same age-gradient trend control, the post-rule break is ",
    full_trend_row$estimate,
    " (SE ",
    full_trend_row$std.error,
    ", p ",
    full_trend_row$p.value,
    ")."
  ),
  "",
  "## Output Files",
  "- `sample_summary.csv`",
  "- `descriptive_means_by_age_group.csv`",
  "- `hypothesis_tests.csv`",
  "- `all_model_coefficients.csv`",
  "- `round_gt2_baseline_sensitivity.csv`",
  "- `event_month_age_coefficients.csv`",
  "- `pretrend_check.csv`",
  "- `trend_adjusted_robustness.csv`",
  "- `selected_models.html`"
)

writeLines(findings, file.path(OUT_DIR, "findings.md"))

message("Done. Outputs written to: ", OUT_DIR)
