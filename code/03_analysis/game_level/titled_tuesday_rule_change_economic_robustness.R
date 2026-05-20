library(dplyr)
library(fixest)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)

fixest::setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/titled_tuesday_rule_change_economic"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

clean_term <- function(x) {
  str_replace_all(x, "`", "")
}

center <- function(x) {
  x - mean(x, na.rm = TRUE)
}

extract_target <- function(model, target_terms) {
  tidy_out <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(term = clean_term(term))

  hit <- tidy_out %>%
    filter(term %in% target_terms) %>%
    slice(1)

  if (nrow(hit) == 0) {
    return(tibble(
      target_term = paste(target_terms, collapse = " OR "),
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }

  hit %>%
    transmute(target_term = term, estimate, std.error, conf.low, conf.high, p.value)
}

build_controls <- function(variable) {
  controls <- c("player_rating", "opponent_rating", "is_white", "i(round)")
  if (variable == "is_white") {
    controls <- setdiff(controls, "is_white")
  }
  controls
}

fit_model <- function(data, outcome, variable, model_type) {
  controls <- build_controls(variable)

  if (model_type == "local_baseline") {
    rhs <- paste(c(paste0("format_5_0 * ", variable), controls), collapse = " + ")
    target_terms <- c(paste0("format_5_0:", variable), paste0(variable, ":format_5_0"))
  } else if (model_type == "local_trend_adjusted") {
    rhs <- paste(c(
      paste0("format_5_0 * ", variable),
      paste0("event_month:", variable),
      controls
    ), collapse = " + ")
    target_terms <- c(paste0("format_5_0:", variable), paste0(variable, ":format_5_0"))
  } else if (model_type == "pretrend_only") {
    rhs <- paste(c(paste0("event_month:", variable), controls), collapse = " + ")
    target_terms <- c(paste0("event_month:", variable), paste0(variable, ":event_month"))
  } else {
    stop("Unknown model_type")
  }

  d <- data %>%
    filter(!is.na(.data[[variable]]), is.finite(.data[[variable]]))

  fit <- feols(
    as.formula(paste0(outcome, " ~ ", rhs, " | player_name + tournament_id")),
    data = d,
    cluster = ~ player_name + tournament_id
  )

  extract_target(fit, target_terms) %>%
    mutate(
      outcome = outcome,
      variable = variable,
      model_type = model_type,
      nobs = nobs(fit),
      r2_within = fixest::fitstat(fit, "wr2")[[1]],
      .before = target_term
    )
}

message("Reading data for robustness checks...")
df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

df_players <- df_raw %>%
  mutate(
    tournament_id = date,
    tournament_date = as.Date(date),
    tournament_hour = as.integer(str_sub(date, 12, 13)),
    event_slot = if_else(tournament_hour <= 10, "first", "second"),
    format_5_0 = as.integer(tournament_date >= as.Date("2025-09-01")),
    date_month = as.integer(format(tournament_date, "%Y")) * 12 +
      as.integer(format(tournament_date, "%m")),
    event_month = date_month - (2025 * 12 + 9),
    rating_diff_100 = (player_rating - opponent_rating) / 100,
    player_rating_100c = center(player_rating) / 100,
    online_premium_100 = (player_rating - blitz_rating) / 100,
    classical_premium_100 = (classic_rating - blitz_rating) / 100,
    final_score_pregame_c = center(final_score_pregame),
    tiebreak_avg_opp_score_c = center(opponents_sum_score / pmax(round - 1, 1))
  ) %>%
  group_by(tournament_id) %>%
  mutate(
    field_size = max(rank, na.rm = TRUE),
    rank_pct = if_else(field_size > 1, (rank - 1) / (field_size - 1), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(rank_pct_10c = center(rank_pct) / 0.10) %>%
  filter(
    event_slot == "first",
    event_month >= -18,
    event_month <= 6,
    player_title != "No Title",
    round > 1,
    !is.na(player_accuracy),
    player_accuracy != 0,
    player_accuracy != 100,
    !is.na(player_result),
    player_rating > 0,
    opponent_rating > 0,
    !is.na(player_name),
    !is.na(tournament_id)
  )

robustness_vars <- tribble(
  ~variable, ~mechanism,
  "online_premium_100", "Online platform-specific skill",
  "rating_diff_100", "Favorite advantage",
  "classical_premium_100", "Classical chess-skill premium",
  "player_rating_100c", "Elite skill lowers variance",
  "is_white", "First-mover advantage",
  "tiebreak_avg_opp_score_c", "Strength-of-schedule pressure",
  "bubble", "Bubble pressure",
  "eliminated", "Eliminated-player opportunity cost",
  "final_score_pregame_c", "Standing-contingent incentives",
  "rank_pct_10c", "Rank-gradient incentives"
)

grid <- tidyr::crossing(
  outcome = c("player_accuracy", "player_result"),
  robustness_vars,
  model_type = c("local_baseline", "local_trend_adjusted", "pretrend_only")
)

message("Fitting ", nrow(grid), " trend robustness models...")
trend_robustness <- pmap_dfr(
  grid,
  function(outcome, variable, mechanism, model_type) {
    data <- if (model_type == "pretrend_only") {
      df_players %>% filter(event_month < 0)
    } else {
      df_players
    }

    fit_model(data, outcome, variable, model_type) %>%
      mutate(mechanism = mechanism, .after = variable)
  }
) %>%
  group_by(outcome, model_type) %>%
  mutate(q.value = if_else(is.na(p.value), NA_real_, p.adjust(p.value, method = "BH"))) %>%
  ungroup() %>%
  mutate(
    estimate = round(estimate, 5),
    std.error = round(std.error, 5),
    conf.low = round(conf.low, 5),
    conf.high = round(conf.high, 5),
    p.value = round(p.value, 5),
    q.value = round(q.value, 5),
    r2_within = round(r2_within, 5)
  ) %>%
  arrange(outcome, variable, model_type)

write_csv(trend_robustness, file.path(OUT_DIR, "trend_robustness_top_mechanisms.csv"))

wide <- trend_robustness %>%
  select(outcome, variable, mechanism, model_type, estimate, std.error, p.value, q.value, nobs) %>%
  pivot_wider(
    names_from = model_type,
    values_from = c(estimate, std.error, p.value, q.value, nobs),
    names_glue = "{model_type}_{.value}"
  )

write_csv(wide, file.path(OUT_DIR, "trend_robustness_top_mechanisms_wide.csv"))

top_lines <- wide %>%
  arrange(outcome, local_trend_adjusted_q.value) %>%
  mutate(line = paste0(
    "- ", outcome, " / ", mechanism,
    ": local baseline = ", local_baseline_estimate,
    " (p ", local_baseline_p.value,
    "), pretrend = ", pretrend_only_estimate,
    " (p ", pretrend_only_p.value,
    "), trend-adjusted break = ", local_trend_adjusted_estimate,
    " (p ", local_trend_adjusted_p.value,
    ", q ", local_trend_adjusted_q.value,
    ")."
  )) %>%
  pull(line)

writeLines(
  c(
    "# Economic Mechanism Trend Robustness",
    "",
    "- Sample: first Titled Tuesday event only, event months -18 to +6 around September 2025.",
    "- Fixed effects: player and exact tournament timestamp.",
    "- Each row compares the local baseline treatment interaction, the pre-period linear trend in that interaction gradient, and the treatment break after adding `event_month:variable`.",
    "",
    top_lines
  ),
  file.path(OUT_DIR, "trend_robustness_findings.md")
)

message("Done. Outputs written to: ", OUT_DIR)
