library(dplyr)
library(fixest)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)

fixest::setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
CAPITAL_TIMES_PATH <- "capital_times.csv"
OUT_DIR <- "analysis_outputs/titled_tuesday_rule_change_economic"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

clean_term <- function(x) {
  str_replace_all(x, "`", "")
}

parse_hour <- function(x) {
  x <- as.character(x)
  parts <- str_split_fixed(x, ":", 3)
  as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
}

center <- function(x) {
  x - mean(x, na.rm = TRUE)
}

distance_from_hour <- function(hour, target_hour) {
  raw_distance <- abs(hour - target_hour)
  pmin(raw_distance, 24 - raw_distance)
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

prediction_label <- function(estimate, q_value, predicted_sign) {
  case_when(
    is.na(estimate) | is.na(q_value) ~ "not estimated",
    predicted_sign == "ambiguous" & q_value < 0.10 ~ "clear exploratory pattern",
    predicted_sign == "ambiguous" ~ "exploratory, not clear",
    predicted_sign == "positive" & estimate > 0 & q_value < 0.10 ~ "supports prediction",
    predicted_sign == "negative" & estimate < 0 & q_value < 0.10 ~ "supports prediction",
    q_value < 0.10 ~ "opposes prediction",
    TRUE ~ "not clear after FDR"
  )
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
    transmute(
      target_term = term,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )
}

build_formula <- function(outcome, variable) {
  base_controls <- c("player_rating", "opponent_rating", "is_white", "i(round)")

  if (variable == "is_white") {
    base_controls <- setdiff(base_controls, "is_white")
  }

  rhs <- paste(c(paste0("format_5_0 * ", variable), base_controls), collapse = " + ")
  as.formula(paste0(outcome, " ~ ", rhs, " | player_name + tournament_id"))
}

fit_interaction <- function(data, sample_name, outcome, variable, hypothesis_id, mechanism,
                            predicted_sign, rationale) {
  target_terms <- c(
    paste0("format_5_0:", variable),
    paste0(variable, ":format_5_0")
  )

  d <- data %>%
    filter(!is.na(.data[[variable]]), is.finite(.data[[variable]]))

  if (nrow(d) < 10000 || n_distinct(d$format_5_0) < 2) {
    return(list(
      result = tibble(
        sample = sample_name,
        outcome = outcome,
        hypothesis_id = hypothesis_id,
        mechanism = mechanism,
        variable = variable,
        predicted_sign = predicted_sign,
        rationale = rationale,
        target_term = paste(target_terms, collapse = " OR "),
        estimate = NA_real_,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        nobs = nrow(d),
        r2_within = NA_real_,
        support = "not estimated"
      ),
      coefficients = tibble()
    ))
  }

  model <- tryCatch(
    feols(
      build_formula(outcome, variable),
      data = d,
      cluster = ~ player_name + tournament_id
    ),
    error = function(e) e
  )

  if (inherits(model, "error")) {
    return(list(
      result = tibble(
        sample = sample_name,
        outcome = outcome,
        hypothesis_id = hypothesis_id,
        mechanism = mechanism,
        variable = variable,
        predicted_sign = predicted_sign,
        rationale = rationale,
        target_term = paste(target_terms, collapse = " OR "),
        estimate = NA_real_,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        nobs = nrow(d),
        r2_within = NA_real_,
        support = paste("model error:", model$message)
      ),
      coefficients = tibble()
    ))
  }

  target <- extract_target(model, target_terms)
  result <- target %>%
    mutate(
      sample = sample_name,
      outcome = outcome,
      hypothesis_id = hypothesis_id,
      mechanism = mechanism,
      variable = variable,
      predicted_sign = predicted_sign,
      rationale = rationale,
      nobs = nobs(model),
      r2_within = fixest::fitstat(model, "wr2")[[1]],
      support = support_label(estimate, p.value),
      .before = target_term
    )

  coefficients <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      sample = sample_name,
      outcome = outcome,
      hypothesis_id = hypothesis_id,
      variable = variable,
      term = clean_term(term),
      nobs = nobs(model)
    ) %>%
    select(sample, outcome, hypothesis_id, variable, term, estimate, std.error,
           conf.low, conf.high, p.value, nobs)

  list(result = result, coefficients = coefficients)
}

message("Reading data...")
df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)
capital_times <- read_delim(CAPITAL_TIMES_PATH, delim = ";", show_col_types = FALSE) %>%
  mutate(
    first_local_hour = parse_hour(first_titled_tuesday),
    second_local_hour = parse_hour(second_titled_tuesday)
  )

df_players <- df_raw %>%
  mutate(
    tournament_id = date,
    tournament_date = as.Date(date),
    tournament_hour = as.integer(str_sub(date, 12, 13)),
    event_slot = if_else(tournament_hour <= 10, "first", "second"),
    format_5_0 = as.integer(tournament_date >= as.Date("2025-09-01")),
    rating_diff_100 = (player_rating - opponent_rating) / 100,
    abs_rating_diff_100 = abs(player_rating - opponent_rating) / 100,
    player_rating_100c = center(player_rating) / 100,
    opponent_rating_100c = center(opponent_rating) / 100,
    fide_blitz_100c = center(blitz_rating) / 100,
    fide_classical_100c = center(classic_rating) / 100,
    online_premium_100 = (player_rating - blitz_rating) / 100,
    classical_premium_100 = (classic_rating - blitz_rating) / 100,
    rapid_premium_100 = (rapid_rating - blitz_rating) / 100,
    gdp_log_c = center(gdp_per_capita_ppp_logged),
    is_rich_country = as.integer(gdp_per_capita_ppp_logged >= median(gdp_per_capita_ppp_logged, na.rm = TRUE)),
    is_gm = as.integer(player_title == "GM"),
    is_w_title = as.integer(player_title %in% c("WCM", "WFM", "WGM", "WIM", "WNM")),
    late_round = as.integer(round >= 8),
    round_c = round - mean(round, na.rm = TRUE),
    final_score_pregame_c = center(final_score_pregame),
    tiebreak_avg_opp_score_c = center(opponents_sum_score / pmax(round - 1, 1))
  ) %>%
  group_by(tournament_id) %>%
  mutate(
    field_size = max(rank, na.rm = TRUE),
    rank_pct = if_else(field_size > 1, (rank - 1) / (field_size - 1), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    rank_pct_10c = center(rank_pct) / 0.10
  ) %>%
  left_join(capital_times, by = c("country_name" = "country")) %>%
  mutate(
    local_start_hour = if_else(event_slot == "first", first_local_hour, second_local_hour),
    first_sleep_cost_6h = distance_from_hour(first_local_hour, 20) / 6,
    slot_sleep_cost_6h = distance_from_hour(local_start_hour, 20) / 6,
    first_night_start = as.integer(first_local_hour < 7 | first_local_hour >= 23),
    slot_night_start = as.integer(local_start_hour < 7 | local_start_hour >= 23)
  ) %>%
  filter(
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

sample_summary <- bind_rows(
  df_players %>%
    summarise(
      sample = "all_events",
      rows = n(),
      players = n_distinct(player_name),
      tournaments = n_distinct(tournament_id),
      pre_rows = sum(format_5_0 == 0),
      post_rows = sum(format_5_0 == 1),
      matched_local_time_rows = sum(!is.na(slot_sleep_cost_6h)),
      first_event_rows = sum(event_slot == "first")
    ),
  df_players %>%
    filter(event_slot == "first") %>%
    summarise(
      sample = "first_event_only",
      rows = n(),
      players = n_distinct(player_name),
      tournaments = n_distinct(tournament_id),
      pre_rows = sum(format_5_0 == 0),
      post_rows = sum(format_5_0 == 1),
      matched_local_time_rows = sum(!is.na(first_sleep_cost_6h)),
      first_event_rows = sum(event_slot == "first")
    )
)

write_csv(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

hypotheses <- tribble(
  ~hypothesis_id, ~mechanism, ~variable, ~predicted_sign, ~rationale,
  "H01", "Online platform-specific skill", "online_premium_100", "positive",
  "No-increment online play should reward players whose Chess.com rating is high relative to FIDE blitz, a proxy for platform-specific speed, mouse, premove, and online adaptation capital.",
  "H02", "Classical chess-skill premium", "classical_premium_100", "positive",
  "A longer base time should raise the return to classical chess skill relative to pure blitz skill.",
  "H03", "Rapid chess-skill premium", "rapid_premium_100", "positive",
  "The 5+0 format is closer to rapid/blitz hybrid decision-making than the old shorter format, so rapid-over-blitz specialists may gain.",
  "H04", "FIDE blitz skill complement", "fide_blitz_100c", "positive",
  "If no increment increases time-scramble value, stronger FIDE blitz players should gain even conditional on Chess.com rating.",
  "H05", "Elite skill lowers variance", "player_rating_100c", "positive",
  "Longer games should reduce noise and let stronger online players convert skill into accuracy and points.",
  "H06", "Favorite advantage", "rating_diff_100", "positive",
  "If longer games reduce upset variance, players with an Elo advantage should gain more after the rule switch.",
  "H07", "Mismatch/effort slack", "abs_rating_diff_100", "negative",
  "Large rating gaps may reduce effort or induce faster practical wins, lowering accuracy relative to balanced games.",
  "H08", "First-mover advantage", "is_white", "positive",
  "More time may make opening preparation and initiative easier to convert, increasing White's relative advantage.",
  "H09", "Late-round fatigue", "late_round", "negative",
  "Longer no-increment games make the event more fatiguing, so late rounds should suffer.",
  "H10", "Linear fatigue accumulation", "round_c", "negative",
  "If cognitive effort accumulates across games, the post-rule accuracy/result effect should decline with each later round.",
  "H11", "Country resources", "gdp_log_c", "positive",
  "Higher-income countries proxy better connectivity, equipment, coaching, and professional support, which may matter more in longer online events.",
  "H12", "Rich-country threshold", "is_rich_country", "positive",
  "A coarse high-income indicator tests whether the resource mechanism is nonlinear.",
  "H13", "Circadian scheduling cost", "first_sleep_cost_6h", "negative",
  "For the first event, countries farther from a 20:00 local start face higher opportunity and circadian costs.",
  "H14", "Night-start scheduling cost", "first_night_start", "negative",
  "First-event starts before 07:00 or after 23:00 should be especially costly.",
  "H15", "Professional title adaptation", "is_gm", "positive",
  "GMs may adapt more quickly to rule changes because chess is more likely to be professional labor for them.",
  "H16", "Women's-title segment", "is_w_title", "ambiguous",
  "Women's title categories may face different tournament selection and opportunity-cost changes, so this is a heterogeneity test without a signed prediction.",
  "H17", "Prize-position effort", "in_prizes", "positive",
  "Players already in prize positions have high marginal stakes and should invest more effort when each game lasts longer.",
  "H18", "Bubble effort", "bubble", "positive",
  "Bubble players have high marginal prize probability, so longer games should elicit more effort from them.",
  "H19", "Leader risk management", "leader", "ambiguous",
  "Leaders may either exert more effort to protect first place or choose lower-variance practical play, so the sign is ambiguous.",
  "H20", "Eliminated-player opportunity cost", "eliminated", "negative",
  "When no prize is reachable, the higher time cost of 5+0 should reduce effort and performance.",
  "H21", "Standing-contingent incentives", "final_score_pregame_c", "positive",
  "Higher pregame score means larger marginal tournament stakes, so effort should rise more under longer games.",
  "H22", "Rank-gradient incentives", "rank_pct_10c", "negative",
  "Worse current rank means lower marginal prize probability, so the longer format should reduce effort more for these players.",
  "H23", "Strength-of-schedule pressure", "tiebreak_avg_opp_score_c", "positive",
  "Players in stronger Swiss brackets may face higher-status games and exert more effort in the longer format.",
  "H24", "Opponent prize pressure", "played_against_prizes", "ambiguous",
  "A motivated prize-position opponent can raise the player's effort but lower the player's score; this tests strategic spillovers.",
  "H25", "Opponent bubble pressure", "played_against_bubble", "ambiguous",
  "Bubble opponents have strong incentives, creating strategic spillovers with an ambiguous sign.",
  "H26", "Opponent eliminated slack", "played_against_eliminated", "positive",
  "Facing eliminated opponents should become more favorable if those opponents reduce effort in longer games.",
  "H27", "Opponent leader pressure", "played_against_leader", "ambiguous",
  "Leaders may be unusually motivated or unusually risk-averse; either can change opponent outcomes."
)

write_csv(hypotheses, file.path(OUT_DIR, "hypotheses_with_rationale.csv"))

primary_data <- df_players %>%
  filter(event_slot == "first")

all_events_data <- df_players

run_grid <- bind_rows(
  tidyr::crossing(
    sample = "first_event_only",
    outcome = c("player_accuracy", "player_result"),
    hypotheses
  ),
  tidyr::crossing(
    sample = "all_events",
    outcome = c("player_accuracy", "player_result"),
    hypotheses
  )
)

message("Fitting ", nrow(run_grid), " economic interaction regressions...")
runs <- pmap(
  run_grid,
  function(sample, outcome, hypothesis_id, mechanism, variable, predicted_sign, rationale) {
    data <- if (sample == "first_event_only") primary_data else all_events_data
    fit_interaction(
      data = data,
      sample_name = sample,
      outcome = outcome,
      variable = variable,
      hypothesis_id = hypothesis_id,
      mechanism = mechanism,
      predicted_sign = predicted_sign,
      rationale = rationale
    )
  }
)

results <- bind_rows(map(runs, "result")) %>%
  group_by(sample, outcome) %>%
  mutate(
    q.value = if_else(is.na(p.value), NA_real_, p.adjust(p.value, method = "BH")),
    prediction_result = prediction_label(estimate, q.value, predicted_sign)
  ) %>%
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
  arrange(sample, outcome, q.value, p.value)

coefficients <- bind_rows(map(runs, "coefficients")) %>%
  mutate(
    estimate = round(estimate, 6),
    std.error = round(std.error, 6),
    conf.low = round(conf.low, 6),
    conf.high = round(conf.high, 6),
    p.value = round(p.value, 6)
  )

write_csv(results, file.path(OUT_DIR, "economic_hypothesis_tests.csv"))
write_csv(coefficients, file.path(OUT_DIR, "all_model_coefficients.csv"))

primary_top_accuracy <- results %>%
  filter(sample == "first_event_only", outcome == "player_accuracy") %>%
  arrange(q.value, p.value) %>%
  slice_head(n = 12)

primary_top_result <- results %>%
  filter(sample == "first_event_only", outcome == "player_result") %>%
  arrange(q.value, p.value) %>%
  slice_head(n = 12)

supported_predictions <- results %>%
  filter(sample == "first_event_only", str_detect(prediction_result, "supports|opposes|clear exploratory")) %>%
  arrange(q.value, p.value)

format_line <- function(df) {
  paste0(
    "- ", df$hypothesis_id, " ", df$mechanism,
    ": estimate = ", df$estimate,
    ", SE = ", df$std.error,
    ", p = ", df$p.value,
    ", q = ", df$q.value,
    ", support = ", df$support,
    ", prediction = ", df$prediction_result,
    "."
  )
}

findings <- c(
  "# Titled Tuesday Rule Change: Economic Hypotheses",
  "",
  paste0("- Data: `", DATA_PATH, "`."),
  "- Treatment: `format_5_0 = 1` for tournaments on or after 2025-09-01.",
  "- Fixed effects: player and exact tournament timestamp.",
  "- Standard errors: two-way clustered by player and tournament.",
  "- Controls: player rating, opponent rating, color, and round fixed effects. The color control is omitted only when color is the interacted variable.",
  "- Primary sample: first Titled Tuesday event only (`hour <= 10`), because the post-period contains first events but no second events.",
  paste0("- Primary rows: ", sample_summary$rows[sample_summary$sample == "first_event_only"],
         ", post rows: ", sample_summary$post_rows[sample_summary$sample == "first_event_only"], "."),
  paste0("- All-events robustness rows: ", sample_summary$rows[sample_summary$sample == "all_events"],
         ", post rows: ", sample_summary$post_rows[sample_summary$sample == "all_events"], "."),
  "",
  "## Top Primary Accuracy Patterns",
  format_line(primary_top_accuracy),
  "",
  "## Top Primary Result Patterns",
  format_line(primary_top_result),
  "",
  "## Prediction-Oriented Summary",
  if (nrow(supported_predictions) == 0) {
    "- No first-event hypothesis survives the q < 0.10 prediction/exploratory threshold."
  } else {
    format_line(supported_predictions)
  },
  "",
  "## Output Files",
  "- `hypotheses_with_rationale.csv`",
  "- `sample_summary.csv`",
  "- `economic_hypothesis_tests.csv`",
  "- `all_model_coefficients.csv`"
)

writeLines(findings, file.path(OUT_DIR, "findings.md"))

message("Done. Outputs written to: ", OUT_DIR)
