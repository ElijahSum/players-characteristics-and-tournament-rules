suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lfe)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_age_hypotheses"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "player_rating", "player_title", "player_accuracy",
  "round", "date", "opponent_rating", "player_result", "opponent_name",
  "is_white", "final_score", "final_score_pregame", "classic_rating",
  "rapid_rating", "blitz_rating", "federation", "country_name",
  "gdp_per_capita_ppp_logged", "birthday", "opponents_sum_score",
  "opponents_sum_score_end_round", "buchholz_score",
  "buchholz_score_end_round", "sonneborn_berger_score",
  "sonneborn_berger_score_end_round", "rank", "rank_end_round",
  "leader", "in_prizes", "bubble", "eliminated",
  "played_against_bubble", "played_against_prizes",
  "played_against_eliminated", "played_against_leader"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, date := as.Date(date)]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]

# Use a fixed age at the rule-change year. This keeps age as a player category
# rather than letting everyone mechanically age through the panel.
df[, age := 2025L - birthday]
df[, age10 := (age - 35) / 10]

df[, player_rating100 := (player_rating - 2500) / 100]
df[, opponent_rating100 := (opponent_rating - 2500) / 100]
df[, rating_diff100 := (player_rating - opponent_rating) / 100]
df[, rating_favorite := as.integer(player_rating > opponent_rating)]
df[, gm := as.integer(player_title == "GM")]
df[, w_title := as.integer(grepl("^W", player_title))]
df[, late_round := as.integer(round >= 8)]
df[, final_rounds := as.integer(round >= 10)]
df[, prize_zone := as.integer(in_prizes == 1)]
df[, bubble_zone := as.integer(bubble == 1)]
df[, leader_zone := as.integer(leader == 1)]
df[, opponent_prize_zone := as.integer(played_against_prizes == 1)]
df[, opponent_bubble_zone := as.integer(played_against_bubble == 1)]
df[, pregame_score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
df[, gdp_log_c := gdp_per_capita_ppp_logged - mean(gdp_per_capita_ppp_logged, na.rm = TRUE)]

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

rating_q75 <- quantile(main$player_rating, 0.75, na.rm = TRUE)
main[, high_rating := as.integer(player_rating >= rating_q75)]

main[, event_month := (
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
) - (
  as.integer(format(rule_change_date, "%Y")) * 12L +
    as.integer(format(rule_change_date, "%m"))
)]

sample_summary <- rbindlist(list(
  main[, .(
    statistic = "rows",
    value = as.numeric(.N)
  )],
  main[, .(
    statistic = "players",
    value = as.numeric(uniqueN(player_name))
  )],
  main[, .(
    statistic = "players_with_pre_and_post_rows",
    value = as.numeric(uniqueN(player_name[
      player_name %in% main[, .(has_pre = any(format_5_0 == 0), has_post = any(format_5_0 == 1)), by = player_name][
        has_pre == TRUE & has_post == TRUE, player_name
      ]
    ]))
  )],
  data.table(statistic = "rule_change_date", value = NA_real_),
  data.table(statistic = "high_rating_threshold_q75", value = as.numeric(rating_q75))
), fill = TRUE)
sample_summary[statistic == "rule_change_date", value_text := as.character(rule_change_date)]
sample_summary[is.na(value_text), value_text := as.character(value)]
fwrite(sample_summary, file.path(output_dir, "sample_summary.csv"))

descriptives <- main[, .(
  rows = .N,
  players = uniqueN(player_name),
  mean_accuracy = mean(player_accuracy, na.rm = TRUE),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE)
), by = .(
  format_5_0,
  age_group = cut(
    age,
    breaks = c(0, 20, 30, 40, 50, Inf),
    labels = c("<=20", "21-30", "31-40", "41-50", "51+")
  )
)][order(format_5_0, age_group)]
fwrite(descriptives, file.path(output_dir, "descriptives_by_age_group.csv"))

# Baseline lfe models, matching the style of chess_2026.R.
model1_round_gt1 <- felm(
  player_accuracy ~ player_rating + is_white + format_5_0 * age + opponent_rating,
  data = main
)
model1_round_gt2 <- felm(
  player_accuracy ~ player_rating + is_white + format_5_0 * age + opponent_rating,
  data = main[round > 2]
)
capture.output(
  summary(model1_round_gt1),
  file = file.path(output_dir, "felm_baseline_accuracy_round_gt1.txt")
)
capture.output(
  summary(model1_round_gt2),
  file = file.path(output_dir, "felm_baseline_accuracy_round_gt2.txt")
)

controls <- "+ player_rating100 + opponent_rating100 + is_white + factor(round)"
fixed_effects <- "| player_name + date"

make_formula <- function(outcome, interaction) {
  as.formula(paste(outcome, "~", interaction, controls, fixed_effects))
}

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

extract_target <- function(model, hypothesis, outcome, target_pieces, interpretation) {
  term <- find_term(model, target_pieces)
  tt <- broom::tidy(model, conf.int = TRUE)
  row <- as.data.table(tt[tt$term == term, ])
  row[, `:=`(
    hypothesis = hypothesis,
    outcome = outcome,
    interpretation = interpretation
  )]
  setcolorder(row, c(
    "hypothesis", "outcome", "interpretation", "term", "estimate",
    "std.error", "statistic", "p.value", "conf.low", "conf.high"
  ))
  row
}

hypotheses <- list(
  list(
    id = "H1_age_post",
    interaction = "format_5_0 * age10",
    target = c("format_5_0", "age10"),
    interpretation = "Post-rule change in the accuracy/result slope for a 10-year older player."
  ),
  list(
    id = "H2_age_x_rating",
    interaction = "format_5_0 * age10 * player_rating100",
    target = c("format_5_0", "age10", "player_rating100"),
    interpretation = "Additional post-rule age slope per 100 extra Chess.com rating points."
  ),
  list(
    id = "H3_age_x_rating_diff",
    interaction = "format_5_0 * age10 * rating_diff100",
    target = c("format_5_0", "age10", "rating_diff100"),
    interpretation = "Additional post-rule age slope per 100 points of player rating advantage over opponent."
  ),
  list(
    id = "H4_age_x_white",
    interaction = "format_5_0 * age10 * is_white",
    target = c("format_5_0", "age10", "is_white"),
    interpretation = "Additional post-rule age slope when the player has White."
  ),
  list(
    id = "H5_age_x_gm",
    interaction = "format_5_0 * age10 * gm",
    target = c("format_5_0", "age10", "gm"),
    interpretation = "Additional post-rule age slope for GMs relative to other titled players."
  ),
  list(
    id = "H6_age_x_w_title",
    interaction = "format_5_0 * age10 * w_title",
    target = c("format_5_0", "age10", "w_title"),
    interpretation = "Additional post-rule age slope for W-title players relative to other titled players."
  ),
  list(
    id = "H7_age_x_late_round",
    interaction = "format_5_0 * age10 * late_round",
    target = c("format_5_0", "age10", "late_round"),
    interpretation = "Additional post-rule age slope in rounds 8+ relative to rounds 2-7."
  ),
  list(
    id = "H8_age_x_final_rounds",
    interaction = "format_5_0 * age10 * final_rounds",
    target = c("format_5_0", "age10", "final_rounds"),
    interpretation = "Additional post-rule age slope in rounds 10-11 relative to earlier included rounds."
  ),
  list(
    id = "H9_age_x_prize_zone",
    interaction = "format_5_0 * age10 * prize_zone",
    target = c("format_5_0", "age10", "prize_zone"),
    interpretation = "Additional post-rule age slope for players currently in prize positions."
  ),
  list(
    id = "H10_age_x_bubble_zone",
    interaction = "format_5_0 * age10 * bubble_zone",
    target = c("format_5_0", "age10", "bubble_zone"),
    interpretation = "Additional post-rule age slope for players on the prize bubble."
  ),
  list(
    id = "H11_age_x_leader",
    interaction = "format_5_0 * age10 * leader_zone",
    target = c("format_5_0", "age10", "leader_zone"),
    interpretation = "Additional post-rule age slope for tournament leaders."
  ),
  list(
    id = "H12_age_x_opponent_prize",
    interaction = "format_5_0 * age10 * opponent_prize_zone",
    target = c("format_5_0", "age10", "opponent_prize_zone"),
    interpretation = "Additional post-rule age slope when facing a player in prize positions."
  ),
  list(
    id = "H13_age_x_opponent_bubble",
    interaction = "format_5_0 * age10 * opponent_bubble_zone",
    target = c("format_5_0", "age10", "opponent_bubble_zone"),
    interpretation = "Additional post-rule age slope when facing a player on the prize bubble."
  ),
  list(
    id = "H14_age_x_gdp",
    interaction = "format_5_0 * age10 * gdp_log_c",
    target = c("format_5_0", "age10", "gdp_log_c"),
    interpretation = "Additional post-rule age slope per log point of player-country GDP per capita."
  ),
  list(
    id = "H15_age_x_pregame_score",
    interaction = "format_5_0 * age10 * pregame_score_c",
    target = c("format_5_0", "age10", "pregame_score_c"),
    interpretation = "Additional post-rule age slope per extra pregame tournament point."
  ),
  list(
    id = "H16_age_x_high_rating",
    interaction = "format_5_0 * age10 * high_rating",
    target = c("format_5_0", "age10", "high_rating"),
    interpretation = "Additional post-rule age slope for top-quartile rating rows."
  )
)

outcomes <- c("player_accuracy", "player_result")
all_targets <- list()
all_models <- list()

for (outcome in outcomes) {
  for (h in hypotheses) {
    model_name <- paste(outcome, h$id, sep = "__")
    model <- feols(
      make_formula(outcome, h$interaction),
      data = main,
      cluster = ~ player_name
    )
    all_models[[model_name]] <- model
    all_targets[[model_name]] <- extract_target(
      model = model,
      hypothesis = h$id,
      outcome = outcome,
      target_pieces = h$target,
      interpretation = h$interpretation
    )
  }
}

target_tests <- rbindlist(all_targets, fill = TRUE)
target_tests[, p_bh_within_outcome := p.adjust(p.value, method = "BH"), by = outcome]
target_tests[, significant_5pct := p.value < 0.05]
target_tests[, significant_bh_10pct := p_bh_within_outcome < 0.10]
fwrite(target_tests, file.path(output_dir, "hypothesis_target_coefficients.csv"))

saveRDS(all_models, file.path(output_dir, "all_hypothesis_models.rds"))

write_etable <- function(models, path) {
  tryCatch(
    capture.output(
      etable(models, fitstat = ~ n + r2 + wr2),
      file = path
    ),
    error = function(e) {
      writeLines(
        c("etable failed; coefficient CSV and RDS outputs are authoritative.", e$message),
        path
      )
    }
  )
}

write_etable(
  all_models[grep("^player_accuracy", names(all_models))],
  file.path(output_dir, "accuracy_models_etable.txt")
)
write_etable(
  all_models[grep("^player_result", names(all_models))],
  file.path(output_dir, "result_models_etable.txt")
)

title_models <- list(
  accuracy = feols(
    player_accuracy ~ format_5_0:age10 +
      i(player_title, I(format_5_0 * age10), ref = "GM") +
      player_rating100 + opponent_rating100 + is_white + factor(round) |
      player_name + date,
    data = main,
    cluster = ~ player_name
  ),
  result = feols(
    player_result ~ format_5_0:age10 +
      i(player_title, I(format_5_0 * age10), ref = "GM") +
      player_rating100 + opponent_rating100 + is_white + factor(round) |
      player_name + date,
    data = main,
    cluster = ~ player_name
  )
)

title_tests <- rbindlist(lapply(names(title_models), function(outcome_name) {
  tt <- as.data.table(broom::tidy(title_models[[outcome_name]], conf.int = TRUE))
  tt <- tt[grepl("player_title::", term, fixed = TRUE)]
  tt[, outcome := ifelse(outcome_name == "accuracy", "player_accuracy", "player_result")]
  tt[]
}), fill = TRUE)
title_tests[, p_bh_within_outcome := p.adjust(p.value, method = "BH"), by = outcome]
fwrite(title_tests, file.path(output_dir, "title_specific_age_post_coefficients.csv"))
saveRDS(title_models, file.path(output_dir, "title_models.rds"))

round_models <- list(
  accuracy = feols(
    player_accuracy ~ format_5_0:age10 +
      i(round, I(format_5_0 * age10), ref = 2) +
      player_rating100 + opponent_rating100 + is_white |
      player_name + date,
    data = main,
    cluster = ~ player_name
  ),
  result = feols(
    player_result ~ format_5_0:age10 +
      i(round, I(format_5_0 * age10), ref = 2) +
      player_rating100 + opponent_rating100 + is_white |
      player_name + date,
    data = main,
    cluster = ~ player_name
  )
)

round_tests <- rbindlist(lapply(names(round_models), function(outcome_name) {
  tt <- as.data.table(broom::tidy(round_models[[outcome_name]], conf.int = TRUE))
  tt <- tt[grepl("round::", term, fixed = TRUE)]
  tt[, outcome := ifelse(outcome_name == "accuracy", "player_accuracy", "player_result")]
  tt[]
}), fill = TRUE)
round_tests[, p_bh_within_outcome := p.adjust(p.value, method = "BH"), by = outcome]
fwrite(round_tests, file.path(output_dir, "round_specific_age_post_coefficients.csv"))
saveRDS(round_models, file.path(output_dir, "round_models.rds"))

event_sample <- main[event_month >= -18 & event_month <= 7]
event_model_accuracy <- feols(
  player_accuracy ~ i(event_month, age10, ref = -1) +
    player_rating100 + opponent_rating100 + is_white + factor(round) |
    player_name + date,
  data = event_sample,
  cluster = ~ player_name
)

event_terms <- as.data.table(broom::tidy(event_model_accuracy, conf.int = TRUE))
event_terms <- event_terms[grepl("event_month::", term, fixed = TRUE)]
event_terms[, event_month := as.integer(sub("event_month::(-?[0-9]+):age10", "\\1", term))]
setorder(event_terms, event_month)
fwrite(event_terms, file.path(output_dir, "event_study_age_accuracy.csv"))
saveRDS(event_model_accuracy, file.path(output_dir, "event_model_accuracy.rds"))

event_plot <- ggplot(event_terms, aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, color = "gray45") +
  geom_point(size = 1.8, color = "#1f77b4") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Age coefficient by event month, per 10 years",
    title = "Event-study age slope for player accuracy",
    subtitle = "Player and date fixed effects; ref. month = -1; rounds > 1"
  ) +
  theme_minimal(base_size = 12)
ggsave(
  file.path(output_dir, "event_study_age_accuracy.png"),
  event_plot,
  width = 8,
  height = 4.8,
  dpi = 200
)

placebo_pre <- main[date < rule_change_date & date >= as.Date("2024-01-01")]
placebo_pre[, placebo_post_2024_09 := as.integer(date >= as.Date("2024-09-01"))]
placebo_model_accuracy <- feols(
  player_accuracy ~ placebo_post_2024_09 * age10 +
    player_rating100 + opponent_rating100 + is_white + factor(round) |
    player_name + date,
  data = placebo_pre,
  cluster = ~ player_name
)
placebo_model_result <- feols(
  player_result ~ placebo_post_2024_09 * age10 +
    player_rating100 + opponent_rating100 + is_white + factor(round) |
    player_name + date,
  data = placebo_pre,
  cluster = ~ player_name
)
placebo_tests <- rbindlist(list(
  extract_target(
    placebo_model_accuracy,
    "P1_placebo_2024_09",
    "player_accuracy",
    c("placebo_post_2024_09", "age10"),
    "Placebo post-September-2024 change in age slope, using only pre-rule-change rows."
  ),
  extract_target(
    placebo_model_result,
    "P1_placebo_2024_09",
    "player_result",
    c("placebo_post_2024_09", "age10"),
    "Placebo post-September-2024 change in age slope, using only pre-rule-change rows."
  )
))
fwrite(placebo_tests, file.path(output_dir, "placebo_2024_09_age_coefficients.csv"))
saveRDS(
  list(accuracy = placebo_model_accuracy, result = placebo_model_result),
  file.path(output_dir, "placebo_models.rds")
)

robust_round_gt2 <- list(
  accuracy = feols(
    player_accuracy ~ format_5_0 * age10 +
      player_rating100 + opponent_rating100 + is_white + factor(round) |
      player_name + date,
    data = main[round > 2],
    cluster = ~ player_name
  ),
  result = feols(
    player_result ~ format_5_0 * age10 +
      player_rating100 + opponent_rating100 + is_white + factor(round) |
      player_name + date,
    data = main[round > 2],
    cluster = ~ player_name
  )
)
robust_tests <- rbindlist(list(
  extract_target(
    robust_round_gt2$accuracy,
    "R1_round_gt2_age_post",
    "player_accuracy",
    c("format_5_0", "age10"),
    "Post-rule age slope after excluding rounds 1 and 2."
  ),
  extract_target(
    robust_round_gt2$result,
    "R1_round_gt2_age_post",
    "player_result",
    c("format_5_0", "age10"),
    "Post-rule age slope after excluding rounds 1 and 2."
  )
))
fwrite(robust_tests, file.path(output_dir, "robustness_round_gt2_age_coefficients.csv"))
saveRDS(robust_round_gt2, file.path(output_dir, "robust_round_gt2_models.rds"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
