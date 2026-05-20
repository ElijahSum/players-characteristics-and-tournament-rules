library(data.table)
library(fixest)
library(ggplot2)
library(broom)

setFixest_nthreads(0)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_fatigue_iteration"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")
fake_cutoffs <- as.Date(c("2024-09-01", "2025-03-01"))

required_cols <- c(
  "player_name", "player_rating", "player_title", "player_accuracy",
  "round", "date", "opponent_rating", "player_result", "opponent_name",
  "is_white", "final_score_pregame", "rank", "rank_end_round",
  "leader", "in_prizes", "bubble", "eliminated",
  "played_against_prizes", "played_against_leader"
)

df <- fread(input_file)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0L) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df[, event_id := as.character(date)]
df[, event_time := as.POSIXct(date, tz = "UTC")]
df[, event_date := as.Date(event_time)]
df[, event_month := as.integer(format(event_date, "%Y")) * 12L + as.integer(format(event_date, "%m"))]
rule_month <- as.integer(format(rule_change_date, "%Y")) * 12L + as.integer(format(rule_change_date, "%m"))
df[, event_month_rel := event_month - rule_month]
df[, format_5_0 := as.integer(event_date >= rule_change_date)]

main <- df[
  player_title != "No Title" &
    !is.na(player_name) & !is.na(opponent_name) &
    !is.na(player_rating) & !is.na(opponent_rating) &
    player_rating > 0 & opponent_rating > 0 &
    !is.na(player_result) & !is.na(round) & round > 0
]

main[, `:=`(
  player_rating100 = player_rating / 100,
  opponent_rating100 = opponent_rating / 100,
  rating_diff100 = (player_rating - opponent_rating) / 100,
  expected_score = 1 / (1 + 10^(-(player_rating - opponent_rating) / 400)),
  valid_accuracy = !is.na(player_accuracy) & player_accuracy != 0 & player_accuracy != 100,
  round_index = round - 1,
  round_c = round - 6,
  late_round = as.integer(round >= 8),
  final_rounds = as.integer(round >= 10),
  prize_zone = as.integer(in_prizes == 1),
  leader_zone = as.integer(leader == 1),
  bubble_zone = as.integer(bubble == 1),
  eliminated_zone = as.integer(eliminated == 1),
  opponent_prize_zone = as.integer(played_against_prizes == 1),
  opponent_leader_zone = as.integer(played_against_leader == 1)
)]
main[, result_over_expected := player_result - expected_score]

early_accuracy <- main[round <= 3 & valid_accuracy == TRUE, .(
  early_accuracy = mean(player_accuracy, na.rm = TRUE)
), by = .(player_name, event_id)]
main <- early_accuracy[main, on = .(player_name, event_id)]
main[, accuracy_delta_first3 := player_accuracy - early_accuracy]

add_streak <- function(dt, state_col, out_col) {
  setorder(dt, player_name, event_time, event_id, round)
  run_col <- paste0(out_col, "_run_id")
  dt[, (run_col) := rleid(get(state_col) == 1), by = .(player_name, event_id)]
  dt[, (out_col) := fifelse(get(state_col) == 1, seq_len(.N), 0L),
    by = c("player_name", "event_id", run_col)
  ]
  dt[, (run_col) := NULL]
}

add_streak(main, "prize_zone", "prize_streak")
add_streak(main, "leader_zone", "leader_streak")
add_streak(main, "bubble_zone", "bubble_streak")
add_streak(main, "eliminated_zone", "eliminated_streak")
add_streak(main, "opponent_prize_zone", "opponent_prize_streak")
add_streak(main, "opponent_leader_zone", "opponent_leader_streak")

main[, `:=`(
  pressure_streak = pmax(prize_streak, leader_streak, bubble_streak, na.rm = TRUE),
  sustained_prize = as.integer(prize_streak >= 2),
  sustained_leader = as.integer(leader_streak >= 2),
  sustained_bubble = as.integer(bubble_streak >= 2),
  sustained_pressure = as.integer(pmax(prize_streak, leader_streak, bubble_streak, na.rm = TRUE) >= 2)
)]

accuracy_sample <- main[valid_accuracy == TRUE]
delta_sample <- accuracy_sample[!is.na(accuracy_delta_first3)]
near_window <- function(data) data[event_date >= rule_change_date - 365 & event_date <= rule_change_date + 365]

term_has_parts <- function(parts) {
  force(parts)
  function(terms) {
    vapply(strsplit(terms, ":", fixed = TRUE), function(x) all(parts %in% x), logical(1))
  }
}

tidy_target <- function(model, keep_fun, effect_id, label, concept, outcome, specification, sample_name) {
  tt <- as.data.table(tidy(model, conf.int = TRUE))
  tt <- tt[keep_fun(term)]
  if (nrow(tt) == 0L) {
    return(data.table())
  }
  tt[, `:=`(
    effect_id = effect_id,
    label = label,
    concept = concept,
    outcome = outcome,
    specification = specification,
    sample = sample_name,
    nobs = nobs(model)
  )]
  tt
}

safe_feols <- function(fml, data, cluster = ~ player_name + event_id) {
  feols(as.formula(fml), data = data, cluster = cluster, warn = FALSE, notes = FALSE)
}

fit_effect <- function(spec, data, specification_name, sample_name, post_var = "format_5_0") {
  rhs <- gsub("POST", post_var, spec$rhs, fixed = TRUE)
  target_parts <- gsub("POST", post_var, spec$target_parts, fixed = TRUE)
  controls <- "rating_diff100 + is_white + factor(round)"
  fml <- paste0(spec$outcome, " ~ ", rhs, " + ", controls, " | player_name + event_id")
  mod <- safe_feols(fml, data)
  tidy_target(
    mod,
    term_has_parts(target_parts),
    spec$id,
    spec$label,
    spec$concept,
    spec$outcome,
    specification_name,
    sample_name
  )
}

specs <- list(
  list(
    id = "F01_accuracy_round_decay",
    label = "Accuracy decay per additional round",
    concept = "round-by-round fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * round_index",
    target_parts = c("POST", "round_index")
  ),
  list(
    id = "F02_result_round_decay",
    label = "Result decay per additional round",
    concept = "round-by-round fatigue",
    outcome = "player_result",
    sample = "result",
    rhs = "POST * round_index",
    target_parts = c("POST", "round_index")
  ),
  list(
    id = "F03_result_over_expected_round_decay",
    label = "Result-over-expected decay per additional round",
    concept = "rating-adjusted round fatigue",
    outcome = "result_over_expected",
    sample = "result",
    rhs = "POST * round_index",
    target_parts = c("POST", "round_index")
  ),
  list(
    id = "F04_accuracy_drop_from_early_baseline",
    label = "Accuracy drop from first-three baseline per round",
    concept = "own-baseline accuracy decay",
    outcome = "accuracy_delta_first3",
    sample = "delta",
    rhs = "POST * round_index",
    target_parts = c("POST", "round_index")
  ),
  list(
    id = "F05_late_round_accuracy_penalty",
    label = "Late-round accuracy penalty",
    concept = "late-round fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * late_round",
    target_parts = c("POST", "late_round")
  ),
  list(
    id = "F06_final_round_accuracy_penalty",
    label = "Final-round accuracy penalty",
    concept = "endgame fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * final_rounds",
    target_parts = c("POST", "final_rounds")
  ),
  list(
    id = "F07_prize_streak_accuracy",
    label = "Prize-zone streak effect on accuracy",
    concept = "sustained prize-pressure fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * prize_streak",
    target_parts = c("POST", "prize_streak")
  ),
  list(
    id = "F08_prize_streak_result",
    label = "Prize-zone streak effect on result",
    concept = "sustained prize-pressure fatigue",
    outcome = "player_result",
    sample = "result",
    rhs = "POST * prize_streak",
    target_parts = c("POST", "prize_streak")
  ),
  list(
    id = "F09_leader_streak_accuracy",
    label = "Leader streak effect on accuracy",
    concept = "leader-defense fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * leader_streak",
    target_parts = c("POST", "leader_streak")
  ),
  list(
    id = "F10_leader_streak_result",
    label = "Leader streak effect on result",
    concept = "leader-defense fatigue",
    outcome = "player_result",
    sample = "result",
    rhs = "POST * leader_streak",
    target_parts = c("POST", "leader_streak")
  ),
  list(
    id = "F11_bubble_streak_accuracy",
    label = "Bubble streak effect on accuracy",
    concept = "bubble-pressure fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * bubble_streak",
    target_parts = c("POST", "bubble_streak")
  ),
  list(
    id = "F12_bubble_streak_result",
    label = "Bubble streak effect on result",
    concept = "bubble-pressure fatigue",
    outcome = "player_result",
    sample = "result",
    rhs = "POST * bubble_streak",
    target_parts = c("POST", "bubble_streak")
  ),
  list(
    id = "F13_eliminated_streak_accuracy",
    label = "Eliminated streak effect on accuracy",
    concept = "low-stakes fatigue contrast",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * eliminated_streak",
    target_parts = c("POST", "eliminated_streak")
  ),
  list(
    id = "F14_eliminated_streak_result",
    label = "Eliminated streak effect on result",
    concept = "low-stakes fatigue contrast",
    outcome = "player_result",
    sample = "result",
    rhs = "POST * eliminated_streak",
    target_parts = c("POST", "eliminated_streak")
  ),
  list(
    id = "F15_pressure_streak_accuracy",
    label = "Any pressure streak effect on accuracy",
    concept = "general sustained-pressure fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * pressure_streak",
    target_parts = c("POST", "pressure_streak")
  ),
  list(
    id = "F16_sustained_prize_late_accuracy",
    label = "Sustained prize-zone late-round accuracy penalty",
    concept = "pressure-specific late fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * sustained_prize * late_round",
    target_parts = c("POST", "sustained_prize", "late_round")
  ),
  list(
    id = "F17_sustained_leader_late_accuracy",
    label = "Sustained leader late-round accuracy penalty",
    concept = "leader-specific late fatigue",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * sustained_leader * late_round",
    target_parts = c("POST", "sustained_leader", "late_round")
  ),
  list(
    id = "F18_pressure_streak_round_slope_accuracy",
    label = "Pressure-streak acceleration of round-by-round accuracy decay",
    concept = "pressure-specific decay slope",
    outcome = "player_accuracy",
    sample = "accuracy",
    rhs = "POST * pressure_streak * round_index",
    target_parts = c("POST", "pressure_streak", "round_index")
  )
)

sample_for <- function(sample_name) {
  if (sample_name == "accuracy") return(accuracy_sample)
  if (sample_name == "delta") return(delta_sample)
  main
}

model_rows <- list()
row_id <- 1L
for (spec in specs) {
  base <- sample_for(spec$sample)
  model_rows[[row_id]] <- fit_effect(spec, base, "event_fe_all", "all_rounds")
  row_id <- row_id + 1L
  model_rows[[row_id]] <- fit_effect(spec, base[round > 2], "event_fe_round_gt2", "round_gt2")
  row_id <- row_id + 1L
  model_rows[[row_id]] <- fit_effect(spec, near_window(base), "event_fe_near_window_pm12m", "near_window_pm12m")
  row_id <- row_id + 1L
}

effect_rows <- rbindlist(model_rows, fill = TRUE)
effect_rows[, p_bh_by_spec_outcome := p.adjust(p.value, method = "BH"), by = .(specification, outcome)]
fwrite(effect_rows, file.path(output_dir, "fatigue_effect_coefficients.csv"))

headline_ids <- vapply(specs[1:15], `[[`, character(1), "id")
placebo_rows <- list()
placebo_i <- 1L
for (spec in specs[vapply(specs, function(x) x$id %in% headline_ids, logical(1))]) {
  base <- sample_for(spec$sample)
  for (cutoff in fake_cutoffs) {
    fake <- copy(base[event_date < rule_change_date])
    fake[, placebo_post := as.integer(event_date >= cutoff)]
    out <- fit_effect(spec, fake, paste0("fake_cutoff_", cutoff), "pre_rule_only", post_var = "placebo_post")
    out[, cutoff := as.character(cutoff)]
    placebo_rows[[placebo_i]] <- out
    placebo_i <- placebo_i + 1L
  }
}
placebo_effects <- rbindlist(placebo_rows, fill = TRUE)
if (nrow(placebo_effects) > 0L) {
  placebo_effects[, p_bh_by_spec_outcome := p.adjust(p.value, method = "BH"), by = .(specification, outcome)]
}
fwrite(placebo_effects, file.path(output_dir, "fatigue_placebo_coefficients.csv"))

operationalizations <- data.table(
  concept = c(
    "round-by-round fatigue",
    "late-round fatigue",
    "endgame fatigue",
    "own-baseline accuracy decay",
    "sustained prize-pressure fatigue",
    "leader-defense fatigue",
    "bubble-pressure fatigue",
    "low-stakes fatigue contrast",
    "general sustained-pressure fatigue",
    "pressure-specific late fatigue",
    "pressure-specific decay slope"
  ),
  operationalization = c(
    "Post-change change in the slope of accuracy/result as round number rises.",
    "Post-change shift in rounds 8+ after round fixed effects and event/player fixed effects.",
    "Post-change shift in rounds 10+.",
    "Post-change round slope in accuracy relative to the player's own first-three-round event baseline.",
    "Post-change effect of consecutive rounds spent in prize positions before the game.",
    "Post-change effect of consecutive rounds spent as tournament leader before the game.",
    "Post-change effect of consecutive rounds spent on the prize bubble before the game.",
    "Post-change effect of consecutive rounds spent eliminated from realistic prizes.",
    "Post-change effect of the longest current leader/prize/bubble streak.",
    "Post-change late-round effect for players with at least two consecutive prize/leader rounds.",
    "Post-change interaction between pressure streak length and the round index."
  )
)
fwrite(operationalizations, file.path(output_dir, "fatigue_operationalizations.csv"))

streak_descriptives <- main[, .(
  n = .N,
  mean_accuracy = mean(player_accuracy[valid_accuracy == TRUE], na.rm = TRUE),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_prize_streak = mean(prize_streak, na.rm = TRUE),
  mean_leader_streak = mean(leader_streak, na.rm = TRUE),
  mean_bubble_streak = mean(bubble_streak, na.rm = TRUE),
  mean_eliminated_streak = mean(eliminated_streak, na.rm = TRUE),
  sustained_prize_rate = mean(sustained_prize, na.rm = TRUE),
  sustained_leader_rate = mean(sustained_leader, na.rm = TRUE),
  sustained_bubble_rate = mean(sustained_bubble, na.rm = TRUE)
), by = .(format_5_0, round)]
fwrite(streak_descriptives, file.path(output_dir, "fatigue_streak_descriptives_by_round.csv"))

round_profile <- main[, .(
  n = .N,
  mean_accuracy = mean(player_accuracy[valid_accuracy == TRUE], na.rm = TRUE),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_result_over_expected = mean(result_over_expected, na.rm = TRUE),
  mean_prize_streak = mean(prize_streak, na.rm = TRUE),
  mean_leader_streak = mean(leader_streak, na.rm = TRUE),
  mean_pressure_streak = mean(pressure_streak, na.rm = TRUE)
), by = .(format_5_0, round)]
fwrite(round_profile, file.path(output_dir, "fatigue_round_profile.csv"))

ggplot(round_profile, aes(x = round, y = mean_accuracy, color = factor(format_5_0), group = factor(format_5_0))) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(size = n), alpha = 0.75) +
  scale_color_manual(values = c("0" = "#777777", "1" = "#2364aa"), labels = c("3+1 pre", "5+0 post"), name = NULL) +
  scale_size_area(max_size = 4) +
  labs(
    title = "Accuracy profile by tournament round",
    subtitle = "Raw round means; model estimates use player and event fixed effects",
    x = "Round",
    y = "Mean accuracy",
    size = "Rows"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(output_dir, "fatigue_accuracy_decay_by_round.png"), width = 7, height = 4.5, dpi = 200)

headline <- effect_rows[specification == "event_fe_all"]
setorder(headline, p.value)
headline10 <- headline[1:min(.N, 10)]
fwrite(headline10, file.path(output_dir, "fatigue_headline_10_effects.csv"))

plot_rows <- headline[effect_id %in% headline_ids]
plot_rows[, label_plot := paste0(effect_id, ": ", label)]
ggplot(plot_rows, aes(x = reorder(label_plot, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, color = "gray45") +
  geom_point(aes(color = outcome), size = 2) +
  coord_flip() +
  labs(
    title = "Fatigue effects after the rule change",
    subtitle = "Main event-FE specifications; two-way clustered standard errors",
    x = NULL,
    y = "Coefficient on post-change fatigue term",
    color = "Outcome"
  ) +
  theme_minimal(base_size = 10)
ggsave(file.path(output_dir, "fatigue_headline_effects.png"), width = 8, height = 7.5, dpi = 200)

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}
fmt_p <- function(x) {
  ifelse(is.na(x), "NA", formatC(x, format = "e", digits = 2))
}
coef_line <- function(row) {
  if (is.null(row) || nrow(row) == 0L) return("No estimable coefficient.")
  paste0(
    "`", row$term, "` = ", fmt(row$estimate),
    " (SE ", fmt(row$std.error), ", p = ", fmt_p(row$p.value), ")"
  )
}
pick_main <- function(effect_id) {
  rows <- effect_rows[effect_id == effect_id & specification == "event_fe_all"]
  if (nrow(rows) == 0L) return(data.table())
  rows[1L]
}
pick_by_id <- function(id) {
  rows <- effect_rows[effect_id == id & specification == "event_fe_all"]
  if (nrow(rows) == 0L) return(data.table())
  rows[1L]
}

selected_ids <- c(
  "F01_accuracy_round_decay", "F02_result_round_decay",
  "F04_accuracy_drop_from_early_baseline", "F05_late_round_accuracy_penalty",
  "F06_final_round_accuracy_penalty", "F07_prize_streak_accuracy",
  "F08_prize_streak_result", "F09_leader_streak_accuracy",
  "F10_leader_streak_result", "F11_bubble_streak_accuracy",
  "F12_bubble_streak_result", "F13_eliminated_streak_accuracy",
  "F15_pressure_streak_accuracy", "F16_sustained_prize_late_accuracy",
  "F17_sustained_leader_late_accuracy"
)
selected <- effect_rows[effect_id %in% selected_ids & specification == "event_fe_all"]
selected[, selected_order := match(effect_id, selected_ids)]
setorder(selected, selected_order)
selected[, selected_order := NULL]
fwrite(selected, file.path(output_dir, "fatigue_selected_effects_summary.csv"))

sample_summary <- data.table(
  statistic = c(
    "input_file", "rule_change_date", "rows_raw", "rows_main",
    "accuracy_rows", "delta_accuracy_rows", "events", "post_events",
    "players", "effects_estimated", "placebo_rows"
  ),
  value = c(
    input_file, as.character(rule_change_date), nrow(df), nrow(main),
    nrow(accuracy_sample), nrow(delta_sample), uniqueN(main$event_id),
    uniqueN(main[format_5_0 == 1]$event_id), uniqueN(main$player_name),
    nrow(effect_rows), nrow(placebo_effects)
  )
)
fwrite(sample_summary, file.path(output_dir, "sample_summary.csv"))

report <- c(
  "# Fatigue Iteration: Tiredness After The Titled Tuesday Rule Change",
  "",
  paste0("Input: `", input_file, "`."),
  "",
  "Treatment definition: `format_5_0 = 1` on and after 2025-09-01.",
  "",
  "This iteration operationalizes tiredness in four ways: round-by-round decay, late/final-round penalties, accuracy decline relative to a player's own first-three-round baseline, and sustained pressure streaks for players who remain leaders, prize-zone players, bubble players, or eliminated players for consecutive rounds.",
  "",
  "## Model Template",
  "",
  "Main specifications use player and event fixed effects with two-way clustered standard errors:",
  "",
  "```r",
  "outcome ~ post * fatigue_variable + rating_diff100 + is_white + factor(round) | player_name + event_id",
  "```",
  "",
  "Robustness adds round > 2 and +/-12 month window versions. Fake pre-rule cutoffs at 2024-09-01 and 2025-03-01 are saved for the headline set.",
  "",
  "## Selected Effects",
  "",
  paste0("- ", selected$effect_id, " | ", selected$label, " | ", vapply(seq_len(nrow(selected)), function(i) coef_line(selected[i]), character(1))),
  "",
  "## Interpretation Notes",
  "",
  "- `round_index` effects measure whether the post-change format steepens or flattens within-event deterioration as rounds accumulate.",
  "- `accuracy_delta_first3` uses each player-event's own first-three-round accuracy as the benchmark, so it is a direct accuracy-decay measure.",
  "- `prize_streak`, `leader_streak`, and `bubble_streak` measure consecutive rounds entering the game in that category.",
  "- Sustained-pressure late-round effects test whether players who stay in high-stakes categories for at least two consecutive rounds become especially vulnerable late in the event.",
  "",
  "## Output Files",
  "",
  "- `fatigue_effect_coefficients.csv`: main and robustness coefficients for all fatigue effects.",
  "- `fatigue_placebo_coefficients.csv`: fake pre-rule cutoff tests.",
  "- `fatigue_selected_effects_summary.csv`: selected effect table with at least 10 fatigue effects.",
  "- `fatigue_headline_10_effects.csv`: ten smallest-p-value main fatigue estimates.",
  "- `fatigue_operationalizations.csv`: concept-to-variable map.",
  "- `fatigue_round_profile.csv` and `fatigue_accuracy_decay_by_round.png`: round-level profile.",
  "- `fatigue_streak_descriptives_by_round.csv`: pressure-streak descriptive table.",
  "- `fatigue_headline_effects.png`: coefficient plot.",
  "- `sample_summary.csv` and `session_info.txt`."
)
writeLines(report, file.path(output_dir, "fatigue_report.md"))

writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

cat("Wrote fatigue iteration outputs to", normalizePath(output_dir), "\n")
