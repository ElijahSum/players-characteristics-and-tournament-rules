library(data.table)
library(fixest)
library(ggplot2)
library(broom)

setFixest_nthreads(0)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_third_iteration_final_outcomes"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")
fake_cutoffs <- as.Date(c("2024-09-01", "2025-03-01"))

required_cols <- c(
  "player_name", "player_rating", "player_title", "player_accuracy",
  "round", "date", "opponent_rating", "player_result", "opponent_name",
  "is_white", "final_score", "final_score_pregame", "rank", "rank_end_round",
  "leader", "in_prizes", "bubble", "eliminated", "classic_rating",
  "blitz_rating", "country_name", "birthday"
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
df[, game_id := paste(
  event_id,
  round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "__"
)]

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
  valid_accuracy = !is.na(player_accuracy) & player_accuracy != 0 & player_accuracy != 100,
  draw = as.integer(player_result == 0.5),
  decisive = as.integer(player_result %in% c(0, 1)),
  win = as.integer(player_result == 1),
  loss = as.integer(player_result == 0),
  gm = as.integer(player_title == "GM")
)]

event_info <- main[, .(
  event_max_round = max(round, na.rm = TRUE),
  event_players = uniqueN(player_name),
  event_mean_rating = mean(player_rating, na.rm = TRUE)
), by = .(event_id, event_date, event_month_rel, format_5_0)]

main <- event_info[main, on = .(event_id, event_date, event_month_rel, format_5_0)]
setorder(main, player_name, event_time, event_id, round)
main[, prev_is_white := shift(is_white), by = .(player_name, event_id)]
main[, black_streak := as.integer(is_white == 0 & prev_is_white == 0)]

player_event <- main[, .(
  event_date = first(event_date),
  event_month_rel = first(event_month_rel),
  format_5_0 = first(format_5_0),
  event_max_round = first(event_max_round),
  event_players = first(event_players),
  event_mean_rating = first(event_mean_rating),
  player_rating = mean(player_rating, na.rm = TRUE),
  player_rating100 = mean(player_rating100, na.rm = TRUE),
  gm = max(gm, na.rm = TRUE),
  player_title = first(player_title),
  country_name = first(country_name),
  games_played = .N,
  final_round_played = max(round, na.rm = TRUE),
  completed_event = as.integer(max(round, na.rm = TRUE) >= first(event_max_round)),
  final_score = max(final_score, na.rm = TRUE),
  first_round = min(round, na.rm = TRUE)
), by = .(player_name, event_id)]
player_event[!is.finite(final_score), final_score := NA_real_]
player_event[, final_score_pct := final_score / event_max_round]
player_event[, final_score_remaining_after_r3_pct := NA_real_]

score_rank <- player_event[!is.na(final_score), .(
  player_name,
  event_id,
  final_score_rank = frank(-final_score, ties.method = "average"),
  final_score_rank_min = frank(-final_score, ties.method = "min"),
  event_score_winner_score = max(final_score, na.rm = TRUE)
), by = event_id]
player_event <- score_rank[player_event, on = .(player_name, event_id)]
player_event[, final_rank_quality := fifelse(
  event_players > 1 & !is.na(final_score_rank),
  1 - (final_score_rank - 1) / (event_players - 1),
  NA_real_
)]
player_event[, final_in_prizes := as.integer(!is.na(final_score_rank_min) & final_score_rank_min <= 6)]
player_event[, final_leader := as.integer(!is.na(final_score) & final_score == event_score_winner_score)]

early3 <- main[round <= 3, .(
  score_first3 = sum(player_result, na.rm = TRUE),
  games_first3 = .N,
  white_share_first3 = mean(is_white, na.rm = TRUE),
  black_streak_first3 = as.integer(any(black_streak == 1, na.rm = TRUE)),
  draw_share_first3 = mean(draw, na.rm = TRUE),
  decisive_share_first3 = mean(decisive, na.rm = TRUE),
  win_share_first3 = mean(win, na.rm = TRUE),
  accuracy_mean_first3 = {
    x <- player_accuracy[valid_accuracy == TRUE]
    if (length(x) == 0L) NA_real_ else mean(x)
  },
  accuracy_sd_first3 = {
    x <- player_accuracy[valid_accuracy == TRUE]
    if (length(x) <= 1L) 0 else sd(x)
  },
  accuracy_min_first3 = {
    x <- player_accuracy[valid_accuracy == TRUE]
    if (length(x) == 0L) NA_real_ else min(x)
  }
), by = .(player_name, event_id)]

state_r4 <- main[round == 4, .(
  score_after_r3 = final_score_pregame,
  rank_after_r3 = rank,
  leader_after_r3 = as.integer(leader == 1),
  prize_after_r3 = as.integer(in_prizes == 1),
  bubble_after_r3 = as.integer(bubble == 1),
  eliminated_after_r3 = as.integer(eliminated == 1)
), by = .(player_name, event_id)]

state_r5 <- main[round == 5, .(
  score_after_r4 = final_score_pregame,
  rank_after_r4 = rank,
  leader_after_r4 = as.integer(leader == 1),
  prize_after_r4 = as.integer(in_prizes == 1),
  bubble_after_r4 = as.integer(bubble == 1),
  eliminated_after_r4 = as.integer(eliminated == 1)
), by = .(player_name, event_id)]

pe <- player_event[early3, on = .(player_name, event_id)]
pe <- state_r4[pe, on = .(player_name, event_id)]
pe <- state_r5[pe, on = .(player_name, event_id)]

pe[, `:=`(
  score_after_r3_pct = score_after_r3 / 3,
  score_after_r4_pct = score_after_r4 / 4,
  rank_after_r3_pct = fifelse(event_players > 1, (rank_after_r3 - 1) / (event_players - 1), NA_real_),
  rank_after_r4_pct = fifelse(event_players > 1, (rank_after_r4 - 1) / (event_players - 1), NA_real_),
  rank_quality_after_r3 = fifelse(event_players > 1, 1 - (rank_after_r3 - 1) / (event_players - 1), NA_real_),
  rank_quality_after_r4 = fifelse(event_players > 1, 1 - (rank_after_r4 - 1) / (event_players - 1), NA_real_),
  final_score_remaining_after_r3_pct = (final_score - score_after_r3) / pmax(event_max_round - 3, 1),
  final_score_remaining_after_r4_pct = (final_score - score_after_r4) / pmax(event_max_round - 4, 1),
  balanced_color_first3 = as.integer(white_share_first3 > 0 & white_share_first3 < 1),
  all_decisive_first3 = as.integer(decisive_share_first3 == 1),
  any_draw_first3 = as.integer(draw_share_first3 > 0),
  accuracy_sd_first3_10 = accuracy_sd_first3 / 10,
  accuracy_min_first3_10 = accuracy_min_first3 / 10
)]

model_rows <- list()
placebo_rows <- list()

safe_feols <- function(fml, data, cluster) {
  feols(as.formula(fml), data = data, cluster = cluster, warn = FALSE, notes = FALSE)
}

tidy_target <- function(model, keep_fun, idea, outcome, specification, sample_name) {
  tt <- as.data.table(tidy(model, conf.int = TRUE))
  tt <- tt[keep_fun(term)]
  if (nrow(tt) == 0L) {
    return(data.table())
  }
  tt[, `:=`(
    idea = idea,
    outcome = outcome,
    specification = specification,
    sample = sample_name,
    nobs = nobs(model)
  )]
  tt
}

add_model <- function(name, model, target_terms, idea, outcome, specification, sample_name) {
  model_rows[[name]] <<- tidy_target(
    model,
    function(x) x %in% target_terms,
    idea,
    outcome,
    specification,
    sample_name
  )
}

fit_interaction <- function(data, outcome, variable, controls = "player_rating100 + gm + games_played") {
  safe_feols(
    paste0(outcome, " ~ format_5_0 * ", variable, " + ", controls, " | player_name + event_id"),
    data,
    ~ player_name + event_id
  )
}

fit_placebo <- function(data, cutoff, outcome, variable, controls = "player_rating100 + gm + games_played") {
  fake <- copy(data[event_date < rule_change_date])
  fake[, fake_post := as.integer(event_date >= cutoff)]
  safe_feols(
    paste0(outcome, " ~ fake_post * ", variable, " + ", controls, " | player_name + event_id"),
    fake,
    ~ player_name + event_id
  )
}

near_window <- function(data) {
  data[event_date >= rule_change_date - 365 & event_date <= rule_change_date + 365]
}

main_sample <- pe[!is.na(score_after_r3) & !is.na(final_rank_quality)]
round4_sample <- pe[!is.na(score_after_r4) & !is.na(final_rank_quality)]

# Idea 1: early score lock-in.
early_score_specs <- list(
  score_lockin_remaining = list(data = main_sample, outcome = "final_score_remaining_after_r3_pct", var = "score_after_r3_pct"),
  score_lockin_rank = list(data = main_sample, outcome = "final_rank_quality", var = "score_after_r3_pct"),
  score_lockin_prize = list(data = main_sample, outcome = "final_in_prizes", var = "score_after_r3_pct"),
  score_lockin_leader = list(data = main_sample, outcome = "final_leader", var = "score_after_r3_pct"),
  score_lockin_rank_round4 = list(data = round4_sample, outcome = "final_rank_quality", var = "score_after_r4_pct"),
  score_lockin_prize_round4 = list(data = round4_sample, outcome = "final_in_prizes", var = "score_after_r4_pct"),
  score_lockin_rank_near = list(data = near_window(main_sample), outcome = "final_rank_quality", var = "score_after_r3_pct")
)
for (nm in names(early_score_specs)) {
  s <- early_score_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "T1_early_score_lock_in",
    s$outcome,
    nm,
    "player_event_after_round3"
  )
}

# Idea 2: early leader persistence.
leader_specs <- list(
  leader_to_final_leader = list(data = main_sample, outcome = "final_leader", var = "leader_after_r3"),
  leader_to_final_prize = list(data = main_sample, outcome = "final_in_prizes", var = "leader_after_r3"),
  leader_to_final_rank = list(data = main_sample, outcome = "final_rank_quality", var = "leader_after_r3"),
  leader_to_remaining_score = list(data = main_sample, outcome = "final_score_remaining_after_r3_pct", var = "leader_after_r3"),
  leader_to_final_leader_round4 = list(data = round4_sample, outcome = "final_leader", var = "leader_after_r4"),
  leader_to_final_leader_near = list(data = near_window(main_sample), outcome = "final_leader", var = "leader_after_r3")
)
for (nm in names(leader_specs)) {
  s <- leader_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "T2_early_leader_persistence",
    s$outcome,
    nm,
    "player_event_after_round3"
  )
}

# Idea 3: bubble conversion to final prizes.
bubble_specs <- list(
  bubble_to_prize = list(data = main_sample, outcome = "final_in_prizes", var = "bubble_after_r3"),
  bubble_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "bubble_after_r3"),
  bubble_to_remaining_score = list(data = main_sample, outcome = "final_score_remaining_after_r3_pct", var = "bubble_after_r3"),
  prize_to_prize = list(data = main_sample, outcome = "final_in_prizes", var = "prize_after_r3"),
  bubble_to_prize_round4 = list(data = round4_sample, outcome = "final_in_prizes", var = "bubble_after_r4"),
  bubble_to_prize_near = list(data = near_window(main_sample), outcome = "final_in_prizes", var = "bubble_after_r3")
)
for (nm in names(bubble_specs)) {
  s <- bubble_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "T3_bubble_prize_conversion",
    s$outcome,
    nm,
    "player_event_after_round3"
  )
}

# Idea 4: early color path and final outcomes.
color_specs <- list(
  white_share_to_remaining_score = list(data = main_sample, outcome = "final_score_remaining_after_r3_pct", var = "white_share_first3"),
  white_share_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "white_share_first3"),
  white_share_to_prize = list(data = main_sample, outcome = "final_in_prizes", var = "white_share_first3"),
  black_streak_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "black_streak_first3"),
  balanced_color_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "balanced_color_first3"),
  white_share_to_rank_near = list(data = near_window(main_sample), outcome = "final_rank_quality", var = "white_share_first3")
)
for (nm in names(color_specs)) {
  s <- color_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "T4_early_color_path",
    s$outcome,
    nm,
    "player_event_first3_games"
  )
}

# Idea 5: decisive/draw style and final outcomes.
draw_specs <- list(
  draw_share_to_remaining_score = list(data = main_sample, outcome = "final_score_remaining_after_r3_pct", var = "draw_share_first3"),
  draw_share_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "draw_share_first3"),
  draw_share_to_prize = list(data = main_sample, outcome = "final_in_prizes", var = "draw_share_first3"),
  all_decisive_to_rank = list(data = main_sample, outcome = "final_rank_quality", var = "all_decisive_first3"),
  any_draw_to_prize = list(data = main_sample, outcome = "final_in_prizes", var = "any_draw_first3"),
  draw_share_to_rank_near = list(data = near_window(main_sample), outcome = "final_rank_quality", var = "draw_share_first3")
)
for (nm in names(draw_specs)) {
  s <- draw_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "T5_early_draw_decisive_style",
    s$outcome,
    nm,
    "player_event_first3_games"
  )
}

# Auxiliary: accuracy consistency as a final-outcome mechanism.
accuracy_specs <- list(
  acc_sd_to_rank = list(data = main_sample[!is.na(accuracy_sd_first3_10)], outcome = "final_rank_quality", var = "accuracy_sd_first3_10"),
  acc_min_to_rank = list(data = main_sample[!is.na(accuracy_min_first3_10)], outcome = "final_rank_quality", var = "accuracy_min_first3_10"),
  acc_sd_to_prize = list(data = main_sample[!is.na(accuracy_sd_first3_10)], outcome = "final_in_prizes", var = "accuracy_sd_first3_10")
)
for (nm in names(accuracy_specs)) {
  s <- accuracy_specs[[nm]]
  mod <- fit_interaction(s$data, s$outcome, s$var)
  add_model(
    nm, mod,
    c(paste0("format_5_0:", s$var), paste0(s$var, ":format_5_0")),
    "X1_accuracy_consistency_final_outcomes",
    s$outcome,
    nm,
    "player_event_first3_games"
  )
}

headline_specs <- list(
  T1_early_score_lock_in = list(outcome = "final_in_prizes", var = "score_after_r3_pct", data = main_sample),
  T2_early_leader_persistence = list(outcome = "final_leader", var = "leader_after_r3", data = main_sample),
  T3_bubble_prize_conversion = list(outcome = "final_in_prizes", var = "bubble_after_r3", data = main_sample),
  T4_early_color_path = list(outcome = "final_rank_quality", var = "black_streak_first3", data = main_sample),
  T5_early_draw_decisive_style = list(outcome = "final_in_prizes", var = "draw_share_first3", data = main_sample)
)

placebo_index <- 1L
for (idea in names(headline_specs)) {
  s <- headline_specs[[idea]]
  for (cutoff in fake_cutoffs) {
    mod <- fit_placebo(s$data, cutoff, s$outcome, s$var)
    placebo_rows[[placebo_index]] <- tidy_target(
      mod,
      function(x) x %in% c(paste0("fake_post:", s$var), paste0(s$var, ":fake_post")),
      paste0(idea, "_placebo"),
      s$outcome,
      paste0("fake_cutoff_", cutoff),
      "pre_rule_player_event"
    )
    placebo_rows[[placebo_index]][, cutoff := as.character(cutoff)]
    placebo_index <- placebo_index + 1L
  }
}

all_models <- rbindlist(model_rows, fill = TRUE)
all_models[, p_bh_by_idea_outcome := p.adjust(p.value, method = "BH"), by = .(idea, outcome)]
fwrite(all_models, file.path(output_dir, "third_iteration_model_coefficients.csv"))

all_placebos <- rbindlist(placebo_rows, fill = TRUE)
if (nrow(all_placebos) > 0L) {
  all_placebos[, p_bh_by_idea_outcome := p.adjust(p.value, method = "BH"), by = .(idea, outcome)]
}
fwrite(all_placebos, file.path(output_dir, "third_iteration_placebo_coefficients.csv"))

sample_summary <- data.table(
  statistic = c(
    "input_file", "rule_change_date", "rows_raw", "rows_main", "events",
    "post_events", "player_event_rows", "main_after_round3_rows",
    "post_after_round3_rows", "players_after_round3", "events_after_round3"
  ),
  value = c(
    input_file, as.character(rule_change_date), nrow(df), nrow(main),
    uniqueN(main$event_id), uniqueN(main[format_5_0 == 1]$event_id),
    nrow(player_event), nrow(main_sample), nrow(main_sample[format_5_0 == 1]),
    uniqueN(main_sample$player_name), uniqueN(main_sample$event_id)
  )
)
fwrite(sample_summary, file.path(output_dir, "sample_summary.csv"))

descriptives <- main_sample[, .(
  n = .N,
  mean_final_score_pct = mean(final_score_pct, na.rm = TRUE),
  mean_final_rank_quality = mean(final_rank_quality, na.rm = TRUE),
  final_in_prizes_rate = mean(final_in_prizes, na.rm = TRUE),
  final_leader_rate = mean(final_leader, na.rm = TRUE),
  mean_score_after_r3_pct = mean(score_after_r3_pct, na.rm = TRUE),
  leader_after_r3_rate = mean(leader_after_r3, na.rm = TRUE),
  bubble_after_r3_rate = mean(bubble_after_r3, na.rm = TRUE),
  mean_white_share_first3 = mean(white_share_first3, na.rm = TRUE),
  mean_draw_share_first3 = mean(draw_share_first3, na.rm = TRUE)
), by = format_5_0]
fwrite(descriptives, file.path(output_dir, "descriptives_by_period.csv"))

event_bins <- main_sample[, .(
  final_rank_quality = mean(final_rank_quality, na.rm = TRUE),
  final_in_prizes = mean(final_in_prizes, na.rm = TRUE),
  n = .N
), by = .(
  format_5_0,
  score_bin = cut(score_after_r3_pct, breaks = c(-Inf, 0.33, 0.50, 0.67, 0.83, Inf))
)]
fwrite(event_bins, file.path(output_dir, "early_score_bins_final_outcomes.csv"))

ggplot(event_bins, aes(x = score_bin, y = final_rank_quality, group = factor(format_5_0), color = factor(format_5_0))) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(size = n)) +
  scale_color_manual(values = c("0" = "#777777", "1" = "#2364aa"), labels = c("3+1 pre", "5+0 post"), name = NULL) +
  scale_size_area(max_size = 5) +
  labs(
    title = "Final rank quality by score after round 3",
    subtitle = "Player-event observations; rank quality is higher for better final standing",
    x = "Score share after round 3",
    y = "Final rank quality",
    size = "Rows"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(output_dir, "early_score_bins_final_rank.png"), width = 7, height = 4.5, dpi = 200)

plot_terms <- all_models[
  idea %in% names(headline_specs) &
    specification %in% c(
      "score_lockin_prize", "leader_to_final_leader", "bubble_to_prize",
      "black_streak_to_rank", "draw_share_to_prize"
    )
]
plot_terms[, label := fifelse(
  idea == "T1_early_score_lock_in", "Early score -> final top 6",
  fifelse(
    idea == "T2_early_leader_persistence", "Early leader -> final winner",
    fifelse(
      idea == "T3_bubble_prize_conversion", "Early bubble -> final top 6",
      fifelse(
        idea == "T4_early_color_path", "Early black streak -> final rank",
        "Draw share -> final top 6"
      )
    )
  )
)]
fwrite(plot_terms, file.path(output_dir, "headline_coefficients.csv"))

ggplot(plot_terms, aes(x = reorder(label, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, color = "gray45") +
  geom_point(size = 2.2, color = "#2364aa") +
  coord_flip() +
  labs(
    title = "Headline post-change interactions for final outcomes",
    subtitle = "Player and event fixed effects; two-way clustered standard errors",
    x = NULL,
    y = "Coefficient on post x early-tournament variable"
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(output_dir, "headline_final_outcome_coefficients.png"), width = 7, height = 4.8, dpi = 200)

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
pick <- function(idea_id, specification_name, outcome_name = NULL) {
  rows <- all_models[idea == idea_id & specification == specification_name]
  if (!is.null(outcome_name)) rows <- rows[outcome == outcome_name]
  if (nrow(rows) == 0L) return(data.table())
  rows[1L]
}

t1 <- pick("T1_early_score_lock_in", "score_lockin_prize", "final_in_prizes")
t1_rob <- pick("T1_early_score_lock_in", "score_lockin_prize_round4", "final_in_prizes")
t2 <- pick("T2_early_leader_persistence", "leader_to_final_leader", "final_leader")
t2_rob <- pick("T2_early_leader_persistence", "leader_to_final_leader_round4", "final_leader")
t3 <- pick("T3_bubble_prize_conversion", "bubble_to_prize", "final_in_prizes")
t3_rob <- pick("T3_bubble_prize_conversion", "bubble_to_prize_round4", "final_in_prizes")
t4 <- pick("T4_early_color_path", "black_streak_to_rank", "final_rank_quality")
t4_rob <- pick("T4_early_color_path", "balanced_color_to_rank", "final_rank_quality")
t5 <- pick("T5_early_draw_decisive_style", "draw_share_to_prize", "final_in_prizes")
t5_rob <- pick("T5_early_draw_decisive_style", "any_draw_to_prize", "final_in_prizes")

summary_rows <- data.table(
  result_id = paste0("F", 1:5),
  title = c(
    "Early-score payoff to final top-six finishes",
    "Early leaders and final winner persistence",
    "Bubble conversion into final top-six finishes",
    "Early black-streak penalty in final rank",
    "Early draw style and final top-six finishes"
  ),
  headline = c(
    coef_line(t1),
    coef_line(t2),
    coef_line(t3),
    coef_line(t4),
    coef_line(t5)
  ),
  robustness = c(
    paste0("Round-4 score state: ", coef_line(t1_rob)),
    paste0("Round-4 leader state: ", coef_line(t2_rob)),
    paste0("Round-4 bubble state: ", coef_line(t3_rob)),
    paste0("Balanced-color version: ", coef_line(t4_rob)),
    paste0("Any-draw top-six version: ", coef_line(t5_rob))
  ),
  dependent_variables = c(
    "final_rank_quality; final_score_remaining_after_r3_pct; final_in_prizes; final_leader",
    "final_leader; final_in_prizes; final_rank_quality",
    "final_in_prizes; final_rank_quality; final_score_remaining_after_r3_pct",
    "final_rank_quality; final_in_prizes; final_score_remaining_after_r3_pct",
    "final_rank_quality; final_in_prizes; final_score_remaining_after_r3_pct"
  )
)
fwrite(summary_rows, file.path(output_dir, "five_final_outcome_ideas_summary.csv"))

report <- c(
  "# Third Iteration: Final Tournament Outcomes",
  "",
  paste0("Input: `", input_file, "`."),
  "",
  "Treatment definition: `format_5_0 = 1` on and after 2025-09-01.",
  "",
  "This iteration switches the dependent variables from per-game accuracy/result to final tournament outcomes: final score share, final rank quality, final top-six finish (`final_in_prizes`), and final winner/leader status (`final_leader`). The unit of observation is player-event, usually conditional on being observed after round 3 so that early-tournament states are measured before the final outcome.",
  "",
  "## F1. Early-score payoff to final top-six finishes",
  "",
  "Question: did early points translate differently into final top-six finishes after the format change?",
  "",
  paste0("Headline: ", coef_line(t1)),
  "",
  paste0("Robustness: ", coef_line(t1_rob), ". Additional outcomes are in `third_iteration_model_coefficients.csv`: remaining final score, final rank quality, and final winner status. The negative sign means early points became less predictive of final top-six conversion conditional on player and event fixed effects."),
  "",
  "## F2. Early leaders and final winner persistence",
  "",
  "Question: are players who lead after three rounds more likely to remain final winners after the rule change?",
  "",
  paste0("Headline: ", coef_line(t2)),
  "",
  paste0("Robustness: ", coef_line(t2_rob), ". I also estimate final top-six and final-rank outcomes for early leaders."),
  "",
  "## F3. Bubble conversion into final top-six finishes",
  "",
  "Question: did the rule change alter the probability that round-3 bubble players convert into final prize positions?",
  "",
  paste0("Headline: ", coef_line(t3)),
  "",
  paste0("Robustness: ", coef_line(t3_rob), ". This uses final `in_prizes` status as the dependent variable rather than current-game result."),
  "",
  "## F4. Early black-streak penalty in final rank",
  "",
  "Question: does suffering consecutive Blacks early in the event matter more for final standing under 5+0?",
  "",
  paste0("Headline: ", coef_line(t4)),
  "",
  paste0("Robustness: ", coef_line(t4_rob), ". White-share, final-score, and final top-six versions are also saved."),
  "",
  "## F5. Early draw style and final top-six finishes",
  "",
  "Question: under no increment, does an early draw-heavy style predict final prize finishes differently?",
  "",
  paste0("Headline: ", coef_line(t5)),
  "",
  paste0("Robustness: ", coef_line(t5_rob), ". The model set also includes remaining-score and all-decisive specifications."),
  "",
  "## Placebos And Robustness",
  "",
  "For each headline mechanism I ran fake pre-rule cutoffs at 2024-09-01 and 2025-03-01. The script also saves round-4-state, near-window, and alternate-variable robustness checks where applicable.",
  "",
  "## Output Files",
  "",
  "- `third_iteration_model_coefficients.csv`: all main and robustness coefficients.",
  "- `third_iteration_placebo_coefficients.csv`: fake-cutoff placebo coefficients.",
  "- `five_final_outcome_ideas_summary.csv`: compact five-result table.",
  "- `headline_coefficients.csv`: headline rows used for the coefficient plot.",
  "- `descriptives_by_period.csv`: outcome and early-state means before and after the cutoff.",
  "- `early_score_bins_final_outcomes.csv` and `early_score_bins_final_rank.png`: descriptive early-score-to-final-rank profile.",
  "- `headline_final_outcome_coefficients.png`: coefficient plot for the five headline ideas.",
  "- `sample_summary.csv` and `session_info.txt`."
)
writeLines(report, file.path(output_dir, "third_iteration_report.md"))

writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

cat("Wrote third-iteration outputs to", normalizePath(output_dir), "\n")
