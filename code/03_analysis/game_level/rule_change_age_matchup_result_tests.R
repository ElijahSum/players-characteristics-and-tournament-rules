suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_age_matchup_result_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_result", "round", "date", "is_white",
  "birthday", "final_score_pregame", "rank", "in_prizes", "bubble",
  "eliminated", "leader"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, event_id := as.character(date)]
df[, date := as.Date(date)]
df[, event_year := as.integer(format(date, "%Y"))]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]
df[, age_event := event_year - birthday]

main <- df[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0 &
    !is.na(age_event) &
    age_event >= 8 &
    age_event <= 90
]

main[, player_rating100 := (player_rating - 2500) / 100]
main[, opponent_rating100 := (opponent_rating - 2500) / 100]
main[, rating_diff100 := (player_rating - opponent_rating) / 100]
main[, expected_score := 1 / (1 + 10^((opponent_rating - player_rating) / 400))]
main[, result_over_expected := player_result - expected_score]
main[, score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, bubble_zone := as.integer(bubble == 1)]
main[, prize_zone := as.integer(in_prizes == 1)]
main[, eliminated_zone := as.integer(eliminated == 1)]
main[, leader_zone := as.integer(leader == 1)]

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
  opponent_age_event = age_event,
  opponent_birthday_observed = birthday
)]
setkey(opponent_lookup, event_id, round, player_name)
main[, `:=`(
  opponent_age_event_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_age_event
  ],
  opponent_birthday_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_birthday_observed
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

pair <- main[
  !is.na(opponent_age_event_observed) &
    opponent_age_event_observed >= 8 &
    opponent_age_event_observed <= 90
]

classify_age <- function(age, young_cut = 25, old_cut = 40) {
  fifelse(age <= young_cut, "young", fifelse(age >= old_cut, "old", "middle"))
}

pair[, age_group := classify_age(age_event)]
pair[, opponent_age_group := classify_age(opponent_age_event_observed)]
pair[, pair_cell := paste(age_group, "vs", opponent_age_group, sep = "_")]

pair_cells <- c(
  "young_vs_young", "young_vs_middle", "young_vs_old",
  "middle_vs_young", "middle_vs_old",
  "old_vs_young", "old_vs_middle", "old_vs_old"
)
for (cell in pair_cells) {
  pair[, (cell) := as.integer(pair_cell == cell)]
  pair[, paste0("post_", cell) := format_5_0 * get(cell)]
}

fwrite(
  pair[, .(
    rows = .N,
    players = uniqueN(player_name),
    events = uniqueN(event_id),
    games = uniqueN(game_id),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    mean_age = mean(age_event),
    mean_opponent_age = mean(opponent_age_event_observed)
  )],
  file.path(output_dir, "sample_summary.csv")
)

pair_descriptives <- pair[, .(
  rows = .N,
  players = uniqueN(player_name),
  games = uniqueN(game_id),
  mean_result = mean(player_result),
  mean_result_over_expected = mean(result_over_expected),
  mean_rating = mean(player_rating),
  mean_opponent_rating = mean(opponent_rating),
  mean_rating_diff = mean(player_rating - opponent_rating),
  mean_age = mean(age_event),
  mean_opponent_age = mean(opponent_age_event_observed)
), by = .(format_5_0, pair_cell)][order(format_5_0, pair_cell)]
fwrite(pair_descriptives, file.path(output_dir, "pair_cell_descriptives.csv"))

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

extract_terms <- function(model, keep_terms = NULL) {
  tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
  if (!is.null(keep_terms)) {
    tt <- tt[term %in% keep_terms]
  }
  tt[, nobs := nobs(model)]
  tt
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

cell_rhs <- paste(pair_cells, collapse = " + ")
post_cell_rhs <- paste(paste0("post_", pair_cells), collapse = " + ")
controls <- "+ player_rating100 + opponent_rating100 + is_white + score_c + factor(round)"

model_pair_event <- feols(
  as.formula(paste("player_result ~", cell_rhs, controls, "| event_id")),
  data = pair,
  cluster = ~ player_name + event_id
)

model_pair_player_event <- feols(
  as.formula(paste("player_result ~", cell_rhs, controls, "| player_name + event_id")),
  data = pair,
  cluster = ~ player_name + event_id
)

model_pair_post <- feols(
  as.formula(paste("player_result ~", cell_rhs, "+", post_cell_rhs, controls, "| event_id")),
  data = pair,
  cluster = ~ player_name + event_id
)

pair_models <- list(
  event_fe = model_pair_event,
  player_event_fe = model_pair_player_event,
  post_interactions_event_fe = model_pair_post
)

pair_rows <- rbindlist(lapply(names(pair_models), function(nm) {
  tt <- extract_terms(pair_models[[nm]])
  tt <- tt[term %in% c(pair_cells, paste0("post_", pair_cells))]
  tt[, specification := nm]
  tt
}), fill = TRUE)
pair_rows[, p_bh_by_spec := p.adjust(p.value, method = "BH"), by = specification]
setorder(pair_rows, specification, p.value)
fwrite(pair_rows, file.path(output_dir, "pair_cell_regression_coefficients.csv"))

contrast_list <- list(
  event_young_old_vs_young_young = contrast_terms(
    model_pair_event, "young_vs_old", "young_vs_young",
    "young_vs_old_minus_young_vs_young"
  )[, specification := "event_fe"],
  event_young_old_vs_young_middle = contrast_terms(
    model_pair_event, "young_vs_old", "young_vs_middle",
    "young_vs_old_minus_young_vs_middle"
  )[, specification := "event_fe"],
  event_old_young_vs_old_old = contrast_terms(
    model_pair_event, "old_vs_young", "old_vs_old",
    "old_vs_young_minus_old_vs_old"
  )[, specification := "event_fe"],
  event_young_old_vs_old_young = contrast_terms(
    model_pair_event, "young_vs_old", "old_vs_young",
    "young_vs_old_minus_old_vs_young"
  )[, specification := "event_fe"],
  player_fe_young_old_vs_young_young = contrast_terms(
    model_pair_player_event, "young_vs_old", "young_vs_young",
    "young_vs_old_minus_young_vs_young"
  )[, specification := "player_event_fe"],
  player_fe_young_old_vs_young_middle = contrast_terms(
    model_pair_player_event, "young_vs_old", "young_vs_middle",
    "young_vs_old_minus_young_vs_middle"
  )[, specification := "player_event_fe"],
  player_fe_old_young_vs_old_old = contrast_terms(
    model_pair_player_event, "old_vs_young", "old_vs_old",
    "old_vs_young_minus_old_vs_old"
  )[, specification := "player_event_fe"],
  player_fe_young_old_vs_old_young = contrast_terms(
    model_pair_player_event, "young_vs_old", "old_vs_young",
    "young_vs_old_minus_old_vs_young"
  )[, specification := "player_event_fe"]
)
pair_contrasts <- rbindlist(contrast_list, fill = TRUE)
pair_contrasts[, p_bh_by_spec := p.adjust(p.value, method = "BH"), by = specification]
fwrite(pair_contrasts, file.path(output_dir, "pair_cell_contrasts.csv"))

young_old <- pair[pair_cell %in% c("young_vs_old", "old_vs_young")]
young_old[, player_young := as.integer(age_group == "young")]
young_old[, post_player_young := format_5_0 * player_young]

model_young_old_game <- feols(
  player_result ~ player_young + post_player_young + rating_diff100 + is_white |
    game_id,
  data = young_old,
  cluster = ~ player_name + game_id
)

young_old_game_terms <- extract_terms(
  model_young_old_game,
  keep_terms = c("player_young", "post_player_young", "rating_diff100", "is_white")
)
young_old_game_terms[, `:=`(
  score_share_effect = estimate / 2,
  score_share_conf.low = conf.low / 2,
  score_share_conf.high = conf.high / 2
)]
fwrite(young_old_game_terms, file.path(output_dir, "young_old_game_fe_coefficients.csv"))

young_rows <- young_old[player_young == 1]
young_old_direct <- rbindlist(list(
  young_rows[, .(
    estimand = "young_score_vs_old_raw",
    estimate = mean(player_result),
    std.error = sd(player_result) / sqrt(.N),
    nobs = .N
  )],
  young_rows[, .(
    estimand = "young_score_minus_elo_expected",
    estimate = mean(result_over_expected),
    std.error = sd(result_over_expected) / sqrt(.N),
    nobs = .N
  )]
), fill = TRUE)
young_old_direct[, `:=`(
  statistic = estimate / std.error,
  p.value = 2 * pnorm(abs(estimate / std.error), lower.tail = FALSE)
)]
fwrite(young_old_direct, file.path(output_dir, "young_old_direct_means.csv"))

cutoff_rows <- list()
for (young_cut in c(20, 25, 30)) {
  for (old_cut in c(35, 40, 45)) {
    tmp <- copy(pair)
    tmp[, age_group_s := classify_age(age_event, young_cut, old_cut)]
    tmp[, opponent_age_group_s := classify_age(opponent_age_event_observed, young_cut, old_cut)]
    tmp <- tmp[
      (age_group_s == "young" & opponent_age_group_s == "old") |
        (age_group_s == "old" & opponent_age_group_s == "young")
    ]
    if (nrow(tmp) < 1000) next
    tmp[, player_young_s := as.integer(age_group_s == "young")]
    tmp[, post_player_young_s := format_5_0 * player_young_s]
    model <- feols(
      player_result ~ player_young_s + post_player_young_s + rating_diff100 + is_white |
        game_id,
      data = tmp,
      cluster = ~ player_name + game_id
    )
    tt <- extract_terms(model, keep_terms = c("player_young_s", "post_player_young_s"))
    tt[, `:=`(
      young_cut = young_cut,
      old_cut = old_cut,
      games = uniqueN(tmp$game_id),
      rows = nrow(tmp),
      score_share_effect = estimate / 2,
      score_share_conf.low = conf.low / 2,
      score_share_conf.high = conf.high / 2
    )]
    cutoff_rows[[paste(young_cut, old_cut, sep = "_")]] <- tt
  }
}
cutoff_sensitivity <- rbindlist(cutoff_rows, fill = TRUE)
cutoff_sensitivity[, p_bh_by_term := p.adjust(p.value, method = "BH"), by = term]
fwrite(cutoff_sensitivity, file.path(output_dir, "young_old_cutoff_sensitivity.csv"))

# Recompute event month explicitly for event-study.
young_old[, event_month := (
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
) - (
  as.integer(format(rule_change_date, "%Y")) * 12L +
    as.integer(format(rule_change_date, "%m"))
)]
event_sample <- young_old[event_month >= -18 & event_month <= 6]
event_model <- feols(
  player_result ~ i(event_month, player_young, ref = -1) +
    rating_diff100 + is_white |
    game_id,
  data = event_sample,
  cluster = ~ player_name + game_id
)
event_terms <- as.data.table(broom::tidy(event_model, conf.int = TRUE))
event_terms <- event_terms[grepl("event_month::", term, fixed = TRUE)]
event_terms[, event_month := as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term))]
event_terms[, `:=`(
  score_share_effect = estimate / 2,
  score_share_conf.low = conf.low / 2,
  score_share_conf.high = conf.high / 2,
  nobs = nobs(event_model)
)]
setorder(event_terms, event_month)
fwrite(event_terms, file.path(output_dir, "young_old_event_study_coefficients.csv"))

event_plot <- ggplot(event_terms, aes(x = event_month, y = score_share_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = score_share_conf.low, ymax = score_share_conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.35, color = "#2364aa") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Young score-share advantage vs old, relative to month -1",
    title = "Event-study: young-vs-old game fixed-effect advantage"
  ) +
  theme_minimal(base_size = 11)
ggsave(
  file.path(output_dir, "young_old_event_study.png"),
  event_plot,
  width = 9,
  height = 5.5,
  dpi = 200
)

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
