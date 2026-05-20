suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_time_slot_displacement_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "final_score", "final_score_pregame", "rank", "rank_end_round",
  "country_name", "federation"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, event_id := as.character(date)]
df[, event_date := as.Date(date)]
df[, event_hour := as.integer(format(date, "%H", tz = "UTC"))]
df[, event_minute := as.integer(format(date, "%M", tz = "UTC"))]
df[, format_5_0 := as.integer(event_date >= rule_change_date)]

df[, `:=`(
  early_slot = as.integer(event_hour < 12),
  late_slot = as.integer(event_hour >= 12),
  rating_diff100 = (player_rating - opponent_rating) / 100,
  expected_score = 1 / (1 + 10^((opponent_rating - player_rating) / 400)),
  score_c = final_score_pregame - mean(final_score_pregame, na.rm = TRUE)
)]
df[, result_over_expected := player_result - expected_score]

base <- df[
  player_title != "No Title" &
    !is.na(player_name) &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0
]

base[, event_n_players := uniqueN(player_name), by = event_id]
base[, event_max_round := max(round, na.rm = TRUE), by = event_id]

event_slots <- unique(base[, .(event_id, event_date, event_hour, event_minute, format_5_0)])
fwrite(
  event_slots[, .N, by = .(format_5_0, event_hour, event_minute)][
    order(format_5_0, event_hour, event_minute)
  ],
  file.path(output_dir, "event_slot_counts.csv")
)

player_event <- base[, .(
  event_date = first(event_date),
  event_hour = first(event_hour),
  event_minute = first(event_minute),
  format_5_0 = first(format_5_0),
  early_slot = first(early_slot),
  late_slot = first(late_slot),
  event_n_players = first(event_n_players),
  event_max_round = first(event_max_round),
  player_title = first(player_title),
  country_name = first(country_name),
  federation = first(federation),
  mean_player_rating = mean(player_rating, na.rm = TRUE),
  mean_opponent_rating = mean(opponent_rating, na.rm = TRUE),
  mean_rating_diff100 = mean(rating_diff100, na.rm = TRUE),
  white_share = mean(is_white, na.rm = TRUE),
  games_after_r1 = sum(round > 1, na.rm = TRUE),
  score_after_r1 = sum(player_result[round > 1], na.rm = TRUE),
  mean_result_after_r1 = mean(player_result[round > 1], na.rm = TRUE),
  mean_accuracy_after_r1 = mean(
    player_accuracy[round > 1 & player_accuracy > 0 & player_accuracy < 100],
    na.rm = TRUE
  ),
  final_score = max(final_score, na.rm = TRUE),
  final_rank_end = rank_end_round[which.max(round)]
), by = .(player_name, event_id)]

player_event[!is.finite(final_score), final_score := NA_real_]
player_event[, score_after_r1_pct := score_after_r1 / pmax(event_max_round - 1, 1)]
player_event[, games_after_r1_pct := games_after_r1 / pmax(event_max_round - 1, 1)]
player_event[, completed_after_r1 := as.integer(games_after_r1 >= pmax(event_max_round - 1, 1))]
player_event[, final_score_pct := final_score / event_max_round]
player_event[, rank_percentile_high_good := fifelse(
  event_n_players > 1 & !is.na(final_rank_end),
  1 - (final_rank_end - 1) / (event_n_players - 1),
  NA_real_
)]

pre_exposure <- player_event[format_5_0 == 0, .(
  pre_events = .N,
  pre_late_events = sum(late_slot),
  pre_early_events = sum(early_slot),
  pre_late_share = mean(late_slot),
  pre_early_share = mean(early_slot),
  first_pre_event = min(event_date),
  last_pre_event = max(event_date),
  mean_pre_rating = mean(mean_player_rating, na.rm = TRUE),
  modal_title = names(sort(table(player_title), decreasing = TRUE))[1],
  modal_country = names(sort(table(country_name), decreasing = TRUE))[1]
), by = player_name]

post_exposure <- player_event[format_5_0 == 1, .(
  post_events = .N,
  first_post_event = min(event_date),
  last_post_event = max(event_date)
), by = player_name]

exposure <- merge(pre_exposure, post_exposure, by = "player_name", all.x = TRUE)
exposure[is.na(post_events), post_events := 0L]
exposure[, `:=`(
  has_post = as.integer(post_events > 0),
  switcher = as.integer(pre_events >= 4 & post_events >= 2),
  late_regular = as.integer(pre_events >= 4 & pre_late_events >= 3 & pre_late_share >= 0.60),
  early_regular = as.integer(pre_events >= 4 & pre_early_events >= 3 & pre_early_share >= 0.60),
  pure_late = as.integer(pre_events >= 4 & pre_late_share >= 0.75),
  pure_early = as.integer(pre_events >= 4 & pre_early_share >= 0.75),
  pre_slot_balance = pre_late_share - pre_early_share,
  log_pre_events = log1p(pre_events),
  gm_title = as.integer(modal_title == "GM")
)]
exposure[, slot_type := fifelse(
  late_regular == 1 & early_regular == 0, "late_regular",
  fifelse(early_regular == 1 & late_regular == 0, "early_regular", "mixed")
)]
exposure[, late_regular_vs_early := fifelse(
  late_regular == 1 & early_regular == 0, 1,
  fifelse(early_regular == 1 & late_regular == 0, 0, NA_real_)
)]

fwrite(exposure, file.path(output_dir, "player_pre_slot_exposure.csv"))

sample_summary <- rbindlist(list(
  data.table(statistic = "players_with_pre_events", value = nrow(exposure)),
  data.table(statistic = "players_with_pre_and_post_any", value = nrow(exposure[has_post == 1])),
  data.table(statistic = "switchers_pre4_post2", value = nrow(exposure[switcher == 1])),
  data.table(statistic = "switcher_late_regular", value = nrow(exposure[switcher == 1 & slot_type == "late_regular"])),
  data.table(statistic = "switcher_early_regular", value = nrow(exposure[switcher == 1 & slot_type == "early_regular"])),
  data.table(statistic = "switcher_mixed", value = nrow(exposure[switcher == 1 & slot_type == "mixed"])),
  data.table(statistic = "post_events_available", value = nrow(event_slots[format_5_0 == 1])),
  data.table(statistic = "pre_events_available", value = nrow(event_slots[format_5_0 == 0]))
))
fwrite(sample_summary, file.path(output_dir, "sample_summary.csv"))

fwrite(
  exposure[switcher == 1, .(
    players = .N,
    mean_pre_late_share = mean(pre_late_share),
    mean_pre_events = mean(pre_events),
    mean_post_events = mean(post_events),
    mean_pre_rating = mean(mean_pre_rating, na.rm = TRUE)
  ), by = slot_type][order(slot_type)],
  file.path(output_dir, "switcher_slot_type_summary.csv")
)

base <- merge(
  base,
  exposure[, .(
    player_name, pre_events, pre_late_events, pre_early_events, pre_late_share,
    pre_early_share, pre_slot_balance, post_events, has_post, switcher,
    late_regular, early_regular, late_regular_vs_early, pure_late, pure_early,
    slot_type
  )],
  by = "player_name",
  all.x = TRUE
)

player_event <- merge(
  player_event,
  exposure[, .(
    player_name, pre_events, pre_late_events, pre_early_events, pre_late_share,
    pre_early_share, pre_slot_balance, post_events, has_post, switcher,
    late_regular, early_regular, late_regular_vs_early, pure_late, pure_early,
    slot_type
  )],
  by = "player_name",
  all.x = TRUE
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
  tt[term == target_term][, nobs := nobs(model)]
}

safe_model_row <- function(id, model_call, target, metadata = list()) {
  tryCatch({
    model <- model_call()
    row <- extract_target(model, target)
    for (nm in names(metadata)) row[, (nm) := metadata[[nm]]]
    row[, test_id := id]
    row
  }, error = function(e) {
    row <- data.table(test_id = id, error = e$message)
    for (nm in names(metadata)) row[, (nm) := metadata[[nm]]]
    row
  })
}

game_sample <- base[round > 1 & switcher == 1]
game_sample[, event_month := (
  as.integer(format(event_date, "%Y")) * 12L + as.integer(format(event_date, "%m"))
) - (2025L * 12L + 9L)]
accuracy_sample <- game_sample[
  !is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100
]
late_early_game <- game_sample[!is.na(late_regular_vs_early)]
late_early_accuracy <- accuracy_sample[!is.na(late_regular_vs_early)]

game_specs <- list(
  list(
    id = "G01_accuracy_continuous_basic",
    outcome = "player_accuracy",
    data = accuracy_sample,
    rhs = "format_5_0:pre_late_share + rating_diff100 + is_white + factor(round)",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers",
    controls = "rating_diff_color_round"
  ),
  list(
    id = "G02_accuracy_continuous_score_control",
    outcome = "player_accuracy",
    data = accuracy_sample,
    rhs = "format_5_0:pre_late_share + rating_diff100 + is_white + score_c + factor(round)",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers",
    controls = "rating_diff_color_score_round"
  ),
  list(
    id = "G03_result_continuous_basic",
    outcome = "player_result",
    data = game_sample,
    rhs = "format_5_0:pre_late_share + rating_diff100 + is_white + factor(round)",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers",
    controls = "rating_diff_color_round"
  ),
  list(
    id = "G04_result_continuous_score_control",
    outcome = "player_result",
    data = game_sample,
    rhs = "format_5_0:pre_late_share + rating_diff100 + is_white + score_c + factor(round)",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers",
    controls = "rating_diff_color_score_round"
  ),
  list(
    id = "G05_roe_continuous_basic",
    outcome = "result_over_expected",
    data = game_sample,
    rhs = "format_5_0:pre_late_share + is_white + factor(round)",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers",
    controls = "color_round_expected_score_netout"
  ),
  list(
    id = "G06_accuracy_late_vs_early",
    outcome = "player_accuracy",
    data = late_early_accuracy,
    rhs = "format_5_0:late_regular_vs_early + rating_diff100 + is_white + factor(round)",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular",
    controls = "rating_diff_color_round"
  ),
  list(
    id = "G07_result_late_vs_early",
    outcome = "player_result",
    data = late_early_game,
    rhs = "format_5_0:late_regular_vs_early + rating_diff100 + is_white + factor(round)",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular",
    controls = "rating_diff_color_round"
  ),
  list(
    id = "G08_roe_late_vs_early",
    outcome = "result_over_expected",
    data = late_early_game,
    rhs = "format_5_0:late_regular_vs_early + is_white + factor(round)",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular",
    controls = "color_round_expected_score_netout"
  )
)

game_rows <- list()
for (sp in game_specs) {
  game_rows[[sp$id]] <- safe_model_row(
    sp$id,
    function() {
      feols(
        as.formula(paste(sp$outcome, "~", sp$rhs, "| player_name + event_id")),
        data = sp$data,
        cluster = ~ player_name + event_id
      )
    },
    sp$target,
    list(
      outcome = sp$outcome,
      treatment = sp$treatment,
      sample = sp$sample,
      controls = sp$controls,
      specification = "player_event_fe_game_level"
    )
  )
}
game_tests <- rbindlist(game_rows, fill = TRUE)
if ("p.value" %in% names(game_tests)) {
  game_tests[, p_bh := p.adjust(p.value, method = "BH")]
}
fwrite(game_tests, file.path(output_dir, "game_level_displacement_coefficients.csv"))

event_sample <- player_event[switcher == 1]
late_early_event <- event_sample[!is.na(late_regular_vs_early)]

event_specs <- list(
  list(
    id = "E01_score_after_r1_continuous",
    outcome = "score_after_r1_pct",
    data = event_sample,
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E02_mean_accuracy_continuous",
    outcome = "mean_accuracy_after_r1",
    data = event_sample[!is.na(mean_accuracy_after_r1)],
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E03_rank_percentile_continuous",
    outcome = "rank_percentile_high_good",
    data = event_sample[!is.na(rank_percentile_high_good)],
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E04_mean_result_continuous",
    outcome = "mean_result_after_r1",
    data = event_sample[!is.na(mean_result_after_r1)],
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E05_games_after_r1_continuous",
    outcome = "games_after_r1_pct",
    data = event_sample,
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E06_completed_after_r1_continuous",
    outcome = "completed_after_r1",
    data = event_sample,
    rhs = "format_5_0:pre_late_share + mean_rating_diff100 + white_share",
    target = c("format_5_0", "pre_late_share"),
    treatment = "pre_late_share",
    sample = "all_switchers"
  ),
  list(
    id = "E07_score_after_r1_late_vs_early",
    outcome = "score_after_r1_pct",
    data = late_early_event,
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  ),
  list(
    id = "E08_mean_accuracy_late_vs_early",
    outcome = "mean_accuracy_after_r1",
    data = late_early_event[!is.na(mean_accuracy_after_r1)],
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  ),
  list(
    id = "E09_rank_percentile_late_vs_early",
    outcome = "rank_percentile_high_good",
    data = late_early_event[!is.na(rank_percentile_high_good)],
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  ),
  list(
    id = "E10_mean_result_late_vs_early",
    outcome = "mean_result_after_r1",
    data = late_early_event[!is.na(mean_result_after_r1)],
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  ),
  list(
    id = "E11_games_after_r1_late_vs_early",
    outcome = "games_after_r1_pct",
    data = late_early_event,
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  ),
  list(
    id = "E12_completed_after_r1_late_vs_early",
    outcome = "completed_after_r1",
    data = late_early_event,
    rhs = "format_5_0:late_regular_vs_early + mean_rating_diff100 + white_share",
    target = c("format_5_0", "late_regular_vs_early"),
    treatment = "late_regular_vs_early",
    sample = "late_regular_vs_early_regular"
  )
)

event_rows <- list()
for (sp in event_specs) {
  event_rows[[sp$id]] <- safe_model_row(
    sp$id,
    function() {
      feols(
        as.formula(paste(sp$outcome, "~", sp$rhs, "| player_name + event_id")),
        data = sp$data,
        cluster = ~ player_name + event_id
      )
    },
    sp$target,
    list(
      outcome = sp$outcome,
      treatment = sp$treatment,
      sample = sp$sample,
      controls = "mean_rating_diff_white_share",
      specification = "player_event_fe_tournament_level"
    )
  )
}
event_tests <- rbindlist(event_rows, fill = TRUE)
if ("p.value" %in% names(event_tests)) {
  event_tests[, p_bh := p.adjust(p.value, method = "BH")]
}
fwrite(event_tests, file.path(output_dir, "event_level_displacement_coefficients.csv"))

post_events_available <- nrow(event_slots[format_5_0 == 1])
participation <- copy(exposure[pre_events >= 4])
participation[, post_event_share := post_events / post_events_available]

participation_rows <- list(
  has_post_linear = safe_model_row(
    "P01_has_post_linear",
    function() {
      feols(
        has_post ~ pre_late_share + log_pre_events + mean_pre_rating + gm_title,
        data = participation,
        vcov = "hetero"
      )
    },
    c("pre_late_share"),
    list(outcome = "has_post", specification = "player_level_linear_probability")
  ),
  post_count_linear = safe_model_row(
    "P02_post_events_linear",
    function() {
      feols(
        post_events ~ pre_late_share + log_pre_events + mean_pre_rating + gm_title,
        data = participation,
        vcov = "hetero"
      )
    },
    c("pre_late_share"),
    list(outcome = "post_events", specification = "player_level_ols")
  ),
  post_share_linear = safe_model_row(
    "P03_post_event_share_linear",
    function() {
      feols(
        post_event_share ~ pre_late_share + log_pre_events + mean_pre_rating + gm_title,
        data = participation,
        vcov = "hetero"
      )
    },
    c("pre_late_share"),
    list(outcome = "post_event_share", specification = "player_level_ols")
  )
)
participation_tests <- rbindlist(participation_rows, fill = TRUE)
fwrite(participation_tests, file.path(output_dir, "participation_selection_coefficients.csv"))

placebo_cutoffs <- as.Date(c("2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"))
placebo_rows <- list()

for (cutoff in placebo_cutoffs) {
  pre_events_before <- player_event[format_5_0 == 0 & event_date < cutoff, .(
    placebo_pre_events = .N,
    placebo_late_share = mean(late_slot),
    placebo_late_events = sum(late_slot),
    placebo_early_events = sum(early_slot)
  ), by = player_name]
  pre_events_after <- player_event[format_5_0 == 0 & event_date >= cutoff, .(
    placebo_after_events = .N
  ), by = player_name]
  placebo_exposure <- merge(pre_events_before, pre_events_after, by = "player_name", all.x = TRUE)
  placebo_exposure[is.na(placebo_after_events), placebo_after_events := 0L]
  placebo_exposure <- placebo_exposure[placebo_pre_events >= 4 & placebo_after_events >= 2]

  pbase <- merge(
    base[format_5_0 == 0 & round > 1],
    placebo_exposure[, .(player_name, placebo_late_share)],
    by = "player_name",
    all.x = FALSE
  )
  pbase[, placebo_post := as.integer(event_date >= cutoff)]
  pacc <- pbase[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]

  placebo_specs <- list(
    list(
      id = "PB_accuracy",
      outcome = "player_accuracy",
      data = pacc,
      rhs = "placebo_post:placebo_late_share + rating_diff100 + is_white + factor(round)",
      target = c("placebo_post", "placebo_late_share")
    ),
    list(
      id = "PB_result",
      outcome = "player_result",
      data = pbase,
      rhs = "placebo_post:placebo_late_share + rating_diff100 + is_white + factor(round)",
      target = c("placebo_post", "placebo_late_share")
    ),
    list(
      id = "PB_roe",
      outcome = "result_over_expected",
      data = pbase,
      rhs = "placebo_post:placebo_late_share + is_white + factor(round)",
      target = c("placebo_post", "placebo_late_share")
    )
  )

  for (sp in placebo_specs) {
    key <- paste(cutoff, sp$id, sep = "__")
    placebo_rows[[key]] <- safe_model_row(
      key,
      function() {
        feols(
          as.formula(paste(sp$outcome, "~", sp$rhs, "| player_name + event_id")),
          data = sp$data,
          cluster = ~ player_name + event_id
        )
      },
      sp$target,
      list(
        cutoff = as.character(cutoff),
        outcome = sp$outcome,
        specification = "pre_rule_placebo_player_event_fe"
      )
    )
  }
}

placebo_tests <- rbindlist(placebo_rows, fill = TRUE)
if ("p.value" %in% names(placebo_tests)) {
  placebo_tests[, p_bh := p.adjust(p.value, method = "BH"), by = cutoff]
}
fwrite(placebo_tests, file.path(output_dir, "pre_rule_placebo_coefficients.csv"))

event_study_sample <- accuracy_sample[event_month >= -18 & event_month <= 6]
event_study_accuracy <- feols(
  player_accuracy ~ i(event_month, pre_late_share, ref = -1) +
    rating_diff100 + is_white + factor(round) |
    player_name + event_id,
  data = event_study_sample,
  cluster = ~ player_name + event_id
)
event_study_result <- feols(
  player_result ~ i(event_month, pre_late_share, ref = -1) +
    rating_diff100 + is_white + factor(round) |
    player_name + event_id,
  data = game_sample[event_month >= -18 & event_month <= 6],
  cluster = ~ player_name + event_id
)

parse_event_terms <- function(model, outcome) {
  out <- as.data.table(broom::tidy(model, conf.int = TRUE))
  out <- out[grepl("event_month::", term, fixed = TRUE)]
  out[, event_month := as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term))]
  out[, `:=`(outcome = outcome, nobs = nobs(model))]
  setorder(out, event_month)
  out
}

event_study_terms <- rbindlist(list(
  parse_event_terms(event_study_accuracy, "player_accuracy"),
  parse_event_terms(event_study_result, "player_result")
), fill = TRUE)
fwrite(event_study_terms, file.path(output_dir, "event_study_pre_late_share.csv"))

event_plot <- ggplot(event_study_terms, aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.25, color = "#2364aa") +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Coefficient on pre-rule late-slot share, relative to month -1",
    title = "Event study: performance of pre-rule late-slot regulars"
  ) +
  theme_minimal(base_size = 11)
ggsave(
  file.path(output_dir, "event_study_pre_late_share.png"),
  event_plot,
  width = 9,
  height = 5.5,
  dpi = 200
)

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
