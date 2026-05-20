suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
capital_times_file <- "capital_times.csv"
output_dir <- "analysis_outputs/rule_change_country_time_hypotheses"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

parse_clock <- function(x) {
  x <- trimws(x)
  parts <- tstrsplit(x, ":", fixed = TRUE)
  as.numeric(parts[[1]]) + as.numeric(parts[[2]]) / 60
}

wrap24 <- function(x) {
  x %% 24
}

clock_convenience <- function(h) {
  fifelse(h >= 17 & h < 21.5, 1.00,
    fifelse(h >= 8 & h < 17, 0.60,
      fifelse(h >= 21.5 & h < 23.5, 0.35,
        fifelse(h >= 6.5 & h < 8, 0.40, 0.05)
      )
    )
  )
}

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "final_score", "final_score_pregame", "rank_end_round",
  "country_name", "federation", "gdp_per_capita_ppp_logged"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)
ct <- fread(capital_times_file, sep = ";")
setnames(ct, c("country", "first_titled_tuesday", "second_titled_tuesday"))
ct[, `:=`(
  first_local_hour = parse_clock(first_titled_tuesday),
  second_local_hour = parse_clock(second_titled_tuesday)
)]
ct[, `:=`(
  first_convenience = clock_convenience(first_local_hour),
  second_convenience = clock_convenience(second_local_hour)
)]
ct[, `:=`(
  first_clock_penalty = 1 - first_convenience,
  second_clock_penalty = 1 - second_convenience,
  lost_second_convenience = pmax(second_convenience - first_convenience, 0),
  first_work_hour = as.integer(first_local_hour >= 9 & first_local_hour < 17),
  first_sleep_hour = as.integer(first_local_hour < 7 | first_local_hour >= 23),
  first_prime_evening = as.integer(first_local_hour >= 17 & first_local_hour < 21.5),
  second_better_than_first = as.integer(second_convenience > first_convenience),
  americas_clock = as.integer(first_local_hour >= 8 & first_local_hour < 14),
  europe_africa_clock = as.integer(first_local_hour >= 14 & first_local_hour < 19),
  asia_oceania_clock = as.integer(first_local_hour >= 19 | first_local_hour < 8)
)]
ct[, clock_region := fifelse(americas_clock == 1, "Americas",
  fifelse(asia_oceania_clock == 1, "Asia_Oceania", "Europe_Africa")
)]

southern_countries <- c(
  "Argentina", "Australia", "Bolivia", "Botswana", "Brazil", "Chile",
  "Fiji", "Indonesia", "Lesotho", "Madagascar", "Malawi", "Mauritius",
  "Mozambique", "Namibia", "New Zealand", "Paraguay", "Peru",
  "South Africa", "Tanzania", "Uruguay", "Zambia", "Zimbabwe"
)
tropical_countries <- c(
  "Bangladesh", "Barbados", "Belize", "Cambodia", "Colombia",
  "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador",
  "Ghana", "Guam", "Guyana", "Honduras", "Hong Kong", "India",
  "Indonesia", "Jamaica", "Kenya", "Malaysia", "Mexico", "Myanmar",
  "Nicaragua", "Nigeria", "Panama", "Peru", "Philippines", "Puerto Rico",
  "Singapore", "Sri Lanka", "Thailand", "Trinidad and Tobago", "Vietnam",
  "Venezuela"
)
ct[, `:=`(
  southern_hemisphere = as.integer(country %in% southern_countries),
  tropical_country = as.integer(country %in% tropical_countries)
)]
ct[, northern_temperate := as.integer(southern_hemisphere == 0 & tropical_country == 0)]

fwrite(ct, file.path(output_dir, "country_time_exposures.csv"))

df <- merge(df, ct, by.x = "country_name", by.y = "country", all.x = TRUE)
df[, event_id := as.character(date)]
df[, event_date := as.Date(date)]
df[, event_hour_decimal := as.integer(format(date, "%H", tz = "UTC")) +
  as.integer(format(date, "%M", tz = "UTC")) / 60]
df[, event_month_num := as.integer(format(event_date, "%m"))]
df[, format_5_0 := as.integer(event_date >= rule_change_date)]
df[, event_month := (
  as.integer(format(event_date, "%Y")) * 12L + event_month_num
) - (2025L * 12L + 9L)]

df[, event_slot := fifelse(event_hour_decimal < 12, "first", "second")]
df[, local_hour := fifelse(
  event_slot == "first",
  wrap24(first_local_hour + event_hour_decimal - 8),
  wrap24(second_local_hour + event_hour_decimal - 14)
)]
df[, `:=`(
  local_convenience = clock_convenience(local_hour),
  local_clock_penalty = 1 - clock_convenience(local_hour),
  local_work_hour = as.integer(local_hour >= 9 & local_hour < 17),
  local_sleep_hour = as.integer(local_hour < 7 | local_hour >= 23),
  local_prime_evening = as.integer(local_hour >= 17 & local_hour < 21.5),
  north_winter = as.integer(event_month_num %in% c(12, 1, 2)),
  south_winter = as.integer(event_month_num %in% c(6, 7, 8))
)]
df[, winter_in_country := fifelse(
  southern_hemisphere == 1,
  south_winter,
  fifelse(tropical_country == 1, 0L, north_winter)
)]
df[, northern_winter_country := as.integer(northern_temperate == 1 & north_winter == 1)]

df[, `:=`(
  rating_diff100 = (player_rating - opponent_rating) / 100,
  expected_score = 1 / (1 + 10^((opponent_rating - player_rating) / 400)),
  score_c = final_score_pregame - mean(final_score_pregame, na.rm = TRUE),
  gdp_log_c = gdp_per_capita_ppp_logged - mean(gdp_per_capita_ppp_logged, na.rm = TRUE)
)]
df[, result_over_expected := player_result - expected_score]

base <- df[
  player_title != "No Title" &
    !is.na(country_name) &
    country_name != "" &
    !is.na(first_local_hour) &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0
]

base[, event_n_players := uniqueN(player_name), by = event_id]
base[, event_max_round := max(round, na.rm = TRUE), by = event_id]

coverage <- data.table(
  total_rows = nrow(df),
  usable_rows = nrow(base),
  rows_without_country = nrow(df[is.na(country_name) | country_name == ""]),
  rows_without_capital_time = nrow(df[!is.na(country_name) & country_name != "" & is.na(first_local_hour)]),
  countries_in_data = uniqueN(df$country_name),
  countries_with_time_mapping = uniqueN(base$country_name),
  players_usable = uniqueN(base$player_name)
)
fwrite(coverage, file.path(output_dir, "coverage_summary.csv"))

fwrite(
  unique(base[, .(event_id, event_date, event_hour_decimal, format_5_0)])[
    , .N, by = .(format_5_0, event_hour_decimal)
  ][order(format_5_0, event_hour_decimal)],
  file.path(output_dir, "event_slot_counts.csv")
)

country_summary <- base[, .(
  rows = .N,
  players = uniqueN(player_name),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE),
  first_local_hour = first(first_local_hour),
  second_local_hour = first(second_local_hour),
  first_clock_penalty = first(first_clock_penalty),
  lost_second_convenience = first(lost_second_convenience),
  first_work_hour = first(first_work_hour),
  first_sleep_hour = first(first_sleep_hour),
  clock_region = first(clock_region),
  southern_hemisphere = first(southern_hemisphere),
  tropical_country = first(tropical_country)
), by = country_name][order(-rows)]
fwrite(country_summary, file.path(output_dir, "country_summary.csv"))

player_event <- base[, .(
  event_date = first(event_date),
  event_month = first(event_month),
  event_month_num = first(event_month_num),
  format_5_0 = first(format_5_0),
  country_name = first(country_name),
  federation = first(federation),
  clock_region = first(clock_region),
  first_local_hour = first(first_local_hour),
  second_local_hour = first(second_local_hour),
  first_clock_penalty = first(first_clock_penalty),
  lost_second_convenience = first(lost_second_convenience),
  first_work_hour = first(first_work_hour),
  first_sleep_hour = first(first_sleep_hour),
  americas_clock = first(americas_clock),
  asia_oceania_clock = first(asia_oceania_clock),
  southern_hemisphere = first(southern_hemisphere),
  tropical_country = first(tropical_country),
  winter_in_country = first(winter_in_country),
  northern_winter_country = first(northern_winter_country),
  event_n_players = first(event_n_players),
  event_max_round = first(event_max_round),
  player_title = first(player_title),
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
player_event[, `:=`(
  score_after_r1_pct = score_after_r1 / pmax(event_max_round - 1, 1),
  games_after_r1_pct = games_after_r1 / pmax(event_max_round - 1, 1),
  completed_after_r1 = as.integer(games_after_r1 >= pmax(event_max_round - 1, 1)),
  final_score_pct = final_score / event_max_round,
  rank_percentile_high_good = fifelse(
    event_n_players > 1 & !is.na(final_rank_end),
    1 - (final_rank_end - 1) / (event_n_players - 1),
    NA_real_
  )
)]

pre_player <- player_event[format_5_0 == 0, .(
  pre_events = .N,
  mean_pre_rating = mean(mean_player_rating, na.rm = TRUE),
  modal_title = names(sort(table(player_title), decreasing = TRUE))[1],
  country_name = names(sort(table(country_name), decreasing = TRUE))[1],
  clock_region = names(sort(table(clock_region), decreasing = TRUE))[1],
  first_clock_penalty = first(first_clock_penalty),
  lost_second_convenience = first(lost_second_convenience),
  first_work_hour = first(first_work_hour),
  first_sleep_hour = first(first_sleep_hour),
  americas_clock = first(americas_clock),
  asia_oceania_clock = first(asia_oceania_clock),
  southern_hemisphere = first(southern_hemisphere),
  tropical_country = first(tropical_country)
), by = player_name]
post_player <- player_event[format_5_0 == 1, .(
  post_events = .N
), by = player_name]
participation <- merge(pre_player, post_player, by = "player_name", all.x = TRUE)
participation[is.na(post_events), post_events := 0L]
participation[, `:=`(
  has_post = as.integer(post_events > 0),
  log_pre_events = log1p(pre_events),
  gm_title = as.integer(modal_title == "GM")
)]

post_events_available <- uniqueN(player_event[format_5_0 == 1]$event_id)
participation[, post_event_share := post_events / post_events_available]
participation <- participation[pre_events >= 4]
fwrite(participation, file.path(output_dir, "player_country_time_participation.csv"))

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

game_sample <- base[round > 1]
accuracy_sample <- game_sample[
  !is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100
]

game_specs <- list(
  list("G01_accuracy_post_first_penalty", "player_accuracy", accuracy_sample, "format_5_0:first_clock_penalty + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("G02_result_post_first_penalty", "player_result", game_sample, "format_5_0:first_clock_penalty + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("G03_roe_post_first_penalty", "result_over_expected", game_sample, "format_5_0:first_clock_penalty + is_white + factor(round)", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("G04_accuracy_lost_second", "player_accuracy", accuracy_sample, "format_5_0:lost_second_convenience + rating_diff100 + is_white + factor(round)", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("G05_result_lost_second", "player_result", game_sample, "format_5_0:lost_second_convenience + rating_diff100 + is_white + factor(round)", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("G06_accuracy_first_work_hour", "player_accuracy", accuracy_sample, "format_5_0:first_work_hour + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_work_hour"), "post_x_first_work_hour"),
  list("G07_result_first_work_hour", "player_result", game_sample, "format_5_0:first_work_hour + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_work_hour"), "post_x_first_work_hour"),
  list("G08_accuracy_first_sleep_hour", "player_accuracy", accuracy_sample, "format_5_0:first_sleep_hour + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_sleep_hour"), "post_x_first_sleep_hour"),
  list("G09_result_first_sleep_hour", "player_result", game_sample, "format_5_0:first_sleep_hour + rating_diff100 + is_white + factor(round)", c("format_5_0", "first_sleep_hour"), "post_x_first_sleep_hour"),
  list("G10_accuracy_americas", "player_accuracy", accuracy_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("format_5_0", "americas_clock"), "post_x_americas_vs_europe_africa"),
  list("G11_result_americas", "player_result", game_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("format_5_0", "americas_clock"), "post_x_americas_vs_europe_africa"),
  list("G12_accuracy_asia_oceania", "player_accuracy", accuracy_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("format_5_0", "asia_oceania_clock"), "post_x_asia_oceania_vs_europe_africa"),
  list("G13_result_asia_oceania", "player_result", game_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("format_5_0", "asia_oceania_clock"), "post_x_asia_oceania_vs_europe_africa"),
  list("G14_accuracy_post_winter", "player_accuracy", accuracy_sample, "format_5_0:winter_in_country + rating_diff100 + is_white + factor(round)", c("format_5_0", "winter_in_country"), "post_x_country_winter"),
  list("G15_result_post_winter", "player_result", game_sample, "format_5_0:winter_in_country + rating_diff100 + is_white + factor(round)", c("format_5_0", "winter_in_country"), "post_x_country_winter"),
  list("G16_accuracy_actual_local_penalty", "player_accuracy", accuracy_sample, "local_clock_penalty + rating_diff100 + is_white + factor(round)", c("local_clock_penalty"), "actual_local_clock_penalty"),
  list("G17_result_actual_local_penalty", "player_result", game_sample, "local_clock_penalty + rating_diff100 + is_white + factor(round)", c("local_clock_penalty"), "actual_local_clock_penalty")
)

game_rows <- list()
for (sp in game_specs) {
  game_rows[[sp[[1]]]] <- safe_model_row(
    sp[[1]],
    function() {
      feols(
        as.formula(paste(sp[[2]], "~", sp[[4]], "| player_name + event_id")),
        data = sp[[3]],
        cluster = ~ player_name + event_id
      )
    },
    sp[[5]],
    list(outcome = sp[[2]], hypothesis = sp[[6]], specification = "game_level_player_event_fe")
  )
}
game_tests <- rbindlist(game_rows, fill = TRUE)
if ("p.value" %in% names(game_tests)) {
  game_tests[, p_bh := p.adjust(p.value, method = "BH")]
}
fwrite(game_tests, file.path(output_dir, "game_level_country_time_coefficients.csv"))

event_sample <- player_event
event_specs <- list(
  list("E01_score_first_penalty", "score_after_r1_pct", event_sample, "format_5_0:first_clock_penalty + mean_rating_diff100 + white_share", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("E02_accuracy_first_penalty", "mean_accuracy_after_r1", event_sample[!is.na(mean_accuracy_after_r1)], "format_5_0:first_clock_penalty + mean_rating_diff100 + white_share", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("E03_rank_first_penalty", "rank_percentile_high_good", event_sample[!is.na(rank_percentile_high_good)], "format_5_0:first_clock_penalty + mean_rating_diff100 + white_share", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("E04_games_first_penalty", "games_after_r1_pct", event_sample, "format_5_0:first_clock_penalty + mean_rating_diff100 + white_share", c("format_5_0", "first_clock_penalty"), "post_x_first_clock_penalty"),
  list("E05_score_lost_second", "score_after_r1_pct", event_sample, "format_5_0:lost_second_convenience + mean_rating_diff100 + white_share", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("E06_accuracy_lost_second", "mean_accuracy_after_r1", event_sample[!is.na(mean_accuracy_after_r1)], "format_5_0:lost_second_convenience + mean_rating_diff100 + white_share", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("E07_rank_lost_second", "rank_percentile_high_good", event_sample[!is.na(rank_percentile_high_good)], "format_5_0:lost_second_convenience + mean_rating_diff100 + white_share", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("E08_games_lost_second", "games_after_r1_pct", event_sample, "format_5_0:lost_second_convenience + mean_rating_diff100 + white_share", c("format_5_0", "lost_second_convenience"), "post_x_lost_second_convenience"),
  list("E09_score_americas", "score_after_r1_pct", event_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "americas_clock"), "post_x_americas_vs_europe_africa"),
  list("E10_rank_americas", "rank_percentile_high_good", event_sample[!is.na(rank_percentile_high_good)], "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "americas_clock"), "post_x_americas_vs_europe_africa"),
  list("E11_games_americas", "games_after_r1_pct", event_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "americas_clock"), "post_x_americas_vs_europe_africa"),
  list("E12_score_asia_oceania", "score_after_r1_pct", event_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "asia_oceania_clock"), "post_x_asia_oceania_vs_europe_africa"),
  list("E13_rank_asia_oceania", "rank_percentile_high_good", event_sample[!is.na(rank_percentile_high_good)], "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "asia_oceania_clock"), "post_x_asia_oceania_vs_europe_africa"),
  list("E14_games_asia_oceania", "games_after_r1_pct", event_sample, "format_5_0:americas_clock + format_5_0:asia_oceania_clock + mean_rating_diff100 + white_share", c("format_5_0", "asia_oceania_clock"), "post_x_asia_oceania_vs_europe_africa"),
  list("E15_score_winter", "score_after_r1_pct", event_sample, "format_5_0:winter_in_country + mean_rating_diff100 + white_share", c("format_5_0", "winter_in_country"), "post_x_country_winter"),
  list("E16_accuracy_winter", "mean_accuracy_after_r1", event_sample[!is.na(mean_accuracy_after_r1)], "format_5_0:winter_in_country + mean_rating_diff100 + white_share", c("format_5_0", "winter_in_country"), "post_x_country_winter"),
  list("E17_rank_winter", "rank_percentile_high_good", event_sample[!is.na(rank_percentile_high_good)], "format_5_0:winter_in_country + mean_rating_diff100 + white_share", c("format_5_0", "winter_in_country"), "post_x_country_winter")
)

event_rows <- list()
for (sp in event_specs) {
  event_rows[[sp[[1]]]] <- safe_model_row(
    sp[[1]],
    function() {
      feols(
        as.formula(paste(sp[[2]], "~", sp[[4]], "| player_name + event_id")),
        data = sp[[3]],
        cluster = ~ player_name + event_id
      )
    },
    sp[[5]],
    list(outcome = sp[[2]], hypothesis = sp[[6]], specification = "tournament_level_player_event_fe")
  )
}
event_tests <- rbindlist(event_rows, fill = TRUE)
if ("p.value" %in% names(event_tests)) {
  event_tests[, p_bh := p.adjust(p.value, method = "BH")]
}
fwrite(event_tests, file.path(output_dir, "event_level_country_time_coefficients.csv"))

participation_specs <- list(
  list("P01_has_post_first_penalty", "has_post", "first_clock_penalty", c("first_clock_penalty")),
  list("P02_post_events_first_penalty", "post_events", "first_clock_penalty", c("first_clock_penalty")),
  list("P03_has_post_lost_second", "has_post", "lost_second_convenience", c("lost_second_convenience")),
  list("P04_post_events_lost_second", "post_events", "lost_second_convenience", c("lost_second_convenience")),
  list("P05_has_post_americas", "has_post", "americas_clock + asia_oceania_clock", c("americas_clock")),
  list("P06_has_post_asia_oceania", "has_post", "americas_clock + asia_oceania_clock", c("asia_oceania_clock")),
  list("P07_post_events_americas", "post_events", "americas_clock + asia_oceania_clock", c("americas_clock")),
  list("P08_post_events_asia_oceania", "post_events", "americas_clock + asia_oceania_clock", c("asia_oceania_clock"))
)

part_rows <- list()
for (sp in participation_specs) {
  part_rows[[sp[[1]]]] <- safe_model_row(
    sp[[1]],
    function() {
      feols(
        as.formula(paste(sp[[2]], "~", sp[[3]], "+ log_pre_events + mean_pre_rating + gm_title")),
        data = participation,
        cluster = ~ country_name
      )
    },
    sp[[4]],
    list(outcome = sp[[2]], specification = "player_level_selection_country_cluster")
  )
}
participation_tests <- rbindlist(part_rows, fill = TRUE)
if ("p.value" %in% names(participation_tests)) {
  participation_tests[, p_bh := p.adjust(p.value, method = "BH")]
}
fwrite(participation_tests, file.path(output_dir, "participation_country_time_coefficients.csv"))

placebo_cutoffs <- as.Date(c("2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"))
placebo_rows <- list()
for (cutoff in placebo_cutoffs) {
  before_players <- player_event[format_5_0 == 0 & event_date < cutoff, .N, by = player_name][N >= 4]
  after_players <- player_event[format_5_0 == 0 & event_date >= cutoff, .N, by = player_name][N >= 2]
  eligible <- intersect(before_players$player_name, after_players$player_name)
  pbase <- base[format_5_0 == 0 & round > 1 & player_name %in% eligible]
  pbase[, placebo_post := as.integer(event_date >= cutoff)]
  pacc <- pbase[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]

  placebo_specs <- list(
    list("PB_accuracy_first_penalty", "player_accuracy", pacc, "placebo_post:first_clock_penalty + rating_diff100 + is_white + factor(round)", c("placebo_post", "first_clock_penalty")),
    list("PB_result_first_penalty", "player_result", pbase, "placebo_post:first_clock_penalty + rating_diff100 + is_white + factor(round)", c("placebo_post", "first_clock_penalty")),
    list("PB_accuracy_lost_second", "player_accuracy", pacc, "placebo_post:lost_second_convenience + rating_diff100 + is_white + factor(round)", c("placebo_post", "lost_second_convenience")),
    list("PB_result_lost_second", "player_result", pbase, "placebo_post:lost_second_convenience + rating_diff100 + is_white + factor(round)", c("placebo_post", "lost_second_convenience")),
    list("PB_accuracy_americas", "player_accuracy", pacc, "placebo_post:americas_clock + placebo_post:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("placebo_post", "americas_clock")),
    list("PB_result_americas", "player_result", pbase, "placebo_post:americas_clock + placebo_post:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("placebo_post", "americas_clock")),
    list("PB_accuracy_asia_oceania", "player_accuracy", pacc, "placebo_post:americas_clock + placebo_post:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("placebo_post", "asia_oceania_clock")),
    list("PB_result_asia_oceania", "player_result", pbase, "placebo_post:americas_clock + placebo_post:asia_oceania_clock + rating_diff100 + is_white + factor(round)", c("placebo_post", "asia_oceania_clock"))
  )

  for (sp in placebo_specs) {
    key <- paste(cutoff, sp[[1]], sep = "__")
    placebo_rows[[key]] <- safe_model_row(
      key,
      function() {
        feols(
          as.formula(paste(sp[[2]], "~", sp[[4]], "| player_name + event_id")),
          data = sp[[3]],
          cluster = ~ player_name + event_id
        )
      },
      sp[[5]],
      list(
        cutoff = as.character(cutoff),
        outcome = sp[[2]],
        placebo_test = sp[[1]],
        specification = "pre_rule_placebo_player_event_fe"
      )
    )
  }
}
placebo_tests <- rbindlist(placebo_rows, fill = TRUE)
if ("p.value" %in% names(placebo_tests)) {
  placebo_tests[, p_bh_by_cutoff := p.adjust(p.value, method = "BH"), by = cutoff]
}
fwrite(placebo_tests, file.path(output_dir, "pre_rule_placebo_country_time_coefficients.csv"))

event_study_sample <- accuracy_sample[event_month >= -18 & event_month <= 6]
event_study_accuracy <- feols(
  player_accuracy ~ i(event_month, first_clock_penalty, ref = -1) +
    rating_diff100 + is_white + factor(round) |
    player_name + event_id,
  data = event_study_sample,
  cluster = ~ player_name + event_id
)
event_study_result <- feols(
  player_result ~ i(event_month, first_clock_penalty, ref = -1) +
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
fwrite(event_study_terms, file.path(output_dir, "event_study_first_clock_penalty.csv"))

event_plot <- ggplot(event_study_terms, aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.25, color = "#2364aa") +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Coefficient on first-slot clock penalty, relative to month -1",
    title = "Event study: local-time burden of the remaining slot"
  ) +
  theme_minimal(base_size = 11)
ggsave(
  file.path(output_dir, "event_study_first_clock_penalty.png"),
  event_plot,
  width = 9,
  height = 5.5,
  dpi = 200
)

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
