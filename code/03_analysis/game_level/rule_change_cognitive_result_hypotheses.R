suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_cognitive_result_hypotheses"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_result", "round", "date", "is_white",
  "birthday", "final_score_pregame", "rank", "in_prizes", "bubble",
  "eliminated", "leader", "played_against_bubble", "played_against_prizes",
  "played_against_eliminated", "played_against_leader", "classic_rating",
  "blitz_rating", "rapid_rating"
)

df <- fread(input_file, select = needed_cols, showProgress = TRUE)

df[, event_id := as.character(date)]
df[, date := as.Date(date)]
df[, event_year := as.integer(format(date, "%Y"))]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]
df[, age_event := event_year - birthday]

base <- df[
  player_title != "No Title" &
    !is.na(player_result) &
    !is.na(player_rating) &
    !is.na(opponent_rating) &
    player_rating > 0 &
    opponent_rating > 0 &
    !is.na(age_event) &
    age_event >= 8 &
    age_event <= 90
]

base[, `:=`(
  player_rating100 = (player_rating - 2500) / 100,
  opponent_rating100 = (opponent_rating - 2500) / 100,
  rating_diff100 = (player_rating - opponent_rating) / 100,
  abs_rating_diff100 = abs(player_rating - opponent_rating) / 100,
  expected_score = 1 / (1 + 10^((opponent_rating - player_rating) / 400)),
  age10 = (age_event - 35) / 10,
  round_c = round - 6,
  late_round = as.integer(round >= 8),
  final_rounds = as.integer(round >= 10),
  bubble_zone = as.integer(bubble == 1),
  prize_zone = as.integer(in_prizes == 1),
  eliminated_zone = as.integer(eliminated == 1),
  leader_zone = as.integer(leader == 1),
  opponent_bubble_zone = as.integer(played_against_bubble == 1),
  opponent_prize_zone = as.integer(played_against_prizes == 1),
  opponent_eliminated_zone = as.integer(played_against_eliminated == 1),
  opponent_leader_zone = as.integer(played_against_leader == 1),
  score_c = final_score_pregame - mean(final_score_pregame, na.rm = TRUE),
  online_classic_gap100 = (player_rating - classic_rating) / 100,
  online_blitz_gap100 = (player_rating - blitz_rating) / 100
)]
base[, result_over_expected := player_result - expected_score]

base[, game_id := paste(
  event_id,
  round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "||"
)]

opponent_lookup <- base[, .(
  event_id,
  round,
  player_name,
  opponent_age_event = age_event,
  opponent_birthday_observed = birthday,
  opponent_final_score_pregame = final_score_pregame
)]
setkey(opponent_lookup, event_id, round, player_name)
base[, `:=`(
  opponent_age_event_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_age_event
  ],
  opponent_birthday_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_birthday_observed
  ],
  opponent_final_score_pregame_observed = opponent_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_final_score_pregame
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

base[, opponent_score_c := opponent_final_score_pregame_observed - mean(final_score_pregame, na.rm = TRUE)]

pair <- base[
  round > 1 &
    !is.na(opponent_age_event_observed) &
    opponent_age_event_observed >= 8 &
    opponent_age_event_observed <= 90
]

pair[, `:=`(
  opponent_age10 = (opponent_age_event_observed - 35) / 10,
  age_gap10 = (age_event - opponent_age_event_observed) / 10,
  young = as.integer(age_event <= 25),
  old = as.integer(age_event >= 40),
  opponent_young = as.integer(opponent_age_event_observed <= 25),
  opponent_old = as.integer(opponent_age_event_observed >= 40),
  young_vs_old = as.integer(age_event <= 25 & opponent_age_event_observed >= 40),
  old_vs_young = as.integer(age_event >= 40 & opponent_age_event_observed <= 25),
  young_old_pair = as.integer(
    (age_event <= 25 & opponent_age_event_observed >= 40) |
      (age_event >= 40 & opponent_age_event_observed <= 25)
  )
)]

setorder(base, player_name, event_id, round)
lag_cols <- c(
  "round", "player_result", "rating_diff100", "expected_score",
  "result_over_expected", "score_c", "bubble_zone", "prize_zone",
  "eliminated_zone", "leader_zone"
)
for (col in lag_cols) {
  base[, paste0("prev_", col) := shift(get(col)), by = .(player_name, event_id)]
}

lagged <- base[
  round > 1 &
    prev_round == round - 1 &
    !is.na(prev_player_result) &
    !is.na(prev_rating_diff100) &
    !is.na(prev_expected_score)
]
lagged[, `:=`(
  prev_loss = as.integer(prev_player_result == 0),
  prev_win = as.integer(prev_player_result == 1),
  prev_draw = as.integer(prev_player_result == 0.5),
  prev_unexpected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 >= 2),
  prev_expected_loss = as.integer(prev_player_result == 0 & prev_rating_diff100 <= -2),
  prev_upset_win = as.integer(prev_player_result == 1 & prev_rating_diff100 <= -2),
  prev_expected_win = as.integer(prev_player_result == 1 & prev_rating_diff100 >= 2),
  prev_negative_surprise = pmax(-prev_result_over_expected, 0),
  prev_positive_surprise = pmax(prev_result_over_expected, 0),
  prev_negative_shock = pmin(prev_result_over_expected, 0),
  prev_positive_shock = pmax(prev_result_over_expected, 0)
)]

young_old <- pair[young_old_pair == 1]
young_old[, player_young := as.integer(young == 1)]
young_old[, `:=`(
  player_young_late = player_young * late_round,
  post_player_young = format_5_0 * player_young,
  post_player_young_late = format_5_0 * player_young * late_round
)]

fwrite(
  data.table(
    pair_rows = nrow(pair),
    pair_players = uniqueN(pair$player_name),
    pair_games = uniqueN(pair$game_id),
    lagged_rows = nrow(lagged),
    lagged_players = uniqueN(lagged$player_name),
    young_old_rows = nrow(young_old),
    young_old_games = uniqueN(young_old$game_id),
    post_pair_rows = nrow(pair[format_5_0 == 1]),
    post_young_old_rows = nrow(young_old[format_5_0 == 1])
  ),
  file.path(output_dir, "sample_summary.csv")
)

fwrite(
  pair[, .(
    rows = .N,
    mean_result = mean(player_result),
    mean_result_over_expected = mean(result_over_expected),
    mean_rating_diff = mean(player_rating - opponent_rating),
    mean_age = mean(age_event),
    mean_opp_age = mean(opponent_age_event_observed)
  ), by = .(
    format_5_0,
    age_matchup = fifelse(young_vs_old == 1, "young_vs_old",
      fifelse(old_vs_young == 1, "old_vs_young",
        fifelse(young == 1 & opponent_young == 1, "young_vs_young",
          fifelse(old == 1 & opponent_old == 1, "old_vs_old", "other")
        )
      )
    )
  )][order(format_5_0, age_matchup)],
  file.path(output_dir, "age_matchup_descriptives.csv")
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
  if (!"p.value" %in% names(dt)) dt[, p.value := NA_real_]
  dt
}

event_controls <- "+ rating_diff100 + is_white + score_c + opponent_score_c + factor(round)"

fit_event <- function(data, outcome, rhs) {
  fml <- as.formula(paste(outcome, "~", rhs, event_controls, "| player_name + event_id"))
  feols(fml, data = data, cluster = ~ player_name + event_id)
}

fit_game <- function(data, outcome, rhs, controls = "+ rating_diff100 + is_white") {
  fml <- as.formula(paste(outcome, "~", rhs, controls, "| game_id"))
  feols(fml, data = data, cluster = ~ player_name + game_id)
}

event_hypotheses <- list(
  list(
    id = "C01_age_late_round_fatigue",
    family = "cognitive_fatigue",
    data = pair,
    rhs = "age10:late_round",
    target = c("age10", "late_round"),
    story = "Older players lose relative score in late rounds after controlling for rating and player/event FE."
  ),
  list(
    id = "C02_age_round_slope",
    family = "cognitive_fatigue",
    data = pair,
    rhs = "age10:round_c",
    target = c("age10", "round_c"),
    story = "Older players decline as rounds accumulate."
  ),
  list(
    id = "C03_age_gap_conversion",
    family = "age_mismatch",
    data = pair,
    rhs = "age_gap10",
    target = c("age_gap10"),
    story = "Player age relative to opponent predicts score after rating controls."
  ),
  list(
    id = "C04_age_gap_late_round",
    family = "age_mismatch_x_fatigue",
    data = pair,
    rhs = "age_gap10:late_round",
    target = c("age_gap10", "late_round"),
    story = "Being older than the opponent is more costly in late rounds."
  ),
  list(
    id = "C05_bubble_favorite_choking",
    family = "threshold_pressure",
    data = pair,
    rhs = "bubble_zone:rating_diff100",
    target = c("bubble_zone", "rating_diff100"),
    story = "Favorites on the prize bubble convert rating advantages less well."
  ),
  list(
    id = "C06_leader_favorite_choking",
    family = "front_runner_pressure",
    data = pair,
    rhs = "leader_zone:rating_diff100",
    target = c("leader_zone", "rating_diff100"),
    story = "Leaders convert rating advantages differently under pressure."
  ),
  list(
    id = "C07_eliminated_freer_play",
    family = "low_downside_risk_taking",
    data = pair,
    rhs = "eliminated_zone:rating_diff100",
    target = c("eliminated_zone", "rating_diff100"),
    story = "Eliminated players convert rating advantages differently when downside is low."
  ),
  list(
    id = "C08_age_pressure",
    family = "age_x_pressure",
    data = pair,
    rhs = "age10:bubble_zone",
    target = c("age10", "bubble_zone"),
    story = "Older players react differently to bubble pressure."
  ),
  list(
    id = "C09_unexpected_loss_tilt",
    family = "tilt",
    data = lagged,
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_unexpected_loss"),
    story = "Unexpected previous losses reduce next-game score beyond rating and pairing controls."
  ),
  list(
    id = "C10_upset_win_overconfidence",
    family = "overconfidence_or_momentum",
    data = lagged,
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_upset_win"),
    story = "Upset wins affect next-game score, consistent with momentum or overconfidence."
  ),
  list(
    id = "C11_tilt_by_age",
    family = "tilt_x_age",
    data = lagged,
    rhs = "prev_unexpected_loss:age10",
    target = c("prev_unexpected_loss", "age10"),
    story = "Unexpected-loss tilt varies with age."
  ),
  list(
    id = "C12_upset_win_by_age",
    family = "overconfidence_x_age",
    data = lagged,
    rhs = "prev_upset_win:age10",
    target = c("prev_upset_win", "age10"),
    story = "Upset-win momentum or overconfidence varies with age."
  ),
  list(
    id = "C13_negative_surprise_magnitude",
    family = "continuous_tilt",
    data = lagged,
    rhs = "prev_negative_surprise + prev_positive_surprise",
    target = c("prev_negative_surprise"),
    story = "Larger previous-game negative surprises reduce the next-game score."
  ),
  list(
    id = "C14_positive_surprise_magnitude",
    family = "continuous_momentum_or_overconfidence",
    data = lagged,
    rhs = "prev_negative_surprise + prev_positive_surprise",
    target = c("prev_positive_surprise"),
    story = "Larger previous-game positive surprises change the next-game score."
  ),
  list(
    id = "C15_negative_surprise_by_age",
    family = "continuous_tilt_x_age",
    data = lagged,
    rhs = "prev_negative_surprise:age10",
    target = c("prev_negative_surprise", "age10"),
    story = "Negative-surprise tilt varies by age."
  ),
  list(
    id = "C16_positive_surprise_by_age",
    family = "continuous_momentum_x_age",
    data = lagged,
    rhs = "prev_positive_surprise:age10",
    target = c("prev_positive_surprise", "age10"),
    story = "Positive-surprise carryover varies by age."
  ),
  list(
    id = "C17_leader_pressure_by_age",
    family = "front_runner_pressure_x_age",
    data = pair,
    rhs = "age10:leader_zone",
    target = c("age10", "leader_zone"),
    story = "Older and younger leaders differ in pressure response."
  ),
  list(
    id = "C18_eliminated_state_by_age",
    family = "low_downside_x_age",
    data = pair,
    rhs = "age10:eliminated_zone",
    target = c("age10", "eliminated_zone"),
    story = "Older and younger eliminated players differ when downside risk is low."
  ),
  list(
    id = "C19_opponent_bubble_disruption",
    family = "opponent_pressure_spillover",
    data = pair,
    rhs = "opponent_bubble_zone",
    target = c("opponent_bubble_zone"),
    story = "Facing a bubble opponent changes performance beyond rating and own score."
  ),
  list(
    id = "C20_opponent_leader_disruption",
    family = "opponent_pressure_spillover",
    data = pair,
    rhs = "opponent_leader_zone",
    target = c("opponent_leader_zone"),
    story = "Facing a tournament leader changes performance beyond rating and own score."
  ),
  list(
    id = "C21_online_specialist_late_round",
    family = "speed_adaptation_x_fatigue",
    data = pair,
    rhs = "online_classic_gap100:late_round",
    target = c("online_classic_gap100", "late_round"),
    story = "Online specialists are more resilient in late rounds."
  )
)

event_rows <- list()
for (h in event_hypotheses) {
  event_rows[[h$id]] <- tryCatch({
    model <- fit_event(h$data, "player_result", h$rhs)
    row <- extract_target(model, h$target)
    row[, `:=`(
      hypothesis = h$id,
      family = h$family,
      specification = "player_event_fe",
      story = h$story
    )]
    row
  }, error = function(e) {
    data.table(
      hypothesis = h$id,
      family = h$family,
      specification = "player_event_fe",
      story = h$story,
      error = e$message
    )
  })
}

event_tests <- rbindlist(event_rows, fill = TRUE)
event_tests <- ensure_p_value(event_tests)
event_tests[, p_bh := p.adjust(p.value, method = "BH")]
setorder(event_tests, p.value)
fwrite(event_tests, file.path(output_dir, "cognitive_event_fe_hypothesis_coefficients.csv"))

game_hypotheses <- list(
  list(
    id = "G01_young_vs_old",
    family = "age_mismatch_game_fe",
    data = young_old,
    rhs = "player_young",
    target = c("player_young"),
    story = "Young player's within-game score advantage against old opponents."
  ),
  list(
    id = "G02_young_vs_old_late_round",
    family = "age_mismatch_x_fatigue_game_fe",
    data = young_old,
    rhs = "player_young * late_round",
    target = c("player_young", "late_round"),
    story = "Young-vs-old advantage is larger in late rounds."
  ),
  list(
    id = "G03_young_vs_old_post",
    family = "age_mismatch_x_rule_change_game_fe",
    data = young_old,
    rhs = "player_young * format_5_0",
    target = c("player_young", "format_5_0"),
    story = "Young-vs-old advantage changes after the 5+0 rule change."
  ),
  list(
    id = "G04_young_vs_old_post_late",
    family = "age_mismatch_x_rule_change_x_fatigue_game_fe",
    data = young_old,
    rhs = "player_young * format_5_0 * late_round",
    target = c("player_young", "format_5_0", "late_round"),
    story = "The post-rule young-vs-old advantage is especially large in late rounds."
  ),
  list(
    id = "G05_bubble_pressure_game_fe",
    family = "threshold_pressure_game_fe",
    data = pair,
    rhs = "bubble_zone:rating_diff100",
    target = c("bubble_zone", "rating_diff100"),
    story = "Within a game, bubble players convert rating edges differently."
  ),
  list(
    id = "G06_eliminated_freer_play_game_fe",
    family = "low_downside_game_fe",
    data = pair,
    rhs = "eliminated_zone:rating_diff100",
    target = c("eliminated_zone", "rating_diff100"),
    story = "Within a game, eliminated players convert rating edges differently."
  ),
  list(
    id = "G07_age_gap_late_round_game_fe",
    family = "age_mismatch_x_fatigue_game_fe",
    data = pair,
    rhs = "age_gap10:late_round",
    target = c("age_gap10", "late_round"),
    story = "Within a game, being older than the opponent has a late-round penalty."
  ),
  list(
    id = "G08_unexpected_loss_tilt_game_fe",
    family = "tilt_game_fe",
    data = lagged,
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_unexpected_loss"),
    story = "Unexpected-loss tilt survives current-game fixed effects."
  ),
  list(
    id = "G09_upset_win_carryover_game_fe",
    family = "overconfidence_or_momentum_game_fe",
    data = lagged,
    rhs = "prev_unexpected_loss + prev_expected_loss + prev_upset_win",
    target = c("prev_upset_win"),
    story = "Upset-win carryover survives current-game fixed effects."
  ),
  list(
    id = "G10_negative_surprise_game_fe",
    family = "continuous_tilt_game_fe",
    data = lagged,
    rhs = "prev_negative_surprise + prev_positive_surprise",
    target = c("prev_negative_surprise"),
    story = "Continuous negative surprise predicts the next-game result within current games."
  ),
  list(
    id = "G11_positive_surprise_game_fe",
    family = "continuous_momentum_or_overconfidence_game_fe",
    data = lagged,
    rhs = "prev_negative_surprise + prev_positive_surprise",
    target = c("prev_positive_surprise"),
    story = "Continuous positive surprise predicts the next-game result within current games."
  )
)

game_rows <- list()
for (h in game_hypotheses) {
  game_rows[[h$id]] <- tryCatch({
    model <- fit_game(h$data, "player_result", h$rhs)
    row <- extract_target(model, h$target)
    row[, `:=`(
      hypothesis = h$id,
      family = h$family,
      specification = "game_fe",
      story = h$story,
      score_share_effect = estimate / 2,
      score_share_conf.low = conf.low / 2,
      score_share_conf.high = conf.high / 2
    )]
    row
  }, error = function(e) {
    data.table(
      hypothesis = h$id,
      family = h$family,
      specification = "game_fe",
      story = h$story,
      error = e$message
    )
  })
}

game_tests <- rbindlist(game_rows, fill = TRUE)
game_tests <- ensure_p_value(game_tests)
game_tests[, p_bh := p.adjust(p.value, method = "BH")]
setorder(game_tests, p.value)
fwrite(game_tests, file.path(output_dir, "cognitive_game_fe_hypothesis_coefficients.csv"))

rule_change_hypotheses <- list(
  list(
    id = "P01_post_age_late_fatigue",
    family = "rule_change_x_fatigue",
    data = pair,
    rhs = "format_5_0:age10:late_round",
    target = c("format_5_0", "age10", "late_round"),
    story = "The rule change intensifies older-player late-round fatigue."
  ),
  list(
    id = "P02_post_age_gap_late",
    family = "rule_change_x_age_mismatch_x_fatigue",
    data = pair,
    rhs = "format_5_0:age_gap10:late_round",
    target = c("format_5_0", "age_gap10", "late_round"),
    story = "The rule change makes being older than the opponent more costly in late rounds."
  ),
  list(
    id = "P03_post_bubble_favorite_choking",
    family = "rule_change_x_threshold_pressure",
    data = pair,
    rhs = "format_5_0:bubble_zone:rating_diff100",
    target = c("format_5_0", "bubble_zone", "rating_diff100"),
    story = "The rule change changes bubble favorites' conversion of rating advantages."
  ),
  list(
    id = "P04_post_unexpected_loss_tilt",
    family = "rule_change_x_tilt",
    data = lagged,
    rhs = "format_5_0:prev_unexpected_loss",
    target = c("format_5_0", "prev_unexpected_loss"),
    story = "The rule change changes the effect of unexpected-loss tilt."
  ),
  list(
    id = "P05_post_upset_win_overconfidence",
    family = "rule_change_x_overconfidence",
    data = lagged,
    rhs = "format_5_0:prev_upset_win",
    target = c("format_5_0", "prev_upset_win"),
    story = "The rule change changes the effect of upset-win momentum or overconfidence."
  )
)

rule_rows <- list()
for (h in rule_change_hypotheses) {
  rule_rows[[h$id]] <- tryCatch({
    model <- fit_event(h$data, "player_result", h$rhs)
    row <- extract_target(model, h$target)
    row[, `:=`(
      hypothesis = h$id,
      family = h$family,
      specification = "player_event_fe",
      story = h$story
    )]
    row
  }, error = function(e) {
    data.table(hypothesis = h$id, family = h$family, specification = "player_event_fe", story = h$story, error = e$message)
  })
}

rule_tests <- rbindlist(rule_rows, fill = TRUE)
rule_tests <- ensure_p_value(rule_tests)
rule_tests[, p_bh := p.adjust(p.value, method = "BH")]
setorder(rule_tests, p.value)
fwrite(rule_tests, file.path(output_dir, "cognitive_rule_change_interactions.csv"))

fake_cutoffs <- as.Date(c(
  "2023-03-01", "2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"
))
placebo_rows <- list()
pre_pair <- copy(pair[date < rule_change_date])
for (cutoff in fake_cutoffs) {
  pre_pair[, placebo_post := as.integer(date >= cutoff)]
  placebo_specs <- list(
    list(
      id = "PB01_age_late",
      rhs = "placebo_post:age10:late_round",
      target = c("placebo_post", "age10", "late_round")
    ),
    list(
      id = "PB02_bubble_favorite",
      rhs = "placebo_post:bubble_zone:rating_diff100",
      target = c("placebo_post", "bubble_zone", "rating_diff100")
    )
  )
  for (sp in placebo_specs) {
    key <- paste(cutoff, sp$id, sep = "__")
    placebo_rows[[key]] <- tryCatch({
      model <- fit_event(pre_pair, "player_result", sp$rhs)
      row <- extract_target(model, sp$target)
      row[, `:=`(cutoff = cutoff, placebo = sp$id)]
      row
    }, error = function(e) {
      data.table(cutoff = cutoff, placebo = sp$id, error = e$message)
    })
  }
}

placebo_tests <- rbindlist(placebo_rows, fill = TRUE)
placebo_tests <- ensure_p_value(placebo_tests)
placebo_tests[, p_bh_by_cutoff := p.adjust(p.value, method = "BH"), by = cutoff]
fwrite(placebo_tests, file.path(output_dir, "cognitive_placebo_cutoffs.csv"))

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
fwrite(event_terms, file.path(output_dir, "young_old_result_event_study.csv"))

event_plot <- ggplot(event_terms, aes(x = event_month, y = score_share_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = score_share_conf.low, ymax = score_share_conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.35, color = "#2364aa") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Young score-share advantage vs old, relative to month -1",
    title = "Event-study: young-vs-old result advantage"
  ) +
  theme_minimal(base_size = 11)
ggsave(
  file.path(output_dir, "young_old_result_event_study.png"),
  event_plot,
  width = 9,
  height = 5.5,
  dpi = 200
)

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
