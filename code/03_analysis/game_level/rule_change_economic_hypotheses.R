suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_economic_hypotheses"
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

df[, event_id := as.character(date)]
df[, date := as.Date(date)]
df[, format_5_0 := as.integer(date >= rule_change_date)]
df[, birthday := suppressWarnings(as.integer(birthday))]
df[, age := 2025L - birthday]

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

main[, player_rating100 := (player_rating - 2500) / 100]
main[, opponent_rating100 := (opponent_rating - 2500) / 100]
main[, rating_diff100 := (player_rating - opponent_rating) / 100]
main[, abs_rating_diff100 := abs(player_rating - opponent_rating) / 100]
main[, favorite := as.integer(player_rating > opponent_rating)]
main[, underdog := as.integer(player_rating < opponent_rating)]

main[, age10 := (age - 35) / 10]
main[, classic100 := (classic_rating - 2500) / 100]
main[, rapid100 := (rapid_rating - 2500) / 100]
main[, blitz100 := (blitz_rating - 2500) / 100]
main[, blitz_classic_gap100 := (blitz_rating - classic_rating) / 100]
main[, rapid_classic_gap100 := (rapid_rating - classic_rating) / 100]
main[, online_classic_gap100 := (player_rating - classic_rating) / 100]
main[, online_blitz_gap100 := (player_rating - blitz_rating) / 100]

main[, gdp_log_c := gdp_per_capita_ppp_logged - mean(gdp_per_capita_ppp_logged, na.rm = TRUE)]
main[, pregame_score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, buchholz_c := buchholz_score - mean(buchholz_score, na.rm = TRUE)]
main[, opponents_sum_c := opponents_sum_score - mean(opponents_sum_score, na.rm = TRUE)]
main[, sonneborn_c := sonneborn_berger_score - mean(sonneborn_berger_score, na.rm = TRUE)]

main[, field_size_round := .N, by = .(event_id, round)]
main[, rank_pct := fifelse(field_size_round > 1, (rank - 1) / (field_size_round - 1), 0)]
main[, rank_pct_c := rank_pct - mean(rank_pct, na.rm = TRUE)]

main[, prize_zone := as.integer(in_prizes == 1)]
main[, bubble_zone := as.integer(bubble == 1)]
main[, leader_zone := as.integer(leader == 1)]
main[, eliminated_zone := as.integer(eliminated == 1)]
main[, opponent_prize_zone := as.integer(played_against_prizes == 1)]
main[, opponent_bubble_zone := as.integer(played_against_bubble == 1)]
main[, opponent_leader_zone := as.integer(played_against_leader == 1)]
main[, opponent_eliminated_zone := as.integer(played_against_eliminated == 1)]
main[, late_round := as.integer(round >= 8)]
main[, final_rounds := as.integer(round >= 10)]

score_lookup <- main[, .(
  event_id,
  round,
  player_name,
  opponent_score_pregame = final_score_pregame,
  opponent_rank = rank,
  opponent_rank_pct = rank_pct
)]
setkey(score_lookup, event_id, round, player_name)
main[, `:=`(
  opponent_score_pregame = score_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_score_pregame
  ],
  opponent_rank = score_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_rank
  ],
  opponent_rank_pct = score_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_rank_pct
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

main[, score_gap_pregame := final_score_pregame - opponent_score_pregame]
main[, rank_gap_pct := rank_pct - opponent_rank_pct]
main[, score_gap_c := score_gap_pregame - mean(score_gap_pregame, na.rm = TRUE)]
main[, rank_gap_pct_c := rank_gap_pct - mean(rank_gap_pct, na.rm = TRUE)]

rating_q75 <- quantile(main$player_rating, 0.75, na.rm = TRUE)
rating_q25 <- quantile(main$player_rating, 0.25, na.rm = TRUE)
main[, high_rating := as.integer(player_rating >= rating_q75)]
main[, low_rating := as.integer(player_rating <= rating_q25)]

sample_summary <- data.table(
  statistic = c(
    "rows", "players", "pre_rows", "post_rows",
    "players_with_pre_and_post_rows", "rule_change_date",
    "high_rating_threshold_q75", "low_rating_threshold_q25"
  ),
  value = c(
    nrow(main),
    uniqueN(main$player_name),
    nrow(main[format_5_0 == 0]),
    nrow(main[format_5_0 == 1]),
    uniqueN(main[
      player_name %in% main[, .(
        has_pre = any(format_5_0 == 0),
        has_post = any(format_5_0 == 1)
      ), by = player_name][has_pre & has_post, player_name],
      player_name
    ]),
    NA_real_,
    rating_q75,
    rating_q25
  ),
  value_text = c(
    as.character(nrow(main)),
    as.character(uniqueN(main$player_name)),
    as.character(nrow(main[format_5_0 == 0])),
    as.character(nrow(main[format_5_0 == 1])),
    as.character(uniqueN(main[
      player_name %in% main[, .(
        has_pre = any(format_5_0 == 0),
        has_post = any(format_5_0 == 1)
      ), by = player_name][has_pre & has_post, player_name],
      player_name
    ])),
    as.character(rule_change_date),
    as.character(rating_q75),
    as.character(rating_q25)
  )
)
fwrite(sample_summary, file.path(output_dir, "sample_summary.csv"))

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

extract_target <- function(model, hypothesis, mechanism, outcome, target_pieces, economic_hypothesis, target_definition) {
  target_term <- find_term(model, target_pieces)
  tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
  row <- tt[tt[["term"]] == target_term]
  row[, `:=`(
    hypothesis = hypothesis,
    mechanism = mechanism,
    outcome = outcome,
    economic_hypothesis = economic_hypothesis,
    target_definition = target_definition,
    nobs = nobs(model)
  )]
  setcolorder(row, c(
    "hypothesis", "mechanism", "outcome", "economic_hypothesis",
    "target_definition", "term", "estimate", "std.error", "statistic",
    "p.value", "conf.low", "conf.high", "nobs"
  ))
  row
}

hypotheses <- list(
  list(
    id = "E01_skill_return",
    mechanism = "returns_to_skill",
    interaction = "format_5_0 * player_rating100",
    target = c("format_5_0", "player_rating100"),
    target_definition = "Post-change return to 100 Chess.com rating points.",
    story = "If no-increment 5+0 increases the value of technical execution and conversion, stronger players should gain more; if it raises variance, the skill gradient should shrink."
  ),
  list(
    id = "E02_favorite_advantage",
    mechanism = "returns_to_relative_skill",
    interaction = "format_5_0 * rating_diff100",
    target = c("format_5_0", "rating_diff100"),
    target_definition = "Post-change return to a 100-point rating advantage over the opponent.",
    story = "A no-increment format can either amplify favorites' conversion ability or create more clock-driven upset risk for underdogs."
  ),
  list(
    id = "E03_match_imbalance",
    mechanism = "competitive_balance",
    interaction = "format_5_0 * abs_rating_diff100",
    target = c("format_5_0", "abs_rating_diff100"),
    target_definition = "Post-change effect of a 100-point larger absolute rating gap.",
    story = "If the format increases variance, lopsided pairings may become less predictable; if it strengthens conversion, unequal pairings should produce larger margins."
  ),
  list(
    id = "E04_underdog",
    mechanism = "upset_opportunity",
    interaction = "format_5_0 * underdog",
    target = c("format_5_0", "underdog"),
    target_definition = "Post-change shift for players rated below their opponent.",
    story = "Clock pressure and no increment may give underdogs more ways to create practical chances even if objective accuracy falls."
  ),
  list(
    id = "E05_white_advantage",
    mechanism = "first_mover_advantage",
    interaction = "format_5_0 * is_white",
    target = c("format_5_0", "is_white"),
    target_definition = "Post-change shift in White's advantage.",
    story = "The first-mover initiative may become more valuable when players cannot rely on increment to defend long technical positions."
  ),
  list(
    id = "E06_white_favorite_advantage",
    mechanism = "first_mover_x_relative_skill",
    interaction = "format_5_0 * is_white * rating_diff100",
    target = c("format_5_0", "is_white", "rating_diff100"),
    target_definition = "Post-change extra return to rating advantage when the player has White.",
    story = "Strong favorites with White may be better positioned to convert initiative before no-increment time pressure equalizes the game."
  ),
  list(
    id = "E07_blitz_specialist",
    mechanism = "time_control_specific_human_capital",
    interaction = "format_5_0 * blitz_classic_gap100",
    target = c("format_5_0", "blitz_classic_gap100"),
    target_definition = "Post-change return to a 100-point higher blitz rating relative to classical rating.",
    story = "No-increment online play should reward blitz-specific human capital more than classical chess skill."
  ),
  list(
    id = "E08_rapid_specialist",
    mechanism = "time_control_specific_human_capital",
    interaction = "format_5_0 * rapid_classic_gap100",
    target = c("format_5_0", "rapid_classic_gap100"),
    target_definition = "Post-change return to a 100-point higher rapid rating relative to classical rating.",
    story = "If 5+0 rewards medium-speed calculation rather than pure bullet-style execution, rapid specialists should gain relative to classical specialists."
  ),
  list(
    id = "E09_online_specialist",
    mechanism = "platform_specific_human_capital",
    interaction = "format_5_0 * online_classic_gap100",
    target = c("format_5_0", "online_classic_gap100"),
    target_definition = "Post-change return to a 100-point higher Chess.com rating relative to classical rating.",
    story = "The rule change may increase returns to online-specific skills such as premove habits, interface fluency, and fast mouse execution."
  ),
  list(
    id = "E10_chesscom_vs_blitz_gap",
    mechanism = "platform_vs_otb_blitz_capital",
    interaction = "format_5_0 * online_blitz_gap100",
    target = c("format_5_0", "online_blitz_gap100"),
    target_definition = "Post-change return to a 100-point higher Chess.com rating relative to FIDE blitz rating.",
    story = "A large online-over-FIDE-blitz gap proxies for platform-specific rather than general blitz strength."
  ),
  list(
    id = "E11_resource_advantage",
    mechanism = "resource_and_infrastructure_constraints",
    interaction = "format_5_0 * gdp_log_c",
    target = c("format_5_0", "gdp_log_c"),
    target_definition = "Post-change return to one log point higher GDP per capita in the player's country.",
    story = "No-increment online events may magnify infrastructure advantages: better hardware, internet stability, training access, and opportunity costs."
  ),
  list(
    id = "E12_score_pressure",
    mechanism = "stakes_and_reference_points",
    interaction = "format_5_0 * pregame_score_c",
    target = c("format_5_0", "pregame_score_c"),
    target_definition = "Post-change effect of one extra tournament point before the game.",
    story = "Players with more to protect face different risk incentives; no increment may make protecting a good score harder or reward conservative conversion."
  ),
  list(
    id = "E13_relative_score_pressure",
    mechanism = "head_to_head_reference_points",
    interaction = "format_5_0 * score_gap_c",
    target = c("format_5_0", "score_gap_c"),
    target_definition = "Post-change effect of leading the opponent by one pregame tournament point.",
    story = "Relative tournament position against the paired opponent shapes risk taking: leaders can protect, trailers may need to gamble."
  ),
  list(
    id = "E14_rank_pressure",
    mechanism = "rank_based_incentives",
    interaction = "format_5_0 * rank_pct_c",
    target = c("format_5_0", "rank_pct_c"),
    target_definition = "Post-change effect of being one full rank-percentile lower in the field.",
    story = "Rank is a salient tournament signal; no increment may change whether lower-ranked players can catch up through practical chances."
  ),
  list(
    id = "E15_relative_rank_pressure",
    mechanism = "head_to_head_rank_incentives",
    interaction = "format_5_0 * rank_gap_pct_c",
    target = c("format_5_0", "rank_gap_pct_c"),
    target_definition = "Post-change effect of having a worse rank percentile than the opponent.",
    story = "Pair-level rank gaps proxy local tournament pressure beyond rating gaps."
  ),
  list(
    id = "E16_prize_zone",
    mechanism = "high_stakes_incentives",
    interaction = "format_5_0 * prize_zone",
    target = c("format_5_0", "prize_zone"),
    target_definition = "Post-change shift for players currently in prize positions.",
    story = "Prize-position players have more to lose; no increment may increase costly mistakes under pressure."
  ),
  list(
    id = "E17_bubble_zone",
    mechanism = "threshold_incentives",
    interaction = "format_5_0 * bubble_zone",
    target = c("format_5_0", "bubble_zone"),
    target_definition = "Post-change shift for players near but outside prize positions.",
    story = "Bubble players face convex incentives to take risk; format changes can alter the payoff to practical, clock-driven play."
  ),
  list(
    id = "E18_leader",
    mechanism = "front_runner_incentives",
    interaction = "format_5_0 * leader_zone",
    target = c("format_5_0", "leader_zone"),
    target_definition = "Post-change shift for tournament leaders.",
    story = "Leaders may prefer low-variance strategies, but no increment makes defensive conversion and clock management more fragile."
  ),
  list(
    id = "E19_eliminated",
    mechanism = "outside_option_and_effort",
    interaction = "format_5_0 * eliminated_zone",
    target = c("format_5_0", "eliminated_zone"),
    target_definition = "Post-change shift for players outside realistic prize contention.",
    story = "Players with little monetary upside may reduce effort or take more speculative risks; format changes can magnify that moral-hazard margin."
  ),
  list(
    id = "E20_opponent_prize_pressure",
    mechanism = "opponent_incentive_spillovers",
    interaction = "format_5_0 * opponent_prize_zone",
    target = c("format_5_0", "opponent_prize_zone"),
    target_definition = "Post-change shift when facing an opponent in prize positions.",
    story = "An opponent's high stakes can change their risk profile and therefore the player's expected accuracy and result."
  ),
  list(
    id = "E21_opponent_leader_pressure",
    mechanism = "opponent_incentive_spillovers",
    interaction = "format_5_0 * opponent_leader_zone",
    target = c("format_5_0", "opponent_leader_zone"),
    target_definition = "Post-change shift when facing the tournament leader.",
    story = "Facing the leader creates asymmetric incentives: the leader may avoid risk while the challenger may seek complications."
  ),
  list(
    id = "E22_strength_of_schedule",
    mechanism = "competitive_sorting",
    interaction = "format_5_0 * buchholz_c",
    target = c("format_5_0", "buchholz_c"),
    target_definition = "Post-change effect of one more Buchholz point.",
    story = "Strength of schedule captures sorting into harder pairings; the new format may punish players more when their path is already difficult."
  ),
  list(
    id = "E23_opponents_score",
    mechanism = "competitive_sorting",
    interaction = "format_5_0 * opponents_sum_c",
    target = c("format_5_0", "opponents_sum_c"),
    target_definition = "Post-change effect of one more point in opponents' cumulative score.",
    story = "Opponent-score pressure is another measure of tournament difficulty and selection into stronger local competition."
  ),
  list(
    id = "E24_sonneborn_pressure",
    mechanism = "tiebreak_incentives",
    interaction = "format_5_0 * sonneborn_c",
    target = c("format_5_0", "sonneborn_c"),
    target_definition = "Post-change effect of one more Sonneborn-Berger tiebreak point.",
    story = "Tiebreak strength can affect incentives at the margin when money depends on rank among tied scores."
  ),
  list(
    id = "E25_late_round",
    mechanism = "fatigue_and_dynamic_incentives",
    interaction = "format_5_0 * late_round",
    target = c("format_5_0", "late_round"),
    target_definition = "Post-change shift in rounds 8+.",
    story = "No-increment play may increase cognitive and execution fatigue as the event progresses."
  ),
  list(
    id = "E26_final_rounds",
    mechanism = "endgame_tournament_incentives",
    interaction = "format_5_0 * final_rounds",
    target = c("format_5_0", "final_rounds"),
    target_definition = "Post-change shift in rounds 10-11.",
    story = "Final rounds combine fatigue, monetary stakes, and strategic scoreboard incentives."
  ),
  list(
    id = "E27_bubble_favorite",
    mechanism = "threshold_incentives_x_skill",
    interaction = "format_5_0 * bubble_zone * rating_diff100",
    target = c("format_5_0", "bubble_zone", "rating_diff100"),
    target_definition = "Post-change extra return to rating advantage for bubble players.",
    story = "Near a prize threshold, favorites may convert more cautiously or underdogs may gamble harder; the format can change that gradient."
  ),
  list(
    id = "E28_score_gap_favorite",
    mechanism = "reference_points_x_skill",
    interaction = "format_5_0 * score_gap_c * rating_diff100",
    target = c("format_5_0", "score_gap_c", "rating_diff100"),
    target_definition = "Post-change extra return to rating advantage when leading the opponent in tournament score.",
    story = "Skill advantages may matter differently when a player is ahead in the tournament and can choose lower-risk strategies."
  )
)

outcomes <- c("player_accuracy", "player_result")
target_rows <- list()

for (outcome in outcomes) {
  for (h in hypotheses) {
    model <- feols(
      make_formula(outcome, h$interaction),
      data = main,
      cluster = ~ player_name,
      notes = FALSE
    )
    key <- paste(outcome, h$id, sep = "__")
    target_rows[[key]] <- extract_target(
      model = model,
      hypothesis = h$id,
      mechanism = h$mechanism,
      outcome = outcome,
      target_pieces = h$target,
      economic_hypothesis = h$story,
      target_definition = h$target_definition
    )
  }
}

target_tests <- rbindlist(target_rows, fill = TRUE)
target_tests[, p_bh_within_outcome := p.adjust(p.value, method = "BH"), by = outcome]
target_tests[, significant_5pct := p.value < 0.05]
target_tests[, significant_bh_10pct := p_bh_within_outcome < 0.10]
setorder(target_tests, outcome, p.value)
fwrite(target_tests, file.path(output_dir, "economic_hypothesis_coefficients.csv"))

horse_race_formula <- function(outcome) {
  as.formula(paste(
    outcome,
    "~ format_5_0:player_rating100 + format_5_0:rating_diff100 +",
    "format_5_0:is_white + format_5_0:blitz_classic_gap100 +",
    "format_5_0:online_classic_gap100 + format_5_0:gdp_log_c +",
    "format_5_0:pregame_score_c + format_5_0:score_gap_c +",
    "format_5_0:rank_pct_c + format_5_0:bubble_zone +",
    "format_5_0:prize_zone + format_5_0:eliminated_zone +",
    "format_5_0:late_round +",
    "player_rating100 + opponent_rating100 + is_white + factor(round)",
    fixed_effects
  ))
}

horse_rows <- list()
for (outcome in outcomes) {
  model <- feols(
    horse_race_formula(outcome),
    data = main,
    cluster = ~ player_name,
    notes = FALSE
  )
  tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
  tt <- tt[grepl("format_5_0:", term, fixed = TRUE)]
  tt[, `:=`(outcome = outcome, nobs = nobs(model))]
  horse_rows[[outcome]] <- tt
}
horse_race <- rbindlist(horse_rows, fill = TRUE)
horse_race[, p_bh_within_outcome := p.adjust(p.value, method = "BH"), by = outcome]
setorder(horse_race, outcome, p.value)
fwrite(horse_race, file.path(output_dir, "horse_race_mechanism_coefficients.csv"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
