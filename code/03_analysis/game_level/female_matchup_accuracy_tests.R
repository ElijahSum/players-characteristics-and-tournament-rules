suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/female_matchup_accuracy_tests"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

needed_cols <- c(
  "player_name", "opponent_name", "player_title", "player_accuracy",
  "player_rating", "opponent_rating", "round", "date", "is_white", "female"
)

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(
    is.na(x), "",
    ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
  )
}

markdown_table <- function(d) {
  if (!nrow(d)) return(character())
  header <- paste0("| ", paste(names(d), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(d)), collapse = " | "), " |")
  rows <- apply(
    d,
    1,
    function(x) paste0("| ", paste(gsub("\\|", "/", as.character(x)), collapse = " | "), " |")
  )
  c(header, rule, rows)
}

message("Reading ", DATA_PATH)
dt <- fread(DATA_PATH, select = needed_cols, showProgress = TRUE)

dt[, tournament_id := as.character(date)]
dt[, game_id := paste(
  tournament_id, round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "|"
)]
dt[, `:=`(
  rating_diff100 = (player_rating - opponent_rating) / 100,
  player_rating100 = (player_rating - 2500) / 100,
  opponent_rating100 = (opponent_rating - 2500) / 100
)]

base <- dt[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_name) & player_name != "" &
    !is.na(opponent_name) & opponent_name != "" &
    !is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100 &
    !is.na(player_rating) & player_rating > 0 &
    !is.na(opponent_rating) & opponent_rating > 0 &
    !is.na(is_white) &
    !is.na(female)
]

opp_map <- base[, .(
  game_id,
  opponent_name_key = player_name,
  opponent_female = female
)]
opp_map <- unique(opp_map, by = c("game_id", "opponent_name_key"))

paired <- merge(
  base,
  opp_map,
  by.x = c("game_id", "opponent_name"),
  by.y = c("game_id", "opponent_name_key"),
  all.x = FALSE,
  all.y = FALSE
)
paired <- paired[!is.na(opponent_female)]

paired[, matchup := fcase(
  female == 1 & opponent_female == 1, "female_vs_female",
  female == 1 & opponent_female == 0, "female_vs_male",
  female == 0 & opponent_female == 1, "male_vs_female",
  female == 0 & opponent_female == 0, "male_vs_male",
  default = NA_character_
)]

target_matchups <- c("female_vs_female", "female_vs_male", "male_vs_male")
target <- paired[matchup %in% target_matchups]
target[, matchup := factor(matchup, levels = c(
  "male_vs_male", "female_vs_male", "female_vs_female"
))]

sample_summary <- data.table(
  metric = c(
    "input_rows", "valid_accuracy_rows_with_known_player_female",
    "paired_rows_with_known_opponent_female", "target_rows",
    "target_games", "target_players", "target_tournaments"
  ),
  value = c(
    nrow(dt), nrow(base), nrow(paired), nrow(target),
    uniqueN(target$game_id), uniqueN(target$player_name), uniqueN(target$tournament_id)
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

descriptives <- target[, .(
  rows = .N,
  games = uniqueN(game_id),
  players = uniqueN(player_name),
  mean_accuracy = mean(player_accuracy),
  sd_accuracy = sd(player_accuracy),
  median_accuracy = median(player_accuracy),
  mean_player_rating = mean(player_rating),
  mean_opponent_rating = mean(opponent_rating),
  mean_rating_diff = mean(player_rating - opponent_rating),
  white_share = mean(is_white)
), by = matchup][order(matchup)]
fwrite(descriptives, file.path(OUT_DIR, "matchup_accuracy_descriptives.csv"))

pairwise_pairs <- list(
  c("female_vs_female", "female_vs_male"),
  c("female_vs_female", "male_vs_male"),
  c("female_vs_male", "male_vs_male")
)

pairwise_tests <- rbindlist(lapply(pairwise_pairs, function(pair) {
  x <- target[matchup == pair[1], player_accuracy]
  y <- target[matchup == pair[2], player_accuracy]
  test <- t.test(x, y)
  data.table(
    comparison = paste(pair[1], "minus", pair[2]),
    group_1 = pair[1],
    group_2 = pair[2],
    mean_1 = mean(x),
    mean_2 = mean(y),
    difference = mean(x) - mean(y),
    std.error = unname(test$stderr),
    conf.low = unname(test$conf.int[1]),
    conf.high = unname(test$conf.int[2]),
    p.value = unname(test$p.value),
    n_1 = length(x),
    n_2 = length(y)
  )
}))
pairwise_tests[, q.value := p.adjust(p.value, method = "BH")]
fwrite(pairwise_tests, file.path(OUT_DIR, "pairwise_accuracy_tests.csv"))

message("Estimating controlled matchup models")
controlled_model <- feols(
  player_accuracy ~ i(matchup, ref = "male_vs_male") +
    player_rating100 + opponent_rating100 + is_white + i(round) |
    tournament_id,
  data = target,
  cluster = ~ player_name + tournament_id
)

controlled_results <- as.data.table(broom::tidy(controlled_model, conf.int = TRUE))
controlled_results <- controlled_results[grepl("^matchup::", term)]
controlled_results[, `:=`(
  term = gsub("matchup::", "", term),
  reference = "male_vs_male",
  outcome = "player_accuracy",
  specification = "standard controls: player/opponent rating, color, round FE, tournament FE; clustered by player and tournament"
)]
setcolorder(controlled_results, c(
  "specification", "outcome", "reference", "term", "estimate", "std.error",
  "statistic", "p.value", "conf.low", "conf.high"
))
controlled_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(controlled_results, file.path(OUT_DIR, "controlled_accuracy_matchup_tests.csv"))

make_contrast <- function(model, lhs, rhs, label) {
  b <- coef(model)
  v <- vcov(model)
  if (!all(c(lhs, rhs) %in% names(b))) {
    return(data.table(
      comparison = label,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }
  estimate <- unname(b[lhs] - b[rhs])
  variance <- v[lhs, lhs] + v[rhs, rhs] - 2 * v[lhs, rhs]
  std_error <- sqrt(variance)
  statistic <- estimate / std_error
  p_value <- 2 * pnorm(abs(statistic), lower.tail = FALSE)
  data.table(
    comparison = label,
    estimate = estimate,
    std.error = std_error,
    statistic = statistic,
    p.value = p_value,
    conf.low = estimate - 1.96 * std_error,
    conf.high = estimate + 1.96 * std_error
  )
}

controlled_pairwise <- rbindlist(list(
  data.table(
    comparison = "female_vs_male minus male_vs_male",
    estimate = controlled_results[term == "female_vs_male", estimate],
    std.error = controlled_results[term == "female_vs_male", std.error],
    statistic = controlled_results[term == "female_vs_male", statistic],
    p.value = controlled_results[term == "female_vs_male", p.value],
    conf.low = controlled_results[term == "female_vs_male", conf.low],
    conf.high = controlled_results[term == "female_vs_male", conf.high]
  ),
  data.table(
    comparison = "female_vs_female minus male_vs_male",
    estimate = controlled_results[term == "female_vs_female", estimate],
    std.error = controlled_results[term == "female_vs_female", std.error],
    statistic = controlled_results[term == "female_vs_female", statistic],
    p.value = controlled_results[term == "female_vs_female", p.value],
    conf.low = controlled_results[term == "female_vs_female", conf.low],
    conf.high = controlled_results[term == "female_vs_female", conf.high]
  ),
  make_contrast(
    controlled_model,
    "matchup::female_vs_female",
    "matchup::female_vs_male",
    "female_vs_female minus female_vs_male"
  )
), fill = TRUE)
controlled_pairwise[, q.value := p.adjust(p.value, method = "BH")]
controlled_pairwise[, specification := "standard controls: player/opponent rating, color, round FE, tournament FE; clustered by player and tournament"]
setcolorder(controlled_pairwise, c(
  "specification", "comparison", "estimate", "std.error", "statistic",
  "p.value", "conf.low", "conf.high", "q.value"
))
fwrite(controlled_pairwise, file.path(OUT_DIR, "controlled_pairwise_accuracy_tests.csv"))

female_within_model <- feols(
  player_accuracy ~ opponent_female + player_rating100 + opponent_rating100 +
    is_white + i(round) | player_name + tournament_id,
  data = paired[female == 1 & matchup %in% c("female_vs_female", "female_vs_male")],
  cluster = ~ player_name + tournament_id
)

male_within_model <- feols(
  player_accuracy ~ opponent_female + player_rating100 + opponent_rating100 +
    is_white + i(round) | player_name + tournament_id,
  data = paired[female == 0 & matchup %in% c("male_vs_female", "male_vs_male")],
  cluster = ~ player_name + tournament_id
)

within_player_results <- rbindlist(list(
  as.data.table(broom::tidy(female_within_model, conf.int = TRUE))[term == "opponent_female"][, `:=`(
    comparison = "female_vs_female minus female_vs_male",
    sample = "female player rows",
    reference = "female_vs_male"
  )],
  as.data.table(broom::tidy(male_within_model, conf.int = TRUE))[term == "opponent_female"][, `:=`(
    comparison = "male_vs_female minus male_vs_male",
    sample = "male player rows",
    reference = "male_vs_male"
  )]
), fill = TRUE)
within_player_results[, `:=`(
  specification = "within-player standard controls: player FE, tournament FE, round FE, ratings, color; clustered by player and tournament",
  outcome = "player_accuracy"
)]
setcolorder(within_player_results, c(
  "specification", "sample", "outcome", "comparison", "reference", "term",
  "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"
))
within_player_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(within_player_results, file.path(OUT_DIR, "within_player_opponent_gender_accuracy_tests.csv"))

all_direction_descriptives <- paired[, .(
  rows = .N,
  games = uniqueN(game_id),
  players = uniqueN(player_name),
  mean_accuracy = mean(player_accuracy),
  sd_accuracy = sd(player_accuracy),
  mean_player_rating = mean(player_rating),
  mean_opponent_rating = mean(opponent_rating)
), by = matchup][order(matchup)]
fwrite(all_direction_descriptives, file.path(OUT_DIR, "all_direction_matchup_descriptives.csv"))

report_descriptives <- copy(descriptives)
report_descriptives[, `:=`(
  mean_accuracy = fmt(mean_accuracy),
  sd_accuracy = fmt(sd_accuracy),
  median_accuracy = fmt(median_accuracy),
  mean_player_rating = fmt(mean_player_rating, 1),
  mean_opponent_rating = fmt(mean_opponent_rating, 1),
  mean_rating_diff = fmt(mean_rating_diff, 1),
  white_share = fmt(white_share, 3)
)]

report_pairwise <- copy(pairwise_tests)
report_pairwise[, `:=`(
  mean_1 = fmt(mean_1),
  mean_2 = fmt(mean_2),
  difference = fmt(difference),
  std.error = fmt(std.error),
  conf.low = fmt(conf.low),
  conf.high = fmt(conf.high),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value)
)]

report_controlled <- copy(controlled_results)
report_controlled[, `:=`(
  estimate = fmt(estimate),
  std.error = fmt(std.error),
  statistic = fmt(statistic),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value),
  conf.low = fmt(conf.low),
  conf.high = fmt(conf.high)
)]

report_controlled_pairwise <- copy(controlled_pairwise)
report_controlled_pairwise[, `:=`(
  estimate = fmt(estimate),
  std.error = fmt(std.error),
  statistic = fmt(statistic),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value),
  conf.low = fmt(conf.low),
  conf.high = fmt(conf.high)
)]

report_within_player <- copy(within_player_results)
report_within_player[, `:=`(
  estimate = fmt(estimate),
  std.error = fmt(std.error),
  statistic = fmt(statistic),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value),
  conf.low = fmt(conf.low),
  conf.high = fmt(conf.high)
)]

lines <- c(
  "# Accuracy by Female/Male Matchup",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Sample",
  "",
  markdown_table(sample_summary),
  "",
  "The three requested directional groups are defined from the player row: `female_vs_female`, `female_vs_male`, and `male_vs_male`. Rows with unknown player or opponent gender, untitled players, round 1, and invalid accuracy values are excluded.",
  "",
  "## Descriptive Accuracy",
  "",
  markdown_table(report_descriptives),
  "",
  "## Pairwise Welch Tests",
  "",
  markdown_table(report_pairwise),
  "",
  "## Controlled Tests",
  "",
  "`male_vs_male` is the omitted reference group in the main controlled model. The model uses the standard non-player-FE controls: player rating, opponent rating, color, round fixed effects, and tournament fixed effects, with two-way clustered standard errors by player and tournament. Player fixed effects cannot identify the full three-group comparison because own gender is time-invariant; those fixed effects absorb the between-player female-vs-male component.",
  "",
  markdown_table(report_controlled),
  "",
  "## Controlled Pairwise Contrasts",
  "",
  markdown_table(report_controlled_pairwise),
  "",
  "## Within-Player Opponent-Gender Checks",
  "",
  "These models add player fixed effects. They are identifiable only as opponent-gender contrasts within the same player gender, not as the full three-group comparison.",
  "",
  markdown_table(report_within_player),
  "",
  "## Output Files",
  "",
  "- `matchup_accuracy_descriptives.csv`",
  "- `pairwise_accuracy_tests.csv`",
  "- `controlled_accuracy_matchup_tests.csv`",
  "- `controlled_pairwise_accuracy_tests.csv`",
  "- `within_player_opponent_gender_accuracy_tests.csv`",
  "- `all_direction_matchup_descriptives.csv`",
  "- `sample_summary.csv`"
)

writeLines(lines, file.path(OUT_DIR, "female_matchup_accuracy_report.md"))
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
