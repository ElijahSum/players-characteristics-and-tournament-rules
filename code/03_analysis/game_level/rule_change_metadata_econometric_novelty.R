suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/rule_change_metadata_econometric_novelty"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RULE_CHANGE_DATE <- as.IDate("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "country_name", "gdp_per_capita_ppp_logged", "birthday",
  "female", "final_score"
)

center <- function(x) x - mean(x, na.rm = TRUE)

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))
}

clean_term <- function(x) gsub("`", "", x, fixed = TRUE)

tidy_targets <- function(model, design, outcome, target_terms, unit) {
  out <- as.data.table(broom::tidy(model, conf.int = TRUE))
  out[, term := clean_term(term)]

  equivalent_interaction <- function(a, b) {
    a_parts <- sort(strsplit(a, ":", fixed = TRUE)[[1]])
    b_parts <- sort(strsplit(b, ":", fixed = TRUE)[[1]])
    identical(a_parts, b_parts)
  }

  hits <- rbindlist(lapply(target_terms, function(target) {
    idx <- which(vapply(out$term, equivalent_interaction, logical(1), b = target))
    if (length(idx) == 0) {
      return(data.table(
        term = target,
        source_term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_
      ))
    }
    row <- out[idx[1]]
    row[, source_term := term]
    row[, term := target]
    row[, .(term, source_term, estimate, std.error, conf.low, conf.high, p.value)]
  }), fill = TRUE)

  hits[, `:=`(
    design = design,
    outcome = outcome,
    nobs = nobs(model),
    r2_within = as.numeric(fitstat(model, "wr2")[[1]]),
    unit = unit
  )]
  hits[, .(design, outcome, term, source_term, estimate, std.error, conf.low, conf.high,
          p.value, nobs, r2_within, unit)]
}

write_md_table <- function(dt, cols) {
  d <- as.data.table(dt)[, ..cols]
  lines <- c(
    paste0("| ", paste(cols, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  )
  for (i in seq_len(nrow(d))) {
    vals <- vapply(d[i], as.character, character(1))
    vals <- gsub("\\|", "/", vals)
    lines <- c(lines, paste0("| ", paste(vals, collapse = " | "), " |"))
  }
  lines
}

message("Reading ", DATA_PATH)
df <- fread(DATA_PATH, select = needed_cols, showProgress = TRUE)

df[, tournament_id := as.character(date)]
df[, tournament_date := as.IDate(date)]
df[, tournament_year := as.integer(substr(as.character(tournament_date), 1, 4))]
df[, format_5_0 := as.integer(tournament_date >= RULE_CHANGE_DATE)]
df[, event_month := (
  as.integer(format(tournament_date, "%Y")) * 12L +
    as.integer(format(tournament_date, "%m"))
) - (2025L * 12L + 9L)]
df[, event_month_bin := pmax(pmin(event_month, 6L), -12L)]
df[, game_id := paste(
  tournament_id, round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "|"
)]
df[, `:=`(
  player_rating_100c = center(player_rating) / 100,
  opponent_rating_100c = center(opponent_rating) / 100,
  rating_diff100 = (player_rating - opponent_rating) / 100,
  age = tournament_year - birthday,
  gdp_log = gdp_per_capita_ppp_logged
)]

base <- df[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_name) &
    !is.na(opponent_name) &
    !is.na(player_result) &
    !is.na(player_rating) & player_rating > 0 &
    !is.na(opponent_rating) & opponent_rating > 0 &
    !is.na(is_white) &
    !is.na(female) &
    !is.na(country_name) & country_name != "" &
    !is.na(gdp_log) &
    !is.na(age) & age >= 10 & age <= 90
]

base[, `:=`(
  gdp_log_c = center(gdp_log),
  age10_c = (age - mean(age, na.rm = TRUE)) / 10,
  high_accuracy = as.integer(!is.na(player_accuracy) & player_accuracy >= 90 & player_accuracy < 100),
  low_accuracy = as.integer(!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 75),
  win = as.integer(player_result == 1),
  loss = as.integer(player_result == 0)
)]

opp_map <- base[, .(
  game_id,
  opponent_name_key = player_name,
  opp_female = female,
  opp_country_name = country_name,
  opp_gdp_log = gdp_log,
  opp_age = age
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

paired <- paired[
  !is.na(opp_female) &
    !is.na(opp_gdp_log) &
    !is.na(opp_age) &
    opp_age >= 10 & opp_age <= 90
]
paired[, `:=`(
  female_gap = female - opp_female,
  age_gap10 = (age - opp_age) / 10,
  gdp_gap = gdp_log - opp_gdp_log,
  same_country = as.integer(country_name == opp_country_name),
  abs_age_gap10 = abs(age - opp_age) / 10,
  abs_gdp_gap = abs(gdp_log - opp_gdp_log),
  mixed_gender_game = as.integer(abs(female - opp_female) == 1)
)]

sample_summary <- data.table(
  metric = c(
    "input_rows", "base_rows", "base_players", "base_tournaments",
    "base_pre_rows", "base_post_rows", "paired_rows", "paired_games",
    "paired_post_rows", "accuracy_valid_rows", "rule_change_date"
  ),
  value = c(
    nrow(df), nrow(base), uniqueN(base$player_name), uniqueN(base$tournament_id),
    sum(base$format_5_0 == 0), sum(base$format_5_0 == 1),
    nrow(paired), uniqueN(paired$game_id), sum(paired$format_5_0 == 1),
    nrow(base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]),
    as.character(RULE_CHANGE_DATE)
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

message("Estimating within-game fixed-effect models")
wg_result <- feols(
  player_result ~ rating_diff100 + female_gap + age_gap10 + gdp_gap +
    format_5_0:female_gap + format_5_0:age_gap10 + format_5_0:gdp_gap |
    game_id + player_name,
  data = paired,
  cluster = ~ player_name + tournament_id
)

wg_accuracy_sample <- paired[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]
wg_accuracy <- feols(
  player_accuracy ~ rating_diff100 + female_gap + age_gap10 + gdp_gap +
    format_5_0:female_gap + format_5_0:age_gap10 + format_5_0:gdp_gap |
    game_id + player_name,
  data = wg_accuracy_sample,
  cluster = ~ player_name + tournament_id
)

within_game_results <- rbindlist(list(
  tidy_targets(
    wg_result,
    "within_game_player_fe",
    "player_result",
    c("format_5_0:female_gap", "format_5_0:age_gap10", "format_5_0:gdp_gap"),
    "Within the same game, post-change return to player metadata relative to opponent metadata."
  ),
  tidy_targets(
    wg_accuracy,
    "within_game_player_fe",
    "player_accuracy",
    c("format_5_0:female_gap", "format_5_0:age_gap10", "format_5_0:gdp_gap"),
    "Within the same game, post-change return to player metadata relative to opponent metadata."
  )
), fill = TRUE)
within_game_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(within_game_results, file.path(OUT_DIR, "within_game_gap_results.csv"))

message("Estimating distributional performance models")
distribution_models <- list(
  high_accuracy = feols(
    high_accuracy ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c +
      female + age10_c + gdp_log_c + rating_diff100 + is_white + i(round) |
      player_name + tournament_id,
    data = base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100],
    cluster = ~ player_name + tournament_id
  ),
  low_accuracy = feols(
    low_accuracy ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c +
      female + age10_c + gdp_log_c + rating_diff100 + is_white + i(round) |
      player_name + tournament_id,
    data = base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100],
    cluster = ~ player_name + tournament_id
  ),
  win_probability = feols(
    win ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c +
      female + age10_c + gdp_log_c + rating_diff100 + is_white + i(round) |
      player_name + tournament_id,
    data = base,
    cluster = ~ player_name + tournament_id
  ),
  loss_probability = feols(
    loss ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c +
      female + age10_c + gdp_log_c + rating_diff100 + is_white + i(round) |
      player_name + tournament_id,
    data = base,
    cluster = ~ player_name + tournament_id
  )
)

distribution_results <- rbindlist(lapply(names(distribution_models), function(nm) {
  tidy_targets(
    distribution_models[[nm]],
    "player_event_performance_distribution",
    nm,
    c("format_5_0:female", "format_5_0:age10_c", "format_5_0:gdp_log_c"),
    "Post-change distributional shift by own metadata, with player and tournament fixed effects."
  )
}), fill = TRUE)
distribution_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(distribution_results, file.path(OUT_DIR, "distributional_performance_results.csv"))

message("Building player-event participation panel")
player_event <- base[, .(
  played = 1L,
  games_played = .N,
  mean_result = mean(player_result, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE),
  final_score = max(final_score, na.rm = TRUE)
), by = .(player_name, tournament_id)]

player_meta <- base[, .(
  female = first(female),
  country_name = first(country_name),
  gdp_log = first(gdp_log),
  birthday = first(birthday),
  pre_events = uniqueN(tournament_id[format_5_0 == 0]),
  first_event = min(tournament_date),
  last_event = max(tournament_date)
), by = player_name]

events <- unique(base[, .(
  tournament_id,
  tournament_date,
  tournament_year,
  format_5_0,
  event_month
)])

risk_players <- player_meta[pre_events >= 5]
grid <- CJ(player_name = risk_players$player_name, tournament_id = events$tournament_id)
grid <- merge(grid, risk_players, by = "player_name", all.x = TRUE)
grid <- merge(grid, events, by = "tournament_id", all.x = TRUE)
grid <- merge(grid, player_event, by = c("player_name", "tournament_id"), all.x = TRUE)
grid[is.na(played), played := 0L]
grid[is.na(games_played), games_played := 0L]
grid[, `:=`(
  age = tournament_year - birthday,
  gdp_log_c = center(gdp_log)
)]
grid <- grid[age >= 10 & age <= 90]
grid[, age10_c := (age - mean(age, na.rm = TRUE)) / 10]

message("Estimating participation and completion DiD models")
participation_model <- feols(
  played ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c |
    player_name + tournament_id,
  data = grid,
  cluster = ~ player_name + tournament_id
)

completion_model <- feols(
  games_played ~ format_5_0:female + format_5_0:age10_c + format_5_0:gdp_log_c |
    player_name + tournament_id,
  data = grid[played == 1],
  cluster = ~ player_name + tournament_id
)

selection_results <- rbindlist(list(
  tidy_targets(
    participation_model,
    "player_by_event_two_way_fe",
    "played_event",
    c("format_5_0:female", "format_5_0:age10_c", "format_5_0:gdp_log_c"),
    "Change in probability of appearing in an event after the rule change."
  ),
  tidy_targets(
    completion_model,
    "participant_event_two_way_fe",
    "games_played_if_present",
    c("format_5_0:female", "format_5_0:age10_c", "format_5_0:gdp_log_c"),
    "Change in number of games played conditional on appearing."
  )
), fill = TRUE)
selection_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(selection_results, file.path(OUT_DIR, "participation_completion_results.csv"))

message("Estimating age-gap event study")
event_study_age <- feols(
  player_result ~ rating_diff100 + female_gap + gdp_gap +
    i(event_month_bin, age_gap10, ref = -1) |
    game_id + player_name,
  data = paired[event_month_bin >= -12 & event_month_bin <= 6],
  cluster = ~ player_name + tournament_id
)

age_event_terms <- as.data.table(broom::tidy(event_study_age, conf.int = TRUE))
age_event_terms[, term := clean_term(term)]
age_event_terms <- age_event_terms[grepl("event_month_bin::", term)]
age_event_terms[, event_month_bin := as.integer(sub(".*event_month_bin::(-?[0-9]+):age_gap10.*", "\\1", term))]
age_event_terms[, `:=`(
  nobs = nobs(event_study_age),
  r2_within = as.numeric(fitstat(event_study_age, "wr2")[[1]])
)]
fwrite(
  age_event_terms[, .(event_month_bin, term, estimate, std.error, conf.low, conf.high, p.value, nobs, r2_within)],
  file.path(OUT_DIR, "age_gap_event_study.csv")
)

combined <- rbindlist(list(
  within_game_results,
  distribution_results,
  selection_results
), fill = TRUE)
fwrite(combined, file.path(OUT_DIR, "novel_econometric_results_all.csv"))

report <- copy(combined)
report[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  conf.low = fmt(conf.low, 4),
  conf.high = fmt(conf.high, 4),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value),
  r2_within = fmt(r2_within, 4)
)]

sample_md <- copy(sample_summary)
setnames(sample_md, c("Metric", "Value"))

headline <- combined[
  (term %in% c("format_5_0:age_gap10", "format_5_0:age10_c") &
     outcome %in% c("player_result", "player_accuracy", "played_event", "games_played_if_present", "high_accuracy", "low_accuracy")) |
    (term == "format_5_0:gdp_gap" & outcome == "player_accuracy") |
    (term == "format_5_0:female_gap" & outcome == "player_result")
]
headline[, abs_t := abs(estimate / std.error)]
setorder(headline, -abs_t)
headline <- headline[1:min(.N, 10)]
headline_report <- copy(headline)
headline_report[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  p.value = fmt_p(p.value),
  q.value = fmt_p(q.value)
)]

age_event_report <- copy(age_event_terms[order(event_month_bin)])
age_event_report[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  p.value = fmt_p(p.value)
)]

pick_result <- function(design_name, outcome_name, term_name) {
  row <- combined[design == design_name & outcome == outcome_name & term == term_name][1]
  if (nrow(row) == 0) return("not estimated")
  paste0(
    "estimate ", fmt(row$estimate, 4),
    ", SE ", fmt(row$std.error, 4),
    ", p = ", fmt_p(row$p.value)
  )
}

candidate_claims <- c(
  paste0(
    "1. **Younger players gained inside the same games.** With game fixed effects and player fixed effects, a player who is 10 years older than the opponent lost an additional ",
    abs(as.numeric(fmt(combined[design == "within_game_player_fe" & outcome == "player_result" & term == "format_5_0:age_gap10", estimate], 4))),
    " score-share units, about ",
    abs(as.numeric(fmt(100 * combined[design == "within_game_player_fe" & outcome == "player_result" & term == "format_5_0:age_gap10", estimate], 2))),
    " percentage points, after the switch (`",
    pick_result("within_game_player_fe", "player_result", "format_5_0:age_gap10"),
    "`). The same age-gap penalty appears in accuracy (`",
    pick_result("within_game_player_fe", "player_accuracy", "format_5_0:age_gap10"),
    "`)."
  ),
  paste0(
    "2. **The age result is distributional, not only a mean score result.** Ten additional years of age are associated with a lower post-change probability of a 90+ accuracy game (`",
    pick_result("player_event_performance_distribution", "high_accuracy", "format_5_0:age10_c"),
    "`), lower win probability (`",
    pick_result("player_event_performance_distribution", "win_probability", "format_5_0:age10_c"),
    "`), and higher loss probability (`",
    pick_result("player_event_performance_distribution", "loss_probability", "format_5_0:age10_c"),
    "`)."
  ),
  paste0(
    "3. **The post-change field selected younger.** In the player-by-event panel, 10 additional years of age reduced post-change event participation by about 1.1 percentage points (`",
    pick_result("player_by_event_two_way_fe", "played_event", "format_5_0:age10_c"),
    "`). This is separate from conditional game performance."
  ),
  paste0(
    "4. **Country GDP looks more like selection/completion than performance.** GDP gaps are not clearly predictive inside games (`",
    pick_result("within_game_player_fe", "player_result", "format_5_0:gdp_gap"),
    "`), but players from richer countries became less likely to appear (`",
    pick_result("player_by_event_two_way_fe", "played_event", "format_5_0:gdp_log_c"),
    "`) and, conditional on appearing, played slightly more games (`",
    pick_result("participant_event_two_way_fe", "games_played_if_present", "format_5_0:gdp_log_c"),
    "`)."
  ),
  paste0(
    "5. **Female-status effects are not a broad accuracy story, but mixed-gender game scores moved.** In within-game score models, the female-minus-male direction falls after the switch (`",
    pick_result("within_game_player_fe", "player_result", "format_5_0:female_gap"),
    "`), while the corresponding accuracy coefficient is not clear (`",
    pick_result("within_game_player_fe", "player_accuracy", "format_5_0:female_gap"),
    "`). This should be framed cautiously because mixed-gender games are a selected subset."
  )
)

md <- c(
  "# Novel Econometric Results Using Player Metadata",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## What Is New Here",
  "",
  "This analysis goes beyond the earlier subgroup table. It uses four econometric angles that can become thesis results:",
  "",
  "1. **Within-game fixed effects:** compare the two players in the same game and ask whether post-change returns to relative age, GDP, and female status changed.",
  "2. **Distributional performance:** test whether metadata groups moved differently in the tails of performance, not only in mean accuracy or score.",
  "3. **Player-by-event participation DiD:** build a player-event panel and estimate whether the post-change field selected differentially by metadata.",
  "4. **Completion conditional on showing up:** among participants, estimate whether metadata groups played more or fewer games after the format switch.",
  "",
  "## Sample",
  "",
  write_md_table(sample_md, c("Metric", "Value")),
  "",
  "## Suggested Novel Findings",
  "",
  candidate_claims,
  "",
  "## Headline Candidate Results",
  "",
  write_md_table(
    headline_report,
    c("design", "outcome", "term", "estimate", "std.error", "p.value", "q.value", "unit")
  ),
  "",
  "## Within-Game Fixed Effects",
  "",
  "These are the cleanest performance specifications in this file. The game fixed effect absorbs every game-level shock, pairing, round context, event condition, and draw/win/loss environment shared by the two players. Identification comes from which player in the same game is younger, from a richer country, or female relative to the opponent.",
  "",
  write_md_table(
    report[design == "within_game_player_fe"],
    c("outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within", "unit")
  ),
  "",
  "## Distributional Performance",
  "",
  "`high_accuracy` is an indicator for accuracy >= 90. `low_accuracy` is an indicator for accuracy < 75. The win/loss models decompose score effects into decisive outcomes.",
  "",
  write_md_table(
    report[design == "player_event_performance_distribution"],
    c("outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within")
  ),
  "",
  "## Selection and Completion",
  "",
  "The participation model uses a player-by-tournament panel for players with at least five pre-change appearances. Player fixed effects absorb permanent player differences; tournament fixed effects absorb event-level shocks. The completion model is conditional on appearing in the tournament.",
  "",
  write_md_table(
    report[design %in% c("player_by_event_two_way_fe", "participant_event_two_way_fe")],
    c("design", "outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs", "r2_within", "unit")
  ),
  "",
  "## Age-Gap Event Study",
  "",
  "This event study tracks the coefficient on `age_gap10`, where positive values mean the player is older than the opponent. The omitted month is August 2025, immediately before the rule change.",
  "",
  write_md_table(
    age_event_report,
    c("event_month_bin", "estimate", "std.error", "p.value")
  ),
  "",
  "## Econometric Interpretation",
  "",
  "- A negative coefficient on `format_5_0:age_gap10` means older players did worse after the switch relative to younger opponents or younger players.",
  "- A positive coefficient on `format_5_0:gdp_gap` means players from richer countries gained relative to opponents from poorer countries after the switch.",
  "- A coefficient on `format_5_0:female_gap` compares female-versus-male direction within mixed-gender games; same-gender games have a zero gap.",
  "- Participation and completion effects are not conditional game-performance effects; they capture selection into the post-change tournament field and how many rounds players complete.",
  "- These are observational estimates. The within-game specifications are stronger than raw subgroup comparisons, but post-period selection remains an important caveat.",
  "",
  "## Output Files",
  "",
  "- `within_game_gap_results.csv`: within-game/player-FE score and accuracy models.",
  "- `distributional_performance_results.csv`: high-accuracy, low-accuracy, win, and loss models.",
  "- `participation_completion_results.csv`: player-event participation and completion DiD models.",
  "- `age_gap_event_study.csv`: dynamic age-gap coefficients around the rule change.",
  "- `novel_econometric_results_all.csv`: combined coefficient table.",
  "- `sample_summary.csv`: sample counts.",
  "- `session_info.txt`: R session information."
)

writeLines(md, file.path(OUT_DIR, "novel_econometric_metadata_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
