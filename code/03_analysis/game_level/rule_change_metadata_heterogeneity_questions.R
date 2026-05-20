suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/rule_change_metadata_heterogeneity_questions"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RULE_CHANGE_DATE <- as.IDate("2025-09-01")

needed_cols <- c(
  "player_name", "player_rating", "player_title", "player_accuracy",
  "round", "date", "opponent_rating", "player_result", "is_white",
  "country_name", "gdp_per_capita_ppp_logged", "birthday", "female"
)

clean_term <- function(x) gsub("`", "", x, fixed = TRUE)

center <- function(x) x - mean(x, na.rm = TRUE)

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(
    is.na(x), "",
    ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
  )
}

support_label <- function(estimate, p_value, positive_phrase, negative_phrase) {
  if (is.na(estimate) || is.na(p_value)) return("not estimated")
  strength <- if (p_value < 0.01) {
    "strong"
  } else if (p_value < 0.05) {
    "clear"
  } else if (p_value < 0.10) {
    "weak"
  } else {
    "not statistically clear"
  }
  if (strength == "not statistically clear") return(strength)
  paste(strength, if (estimate > 0) positive_phrase else negative_phrase)
}

extract_target <- function(model, target_terms) {
  out <- as.data.table(broom::tidy(model, conf.int = TRUE))
  out[, term := clean_term(term)]
  hit <- out[term %in% target_terms]
  if (nrow(hit) == 0) {
    return(data.table(
      target_term = paste(target_terms, collapse = " OR "),
      estimate = NA_real_,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }
  hit[1, .(target_term = term, estimate, std.error, conf.low, conf.high, p.value)]
}

fit_target_model <- function(data, question_id, question, outcome, rhs, target_terms,
                             interpretation_unit, positive_phrase, negative_phrase,
                             accuracy_sample = FALSE) {
  d <- copy(data)
  if (accuracy_sample) {
    d <- d[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]
  }
  d <- d[!is.na(get(outcome))]
  fml <- as.formula(paste0(
    outcome, " ~ ", rhs,
    " + player_rating_100c + opponent_rating_100c + is_white + i(round)",
    " | player_name + tournament_id"
  ))
  model <- tryCatch(
    feols(fml, data = d, cluster = ~ player_name + tournament_id),
    error = function(e) e
  )
  if (inherits(model, "error")) {
    return(list(
      model = NULL,
      result = data.table(
        question_id = question_id,
        question = question,
        outcome = outcome,
        target_term = paste(target_terms, collapse = " OR "),
        estimate = NA_real_,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        nobs = nrow(d),
        r2_within = NA_real_,
        interpretation_unit = interpretation_unit,
        support = paste("model error:", model$message)
      )
    ))
  }
  target <- extract_target(model, target_terms)
  target[, `:=`(
    question_id = question_id,
    question = question,
    outcome = outcome,
    nobs = nobs(model),
    r2_within = as.numeric(fitstat(model, "wr2")[[1]]),
    interpretation_unit = interpretation_unit,
    support = support_label(estimate, p.value, positive_phrase, negative_phrase)
  )]
  setcolorder(target, c(
    "question_id", "question", "outcome", "target_term", "estimate",
    "std.error", "conf.low", "conf.high", "p.value", "nobs", "r2_within",
    "interpretation_unit", "support"
  ))
  list(model = model, result = target)
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
df[, `:=`(
  player_rating_100c = center(player_rating) / 100,
  opponent_rating_100c = center(opponent_rating) / 100,
  gdp_log_c = center(gdp_per_capita_ppp_logged),
  age = tournament_year - birthday
)]

base <- df[
  player_title != "No Title" &
    !is.na(player_name) &
    !is.na(tournament_id) &
    !is.na(player_result) &
    !is.na(player_rating) & player_rating > 0 &
    !is.na(opponent_rating) & opponent_rating > 0 &
    !is.na(is_white) &
    !is.na(round) &
    round > 1 &
    !is.na(female) &
    !is.na(country_name) & country_name != "" &
    !is.na(gdp_per_capita_ppp_logged) &
    !is.na(birthday) &
    !is.na(age) & age >= 10 & age <= 90
]

country_gdp <- unique(base[, .(country_name, gdp_per_capita_ppp_logged)])
gdp_q25 <- quantile(country_gdp$gdp_per_capita_ppp_logged, 0.25, na.rm = TRUE)
gdp_q75 <- quantile(country_gdp$gdp_per_capita_ppp_logged, 0.75, na.rm = TRUE)

base[, `:=`(
  low_gdp_country = as.integer(gdp_per_capita_ppp_logged <= gdp_q25),
  high_gdp_country = as.integer(gdp_per_capita_ppp_logged >= gdp_q75),
  age10_c = (age - mean(age, na.rm = TRUE)) / 10,
  late_round = as.integer(round >= 8)
)]

country_counts <- base[, .(
  rows = .N,
  pre_rows = sum(format_5_0 == 0),
  post_rows = sum(format_5_0 == 1),
  players = uniqueN(player_name)
), by = country_name][order(-rows)]

top_countries <- country_counts[pre_rows >= 2000 & post_rows >= 200 & players >= 10][1:min(.N, 20), country_name]
base[, country_group := fifelse(country_name %in% top_countries, country_name, "Other")]

sample_summary <- data.table(
  metric = c(
    "input_rows", "usable_result_rows", "usable_accuracy_rows", "players",
    "tournaments", "pre_rows", "post_rows", "countries", "top_country_groups",
    "female_share", "mean_age", "rule_change_date", "sample_filter"
  ),
  value = c(
    nrow(df),
    nrow(base),
    nrow(base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]),
    uniqueN(base$player_name),
    uniqueN(base$tournament_id),
    sum(base$format_5_0 == 0),
    sum(base$format_5_0 == 1),
    uniqueN(base$country_name),
    length(top_countries),
    round(mean(base$female), 4),
    round(mean(base$age), 2),
    as.character(RULE_CHANGE_DATE),
    "titled players, round > 1, valid metadata, valid ratings and result"
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

period_descriptives <- base[, .(
  rows = .N,
  players = uniqueN(player_name),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE),
  female_share = mean(female, na.rm = TRUE),
  mean_age = mean(age, na.rm = TRUE),
  mean_gdp_log = mean(gdp_per_capita_ppp_logged, na.rm = TRUE)
), by = .(format_5_0)]
fwrite(period_descriptives, file.path(OUT_DIR, "period_descriptives.csv"))

group_descriptives <- rbindlist(list(
  base[, .(
    dimension = "female",
    rows = .N,
    players = uniqueN(player_name),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    mean_result = mean(player_result, na.rm = TRUE),
    mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE)
  ), by = .(group = as.character(female))],
  base[, .(
    dimension = "gdp_quartile",
    rows = .N,
    players = uniqueN(player_name),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    mean_result = mean(player_result, na.rm = TRUE),
    mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE)
  ), by = .(group = fifelse(low_gdp_country == 1, "bottom_quartile",
                            fifelse(high_gdp_country == 1, "top_quartile", "middle")))],
  base[, .(
    dimension = "age_group",
    rows = .N,
    players = uniqueN(player_name),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    mean_result = mean(player_result, na.rm = TRUE),
    mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE)
  ), by = .(group = fifelse(age < 25, "under_25",
                            fifelse(age < 35, "25_34",
                                    fifelse(age < 45, "35_44", "45_plus"))))]
), fill = TRUE)
fwrite(group_descriptives[order(dimension, group)], file.path(OUT_DIR, "metadata_group_descriptives.csv"))

models <- list(
  fit_target_model(
    base, "RQ1",
    "Did the 5+0 switch change game scores differently for female players?",
    "player_result", "format_5_0 * female",
    c("format_5_0:female", "female:format_5_0"),
    "additional post-change score share for female players, relative to male players",
    "female relative gain", "female relative loss"
  ),
  fit_target_model(
    base, "RQ2",
    "Did the 5+0 switch change move accuracy differently for female players?",
    "player_accuracy", "format_5_0 * female",
    c("format_5_0:female", "female:format_5_0"),
    "additional post-change accuracy points for female players, relative to male players",
    "female relative accuracy gain", "female relative accuracy loss",
    accuracy_sample = TRUE
  ),
  fit_target_model(
    base, "RQ3",
    "Did female players' late-round game scores change differently after the switch?",
    "player_result", "format_5_0 * female * late_round",
    c(
      "format_5_0:female:late_round", "format_5_0:late_round:female",
      "female:format_5_0:late_round", "female:late_round:format_5_0",
      "late_round:format_5_0:female", "late_round:female:format_5_0"
    ),
    "additional post-change late-round score share for female players",
    "female late-round relative gain", "female late-round relative loss"
  ),
  fit_target_model(
    base, "RQ4",
    "Did players from richer countries gain more game score after the switch?",
    "player_result", "format_5_0 * gdp_log_c",
    c("format_5_0:gdp_log_c", "gdp_log_c:format_5_0"),
    "score-share change per one log point higher GDP per capita PPP",
    "richer-country score gain", "richer-country score loss"
  ),
  fit_target_model(
    base, "RQ5",
    "Did players from richer countries gain more move accuracy after the switch?",
    "player_accuracy", "format_5_0 * gdp_log_c",
    c("format_5_0:gdp_log_c", "gdp_log_c:format_5_0"),
    "accuracy-point change per one log point higher GDP per capita PPP",
    "richer-country accuracy gain", "richer-country accuracy loss",
    accuracy_sample = TRUE
  ),
  fit_target_model(
    base, "RQ6",
    "Did bottom-quartile-GDP countries lose ground in game scores after the switch?",
    "player_result", "format_5_0 * low_gdp_country",
    c("format_5_0:low_gdp_country", "low_gdp_country:format_5_0"),
    "additional post-change score share for bottom-quartile-GDP countries",
    "low-GDP relative gain", "low-GDP relative loss"
  ),
  fit_target_model(
    base, "RQ9",
    "Did younger birth cohorts gain more game score after the switch?",
    "player_result", "format_5_0 * age10_c",
    c("format_5_0:age10_c", "age10_c:format_5_0"),
    "score-share change per 10 additional years of age",
    "older-player score gain", "younger-player score gain"
  ),
  fit_target_model(
    base, "RQ10",
    "Did younger birth cohorts gain more move accuracy after the switch?",
    "player_accuracy", "format_5_0 * age10_c",
    c("format_5_0:age10_c", "age10_c:format_5_0"),
    "accuracy-point change per 10 additional years of age",
    "older-player accuracy gain", "younger-player accuracy gain",
    accuracy_sample = TRUE
  )
)

target_results <- rbindlist(lapply(models, `[[`, "result"), fill = TRUE)

country_formula_result <- player_result ~ i(country_group, format_5_0, ref = "Other") +
  player_rating_100c + opponent_rating_100c + is_white + i(round) |
  player_name + tournament_id
country_model_result <- feols(
  country_formula_result,
  data = base,
  cluster = ~ player_name + tournament_id
)

country_accuracy_sample <- base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]
country_formula_accuracy <- player_accuracy ~ i(country_group, format_5_0, ref = "Other") +
  player_rating_100c + opponent_rating_100c + is_white + i(round) |
  player_name + tournament_id
country_model_accuracy <- feols(
  country_formula_accuracy,
  data = country_accuracy_sample,
  cluster = ~ player_name + tournament_id
)

parse_country_terms <- function(model, outcome) {
  out <- as.data.table(broom::tidy(model, conf.int = TRUE))
  out[, term := clean_term(term)]
  out <- out[grepl("^country_group::", term)]
  out[, country_name := sub("^country_group::", "", term)]
  out[, country_name := sub(":format_5_0$", "", country_name)]
  out[, .(
    outcome,
    country_name,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    nobs = nobs(model),
    r2_within = as.numeric(fitstat(model, "wr2")[[1]])
  )]
}

country_effects <- rbindlist(list(
  parse_country_terms(country_model_result, "player_result"),
  parse_country_terms(country_model_accuracy, "player_accuracy")
), fill = TRUE)
country_effects <- merge(country_effects, country_counts, by = "country_name", all.x = TRUE)
setorder(country_effects, outcome, -estimate)
fwrite(country_effects, file.path(OUT_DIR, "country_post_effects.csv"))

country_period_descriptives <- base[country_group != "Other", .(
  rows = .N,
  players = uniqueN(player_name),
  mean_result = mean(player_result, na.rm = TRUE),
  mean_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE)
), by = .(country_name = country_group, format_5_0)][order(country_name, format_5_0)]
fwrite(country_period_descriptives, file.path(OUT_DIR, "country_period_descriptives.csv"))

country_result_summary <- country_effects[outcome == "player_result"]
top_country_result <- country_result_summary[order(-estimate)][1:min(.N, 5)]
bottom_country_result <- country_result_summary[order(estimate)][1:min(.N, 5)]

country_accuracy_summary <- country_effects[outcome == "player_accuracy"]
top_country_accuracy <- country_accuracy_summary[order(-estimate)][1:min(.N, 5)]
bottom_country_accuracy <- country_accuracy_summary[order(estimate)][1:min(.N, 5)]

country_rq <- rbind(
  data.table(
    question_id = "RQ7",
    question = "Which countries show the largest controlled game-score shifts after the switch?",
    outcome = "player_result",
    target_term = "country-specific post interactions",
    estimate = NA_real_,
    std.error = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = NA_real_,
    nobs = nobs(country_model_result),
    r2_within = as.numeric(fitstat(country_model_result, "wr2")[[1]]),
    interpretation_unit = "country post-change score shift relative to the Other-country baseline",
    support = paste0(
      "largest positive: ", paste(top_country_result$country_name, collapse = ", "),
      "; largest negative: ", paste(bottom_country_result$country_name, collapse = ", ")
    )
  ),
  data.table(
    question_id = "RQ8",
    question = "Which countries show the largest controlled accuracy shifts after the switch?",
    outcome = "player_accuracy",
    target_term = "country-specific post interactions",
    estimate = NA_real_,
    std.error = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = NA_real_,
    nobs = nobs(country_model_accuracy),
    r2_within = as.numeric(fitstat(country_model_accuracy, "wr2")[[1]]),
    interpretation_unit = "country post-change accuracy shift relative to the Other-country baseline",
    support = paste0(
      "largest positive: ", paste(top_country_accuracy$country_name, collapse = ", "),
      "; largest negative: ", paste(bottom_country_accuracy$country_name, collapse = ", ")
    )
  )
)

results <- rbindlist(list(target_results, country_rq), fill = TRUE)
results[, q_value := p.adjust(p.value, method = "BH")]
results[, question_num := as.integer(sub("^RQ", "", question_id))]
setorder(results, question_num)
results[, question_num := NULL]
fwrite(results, file.path(OUT_DIR, "research_question_results.csv"))

report_results <- copy(results)
report_results[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  conf.low = fmt(conf.low, 4),
  conf.high = fmt(conf.high, 4),
  p.value = fmt_p(p.value),
  q_value = fmt_p(q_value),
  r2_within = fmt(r2_within, 4)
)]

country_report <- copy(country_effects)
country_report[, `:=`(
  estimate = fmt(estimate, 4),
  std.error = fmt(std.error, 4),
  p.value = fmt_p(p.value)
)]

sample_md <- copy(sample_summary)
setnames(sample_md, c("Metric", "Value"))

period_md <- copy(period_descriptives)
period_md[, period := fifelse(format_5_0 == 1, "5+0 post-change", "3+1 pre-change")]
period_md[, `:=`(
  mean_result = fmt(mean_result, 3),
  mean_accuracy = fmt(mean_accuracy, 2),
  mean_rating = fmt(mean_rating, 1),
  female_share = fmt(female_share, 3),
  mean_age = fmt(mean_age, 2),
  mean_gdp_log = fmt(mean_gdp_log, 3)
)]

md <- c(
  "# Rule-Change Heterogeneity by Player Metadata",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Purpose",
  "",
  "This report turns the new player metadata into 10 research questions about whether the September 1, 2025 Titled Tuesday switch from 3+1 to 5+0 affected performance differently by female status, player country, country GDP per capita PPP, and birth cohort.",
  "",
  "The estimates use `player_result` or `player_accuracy` as dependent variables. Game-level models include player fixed effects, tournament fixed effects, round fixed effects, color, player rating, opponent rating, and two-way clustered standard errors by player and tournament. The country-specific models estimate post-change interactions for the 20 largest countries with enough pre/post support, relative to all other countries.",
  "",
  "## Sample",
  "",
  write_md_table(sample_md, c("Metric", "Value")),
  "",
  "## Period Descriptives",
  "",
  write_md_table(
    period_md,
    c("period", "rows", "players", "mean_result", "mean_accuracy", "mean_rating", "female_share", "mean_age", "mean_gdp_log")
  ),
  "",
  "## Ten Research Questions and Main Results",
  "",
  write_md_table(
    report_results,
    c("question_id", "question", "outcome", "target_term", "estimate", "std.error", "p.value", "q_value", "interpretation_unit", "support")
  ),
  "",
  "## Country-Specific Score Shifts",
  "",
  "These are controlled post-change interactions for country groups relative to the omitted `Other` country group. They should be treated as descriptive heterogeneity, not as country-level causal effects.",
  "",
  "### Largest Positive Score Shifts",
  "",
  write_md_table(
    country_report[outcome == "player_result"][order(-as.numeric(estimate))][1:min(.N, 10)],
    c("country_name", "estimate", "std.error", "p.value", "rows", "players", "post_rows")
  ),
  "",
  "### Largest Negative Score Shifts",
  "",
  write_md_table(
    country_report[outcome == "player_result"][order(as.numeric(estimate))][1:min(.N, 10)],
    c("country_name", "estimate", "std.error", "p.value", "rows", "players", "post_rows")
  ),
  "",
  "## Country-Specific Accuracy Shifts",
  "",
  "### Largest Positive Accuracy Shifts",
  "",
  write_md_table(
    country_report[outcome == "player_accuracy"][order(-as.numeric(estimate))][1:min(.N, 10)],
    c("country_name", "estimate", "std.error", "p.value", "rows", "players", "post_rows")
  ),
  "",
  "### Largest Negative Accuracy Shifts",
  "",
  write_md_table(
    country_report[outcome == "player_accuracy"][order(as.numeric(estimate))][1:min(.N, 10)],
    c("country_name", "estimate", "std.error", "p.value", "rows", "players", "post_rows")
  ),
  "",
  "## Interpretation Notes",
  "",
  "- The treatment date is September 1, 2025, with pre-period games coded as the old 3+1 format and post-period games coded as the new 5+0 format.",
  "- Coefficients on `format_5_0:*` terms are differences in post-change shifts across metadata groups, net of player and tournament fixed effects.",
  "- Accuracy is a within-game performance measure, so accuracy models are best read as changes in the observed move-quality-performance relationship, not as randomized causal effects of accuracy.",
  "- Country and GDP results can reflect selection into the post-change player pool as well as performance effects; this is especially important because the post-period is shorter than the pre-period.",
  "",
  "## Output Files",
  "",
  "- `sample_summary.csv`: sample construction and key counts.",
  "- `period_descriptives.csv`: pre/post descriptive means.",
  "- `metadata_group_descriptives.csv`: descriptive means by female, GDP quartile, and age group.",
  "- `research_question_results.csv`: the 10 research questions with target coefficients.",
  "- `country_post_effects.csv`: country-specific post-change interactions.",
  "- `country_period_descriptives.csv`: country-period descriptive means for the modeled country groups.",
  "- `session_info.txt`: R session information."
)

writeLines(md, file.path(OUT_DIR, "metadata_heterogeneity_report.md"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
