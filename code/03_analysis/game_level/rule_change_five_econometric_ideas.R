suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

DATA_PATH <- "data/final_regression_data_tournaments_2022_2026.csv"
OUT_DIR <- "analysis_outputs/rule_change_five_econometric_ideas"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RULE_CHANGE_DATE <- as.IDate("2025-09-01")

needed_cols <- c(
  "player_name", "opponent_name", "player_rating", "opponent_rating",
  "player_title", "player_accuracy", "player_result", "round", "date",
  "is_white", "country_name", "gdp_per_capita_ppp_logged", "birthday",
  "female", "final_score_pregame", "in_prizes", "bubble", "leader"
)

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))
}

center <- function(x) x - mean(x, na.rm = TRUE)

clean_term <- function(x) gsub("`", "", x, fixed = TRUE)

same_interaction <- function(a, b) {
  a_parts <- sort(strsplit(clean_term(a), ":", fixed = TRUE)[[1]])
  b_parts <- sort(strsplit(clean_term(b), ":", fixed = TRUE)[[1]])
  identical(a_parts, b_parts)
}

tidy_targets <- function(model, idea, specification, outcome, target_terms, interpretation) {
  out <- as.data.table(broom::tidy(model, conf.int = TRUE))
  out[, term := clean_term(term)]
  hits <- rbindlist(lapply(target_terms, function(target) {
    idx <- which(vapply(out$term, same_interaction, logical(1), b = target))
    if (!length(idx)) {
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
    idea = idea,
    specification = specification,
    outcome = outcome,
    nobs = nobs(model),
    r2_within = suppressWarnings(as.numeric(fitstat(model, "wr2")[[1]])),
    interpretation = interpretation
  )]
  hits[, .(
    idea, specification, outcome, term, source_term, estimate, std.error,
    conf.low, conf.high, p.value, nobs, r2_within, interpretation
  )]
}

md_table <- function(dt, cols = names(dt)) {
  d <- as.data.table(dt)[, ..cols]
  lines <- c(
    paste0("| ", paste(cols, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  )
  if (!nrow(d)) return(lines)
  for (i in seq_len(nrow(d))) {
    vals <- vapply(d[i], as.character, character(1))
    vals <- gsub("\\|", "/", vals)
    lines <- c(lines, paste0("| ", paste(vals, collapse = " | "), " |"))
  }
  lines
}

format_report_table <- function(dt, keep_cols = NULL) {
  d <- copy(as.data.table(dt))
  numeric_cols <- intersect(
    c("estimate", "std.error", "conf.low", "conf.high", "r2_within", "q_value",
      "pre_mean", "post_mean", "weighted_post_mean", "unweighted_estimate",
      "weighted_estimate", "change_after_weighting"),
    names(d)
  )
  for (col in numeric_cols) d[, (col) := fmt(get(col))]
  for (col in intersect(c("p.value", "q.value"), names(d))) d[, (col) := fmt_p(get(col))]
  if (!is.null(keep_cols)) d <- d[, ..keep_cols]
  d
}

rif_values <- function(y, tau) {
  y <- as.numeric(y)
  q <- as.numeric(quantile(y, probs = tau, na.rm = TRUE, type = 7))
  dens <- density(y[is.finite(y)], na.rm = TRUE, n = 2048)
  fq <- approx(dens$x, dens$y, xout = q, rule = 2)$y
  q + (tau - as.integer(y <= q)) / fq
}

message("Reading ", DATA_PATH)
df <- fread(DATA_PATH, select = needed_cols, showProgress = TRUE)

df[, `:=`(
  tournament_id = as.character(date),
  tournament_date = as.IDate(date),
  birthday = suppressWarnings(as.integer(birthday))
)]
df[, tournament_year := as.integer(format(tournament_date, "%Y"))]
df[, format_5_0 := as.integer(tournament_date >= RULE_CHANGE_DATE)]
df[, age := 2025L - birthday]
df[, `:=`(
  rating_diff100 = (player_rating - opponent_rating) / 100,
  player_rating100 = (player_rating - 2500) / 100,
  opponent_rating100 = (opponent_rating - 2500) / 100,
  gdp_log = gdp_per_capita_ppp_logged,
  late_round = as.integer(round >= 8),
  prize_zone = as.integer(in_prizes == 1),
  bubble_zone = as.integer(bubble == 1),
  leader_zone = as.integer(leader == 1)
)]

base <- df[
  player_title != "No Title" &
    round > 1 &
    !is.na(player_name) & player_name != "" &
    !is.na(player_result) &
    !is.na(player_rating) & player_rating > 0 &
    !is.na(opponent_rating) & opponent_rating > 0 &
    !is.na(is_white) &
    !is.na(age) & age >= 10 & age <= 85 &
    !is.na(gdp_log) &
    !is.na(female)
]

base[, `:=`(
  age10_c = (age - mean(age, na.rm = TRUE)) / 10,
  gdp_log_c = gdp_log - mean(gdp_log, na.rm = TRUE),
  high_score = as.integer(final_score_pregame >= quantile(final_score_pregame, 0.75, na.rm = TRUE))
)]

acc_base <- base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]

sample_summary <- data.table(
  metric = c(
    "input_rows", "base_rows", "accuracy_rows", "players",
    "tournaments", "pre_rows", "post_rows", "rule_change_date"
  ),
  value = c(
    nrow(df), nrow(base), nrow(acc_base), uniqueN(base$player_name),
    uniqueN(base$tournament_id), sum(base$format_5_0 == 0),
    sum(base$format_5_0 == 1), as.character(RULE_CHANGE_DATE)
  )
)
fwrite(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

message("Idea 1: selection-adjusted performance effects")
player_panel <- base[, .(
  has_post = as.integer(any(format_5_0 == 1)),
  has_pre = as.integer(any(format_5_0 == 0)),
  pre_age10_c = first(age10_c),
  pre_gdp_log_c = first(gdp_log_c),
  female = first(female),
  pre_rating100 = mean(player_rating100[format_5_0 == 0], na.rm = TRUE),
  title = first(player_title),
  country_name = first(country_name)
), by = player_name]
player_panel <- player_panel[has_pre == 1 & is.finite(pre_rating100)]

selection_model <- glm(
  has_post ~ pre_age10_c + pre_gdp_log_c + female + pre_rating100 + title,
  data = player_panel,
  family = binomial()
)
player_panel[, post_prob := pmin(pmax(as.numeric(predict(selection_model, type = "response")), 0.02), 0.98)]
player_panel[, ipw_post_to_pre := (1 - post_prob) / post_prob]
player_panel[, ipw_post_to_pre := ipw_post_to_pre / mean(ipw_post_to_pre[has_post == 1], na.rm = TRUE)]

base <- merge(
  base,
  player_panel[, .(player_name, post_prob, ipw_post_to_pre)],
  by = "player_name",
  all.x = FALSE,
  all.y = FALSE
)
base[, raw_selection_weight := fifelse(format_5_0 == 1, ipw_post_to_pre, 1)]

balance_vars <- c("age10_c", "gdp_log_c", "female", "player_rating100")
pre_targets <- base[format_5_0 == 0, lapply(.SD, mean, na.rm = TRUE), .SDcols = balance_vars]
post_for_calibration <- base[format_5_0 == 1]
post_x <- as.matrix(post_for_calibration[, ..balance_vars])
target_x <- as.numeric(pre_targets[1, ..balance_vars])
scale_x <- apply(post_x, 2, sd, na.rm = TRUE)
scale_x[!is.finite(scale_x) | scale_x == 0] <- 1
base_w <- post_for_calibration$ipw_post_to_pre
base_w <- base_w / mean(base_w, na.rm = TRUE)

calibration_objective <- function(lambda) {
  eta <- as.numeric((sweep(post_x, 2, target_x, "-") / scale_x) %*% lambda)
  eta <- pmax(pmin(eta, 20), -20)
  w <- base_w * exp(eta)
  weighted_means <- colSums(post_x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
  sum(((weighted_means - target_x) / scale_x)^2)
}

calibration_fit <- optim(
  rep(0, length(balance_vars)),
  calibration_objective,
  method = "BFGS",
  control = list(maxit = 1000)
)
eta <- as.numeric((sweep(post_x, 2, target_x, "-") / scale_x) %*% calibration_fit$par)
eta <- pmax(pmin(eta, 20), -20)
calibrated_post_weights <- base_w * exp(eta)
calibrated_post_weights <- calibrated_post_weights / mean(calibrated_post_weights, na.rm = TRUE)
calibrated_post_weights <- pmin(calibrated_post_weights, quantile(calibrated_post_weights, 0.995, na.rm = TRUE))
calibrated_post_weights <- calibrated_post_weights / mean(calibrated_post_weights, na.rm = TRUE)

base[, calibrated_selection_weight := 1]
base[format_5_0 == 1, calibrated_selection_weight := calibrated_post_weights]

acc_base <- base[!is.na(player_accuracy) & player_accuracy > 0 & player_accuracy < 100]

selection_balance <- rbindlist(list(
  base[, .(
    sample = "unweighted_pre",
    age10_c = mean(age10_c),
    gdp_log_c = mean(gdp_log_c),
    female_share = mean(female),
    rating100 = mean(player_rating100)
  ), by = .(format_5_0)][format_5_0 == 0, !"format_5_0"],
  base[, .(
    sample = "unweighted_post",
    age10_c = mean(age10_c),
    gdp_log_c = mean(gdp_log_c),
    female_share = mean(female),
    rating100 = mean(player_rating100)
  ), by = .(format_5_0)][format_5_0 == 1, !"format_5_0"],
  base[format_5_0 == 1, .(
    sample = "raw_ipw_post_to_pre",
    age10_c = weighted.mean(age10_c, raw_selection_weight),
    gdp_log_c = weighted.mean(gdp_log_c, raw_selection_weight),
    female_share = weighted.mean(female, raw_selection_weight),
    rating100 = weighted.mean(player_rating100, raw_selection_weight)
  )],
  base[format_5_0 == 1, .(
    sample = "calibrated_ipw_post_to_pre",
    age10_c = weighted.mean(age10_c, calibrated_selection_weight),
    gdp_log_c = weighted.mean(gdp_log_c, calibrated_selection_weight),
    female_share = weighted.mean(female, calibrated_selection_weight),
    rating100 = weighted.mean(player_rating100, calibrated_selection_weight)
  )]
), fill = TRUE)
fwrite(selection_balance, file.path(OUT_DIR, "idea1_selection_balance.csv"))

selection_models <- list(
  unweighted_result = feols(
    player_result ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = base,
    cluster = ~ player_name + tournament_id
  ),
  weighted_result = feols(
    player_result ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = base,
    weights = ~ raw_selection_weight,
    cluster = ~ player_name + tournament_id
  ),
  calibrated_weighted_result = feols(
    player_result ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = base,
    weights = ~ calibrated_selection_weight,
    cluster = ~ player_name + tournament_id
  ),
  unweighted_accuracy = feols(
    player_accuracy ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = acc_base,
    cluster = ~ player_name + tournament_id
  ),
  weighted_accuracy = feols(
    player_accuracy ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = acc_base,
    weights = ~ raw_selection_weight,
    cluster = ~ player_name + tournament_id
  ),
  calibrated_weighted_accuracy = feols(
    player_accuracy ~ format_5_0:age10_c + player_rating100 + opponent_rating100 +
      rating_diff100 + is_white + i(round) | player_name + tournament_id,
    data = acc_base,
    weights = ~ calibrated_selection_weight,
    cluster = ~ player_name + tournament_id
  )
)

idea1_results <- rbindlist(list(
  tidy_targets(selection_models$unweighted_result, "I1_selection_adjustment", "unweighted", "player_result", "format_5_0:age10_c", "Post-change age gradient without composition reweighting."),
  tidy_targets(selection_models$weighted_result, "I1_selection_adjustment", "raw_IPW_post_to_pre", "player_result", "format_5_0:age10_c", "Post-change age gradient after raw propensity reweighting."),
  tidy_targets(selection_models$calibrated_weighted_result, "I1_selection_adjustment", "calibrated_IPW_post_to_pre", "player_result", "format_5_0:age10_c", "Post-change age gradient after calibrated IPW reweighting to pre-period metadata composition."),
  tidy_targets(selection_models$unweighted_accuracy, "I1_selection_adjustment", "unweighted", "player_accuracy", "format_5_0:age10_c", "Post-change age gradient without composition reweighting."),
  tidy_targets(selection_models$weighted_accuracy, "I1_selection_adjustment", "raw_IPW_post_to_pre", "player_accuracy", "format_5_0:age10_c", "Post-change age gradient after raw propensity reweighting."),
  tidy_targets(selection_models$calibrated_weighted_accuracy, "I1_selection_adjustment", "calibrated_IPW_post_to_pre", "player_accuracy", "format_5_0:age10_c", "Post-change age gradient after calibrated IPW reweighting to pre-period metadata composition.")
), fill = TRUE)
fwrite(idea1_results, file.path(OUT_DIR, "idea1_selection_adjusted_effects.csv"))

message("Idea 2: age by pressure mechanisms")
pressure_vars <- c("late_round", "bubble_zone", "prize_zone", "leader_zone", "high_score")
idea2_results <- rbindlist(lapply(pressure_vars, function(v) {
  rhs <- paste0(
    "format_5_0 * age10_c * ", v,
    " + player_rating100 + opponent_rating100 + rating_diff100 + is_white + i(round)"
  )
  result_model <- feols(
    as.formula(paste0("player_result ~ ", rhs, " | player_name + tournament_id")),
    data = base,
    cluster = ~ player_name + tournament_id
  )
  accuracy_model <- feols(
    as.formula(paste0("player_accuracy ~ ", rhs, " | player_name + tournament_id")),
    data = acc_base,
    cluster = ~ player_name + tournament_id
  )
  target <- paste0("format_5_0:age10_c:", v)
  rbindlist(list(
    tidy_targets(result_model, "I2_youth_pressure", v, "player_result", target, "Extra post-change age gradient in the pressure condition."),
    tidy_targets(accuracy_model, "I2_youth_pressure", v, "player_accuracy", target, "Extra post-change age gradient in the pressure condition.")
  ), fill = TRUE)
}), fill = TRUE)
idea2_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(idea2_results, file.path(OUT_DIR, "idea2_age_pressure_mechanisms.csv"))

message("Idea 3: return to rating by age, female, and GDP")
skill_specs <- list(
  age10_c = "format_5_0 * rating_diff100 * age10_c",
  female = "format_5_0 * rating_diff100 * female",
  gdp_log_c = "format_5_0 * rating_diff100 * gdp_log_c"
)
idea3_results <- rbindlist(lapply(names(skill_specs), function(v) {
  rhs <- paste0(
    skill_specs[[v]],
    " + player_rating100 + opponent_rating100 + is_white + i(round)"
  )
  model <- feols(
    as.formula(paste0("player_result ~ ", rhs, " | player_name + tournament_id")),
    data = base,
    cluster = ~ player_name + tournament_id
  )
  tidy_targets(
    model,
    "I3_skill_return_heterogeneity",
    v,
    "player_result",
    paste0("format_5_0:rating_diff100:", v),
    "Post-change change in the return to rating advantage by metadata group."
  )
}), fill = TRUE)
idea3_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(idea3_results, file.path(OUT_DIR, "idea3_skill_return_heterogeneity.csv"))

message("Idea 4: RIF quantile accuracy effects")
taus <- c(0.25, 0.50, 0.75, 0.90)
idea4_results <- rbindlist(lapply(taus, function(tau) {
  d <- copy(acc_base)
  d[, rif_accuracy := rif_values(player_accuracy, tau)]
  model <- feols(
    rif_accuracy ~ format_5_0:age10_c + format_5_0:female + format_5_0:gdp_log_c +
      player_rating100 + opponent_rating100 + rating_diff100 + is_white + i(round) |
      player_name + tournament_id,
    data = d,
    cluster = ~ player_name + tournament_id
  )
  out <- tidy_targets(
    model,
    "I4_rif_quantile_accuracy",
    paste0("q", tau),
    "rif_player_accuracy",
    c("format_5_0:age10_c", "format_5_0:female", "format_5_0:gdp_log_c"),
    "Unconditional quantile/RIF effect on player accuracy."
  )
  out[, quantile := tau]
  out[]
}), fill = TRUE)
idea4_results[, q.value := p.adjust(p.value, method = "BH")]
setcolorder(idea4_results, c(
  "idea", "specification", "quantile", "outcome", "term", "source_term",
  "estimate", "std.error", "conf.low", "conf.high", "p.value", "q.value",
  "nobs", "r2_within", "interpretation"
))
fwrite(idea4_results, file.path(OUT_DIR, "idea4_rif_quantile_accuracy.csv"))

message("Idea 5: country/GDP participation constraints")
player_country <- base[, .(
  country_name = first(country_name),
  gdp_log = first(gdp_log),
  had_pre = any(format_5_0 == 0)
), by = player_name]
pre_pool <- player_country[had_pre == TRUE, .(
  pre_pool_players = uniqueN(player_name),
  gdp_log = mean(gdp_log, na.rm = TRUE)
), by = country_name]
pre_pool <- pre_pool[pre_pool_players >= 10 & !is.na(gdp_log)]
pre_pool[, gdp_log_c := gdp_log - mean(gdp_log, na.rm = TRUE)]

events <- unique(base[, .(tournament_id, tournament_date, format_5_0)])
country_grid <- CJ(country_name = pre_pool$country_name, tournament_id = events$tournament_id)
country_grid <- merge(country_grid, events, by = "tournament_id", all.x = TRUE)
country_grid <- merge(country_grid, pre_pool, by = "country_name", all.x = TRUE)

event_rows <- merge(
  base,
  player_country[had_pre == TRUE, .(player_name, baseline_country = country_name)],
  by = "player_name",
  all.x = FALSE,
  all.y = FALSE
)
event_rows <- event_rows[baseline_country %in% pre_pool$country_name]

country_event_observed <- event_rows[, .(
  players_played = uniqueN(player_name),
  row_games = .N,
  avg_games_played = .N / uniqueN(player_name),
  avg_result = mean(player_result, na.rm = TRUE),
  avg_accuracy = mean(player_accuracy[player_accuracy > 0 & player_accuracy < 100], na.rm = TRUE),
  mean_rating = mean(player_rating, na.rm = TRUE)
), by = .(country_name = baseline_country, tournament_id)]

country_event <- merge(
  country_grid,
  country_event_observed,
  by = c("country_name", "tournament_id"),
  all.x = TRUE
)
country_event[is.na(players_played), `:=`(
  players_played = 0L,
  row_games = 0L,
  avg_games_played = 0
)]
country_event[, participation_rate := players_played / pre_pool_players]
country_event[, log_players_plus1 := log1p(players_played)]

country_event_nonzero <- country_event[players_played > 0]

country_models <- list(
  participation_rate = feols(
    participation_rate ~ format_5_0:gdp_log_c | country_name + tournament_id,
    data = country_event,
    cluster = ~ country_name + tournament_id
  ),
  log_players_plus1 = feols(
    log_players_plus1 ~ format_5_0:gdp_log_c | country_name + tournament_id,
    data = country_event,
    cluster = ~ country_name + tournament_id
  ),
  avg_games_played = feols(
    avg_games_played ~ format_5_0:gdp_log_c | country_name + tournament_id,
    data = country_event_nonzero,
    weights = ~ players_played,
    cluster = ~ country_name + tournament_id
  ),
  avg_result = feols(
    avg_result ~ format_5_0:gdp_log_c | country_name + tournament_id,
    data = country_event_nonzero[!is.na(avg_result)],
    weights = ~ players_played,
    cluster = ~ country_name + tournament_id
  ),
  avg_accuracy = feols(
    avg_accuracy ~ format_5_0:gdp_log_c | country_name + tournament_id,
    data = country_event_nonzero[!is.na(avg_accuracy)],
    weights = ~ players_played,
    cluster = ~ country_name + tournament_id
  )
)

idea5_results <- rbindlist(lapply(names(country_models), function(outcome) {
  tidy_targets(
    country_models[[outcome]],
    "I5_country_gdp_participation_constraints",
    "country_event_panel",
    outcome,
    "format_5_0:gdp_log_c",
    "Post-change GDP gradient with country and event fixed effects."
  )
}), fill = TRUE)
idea5_results[, q.value := p.adjust(p.value, method = "BH")]
fwrite(idea5_results, file.path(OUT_DIR, "idea5_country_gdp_panel.csv"))

country_event_summary <- country_event[, .(
  countries = uniqueN(country_name),
  country_event_cells = .N,
  mean_participation_rate = mean(participation_rate),
  mean_players_played = mean(players_played),
  mean_avg_games_played_nonzero = mean(avg_games_played[players_played > 0], na.rm = TRUE),
  mean_avg_result_nonzero = mean(avg_result[players_played > 0], na.rm = TRUE),
  mean_avg_accuracy_nonzero = mean(avg_accuracy[players_played > 0], na.rm = TRUE)
), by = format_5_0]
fwrite(country_event_summary, file.path(OUT_DIR, "idea5_country_event_summary.csv"))

message("Writing summary report")
headline <- rbindlist(list(
  idea1_results[, .SD[term == "format_5_0:age10_c"], by = .(idea, specification, outcome)],
  idea2_results[!is.na(p.value)][order(p.value)][1:min(.N, 6)],
  idea3_results,
  idea4_results[!is.na(p.value)][order(term, quantile)],
  idea5_results
), fill = TRUE)

report_i1 <- format_report_table(
  idea1_results,
  c("specification", "outcome", "term", "estimate", "std.error", "p.value", "nobs")
)
report_i2 <- format_report_table(
  idea2_results[order(outcome, specification)],
  c("specification", "outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs")
)
report_i3 <- format_report_table(
  idea3_results[order(specification)],
  c("specification", "outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs")
)
report_i4 <- format_report_table(
  idea4_results[order(term, quantile)],
  c("quantile", "term", "estimate", "std.error", "p.value", "q.value", "nobs")
)
report_i5 <- format_report_table(
  idea5_results[order(outcome)],
  c("outcome", "term", "estimate", "std.error", "p.value", "q.value", "nobs")
)
report_balance <- copy(selection_balance)
for (col in setdiff(names(report_balance), "sample")) report_balance[, (col) := fmt(get(col))]

lines <- c(
  "# Five Econometric Follow-up Tests",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Input file: `data/final_regression_data_tournaments_2022_2026.csv`.",
  "",
  "## Sample",
  "",
  md_table(sample_summary),
  "",
  "## 1. Selection-Adjusted Performance Effects",
  "",
  "Post-change rows are reweighted by a player-level logit model for appearing post-change, estimated from pre-period metadata. The calibrated version tilts the propensity weights so the weighted post rows match pre-period means for age, GDP, female share, and rating.",
  "",
  "### Balance",
  "",
  md_table(report_balance),
  "",
  "### Age Effects With and Without IPW",
  "",
  md_table(report_i1),
  "",
  "## 2. Youth Returns in Pressure Conditions",
  "",
  "The target term is `format_5_0:age10_c:pressure`. Negative estimates mean older players lost more, or younger players gained more, in that pressure condition after the switch.",
  "",
  md_table(report_i2),
  "",
  "## 3. Return to Rating by Metadata",
  "",
  "The target term is the triple interaction between post-change format, rating advantage, and the metadata variable.",
  "",
  md_table(report_i3),
  "",
  "## 4. RIF Quantile Accuracy Effects",
  "",
  "Each coefficient is an unconditional quantile/RIF effect for player accuracy. Negative age coefficients mean older players shifted down more at that part of the accuracy distribution after the switch.",
  "",
  md_table(report_i4),
  "",
  "## 5. Country/GDP Participation Constraints",
  "",
  "The country-event panel uses countries with at least 10 pre-period players. Participation is measured relative to each country's pre-period player pool; performance outcomes are conditional on a country having participants in the event.",
  "",
  md_table(report_i5),
  "",
  "## Output Files",
  "",
  "- `idea1_selection_balance.csv`",
  "- `idea1_selection_adjusted_effects.csv`",
  "- `idea2_age_pressure_mechanisms.csv`",
  "- `idea3_skill_return_heterogeneity.csv`",
  "- `idea4_rif_quantile_accuracy.csv`",
  "- `idea5_country_gdp_panel.csv`",
  "- `idea5_country_event_summary.csv`"
)

writeLines(lines, file.path(OUT_DIR, "five_econometric_ideas_report.md"))
fwrite(headline, file.path(OUT_DIR, "headline_results_long.csv"))
writeLines(capture.output(summary(selection_model)), file.path(OUT_DIR, "idea1_selection_model_summary.txt"))
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info.txt"))

message("Wrote outputs to ", OUT_DIR)
