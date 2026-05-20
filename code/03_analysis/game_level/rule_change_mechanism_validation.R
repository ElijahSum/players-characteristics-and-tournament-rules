suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(broom)
  library(ggplot2)
})

setFixest_nthreads(0)
setFixest_notes(FALSE)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_mechanism_validation"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rule_change_date <- as.Date("2025-09-01")

needed_cols <- c(
  "player_name", "player_rating", "player_title", "player_accuracy",
  "round", "date", "opponent_rating", "player_result", "opponent_name",
  "is_white", "final_score_pregame", "classic_rating", "rapid_rating",
  "blitz_rating", "gdp_per_capita_ppp_logged", "birthday",
  "opponents_sum_score", "buchholz_score", "sonneborn_berger_score",
  "rank", "leader", "in_prizes", "bubble", "eliminated",
  "played_against_prizes", "played_against_leader"
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
main[, underdog := as.integer(player_rating < opponent_rating)]
main[, favorite := as.integer(player_rating > opponent_rating)]

main[, online_classic_gap100 := (player_rating - classic_rating) / 100]
main[, online_blitz_gap100 := (player_rating - blitz_rating) / 100]
main[, blitz_classic_gap100 := (blitz_rating - classic_rating) / 100]
main[, rapid_classic_gap100 := (rapid_rating - classic_rating) / 100]

main[, buchholz_c := buchholz_score - mean(buchholz_score, na.rm = TRUE)]
main[, opponents_sum_c := opponents_sum_score - mean(opponents_sum_score, na.rm = TRUE)]
main[, pregame_score_c := final_score_pregame - mean(final_score_pregame, na.rm = TRUE)]
main[, gdp_log_c := gdp_per_capita_ppp_logged - mean(gdp_per_capita_ppp_logged, na.rm = TRUE)]

main[, field_size_round := .N, by = .(event_id, round)]
main[, rank_pct := fifelse(field_size_round > 1, (rank - 1) / (field_size_round - 1), 0)]
main[, rank_pct_c := rank_pct - mean(rank_pct, na.rm = TRUE)]

main[, bubble_zone := as.integer(bubble == 1)]
main[, prize_zone := as.integer(in_prizes == 1)]
main[, eliminated_zone := as.integer(eliminated == 1)]
main[, leader_zone := as.integer(leader == 1)]
main[, opponent_prize_zone := as.integer(played_against_prizes == 1)]
main[, opponent_leader_zone := as.integer(played_against_leader == 1)]
main[, late_round := as.integer(round >= 8)]

score_lookup <- main[, .(
  event_id,
  round,
  player_name,
  opponent_score_pregame = final_score_pregame,
  opponent_rank_pct = rank_pct
)]
setkey(score_lookup, event_id, round, player_name)
main[, `:=`(
  opponent_score_pregame = score_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_score_pregame
  ],
  opponent_rank_pct = score_lookup[
    .SD,
    on = .(event_id, round, player_name = opponent_name),
    opponent_rank_pct
  ]
), .SDcols = c("event_id", "round", "opponent_name")]

main[, score_gap_c := (final_score_pregame - opponent_score_pregame) -
  mean(final_score_pregame - opponent_score_pregame, na.rm = TRUE)]
main[, rank_gap_pct_c := (rank_pct - opponent_rank_pct) -
  mean(rank_pct - opponent_rank_pct, na.rm = TRUE)]

main[, event_month := (
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
) - (
  as.integer(format(rule_change_date, "%Y")) * 12L +
    as.integer(format(rule_change_date, "%m"))
)]

main[, game_id := paste(
  event_id,
  round,
  pmin(player_name, opponent_name),
  pmax(player_name, opponent_name),
  sep = "||"
)]

main[, win := as.integer(player_result == 1)]
main[, loss := as.integer(player_result == 0)]
main[, draw := as.integer(player_result == 0.5)]

both_players <- main[, .(
  has_pre = any(format_5_0 == 0),
  has_post = any(format_5_0 == 1)
), by = player_name][has_pre & has_post, player_name]

mechanisms <- data.table(
  mechanism = c(
    "relative_skill",
    "relative_skill",
    "online_platform_capital",
    "online_platform_capital",
    "competitive_sorting",
    "competitive_sorting",
    "threshold_pressure",
    "threshold_pressure",
    "threshold_pressure"
  ),
  variable = c(
    "rating_diff100",
    "underdog",
    "online_classic_gap100",
    "online_blitz_gap100",
    "buchholz_c",
    "opponents_sum_c",
    "bubble_zone",
    "eliminated_zone",
    "prize_zone"
  ),
  label = c(
    "Rating advantage over opponent, per 100 points",
    "Player is rated below opponent",
    "Chess.com rating minus classical rating, per 100 points",
    "Chess.com rating minus FIDE blitz rating, per 100 points",
    "Buchholz score, centered",
    "Opponents' cumulative score, centered",
    "Player is on prize bubble",
    "Player is outside realistic prize contention",
    "Player is currently in prize positions"
  ),
  expected_story = c(
    "No-increment play increases returns to conversion skill if favorites gain more.",
    "No-increment play increases upset opportunity if underdogs gain, or conversion if they lose.",
    "The new format rewards online-specific platform skill beyond OTB classical strength.",
    "The new format rewards Chess.com-specific skill beyond general FIDE blitz strength.",
    "The new format makes hard tournament paths more costly.",
    "The new format makes hard tournament paths more costly.",
    "Threshold pressure near prizes worsens decisions if bubble players lose.",
    "Lower downside and freer play help eliminated players if they gain.",
    "High-stakes prize protection helps or hurts players already in prizes."
  ),
  binary = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
)

fwrite(
  main[, .(
    rows = .N,
    players = uniqueN(player_name),
    games = uniqueN(game_id),
    events = uniqueN(event_id),
    pre_rows = sum(format_5_0 == 0),
    post_rows = sum(format_5_0 == 1),
    both_period_players = uniqueN(player_name[player_name %in% both_players]),
    min_date = min(date),
    max_date = max(date)
  )],
  file.path(output_dir, "sample_summary.csv")
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
  tt[tt[["term"]] == target_term]
}

fit_target <- function(data, outcome, x, post_var = "format_5_0",
                       fe = "player_name + event_id",
                       cluster_formula = ~ player_name + event_id,
                       controls = "+ player_rating100 + opponent_rating100 + is_white + factor(round)") {
  fml <- as.formula(paste(
    outcome,
    "~",
    post_var,
    "*",
    x,
    controls,
    "|",
    fe
  ))
  model <- feols(fml, data = data, cluster = cluster_formula)
  extract_target(model, c(post_var, x))[, nobs := nobs(model)]
}

specs <- list(
  list(
    name = "event_fe_all",
    data = main,
    fe = "player_name + event_id",
    cluster = ~ player_name + event_id
  ),
  list(
    name = "event_fe_round_gt2",
    data = main[round > 2],
    fe = "player_name + event_id",
    cluster = ~ player_name + event_id
  ),
  list(
    name = "event_fe_near_window_pm12m",
    data = main[event_month >= -12 & event_month <= 6],
    fe = "player_name + event_id",
    cluster = ~ player_name + event_id
  ),
  list(
    name = "event_fe_balanced_players",
    data = main[player_name %in% both_players],
    fe = "player_name + event_id",
    cluster = ~ player_name + event_id
  ),
  list(
    name = "paired_game_fe",
    data = main,
    fe = "player_name + game_id",
    cluster = ~ player_name + game_id
  )
)

main_rows <- list()
outcomes <- c("player_accuracy", "player_result")
for (spec in specs) {
  for (outcome in outcomes) {
    for (x in mechanisms$variable) {
      key <- paste(spec$name, outcome, x, sep = "__")
      main_rows[[key]] <- tryCatch({
        row <- fit_target(
          data = spec$data,
          outcome = outcome,
          x = x,
          fe = spec$fe,
          cluster_formula = spec$cluster
        )
        row[, `:=`(specification = spec$name, outcome = outcome, variable = x)]
        row
      }, error = function(e) {
        data.table(
          specification = spec$name,
          outcome = outcome,
          variable = x,
          term = NA_character_,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          nobs = nrow(spec$data),
          error = e$message
        )
      })
    }
  }
}

main_checks <- rbindlist(main_rows, fill = TRUE)
main_checks <- merge(main_checks, mechanisms, by = "variable", all.x = TRUE)
main_checks[, p_bh_by_spec_outcome := p.adjust(p.value, method = "BH"),
            by = .(specification, outcome)]
setorder(main_checks, specification, outcome, p.value)
fwrite(main_checks, file.path(output_dir, "main_robustness_coefficients.csv"))

decomp_rows <- list()
for (outcome in c("win", "loss", "draw")) {
  for (x in mechanisms$variable) {
    key <- paste(outcome, x, sep = "__")
    decomp_rows[[key]] <- tryCatch({
      row <- fit_target(
        data = main,
        outcome = outcome,
        x = x,
        fe = "player_name + event_id",
        cluster_formula = ~ player_name + event_id
      )
      row[, `:=`(outcome = outcome, variable = x)]
      row
    }, error = function(e) {
      data.table(outcome = outcome, variable = x, error = e$message)
    })
  }
}
decomposition <- rbindlist(decomp_rows, fill = TRUE)
decomposition <- merge(decomposition, mechanisms, by = "variable", all.x = TRUE)
decomposition[, p_bh_by_outcome := p.adjust(p.value, method = "BH"), by = outcome]
setorder(decomposition, outcome, p.value)
fwrite(decomposition, file.path(output_dir, "win_loss_draw_decomposition.csv"))

fake_cutoffs <- as.Date(c(
  "2023-03-01", "2023-09-01", "2024-03-01", "2024-09-01", "2025-03-01"
))
placebo_rows <- list()
pre_actual <- copy(main[date < rule_change_date])
for (cutoff in fake_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  for (outcome in outcomes) {
    for (x in mechanisms$variable) {
      key <- paste(cutoff, outcome, x, sep = "__")
      placebo_rows[[key]] <- tryCatch({
        row <- fit_target(
          data = pre_actual,
          outcome = outcome,
          x = x,
          post_var = "placebo_post",
          fe = "player_name + event_id",
          cluster_formula = ~ player_name + event_id
        )
        row[, `:=`(cutoff = cutoff, outcome = outcome, variable = x)]
        row
      }, error = function(e) {
        data.table(cutoff = cutoff, outcome = outcome, variable = x, error = e$message)
      })
    }
  }
}
placebos <- rbindlist(placebo_rows, fill = TRUE)
placebos <- merge(placebos, mechanisms, by = "variable", all.x = TRUE)
placebos[, p_bh_by_cutoff_outcome := p.adjust(p.value, method = "BH"),
         by = .(cutoff, outcome)]
setorder(placebos, outcome, variable, cutoff)
fwrite(placebos, file.path(output_dir, "placebo_cutoff_coefficients.csv"))

grid_cutoffs <- seq.Date(as.Date("2023-01-01"), as.Date("2025-05-01"), by = "2 months")
grid_variables <- c(
  "rating_diff100", "online_classic_gap100", "buchholz_c",
  "bubble_zone", "eliminated_zone"
)
grid_rows <- list()
for (cutoff in grid_cutoffs) {
  pre_actual[, placebo_post := as.integer(date >= cutoff)]
  for (x in grid_variables) {
    key <- paste(cutoff, x, sep = "__")
    grid_rows[[key]] <- tryCatch({
      row <- fit_target(
        data = pre_actual,
        outcome = "player_result",
        x = x,
        post_var = "placebo_post",
        fe = "player_name + event_id",
        cluster_formula = ~ player_name + event_id
      )
      row[, `:=`(cutoff = cutoff, outcome = "player_result", variable = x)]
      row
    }, error = function(e) {
      data.table(cutoff = cutoff, outcome = "player_result", variable = x, error = e$message)
    })
  }
}
placebo_grid <- rbindlist(grid_rows, fill = TRUE)
actual_result <- main_checks[
  specification == "event_fe_all" & outcome == "player_result" &
    variable %in% grid_variables,
  .(variable, actual_estimate = estimate)
]
placebo_grid <- merge(placebo_grid, actual_result, by = "variable", all.x = TRUE)
placebo_grid_summary <- placebo_grid[!is.na(estimate), .(
  n_placebos = .N,
  placebo_mean = mean(estimate),
  placebo_sd = sd(estimate),
  placebo_p10 = quantile(estimate, 0.10),
  placebo_p50 = quantile(estimate, 0.50),
  placebo_p90 = quantile(estimate, 0.90),
  actual_estimate = unique(actual_estimate),
  empirical_two_sided_p = (sum(abs(estimate) >= abs(unique(actual_estimate))) + 1) / (.N + 1)
), by = variable]
placebo_grid <- merge(placebo_grid, mechanisms, by = "variable", all.x = TRUE)
placebo_grid_summary <- merge(placebo_grid_summary, mechanisms, by = "variable", all.x = TRUE)
fwrite(placebo_grid, file.path(output_dir, "placebo_grid_result_coefficients.csv"))
fwrite(placebo_grid_summary, file.path(output_dir, "placebo_grid_result_summary.csv"))

pretrend_rows <- list()
pretrend_sample <- main[event_month >= -18 & event_month <= -1]
pretrend_sample[, event_month_pre := event_month + 1L]
for (outcome in outcomes) {
  for (x in mechanisms$variable) {
    key <- paste(outcome, x, sep = "__")
    fml <- as.formula(paste(
      outcome,
      "~ event_month_pre *",
      x,
      "+ player_rating100 + opponent_rating100 + is_white + factor(round)",
      "| player_name + event_id"
    ))
    pretrend_rows[[key]] <- tryCatch({
      model <- feols(fml, data = pretrend_sample, cluster = ~ player_name + event_id)
      row <- extract_target(model, c("event_month_pre", x))
      row[, `:=`(outcome = outcome, variable = x, nobs = nobs(model))]
      row
    }, error = function(e) {
      data.table(outcome = outcome, variable = x, error = e$message)
    })
  }
}
pretrends <- rbindlist(pretrend_rows, fill = TRUE)
pretrends <- merge(pretrends, mechanisms, by = "variable", all.x = TRUE)
pretrends[, p_bh_by_outcome := p.adjust(p.value, method = "BH"), by = outcome]
setorder(pretrends, outcome, p.value)
fwrite(pretrends, file.path(output_dir, "pretrend_linear_tests.csv"))

event_rows <- list()
event_sample <- main[event_month >= -18 & event_month <= 6]
event_variables <- c(
  "rating_diff100", "online_classic_gap100", "buchholz_c",
  "bubble_zone", "eliminated_zone"
)
for (outcome in outcomes) {
  for (x in event_variables) {
    key <- paste(outcome, x, sep = "__")
    fml <- as.formula(paste(
      outcome,
      "~ i(event_month,",
      x,
      ", ref = -1) + player_rating100 + opponent_rating100 + is_white + factor(round)",
      "| player_name + event_id"
    ))
    event_rows[[key]] <- tryCatch({
      model <- feols(fml, data = event_sample, cluster = ~ player_name + event_id)
      tt <- as.data.table(broom::tidy(model, conf.int = TRUE))
      tt <- tt[grepl("event_month::", term, fixed = TRUE)]
      tt[, `:=`(
        outcome = outcome,
        variable = x,
        event_month = as.integer(sub(".*event_month::(-?[0-9]+):.*", "\\1", term)),
        nobs = nobs(model)
      )]
      tt
    }, error = function(e) {
      data.table(outcome = outcome, variable = x, error = e$message)
    })
  }
}
event_study <- rbindlist(event_rows, fill = TRUE)
event_study <- merge(event_study, mechanisms, by = "variable", all.x = TRUE)
setorder(event_study, outcome, variable, event_month)
fwrite(event_study, file.path(output_dir, "event_study_mechanism_coefficients.csv"))

event_plot_data <- event_study[
  outcome == "player_result" &
    variable %in% c("rating_diff100", "online_classic_gap100", "buchholz_c", "bubble_zone", "eliminated_zone") &
    !is.na(event_month)
]
event_plot <- ggplot(event_plot_data, aes(x = event_month, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "gray50") +
  geom_point(size = 1.4, color = "#2364aa") +
  facet_wrap(~ label, scales = "free_y") +
  labs(
    x = "Months from September 2025 rule change",
    y = "Interaction coefficient relative to month -1",
    title = "Event-study checks for player_result mechanisms"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(size = 8))
ggsave(
  file.path(output_dir, "event_study_result_mechanisms.png"),
  event_plot,
  width = 11,
  height = 7,
  dpi = 200
)

effect_vars <- mechanisms[, variable]
var_stats <- rbindlist(lapply(effect_vars, function(x) {
  z <- main[[x]]
  data.table(
    variable = x,
    mean = mean(z, na.rm = TRUE),
    sd = sd(z, na.rm = TRUE),
    p25 = as.numeric(quantile(z, 0.25, na.rm = TRUE)),
    p75 = as.numeric(quantile(z, 0.75, na.rm = TRUE)),
    iqr = IQR(z, na.rm = TRUE),
    nonmissing = sum(!is.na(z))
  )
}))
effect_sizes <- merge(
  main_checks[specification == "event_fe_all", .(
    variable, outcome, estimate, std.error, p.value, conf.low, conf.high, nobs
  )],
  var_stats,
  by = "variable",
  all.x = TRUE
)
effect_sizes <- merge(effect_sizes, mechanisms, by = "variable", all.x = TRUE)
effect_sizes[, `:=`(
  effect_per_sd = estimate * sd,
  effect_per_iqr = estimate * iqr,
  effect_0_to_1 = fifelse(binary, estimate, NA_real_)
)]
outcome_means <- main[, .(
  mean_accuracy = mean(player_accuracy),
  mean_result = mean(player_result),
  mean_win = mean(win),
  mean_loss = mean(loss),
  mean_draw = mean(draw)
), by = format_5_0]
fwrite(effect_sizes, file.path(output_dir, "effect_size_translations.csv"))
fwrite(outcome_means, file.path(output_dir, "outcome_means_by_period.csv"))

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

cat("Wrote outputs to", normalizePath(output_dir), "\n")
