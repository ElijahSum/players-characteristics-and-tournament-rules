input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_dir <- "analysis_outputs/rule_change_publishable_results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_out <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(x) {
  ifelse(is.na(x), "NA", formatC(x, format = "e", digits = 2))
}

pick_one <- function(data, keep, label) {
  rows <- data[keep, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop("No row found for ", label, call. = FALSE)
  }
  rows[1L, , drop = FALSE]
}

spec_list <- function(data, keep, digits = 4) {
  rows <- data[keep, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return("No robustness rows found")
  }
  paste0(rows$specification, ": ", fmt(rows$estimate, digits), collapse = "; ")
}

mechanism <- read_out("analysis_outputs/rule_change_mechanism_validation/main_robustness_coefficients.csv")
mechanism_placebo <- read_out("analysis_outputs/rule_change_mechanism_validation/placebo_grid_result_summary.csv")
mechanism_pretrend <- read_out("analysis_outputs/rule_change_mechanism_validation/pretrend_linear_tests.csv")
production <- read_out("analysis_outputs/rule_change_production_function_tests/player_production_function_tests.csv")
production_placebo <- read_out("analysis_outputs/rule_change_production_function_tests/player_placebo_grid_summary.csv")
production_pretrend <- read_out("analysis_outputs/rule_change_production_function_tests/player_pretrend_tests.csv")
slot_event <- read_out("analysis_outputs/rule_change_time_slot_displacement_tests/event_level_displacement_coefficients.csv")
slot_participation <- read_out("analysis_outputs/rule_change_time_slot_displacement_tests/participation_selection_coefficients.csv")
rank_round <- read_out("analysis_outputs/rule_change_rank_hypotheses/round_rank_hypothesis_coefficients.csv")
rank_placebo <- read_out("analysis_outputs/rule_change_rank_hypotheses/round_rank_placebo_cutoffs.csv")

rating <- pick_one(
  mechanism,
  mechanism$outcome == "player_result" &
    mechanism$variable == "rating_diff100" &
    mechanism$specification == "event_fe_all",
  "relative skill main result"
)
rating_placebo <- pick_one(
  mechanism_placebo,
  mechanism_placebo$variable == "rating_diff100",
  "relative skill placebo grid"
)
rating_pretrend <- pick_one(
  mechanism_pretrend,
  mechanism_pretrend$outcome == "player_result" &
    mechanism_pretrend$variable == "rating_diff100",
  "relative skill pretrend"
)

accuracy_conversion <- pick_one(
  production,
  production$hypothesis == "PF01_accuracy_conversion" &
    production$outcome == "player_result" &
    production$specification == "event_fe",
  "accuracy conversion main result"
)
accuracy_placebo <- pick_one(
  production_placebo,
  production_placebo$hypothesis == "PF01_accuracy_conversion",
  "accuracy conversion placebo grid"
)
accuracy_pretrend <- pick_one(
  production_pretrend,
  production_pretrend$hypothesis == "PF01_accuracy_conversion",
  "accuracy conversion pretrend"
)

online <- pick_one(
  mechanism,
  mechanism$outcome == "player_result" &
    mechanism$variable == "online_classic_gap100" &
    mechanism$specification == "event_fe_all",
  "online capital main result"
)
online_placebo <- pick_one(
  mechanism_placebo,
  mechanism_placebo$variable == "online_classic_gap100",
  "online capital placebo grid"
)
online_pretrend <- pick_one(
  mechanism_pretrend,
  mechanism_pretrend$outcome == "player_result" &
    mechanism_pretrend$variable == "online_classic_gap100",
  "online capital pretrend"
)

bubble <- pick_one(
  mechanism,
  mechanism$outcome == "player_result" &
    mechanism$variable == "bubble_zone" &
    mechanism$specification == "event_fe_all",
  "bubble main result"
)
eliminated <- pick_one(
  mechanism,
  mechanism$outcome == "player_result" &
    mechanism$variable == "eliminated_zone" &
    mechanism$specification == "event_fe_all",
  "eliminated main result"
)
bubble_placebo <- pick_one(mechanism_placebo, mechanism_placebo$variable == "bubble_zone", "bubble placebo grid")
eliminated_placebo <- pick_one(mechanism_placebo, mechanism_placebo$variable == "eliminated_zone", "eliminated placebo grid")

slot_score <- pick_one(
  slot_event,
  slot_event$test_id == "E07_score_after_r1_late_vs_early",
  "late regular score result"
)
slot_rank <- pick_one(
  slot_event,
  slot_event$test_id == "E09_rank_percentile_late_vs_early",
  "late regular rank result"
)
slot_games <- pick_one(
  slot_event,
  slot_event$test_id == "E11_games_after_r1_late_vs_early",
  "late regular completion result"
)
slot_post <- pick_one(
  slot_participation,
  slot_participation$outcome == "has_post",
  "late-share post participation"
)

rank_rating <- pick_one(
  rank_round,
  rank_round$hypothesis == "R02_rating_edge_to_rank" &
    rank_round$specification == "event_fe",
  "rating edge to rank"
)
rank_online <- pick_one(
  rank_round,
  rank_round$hypothesis == "R04_online_capital_to_rank" &
    rank_round$specification == "event_fe",
  "online capital to rank"
)
rank_rating_placebo <- rank_placebo[
  rank_placebo$hypothesis == "R02_rating_edge_to_rank",
  ,
  drop = FALSE
]

results <- data.frame(
  result_id = paste0("R", 1:5),
  title = c(
    "Relative skill became more valuable under 5+0",
    "Accuracy advantages converted more strongly into points",
    "Online-platform human capital mattered beyond OTB strength",
    "Tournament threshold pressure shifted bubble and eliminated players",
    "Single-slot scheduling hurt old late-slot players through participation and completion"
  ),
  headline_estimate = c(
    paste0(
      "post x rating_diff100 on result = ", fmt(rating$estimate, 4),
      " (p = ", fmt_p(rating$`p.value`), ")"
    ),
    paste0(
      "post x 10-point accuracy edge on result = ", fmt(accuracy_conversion$estimate, 4),
      " (p = ", fmt_p(accuracy_conversion$`p.value`), ")"
    ),
    paste0(
      "post x Chess.com-classical rating gap/100 on result = ", fmt(online$estimate, 4),
      " (p = ", fmt_p(online$`p.value`), ")"
    ),
    paste0(
      "post x bubble = ", fmt(bubble$estimate, 4),
      "; post x eliminated = ", fmt(eliminated$estimate, 4)
    ),
    paste0(
      "late regular post score = ", fmt(slot_score$estimate, 4),
      "; rank percentile = ", fmt(slot_rank$estimate, 4),
      "; games completed = ", fmt(slot_games$estimate, 4)
    )
  ),
  robustness = c(
    spec_list(mechanism, mechanism$outcome == "player_result" & mechanism$variable == "rating_diff100", 4),
    spec_list(production, production$hypothesis == "PF01_accuracy_conversion" & production$outcome == "player_result", 4),
    spec_list(mechanism, mechanism$outcome == "player_result" & mechanism$variable == "online_classic_gap100", 4),
    paste0(
      "bubble: ",
      spec_list(mechanism, mechanism$outcome == "player_result" & mechanism$variable == "bubble_zone", 4),
      " | eliminated: ",
      spec_list(mechanism, mechanism$outcome == "player_result" & mechanism$variable == "eliminated_zone", 4)
    ),
    paste0(
      "late vs early score p-BH = ", fmt_p(slot_score$p_bh),
      "; rank p-BH = ", fmt_p(slot_rank$p_bh),
      "; games p-BH = ", fmt_p(slot_games$p_bh),
      "; any post event effect = ", fmt(slot_post$estimate, 4)
    )
  ),
  validity_checks = c(
    paste0(
      "placebo mean = ", fmt(rating_placebo$placebo_mean, 4),
      ", actual = ", fmt(rating_placebo$actual_estimate, 4),
      ", pretrend slope = ", fmt(rating_pretrend$estimate, 5)
    ),
    paste0(
      "placebo mean = ", fmt(accuracy_placebo$placebo_mean, 4),
      ", actual = ", fmt(accuracy_placebo$actual_estimate, 4),
      ", pretrend slope = ", fmt(accuracy_pretrend$estimate, 5)
    ),
    paste0(
      "placebo mean = ", fmt(online_placebo$placebo_mean, 4),
      ", actual = ", fmt(online_placebo$actual_estimate, 4),
      ", pretrend slope = ", fmt(online_pretrend$estimate, 5)
    ),
    paste0(
      "bubble placebo mean = ", fmt(bubble_placebo$placebo_mean, 4),
      ", eliminated placebo mean = ", fmt(eliminated_placebo$placebo_mean, 4)
    ),
    "Pre-rule performance placebos show some nonparallel performance trends; strongest evidence is participation/completion."
  ),
  publication_caveat = c(
    "Best causal-looking mechanism because the effect is much larger than fake-cutoff estimates and survives paired-game FE.",
    "Accuracy is post-treatment game quality, so frame this as a production-function mechanism rather than an exogenous treatment.",
    "Robust association but placebo/pretrend evidence suggests a broader trend that the rule change may amplify.",
    "Large and robust, but fake cutoffs show similar pre-existing threshold dynamics; frame as amplification, not a clean new discontinuity.",
    "Do not claim lower move quality conditional on playing; the channel is availability, selection, and tournament completion."
  ),
  primary_artifacts = c(
    "analysis_outputs/rule_change_mechanism_validation/validation_report.md; analysis_outputs/rule_change_economic_hypotheses/economic_report.md",
    "analysis_outputs/rule_change_production_function_tests/production_function_report.md",
    "analysis_outputs/rule_change_mechanism_validation/validation_report.md; analysis_outputs/rule_change_rank_hypotheses/rank_hypotheses_report.md",
    "analysis_outputs/rule_change_mechanism_validation/validation_report.md; analysis_outputs/rule_change_cognitive_result_hypotheses/cognitive_result_report.md",
    "analysis_outputs/rule_change_time_slot_displacement_tests/time_slot_displacement_report.md"
  ),
  stringsAsFactors = FALSE
)

write.csv(results, file.path(output_dir, "five_publishable_results_summary.csv"), row.names = FALSE)

lines <- c(
  "# Five Publishable Econometric Results From The September 2025 Titled Tuesday Format Change",
  "",
  paste0("Input data: `", input_file, "`."),
  "",
  "Treatment definition: `format_5_0 = 1` on and after 2025-09-01, when Titled Tuesday moved from the old 3+1/two-slot format to the post-change 5+0/single-slot format.",
  "",
  "The table below is a publication-facing index over the detailed R outputs in `analysis_outputs/`. Each result has a headline estimate, robustness checks, and a publication caveat.",
  ""
)

for (i in seq_len(nrow(results))) {
  lines <- c(
    lines,
    paste0("## ", results$result_id[i], ". ", results$title[i]),
    "",
    paste0("- Headline: ", results$headline_estimate[i]),
    paste0("- Robustness: ", results$robustness[i]),
    paste0("- Validity checks: ", results$validity_checks[i]),
    paste0("- Caveat: ", results$publication_caveat[i]),
    paste0("- Detailed artifacts: ", results$primary_artifacts[i]),
    ""
  )
}

lines <- c(
  lines,
  "## Additional Supporting Results",
  "",
  "- Age heterogeneity and young-old matchup evidence: `analysis_outputs/rule_change_age_hypotheses/summary_report.md` and `analysis_outputs/rule_change_age_matchup_result_tests/age_matchup_result_report.md`.",
  "- Lagged loss, upset, and tilt tests: `analysis_outputs/rule_change_lagged_upset_tests/lagged_upset_report.md`.",
  "- Country-time and season checks: `analysis_outputs/rule_change_country_time_hypotheses/country_time_hypotheses_report.md`.",
  ""
)

writeLines(lines, file.path(output_dir, "five_publishable_results_report.md"))

cat("Wrote publishable results index to", normalizePath(output_dir), "\n")
