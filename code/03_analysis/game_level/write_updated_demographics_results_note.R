library(data.table)

input_file <- "data/final_regression_data_tournaments_2022_2026.csv"
output_file <- "analysis_outputs/updated_demographics_results_note.md"

rerun_scripts <- c(
  "rule_change_age_hypotheses.R",
  "rule_change_age_matchup_result_tests.R",
  "rule_change_cognitive_result_hypotheses.R",
  "rule_change_metadata_heterogeneity_questions.R",
  "rule_change_economic_hypotheses.R",
  "rule_change_country_time_hypotheses.R",
  "rule_change_mechanism_validation.R",
  "rule_change_production_function_tests.R",
  "rule_change_rank_hypotheses.R",
  "rule_change_lagged_upset_tests.R",
  "rule_change_time_slot_displacement_tests.R",
  "rule_change_second_iteration_new_ideas.R",
  "rule_change_third_iteration_final_outcomes.R",
  "rule_change_fatigue_iteration.R",
  "rule_change_publishable_results_index.R"
)

output_dirs <- c(
  "analysis_outputs/rule_change_age_hypotheses",
  "analysis_outputs/rule_change_age_matchup_result_tests",
  "analysis_outputs/rule_change_cognitive_result_hypotheses",
  "analysis_outputs/rule_change_metadata_heterogeneity_questions",
  "analysis_outputs/rule_change_economic_hypotheses",
  "analysis_outputs/rule_change_country_time_hypotheses",
  "analysis_outputs/rule_change_mechanism_validation",
  "analysis_outputs/rule_change_production_function_tests",
  "analysis_outputs/rule_change_rank_hypotheses",
  "analysis_outputs/rule_change_lagged_upset_tests",
  "analysis_outputs/rule_change_time_slot_displacement_tests",
  "analysis_outputs/rule_change_second_iteration_new_ideas",
  "analysis_outputs/rule_change_third_iteration_final_outcomes",
  "analysis_outputs/rule_change_fatigue_iteration",
  "analysis_outputs/rule_change_publishable_results"
)

dt <- fread(
  input_file,
  select = c(
    "player_name", "real_name", "birthday", "country_name",
    "gdp_per_capita_ppp_logged", "female"
  ),
  showProgress = FALSE
)

row_count <- nrow(dt)
player_count <- uniqueN(dt$player_name)

coverage <- data.table(
  variable = c("real_name", "birthday", "country_name", "gdp_per_capita_ppp_logged", "female"),
  non_missing_rows = c(
    sum(!is.na(dt$real_name) & dt$real_name != ""),
    sum(!is.na(dt$birthday)),
    sum(!is.na(dt$country_name) & dt$country_name != ""),
    sum(!is.na(dt$gdp_per_capita_ppp_logged)),
    sum(!is.na(dt$female))
  )
)
coverage[, missing_rows := row_count - non_missing_rows]
coverage[, coverage_pct := round(100 * non_missing_rows / row_count, 2)]

female_counts <- dt[, .N, by = female][order(female)]
female_counts[, female := fifelse(is.na(female), "missing", as.character(female))]

output_status <- data.table(path = output_dirs)
output_status[, exists := file.exists(path)]
output_status[, newest_file := vapply(
  path,
  function(x) {
    files <- list.files(x, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    files <- files[file.info(files)$isdir == FALSE]
    if (!length(files)) return(NA_character_)
    basename(files[which.max(file.info(files)$mtime)])
  },
  character(1)
)]
output_status[, last_modified := vapply(
  path,
  function(x) {
    files <- list.files(x, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    files <- files[file.info(files)$isdir == FALSE]
    if (!length(files)) return(NA_character_)
    format(max(file.info(files)$mtime), "%Y-%m-%d %H:%M:%S %Z")
  },
  character(1)
)]

lines <- c(
  "# Updated Demographic Inputs and Re-run Results",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Input Data",
  "",
  paste0("- Regression file: `", input_file, "`"),
  paste0("- Rows: ", format(row_count, big.mark = ",")),
  paste0("- Unique players: ", format(player_count, big.mark = ",")),
  "- The analysis file now uses the refreshed player-level `real_name`, rating, federation/country, GDP, `birthday`, and `female` fields.",
  "- `female` is encoded as `1` for women, `0` for men, and missing when gender is unknown.",
  "",
  "## Coverage After Refresh",
  "",
  "| Variable | Non-missing rows | Missing rows | Coverage (%) |",
  "|---|---:|---:|---:|",
  apply(
    coverage,
    1,
    function(x) {
      paste0(
        "| `", x[["variable"]], "` | ",
        format(as.integer(x[["non_missing_rows"]]), big.mark = ","), " | ",
        format(as.integer(x[["missing_rows"]]), big.mark = ","), " | ",
        x[["coverage_pct"]], " |"
      )
    }
  ),
  "",
  "## Female Variable Counts",
  "",
  "| female | Rows |",
  "|---:|---:|",
  apply(
    female_counts,
    1,
    function(x) {
      paste0("| ", x[["female"]], " | ", format(as.integer(x[["N"]]), big.mark = ","), " |")
    }
  ),
  "",
  "## Re-run Scripts",
  "",
  paste0("- `", rerun_scripts, "`"),
  "",
  "## Refreshed Output Folders",
  "",
  "| Output folder | Exists | Newest file | Last modified |",
  "|---|---:|---|---|",
  apply(
    output_status,
    1,
    function(x) {
      paste0(
        "| `", x[["path"]], "` | ", x[["exists"]], " | `",
        x[["newest_file"]], "` | ", x[["last_modified"]], " |"
      )
    }
  ),
  "",
  "## Notes",
  "",
  "- The publishable-results index was regenerated after the underlying analysis folders were refreshed.",
  "- Extra gender provenance columns were intentionally not included in the final regression dataset; only `female` remains.",
  "- Remaining missingness is driven by unresolved player-level information after FIDE/name/title enrichment."
)

writeLines(lines, output_file)
cat("Wrote", normalizePath(output_file), "\n")
