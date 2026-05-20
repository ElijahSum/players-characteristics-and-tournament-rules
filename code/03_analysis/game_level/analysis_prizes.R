library(dplyr)
library(fixest)

fixest::setFixest_notes(FALSE)

args_all <- commandArgs(trailingOnly = FALSE)
args <- commandArgs(trailingOnly = TRUE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath("analysis_prizes.R")
}

ROOT <- dirname(script_path)
DATA_PATH <- file.path(ROOT, "data", "my_output.csv")
OUT_DIR <- file.path(ROOT, "analysis_results")
exclude_end_round <- "--exclude-end-round" %in% args
negative_search <- "--negative-search" %in% args
output_prefix <- if (exclude_end_round) {
  "prizes_round_interaction_no_end_round"
} else {
  "prizes_round_interaction"
}
CURRENT_SPEC_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_current_spec.csv"))
SEARCH_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_spec_search.csv"))
TOP_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_top_models.csv"))
BEST_FORMULA_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_best_formula.txt"))
FINDINGS_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_findings.md"))
NEGATIVE_OUTPUT <- file.path(OUT_DIR, paste0(output_prefix, "_negative_models.csv"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

df_players <- read.csv(DATA_PATH)

baseline_controls <- c("player_rating", "is_white", "opponent_rating")
baseline_interaction <- "round*prizes"
baseline_fixed_effects <- c("player_name", "date")
baseline_filter <- "round > 2"

current_spec <- data.frame(
  spec_name = "current_model",
  sample_name = "round_gt_2",
  sample_filter = baseline_filter,
  interaction = baseline_interaction,
  controls = paste(baseline_controls, collapse = " + "),
  fixed_effects = paste(baseline_fixed_effects, collapse = " + "),
  exclude_end_round = exclude_end_round,
  stringsAsFactors = FALSE
)
write.csv(current_spec, CURRENT_SPEC_OUTPUT, row.names = FALSE)

sample_filters <- list(
  list(
    name = "round_gt_2",
    filter = "round > 2",
    description = "Rounds 3-11"
  ),
  list(
    name = "round_gte_5",
    filter = "round >= 5",
    description = "Rounds 5-11"
  ),
  list(
    name = "round_gt_2_matched",
    filter = "round > 2 & opponent_state_match_found == 1",
    description = "Rounds 3-11 with matched opponent-state rows only"
  )
)

if (negative_search) {
  sample_filters <- append(
    sample_filters,
    list(
      list(
        name = "round_gte_6",
        filter = "round >= 6",
        description = "Rounds 6-11"
      ),
      list(
        name = "round_gte_7",
        filter = "round >= 7",
        description = "Rounds 7-11"
      ),
      list(
        name = "round_gte_8",
        filter = "round >= 8",
        description = "Rounds 8-11"
      ),
      list(
        name = "round_gte_5_matched",
        filter = "round >= 5 & opponent_state_match_found == 1",
        description = "Rounds 5-11 with matched opponent-state rows only"
      ),
      list(
        name = "round_gte_6_matched",
        filter = "round >= 6 & opponent_state_match_found == 1",
        description = "Rounds 6-11 with matched opponent-state rows only"
      ),
      list(
        name = "round_gte_7_matched",
        filter = "round >= 7 & opponent_state_match_found == 1",
        description = "Rounds 7-11 with matched opponent-state rows only"
      )
    )
  )
}

fe_options <- list(
  list(
    name = "player_date_fe",
    fes = c("player_name", "date"),
    description = "Player and date fixed effects"
  ),
  list(
    name = "player_date_round_fe",
    fes = c("player_name", "date", "round"),
    description = "Player, date, and round fixed effects"
  ),
  list(
    name = "player_opp_date_fe",
    fes = c("player_name", "opponent_name", "date"),
    description = "Player, opponent, and date fixed effects"
  )
)

control_blocks <- list(
  state_controls = c("bubble", "leader", "eliminated"),
  opponent_state_controls = c("opp_bubble", "opp_leader", "opp_eliminated"),
  matchup_controls = c(
    "bubble_vs_leader",
    "bubble_vs_prizes",
    "bubble_vs_eliminated",
    "leader_vs_bubble",
    "leader_vs_prizes",
    "leader_vs_eliminated",
    "prizes_vs_leader",
    "prizes_vs_bubble",
    "prizes_vs_eliminated",
    "eliminated_vs_leader",
    "eliminated_vs_bubble",
    "eliminated_vs_prizes"
  ),
  standing_start_controls = c(
    "final_score_before_round",
    "final_score_round_start",
    "opponents_sum_score",
    "rank"
  ),
  standing_end_controls = c(
    "final_score_round_end",
    "opponents_sum_score_end_round",
    "rank_end_round"
  ),
  result_controls = c("player_result"),
  opportunity_controls = c("could_win_prizes", "could_win_dynamic"),
  demographic_controls = c("age", "gdp_per_capita_ppp_logged")
)

if (exclude_end_round) {
  control_blocks$standing_end_controls <- NULL
}

combo_specs <- list(
  character(0),
  "state_controls",
  "opponent_state_controls",
  "matchup_controls",
  "standing_start_controls",
  "result_controls",
  "opportunity_controls",
  "demographic_controls",
  c("state_controls", "opponent_state_controls"),
  c("state_controls", "standing_start_controls"),
  c("opponent_state_controls", "standing_start_controls"),
  c("state_controls", "matchup_controls"),
  c("opponent_state_controls", "matchup_controls"),
  c("state_controls", "demographic_controls"),
  c("state_controls", "opportunity_controls")
)

if (!exclude_end_round) {
  combo_specs <- append(
    combo_specs,
    list(
      "standing_end_controls",
      c("state_controls", "standing_end_controls"),
      c("opponent_state_controls", "standing_end_controls")
    )
  )
}

collapse_blocks <- function(block_names) {
  if (length(block_names) == 0) {
    return("baseline_only")
  }
  paste(block_names, collapse = " + ")
}

build_formula <- function(extra_terms, fe_terms) {
  rhs_terms <- unique(c(baseline_controls, baseline_interaction, extra_terms))
  as.formula(
    paste0(
      "player_accuracy ~ ",
      paste(rhs_terms, collapse = " + "),
      " | ",
      paste(fe_terms, collapse = " + ")
    )
  )
}

run_single_spec <- function(data, sample_spec, fe_spec, block_names) {
  filter_index <- with(data, eval(parse(text = sample_spec$filter)))
  d <- data[filter_index, , drop = FALSE]
  extra_terms <- unique(unlist(control_blocks[block_names], use.names = FALSE))
  spec_formula <- build_formula(extra_terms, fe_spec$fes)

  fit <- tryCatch(
    feols(spec_formula, data = d),
    error = function(e) e
  )

  base_row <- data.frame(
    spec_name = paste(sample_spec$name, fe_spec$name, collapse_blocks(block_names), sep = "__"),
    sample_name = sample_spec$name,
    sample_filter = sample_spec$filter,
    sample_description = sample_spec$description,
    fixed_effects_name = fe_spec$name,
    fixed_effects = paste(fe_spec$fes, collapse = " + "),
    control_blocks = collapse_blocks(block_names),
    added_controls = if (length(extra_terms) == 0) "" else paste(extra_terms, collapse = " + "),
    formula = paste(deparse(spec_formula), collapse = " "),
    exclude_end_round = exclude_end_round,
    stringsAsFactors = FALSE
  )

  if (inherits(fit, "error")) {
    base_row$estimate_round_prizes <- NA_real_
    base_row$std_error_round_prizes <- NA_real_
    base_row$p_value_round_prizes <- NA_real_
    base_row$n_obs <- nrow(d)
    base_row$collin_vars <- ""
    base_row$error <- fit$message
    base_row$significant_5pct <- FALSE
    base_row$post_treatment_flag <- grepl("standing_end_controls|result_controls", base_row$control_blocks)
    return(base_row)
  }

  ct <- coeftable(fit)
  idx <- grep("round:prizes|prizes:round", rownames(ct))

  base_row$estimate_round_prizes <- if (length(idx) > 0) ct[idx[1], "Estimate"] else NA_real_
  base_row$std_error_round_prizes <- if (length(idx) > 0) ct[idx[1], "Std. Error"] else NA_real_
  base_row$p_value_round_prizes <- if (length(idx) > 0) ct[idx[1], "Pr(>|t|)"] else NA_real_
  base_row$n_obs <- nobs(fit)
  base_row$collin_vars <- if (length(fit$collin.var) > 0) paste(fit$collin.var, collapse = "; ") else ""
  base_row$error <- ""
  base_row$significant_5pct <- !is.na(base_row$p_value_round_prizes) && base_row$p_value_round_prizes < 0.05
  base_row$post_treatment_flag <- grepl("standing_end_controls|result_controls", base_row$control_blocks)
  base_row
}

results_list <- list()
counter <- 1L

for (sample_spec in sample_filters) {
  for (fe_spec in fe_options) {
    for (block_names in combo_specs) {
      results_list[[counter]] <- run_single_spec(df_players, sample_spec, fe_spec, block_names)
      counter <- counter + 1L
    }
  }
}

search_results <- bind_rows(results_list) %>%
  arrange(p_value_round_prizes, desc(significant_5pct), spec_name)

write.csv(search_results, SEARCH_OUTPUT, row.names = FALSE)

negative_models <- search_results %>%
  filter(!is.na(estimate_round_prizes), estimate_round_prizes < 0) %>%
  arrange(p_value_round_prizes, estimate_round_prizes, spec_name)

write.csv(negative_models, NEGATIVE_OUTPUT, row.names = FALSE)

top_models <- search_results %>%
  filter(!is.na(p_value_round_prizes)) %>%
  arrange(p_value_round_prizes, spec_name) %>%
  slice_head(n = 25)

write.csv(top_models, TOP_OUTPUT, row.names = FALSE)

baseline_result <- search_results %>%
  filter(
    sample_name == "round_gt_2",
    fixed_effects_name == "player_date_fe",
    control_blocks == "baseline_only"
  ) %>%
  slice(1)

best_overall <- search_results %>%
  filter(!is.na(p_value_round_prizes)) %>%
  arrange(p_value_round_prizes, spec_name) %>%
  slice(1)

best_non_post <- search_results %>%
  filter(!is.na(p_value_round_prizes), !post_treatment_flag) %>%
  arrange(p_value_round_prizes, spec_name) %>%
  slice(1)

best_negative <- negative_models %>%
  slice_head(n = 1)

best_negative_non_post <- negative_models %>%
  filter(!post_treatment_flag) %>%
  slice_head(n = 1)

best_formula_text <- c(
  paste0("Best overall spec: ", best_overall$spec_name),
  "",
  best_overall$formula
)
writeLines(best_formula_text, BEST_FORMULA_OUTPUT)

findings_lines <- c(
  if (exclude_end_round) {
    "# Prize Round-Interaction Specification Search Without `_end_round` Variables"
  } else {
    "# Prize Round-Interaction Specification Search"
  },
  "",
  "## Current Specification",
  paste0("- Baseline controls: `", paste(baseline_controls, collapse = " + "), "`."),
  paste0("- Interaction under study: `", baseline_interaction, "`."),
  paste0("- Baseline fixed effects: `", paste(baseline_fixed_effects, collapse = " + "), "`."),
  paste0("- Baseline sample filter: `", baseline_filter, "`."),
  if (exclude_end_round) {
    "- This run excludes all `_end_round` variables from the specification search."
  } else {
    "- This run allows `_end_round` variables through the `standing_end_controls` block."
  },
  if (negative_search) {
    "- This run also expands the sample grid toward later-round windows to search for negative `round:prizes` interactions."
  } else {
    "- This run uses the standard sample grid."
  },
  if (nrow(baseline_result) == 1) {
    paste0(
      "- In the current model, the `round:prizes` estimate is `",
      sprintf("%.4f", baseline_result$estimate_round_prizes),
      "` with p-value `",
      sprintf("%.4g", baseline_result$p_value_round_prizes),
      "`."
    )
  } else {
    "- Baseline result could not be recovered from the search table."
  },
  "",
  "## Search Outcome",
  paste0("- Total models tested: `", nrow(search_results), "`."),
  paste0("- Models with `round:prizes` significant at 5%: `", sum(search_results$significant_5pct, na.rm = TRUE), "`."),
  paste0(
    "- Best overall specification: `", best_overall$spec_name, "` with estimate `",
    sprintf("%.4f", best_overall$estimate_round_prizes),
    "` and p-value `",
    sprintf("%.4g", best_overall$p_value_round_prizes),
    "`."
  ),
  paste0("- Best overall added blocks: `", best_overall$control_blocks, "`."),
  paste0("- Best overall fixed effects: `", best_overall$fixed_effects, "`."),
  paste0("- Best overall sample filter: `", best_overall$sample_filter, "`."),
  paste0("- Models with negative `round:prizes`: `", nrow(negative_models), "`."),
  if (nrow(best_negative) == 1) {
    paste0(
      "- Best negative specification: `", best_negative$spec_name, "` with estimate `",
      sprintf("%.4f", best_negative$estimate_round_prizes),
      "` and p-value `",
      sprintf("%.4g", best_negative$p_value_round_prizes),
      "`."
    )
  } else {
    "- No negative-interaction models were found."
  },
  "",
  "## Conservative Read",
  paste0(
    "- Best specification that avoids explicitly post-treatment blocks (`standing_end_controls`, `result_controls`) is `",
    best_non_post$spec_name,
    "` with estimate `",
    sprintf("%.4f", best_non_post$estimate_round_prizes),
    "` and p-value `",
    sprintf("%.4g", best_non_post$p_value_round_prizes),
    "`."
  ),
  paste0("- Conservative added blocks: `", best_non_post$control_blocks, "`."),
  paste0("- Conservative fixed effects: `", best_non_post$fixed_effects, "`."),
  paste0("- Conservative sample filter: `", best_non_post$sample_filter, "`."),
  if (nrow(best_negative_non_post) == 1) {
    paste0(
      "- Best negative specification without post-treatment blocks is `",
      best_negative_non_post$spec_name,
      "` with estimate `",
      sprintf("%.4f", best_negative_non_post$estimate_round_prizes),
      "` and p-value `",
      sprintf("%.4g", best_negative_non_post$p_value_round_prizes),
      "`."
    )
  } else {
    "- No negative specification survived after excluding post-treatment blocks."
  },
  "",
  "## Output Files",
  paste0("- `", basename(CURRENT_SPEC_OUTPUT), "`"),
  paste0("- `", basename(SEARCH_OUTPUT), "`"),
  paste0("- `", basename(TOP_OUTPUT), "`"),
  paste0("- `", basename(BEST_FORMULA_OUTPUT), "`"),
  paste0("- `", basename(NEGATIVE_OUTPUT), "`")
)

writeLines(findings_lines, FINDINGS_OUTPUT)

print(
  top_models %>%
    select(
      spec_name,
      control_blocks,
      fixed_effects_name,
      sample_name,
      estimate_round_prizes,
      p_value_round_prizes,
      n_obs,
      post_treatment_flag
    ) %>%
    slice_head(n = 10)
)
