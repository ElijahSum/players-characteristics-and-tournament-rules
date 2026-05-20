suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

setFixest_notes(FALSE)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "code/03_analysis/move_clock_level/analyze_exact_clock_requested_specs.R"
ROOT <- normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
if (!dir.exists(file.path(ROOT, "analysis_outputs"))) ROOT <- getwd()

out_dir <- file.path(ROOT, "analysis_outputs", "exact_clock_mechanisms_2022_2026")
deep_dir <- file.path(ROOT, "analysis_outputs", "deep_clock_mechanisms_2022_2026")
metadata_path <- file.path(ROOT, "data", "final_regression_data_tournaments_2022_2026.csv")
bridge_paths <- c(
  file.path(ROOT, "data", "tournaments_1_261_final_v6.csv"),
  file.path(ROOT, "data", "merged_tournaments_1_150_added_missed_links_3.csv")
)

paths <- list(
  snapshots = file.path(deep_dir, "deep_clock_snapshots.csv"),
  panic = file.path(out_dir, "exact_clock_bin_move_cells.csv"),
  thinking = file.path(out_dir, "exact_thinking_time_move_cells.csv"),
  event = file.path(out_dir, "exact_first_time_trouble_event_cells.csv")
)
missing_files <- c(metadata_path, bridge_paths, unlist(paths))[!file.exists(c(metadata_path, bridge_paths, unlist(paths)))]
if (length(missing_files) > 0) stop("Missing files:\n", paste(missing_files, collapse = "\n"))

normalize_event_datetime <- function(x) {
  x <- as.character(x)
  parsed_iso <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  parsed_mdy <- as.POSIXct(x, format = "%b %d, %Y, %I:%M %p", tz = "UTC")
  parsed <- parsed_iso
  parsed[is.na(parsed)] <- parsed_mdy[is.na(parsed)]
  format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

extract_game_id <- function(x) {
  x <- as.character(x)
  out <- sub(".*live/([0-9]+).*", "\\1", x)
  out[!grepl("^[0-9]+$", out)] <- NA_character_
  out
}

read_bridge <- function(path) {
  raw <- fread(
    path,
    select = c("white_name", "black_name", "date", "round", "game_link"),
    colClasses = list(character = c("date", "game_link"))
  )
  raw[, date_key := normalize_event_datetime(date)]
  raw[, game_id := extract_game_id(game_link)]
  raw <- raw[!is.na(game_id) & !is.na(date_key)]
  raw[, round := as.integer(round)]
  white <- raw[, .(date_key, round, game_id, player_name = white_name, opponent_name = black_name, is_white = 1L)]
  black <- raw[, .(date_key, round, game_id, player_name = black_name, opponent_name = white_name, is_white = 0L)]
  rbindlist(list(white, black), use.names = TRUE)
}

sig_label <- function(p) {
  fifelse(is.na(p), "",
    fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.1, "*", "")))
  )
}

tidy_fixest <- function(model, model_id, mechanism, outcome) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  setnames(ct, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
    c("estimate", "std_error", "t_value", "p_value"),
    skip_absent = TRUE
  )
  ct[, `:=`(
    model_id = model_id,
    mechanism = mechanism,
    outcome = outcome,
    n_obs = nobs(model),
    r2_within = tryCatch(as.numeric(fitstat(model, "wr2")), error = function(e) NA_real_),
    significance = sig_label(p_value)
  )]
  setcolorder(ct, c(
    "model_id", "mechanism", "outcome", "term", "estimate", "std_error",
    "t_value", "p_value", "significance", "n_obs", "r2_within"
  ))
  ct[]
}

fit_one <- function(model_id, mechanism, outcome, formula, data, key_patterns = NULL, weights = NULL) {
  cat("Estimating", model_id, "-", mechanism, "\n")
  model <- feols(formula, data = data, cluster = ~player_name, weights = weights, mem.clean = TRUE)
  coefs <- tidy_fixest(model, model_id, mechanism, outcome)
  if (is.null(key_patterns)) {
    headline <- coefs
  } else {
    keep <- Reduce(`|`, lapply(key_patterns, function(pat) grepl(pat, coefs$term)))
    headline <- coefs[keep]
  }
  headline[, is_headline := TRUE]
  list(model = model, coefs = coefs, headline = headline)
}

coef_get <- function(coef_table, model_id_value, pattern) {
  row <- coef_table[model_id == model_id_value & grepl(pattern, term)]
  if (nrow(row) == 0) return("not estimated")
  row <- row[1]
  sprintf("%.4f (se %.4f, p=%.3g)%s", row$estimate, row$std_error, row$p_value, row$significance)
}

cat("Building metadata bridge...\n")
meta <- fread(
  metadata_path,
  select = c("player_name", "player_rating", "round", "date", "opponent_rating", "player_result", "opponent_name", "is_white"),
  colClasses = list(character = "date")
)
meta[, date_key := normalize_event_datetime(date)]
meta[, round := as.integer(round)]
meta[, is_white := as.integer(is_white)]

bridge <- rbindlist(lapply(bridge_paths, read_bridge), use.names = TRUE)
bridge <- unique(bridge)
bridge_key <- c("date_key", "round", "player_name", "opponent_name", "is_white")
bridge <- bridge[order(date_key, round, player_name, opponent_name, is_white, game_id)]
bridge <- bridge[, .SD[1], by = bridge_key]
meta_game <- merge(meta, bridge, by = bridge_key, all.x = TRUE, sort = FALSE)
meta_small <- meta_game[!is.na(game_id), .(
  game_id = as.character(game_id),
  player_name,
  date_key,
  player_result = as.numeric(player_result),
  player_rating = as.numeric(player_rating),
  opponent_rating = as.numeric(opponent_rating),
  rating_diff100 = (as.numeric(player_rating) - as.numeric(opponent_rating)) / 100
)]
meta_small[, event_date := as.IDate(date_key)]
meta_small[, format_5_0 := as.integer(event_date >= as.IDate("2025-09-02"))]
setkey(meta_small, game_id, player_name)

all_coefs <- list()
all_headlines <- list()
sample_summary <- list()

cat("Reading snapshots for shadow price and skill-clock models...\n")
snap <- fread(paths$snapshots, colClasses = list(character = c("game_id", "player_name")))
snap <- merge(snap, meta_small, by = c("game_id", "player_name"), all = FALSE, sort = FALSE)
snap[, eval_adv100 := pmax(pmin(as.numeric(eval_advantage_cp), 1000), -1000) / 100]
snap[, clock_adv_min := as.numeric(clock_advantage_seconds) / 60]
sample_summary[["snapshots"]] <- data.table(dataset = "snapshots", rows = nrow(snap), moves = NA_real_)

m <- fit_one(
  "E01",
  "Shadow price of time with player and event FE",
  "Player result at move 10/20/30 snapshots",
  player_result ~ eval_adv100 + clock_adv_min + clock_adv_min:format_5_0 + i(snapshot_move) |
    player_name + date_key,
  snap,
  c("eval_adv100", "clock_adv_min")
)
all_coefs[["E01"]] <- m$coefs
all_headlines[["E01"]] <- m$headline

shadow_rows <- list()
for (mv in sort(unique(snap$snapshot_move))) {
  sm <- snap[snapshot_move == mv]
  mod <- feols(
    player_result ~ eval_adv100 + clock_adv_min + clock_adv_min:format_5_0 |
      player_name + date_key,
    data = sm,
    cluster = ~player_name,
    mem.clean = TRUE
  )
  b <- coef(mod)
  beta_eval <- unname(b["eval_adv100"])
  beta_clock_pre <- unname(b["clock_adv_min"])
  beta_clock_post <- beta_clock_pre + unname(b["clock_adv_min:format_5_0"])
  shadow_rows[[as.character(mv)]] <- data.table(
    snapshot_move = mv,
    n_obs = nobs(mod),
    beta_eval100 = beta_eval,
    beta_clock_min_pre = beta_clock_pre,
    beta_clock_min_post = beta_clock_post,
    clock_min_equiv_cp_pre = 100 * beta_clock_pre / beta_eval,
    clock_min_equiv_cp_post = 100 * beta_clock_post / beta_eval
  )
}
shadow_price <- rbindlist(shadow_rows)
fwrite(shadow_price, file.path(out_dir, "exact_shadow_price_summary.csv"))

snap20 <- snap[snapshot_move == 20]
m <- fit_one(
  "E02",
  "Skill advantage conditional on clock capital",
  "Player result at move 20 snapshots",
  player_result ~ rating_diff100 * clock_adv_min * format_5_0 + eval_adv100 |
    player_name + date_key,
  snap20,
  c("rating_diff100", "clock_adv_min", "format_5_0:clock_adv_min:rating_diff100", "clock_adv_min:rating_diff100:format_5_0")
)
all_coefs[["E02"]] <- m$coefs
all_headlines[["E02"]] <- m$headline
rm(snap, snap20)
gc()

cat("Reading exact clock-bin move cells...\n")
panic <- fread(paths$panic)
panic[, clock_bin_f := factor(clock_bin, levels = c("gt120", "60_120", "30_60", "10_30", "05_10", "00_05"))]
sample_summary[["panic"]] <- data.table(dataset = "exact_clock_bin_move_cells", rows = nrow(panic), moves = sum(panic$n_moves))
m <- fit_one(
  "E03",
  "Panic threshold shift with move-number FE",
  "Capped centipawn loss by clock bin",
  mean_cp_loss ~ i(clock_bin_f, ref = "gt120") +
    i(clock_bin_f, format_5_0, ref = "gt120") |
    player_name + date_key + fullmove_number,
  panic,
  c("clock_bin_f::"),
  weights = ~n_moves
)
all_coefs[["E03"]] <- m$coefs
all_headlines[["E03"]] <- m$headline
rm(panic)
gc()

cat("Reading exact first-time-trouble event cells...\n")
event <- fread(paths$event)
event[, event_k_f := factor(event_k, levels = as.character(-5:10))]
sample_summary[["event"]] <- data.table(dataset = "exact_first_time_trouble_event_cells", rows = nrow(event), moves = sum(event$n_moves))
for (threshold_value in c(30, 10)) {
  model_id <- paste0("E04_", threshold_value)
  m <- fit_one(
    model_id,
    paste0("Event study around first <=", threshold_value, " seconds"),
    "Capped centipawn loss around first time-trouble entry",
    mean_cp_loss ~ i(event_k_f, ref = "-1") +
      i(event_k_f, format_5_0, ref = "-1") |
      player_name + date_key + fullmove_number,
    event[threshold == threshold_value],
    c("event_k_f::"),
    weights = ~n_moves
  )
  all_coefs[[model_id]] <- m$coefs
  all_headlines[[model_id]] <- m$headline
}
rm(event)
gc()

cat("Reading exact thinking-time move cells...\n")
thinking <- fread(paths$thinking)
sample_summary[["thinking"]] <- data.table(dataset = "exact_thinking_time_move_cells", rows = nrow(thinking), moves = sum(thinking$n_moves))
thinking <- thinking[critical == 1]
thinking[, time_midpoint2 := time_midpoint^2]
m <- fit_one(
  "E05",
  "Marginal product of thinking time in critical positions",
  "Critical-position capped centipawn loss",
  mean_cp_loss ~ time_midpoint + time_midpoint2 +
    time_midpoint:format_5_0 + time_midpoint2:format_5_0 |
    player_name + date_key + fullmove_number,
  thinking,
  c("time_midpoint", "time_midpoint2"),
  weights = ~n_moves
)
all_coefs[["E05"]] <- m$coefs
all_headlines[["E05"]] <- m$headline
rm(thinking)
gc()

coef_table <- rbindlist(all_coefs, fill = TRUE)
headline_table <- rbindlist(all_headlines, fill = TRUE)
sample_table <- rbindlist(sample_summary, fill = TRUE)

fwrite(coef_table, file.path(out_dir, "exact_requested_model_coefficients.csv"))
fwrite(headline_table, file.path(out_dir, "exact_requested_headline_coefficients.csv"))
fwrite(sample_table, file.path(out_dir, "exact_requested_sample_summary.csv"))

panic_terms <- headline_table[model_id == "E03" & grepl("format_5_0", term)]
event30_terms <- headline_table[model_id == "E04_30" & grepl("format_5_0", term)]
event10_terms <- headline_table[model_id == "E04_10" & grepl("format_5_0", term)]

md <- c(
  "# Exact Requested Clock Specifications",
  "",
  "This file reports the exact follow-up tests requested by the user. The thesis text was not edited.",
  "",
  "## Samples",
  "",
  paste0("- Snapshot rows: ", format(sample_table[dataset == "snapshots", rows], big.mark = ","), "."),
  paste0("- Panic-threshold cells: ", format(sample_table[dataset == "exact_clock_bin_move_cells", rows], big.mark = ","), " cells over ", format(round(sample_table[dataset == "exact_clock_bin_move_cells", moves]), big.mark = ","), " weighted moves."),
  paste0("- First-time-trouble event cells: ", format(sample_table[dataset == "exact_first_time_trouble_event_cells", rows], big.mark = ","), " cells."),
  paste0("- Thinking-time cells: ", format(sample_table[dataset == "exact_thinking_time_move_cells", rows], big.mark = ","), " cells."),
  "",
  "## Results",
  "",
  "1. **Shadow price of time.** Player/event-FE model at moves 10/20/30. One minute of clock advantage is equivalent to:",
  paste0("   - move 10: pre ", sprintf("%.1f", shadow_price[snapshot_move == 10, clock_min_equiv_cp_pre]), " cp, post ", sprintf("%.1f", shadow_price[snapshot_move == 10, clock_min_equiv_cp_post]), " cp."),
  paste0("   - move 20: pre ", sprintf("%.1f", shadow_price[snapshot_move == 20, clock_min_equiv_cp_pre]), " cp, post ", sprintf("%.1f", shadow_price[snapshot_move == 20, clock_min_equiv_cp_post]), " cp."),
  paste0("   - move 30: pre ", sprintf("%.1f", shadow_price[snapshot_move == 30, clock_min_equiv_cp_pre]), " cp, post ", sprintf("%.1f", shadow_price[snapshot_move == 30, clock_min_equiv_cp_post]), " cp."),
  paste0("   Main post interaction: ", coef_get(headline_table, "E01", "clock_adv_min:format_5_0|format_5_0:clock_adv_min"), "."),
  "",
  paste0("2. **Panic threshold shift.** The exact move-number-FE model shows post-change extra penalties concentrated at very low clock bins. 5-10s interaction: ", coef_get(headline_table, "E03", "05_10:format_5_0"), "; 0-5s interaction: ", coef_get(headline_table, "E03", "00_05:format_5_0"), "."),
  "",
  paste0("3. **Skill advantage conditional on clock capital.** Move-20 model triple interaction rating x clock x 5+0: ", coef_get(headline_table, "E02", "rating_diff100.*clock_adv_min.*format_5_0|clock_adv_min.*rating_diff100.*format_5_0|format_5_0.*clock_adv_min.*rating_diff100"), "."),
  "",
  paste0("4. **Marginal product of thinking time.** Critical-position model with binned time midpoint and move-number FE: time slope ", coef_get(headline_table, "E05", "^time_midpoint$"), "; post time-slope interaction ", coef_get(headline_table, "E05", "time_midpoint:format_5_0|format_5_0:time_midpoint"), "."),
  "",
  paste0("5. **Event study around first time trouble.** Around first <=30s, selected post interactions are k=0 ",
         coef_get(headline_table, "E04_30", "event_k_f::0:format_5_0"),
         ", k=5 ", coef_get(headline_table, "E04_30", "event_k_f::5:format_5_0"),
         ", and k=10 ", coef_get(headline_table, "E04_30", "event_k_f::10:format_5_0"), ". ",
         "Around first <=10s, selected post interactions are k=0 ",
         coef_get(headline_table, "E04_10", "event_k_f::0:format_5_0"),
         ", k=5 ", coef_get(headline_table, "E04_10", "event_k_f::5:format_5_0"),
         ", and k=10 ", coef_get(headline_table, "E04_10", "event_k_f::10:format_5_0"), "."),
  "",
  "## Caveat",
  "",
  "For the thinking-time model, time spent is represented by bin midpoints to keep the all-move fixed-effect regression computationally feasible. The panic-threshold and event-study models use all matched moves through weighted cells."
)
writeLines(md, file.path(out_dir, "exact_requested_clock_results.md"))

cat("Exact requested analysis complete.\n")
print(sample_table)
print(shadow_price)
print(headline_table)
