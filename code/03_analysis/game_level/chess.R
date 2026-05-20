library(dplyr)
library(fixest)
library(ggplot2)
library(plotly)
library(stargazer)
library(modelsummary)
library(patchwork) 
library(tidyr)
library(lfe)
library(modelsummary)
library(ggrepel)



# Creating the sample data (in a real case, read in your dataset with read.csv() or similar function)
df_players <- read.csv('players_regression_data_6_with_opponents_sum_score_with_rank.csv')
personal_info <- read.csv('personal_players_info.csv')

personal_info <- personal_info %>% mutate(date = as.Date(date)) %>% select(-X)
df_players <- df_players %>% mutate(date = as.Date(date)) %>% select(-X)

df_players <- df_players %>%
  left_join(personal_info, by = c("player_name", "date")) %>% distinct()


df_players <- df_players %>%
  mutate(round_11 = as.integer(round == 11))

df_players <- df_players %>%
  mutate(round_10 = as.integer(round == 10))

df_players <- df_players %>% select(-c(final_score, rank, opponents_sum_score))


df_players <- df_players %>%
  mutate(
    round = as.integer(round),                 # in case it's stored as text
    player_result = as.numeric(player_result)  # ensure 1 / 0.5 / 0 is numeric
  ) %>%
  group_by(player_name, date) %>%
  arrange(round, .by_group = TRUE) %>%
  mutate(
    final_score_round_start = lag(cumsum(player_result), default = 0)
  ) %>%
  ungroup()


df_players <- df_players %>%
  mutate(
    round = as.integer(round),
    player_result = as.numeric(player_result)
  ) %>%
  group_by(player_name, date) %>%
  arrange(round, .by_group = TRUE) %>%
  mutate(
    final_score_round_end = cumsum(player_result)
  ) %>%
  ungroup()

df_players <- df_players %>% select(-c(could_win_dynamic))


df_players <- df_players %>%
  group_by(date, round) %>%
  mutate(
    max_score_round_date = max(final_score_round_start, na.rm = TRUE),
    could_win_dynamic = if_else(
      is.na(final_score_round_start) | is.infinite(max_score_round_date),
      0L,
      as.integer(abs(final_score_round_start - max_score_round_date) <= 1)
    )
  ) %>%
  ungroup() %>%
  select(-max_score_round_date)


df_players <- df_players %>%
  group_by(date, round) %>%
  mutate(
    max_score_round_date = max(final_score_round_start, na.rm = TRUE),
    could_win_dynamic_05 = if_else(
      is.na(final_score_round_start) | is.infinite(max_score_round_date),
      0L,
      as.integer(abs(final_score_round_start - max_score_round_date) <= 0.5)
    )
  ) %>%
  ungroup() %>%
  select(-max_score_round_date)




df_players <- df_players %>%
  filter(player_accuracy != 100, player_accuracy != 0, player_title != 'No Title', player_rating >= 2000)
df_players <- df_players %>%
  mutate(round_11 = if_else(round == 11, 1, 0))
df_players <- df_players %>%
  mutate(round_10 = if_else(round == 10, 1, 0))
df_players <- df_players %>%
  mutate(round_10_11 = if_else(round == 10 | round == 11, 1, 0))
df_players <- df_players %>% mutate(rating_difference = player_rating - opponent_rating)




# df_players <- df_players %>%filter(birthday != 'Not found')

df_players$birthday <- as.integer(df_players$birthday)
df_players$age <- 2024 - df_players$birthday

df_players$age_under_35 <- ifelse(df_players$age <= 35, 1, 0)

# df_players$round <- factor(df_players$round, levels = 1:11)

# Create interaction terms for gender and round (this will automatically happen in the model formula)
# Define the regression formula
# Fit the model

df_players <- df_players %>%
  mutate(in_prizes = if_else(rank <= 6, 1, 0))

df_players <- df_players %>%
  mutate(bubble = if_else(rank <= 15 & rank >= 7, 1, 0))

df_players <- df_players %>%
  mutate(leader = if_else(rank == 1, 1, 0))

round_var = 4



model1_no_fe <- felm(
  player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating | 0,
  data = df_players %>% filter(round>round_var)
)
summary(model1_no_fe)



model1 <- felm(
  player_accuracy ~ player_rating + is_white + round*bubble + opponent_rating | player_name + date,
  data = df_players %>% filter(round>round_var)
)
summary(model1)


write.csv(df_players, 'data/players_regression_data_final_2024.csv')



######### bubble, in_prizes, leader
library(dplyr)

df_players <- df_players %>%
  mutate(
    leader = as.integer(leader_11_round == 1),
    prizes = as.integer(in_prizes == 1),
    bubble = as.integer(could_win_prizes == 1 & in_prizes == 0 & leader_11_round == 0),
    eliminated = as.integer(could_win_prizes == 0 & in_prizes == 0 & leader_11_round == 0)
  )

# bring opponent categories into each player's row
df_players <- df_players %>%
  left_join(
    df_players %>%
      select(
        date, round,
        player_name,
        leader, prizes, bubble, eliminated
      ) %>%
      rename(
        opponent_name = player_name,
        opp_leader = leader,
        opp_prizes = prizes,
        opp_bubble = bubble,
        opp_eliminated = eliminated
      ),
    by = c("date", "round", "opponent_name")
  ) %>%
  mutate(
    bubble_vs_leader      = as.integer(bubble == 1 & opp_leader == 1),
    bubble_vs_prizes      = as.integer(bubble == 1 & opp_prizes == 1),
    bubble_vs_eliminated  = as.integer(bubble == 1 & opp_eliminated == 1),
    
    leader_vs_bubble      = as.integer(leader == 1 & opp_bubble == 1),
    leader_vs_prizes      = as.integer(leader == 1 & opp_prizes == 1),
    leader_vs_eliminated  = as.integer(leader == 1 & opp_eliminated == 1),
    
    eliminated_vs_leader  = as.integer(eliminated == 1 & opp_leader == 1),
    eliminated_vs_bubble  = as.integer(eliminated == 1 & opp_bubble == 1),
    eliminated_vs_prizes  = as.integer(eliminated == 1 & opp_prizes == 1)
  )

df_players <- read.csv('data/my_output.csv')

df_players <- df_players %>%
  mutate(bubble = if_else(rank <= 15 & rank >= 7, 1, 0))

model2_bubble <- felm(
  player_accuracy ~ player_rating + is_white + opponent_rating + round*prizes + could_win_prizes + could_win_prizes + player_result | player_name + date,
  data = df_players %>% filter(round>3)
)
summary(model2_bubble)


#########

df_rank_brackets <- df_players %>%
  filter(round > round_var, !is.na(player_accuracy), !is.na(rank)) %>%
  group_by(date, round) %>%
  mutate(
    prize_cutoff_rank = if_else(
      any(in_prizes == 1),
      max(rank[in_prizes == 1], na.rm = TRUE),
      NA_real_
    ),
    dist_to_cutoff = rank - prize_cutoff_rank
  ) %>%
  ungroup() %>%
  mutate(
    rank_bracket = case_when(
      could_win_prizes == 0 ~ "eliminated",
      in_prizes == 0 & could_win_prizes == 1 & dist_to_cutoff >= 3 ~ "alive_but_far",
      in_prizes == 0 & could_win_prizes == 1 & dist_to_cutoff %in% c(1, 2) ~ "bubble_outside",
      in_prizes == 1 & rank == 1 ~ "leader",
      in_prizes == 1 & dist_to_cutoff <= -3 ~ "comfortably_in_prizes",
      in_prizes == 1 & dist_to_cutoff %in% c(-2, -1, 0) ~ "near_cutoff_inside",
      TRUE ~ NA_character_
    ),
    rank_bracket = factor(
      rank_bracket,
      levels = c(
        "eliminated",
        "alive_but_far",
        "bubble_outside",
        "near_cutoff_inside",
        "comfortably_in_prizes",
        "leader"
      )
    )
  ) %>%
  filter(!is.na(rank_bracket))

  
model_brackets <- felm(
  player_accuracy ~ player_rating + is_white + opponent_rating + round * rank_bracket |
    player_name + date,
  data = df_rank_brackets
)

summary(model_brackets)




df_players %>%
  distinct(player_name, .keep_all = TRUE) %>%
  summarise(
    q = list(quantile(gdp_per_capita_ppp_logged,
                      probs = seq(0, 1, 0.5),
                      na.rm = TRUE))
  ) %>%
  pull(q) %>%
  .[[1]]

df_players_rich <- subset(df_players, gdp_per_capita_ppp_logged > 10.798698)
df_players_poor <- subset(df_players, gdp_per_capita_ppp_logged <= 10.798698)

model_poor <- felm(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_poor %>% filter(round>round_var))
model_rich <- felm(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_rich %>% filter(round>round_var))

summary(model_poor)
summary(model_rich)

df_players %>%
  distinct(player_name, .keep_all = TRUE) %>%
  summarise(
    q = list(quantile(birthday,
                      probs = seq(0, 1, 0.5),
                      na.rm = TRUE))
  ) %>%
  pull(q) %>%
  .[[1]]


df_players_young <- subset(df_players, birthday >= 1994)
df_players_old <- subset(df_players, birthday < 1994)


model_young <- felm(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_young %>% filter(round>round_var))
model_old <- felm(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_old %>% filter(round>round_var))

summary(model_young)
summary(model_old)


keep_rows <- c(
  "player_rating",
  "is_white",
  "round:in_prizes",
  "opponent_rating"
)

pretty_labels <- c(
  "Player Rating",
  "White",
  "Round x In Prizes",
  "Opponent Rating"
)



mods <- list(model1_no_fe, model1, model_poor, model_rich, model_young, model_old)

# 1) Inspect exact coefficient names (DO THIS ONCE)
lapply(mods, function(m) names(coef(m)))
# From this output, define your "terms" EXACTLY.

# 2) Define exact term names you want to show (example: adjust to your names!)
terms <- c("player_rating", "is_white", 'round', 'in_prizes', "opponent_rating",  "round:in_prizes")

pretty_labels <- c("Player Rating", "White",'Round','In Prizes', "Opponent Rating", "Round x In Prizes")

# 3) Clustered SE + aligned coef/se/p
vcov_cluster_player <- function(m) vcov(m, cluster = ~ player_name)

extract_aligned <- function(m, terms) {
  b <- coef(m)
  V <- vcov_cluster_player(m)
  se <- sqrt(diag(V))
  
  # aligned vectors (same length/order across models)
  b_al  <- b[terms]
  se_al <- se[terms]
  
  # normal approx p-values (fine for large N; or use df if you prefer)
  t_al <- as.numeric(b_al) / as.numeric(se_al)
  p_al <- 2 * pnorm(abs(t_al), lower.tail = FALSE)
  
  list(
    b  = as.numeric(b_al),
    se = as.numeric(se_al),
    p  = as.numeric(p_al)
  )
}

objs <- lapply(mods, extract_aligned, terms = terms)
coef_list <- lapply(objs, `[[`, "b")
se_list   <- lapply(objs, `[[`, "se")
p_list    <- lapply(objs, `[[`, "p")

# 4) Print with stargazer using your pre-aligned rows ONLY
stargazer(
  mods,
  type = "html",
  out = "chess_simple_model_comparison.html",
  title = "Comparison of Player Accuracy Models",
  dep.var.labels = "Player Accuracy",
  column.labels = c("No Fixed Effects", "With Fixed Effects",
                    "Lower-GDP Cohort", "Upper-GDP Cohort",
                    "Younger Cohort", "Older Cohort"),
  coef = coef_list,
  se   = se_list,
  p    = p_list,
  t.auto = FALSE,
  p.auto = FALSE,
  omit = "^(Intercept|Constant)$",
  covariate.labels = pretty_labels,
  digits = 3,
  no.space = FALSE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  add.lines = list(
    c("Player FE", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("Date FE",   "No", "Yes", "Yes", "Yes", "Yes", "Yes")
  ),
  notes = c("SEs clustered by player.",
            "FE columns absorb player and date fixed effects where indicated.")
)


quantile(df_players$gdp_per_capita_ppp_logged, probs=seq(0, 1, 0.25), na.rm=T)

df_players <- df_players %>%
  mutate(gdp_per_capita_q4 = if_else(gdp_per_capita_ppp_logged <= 10.145531, 1, 0))

df_players <- df_players %>%
  mutate(gdp_per_capita_q3 = if_else(gdp_per_capita_ppp_logged > 10.145531 & gdp_per_capita_ppp_logged <= 10.798698, 1, 0))

df_players <- df_players %>%
  mutate(gdp_per_capita_q2 = if_else(gdp_per_capita_ppp_logged > 10.798698 & gdp_per_capita_ppp_logged <= 10.988761, 1, 0))

df_players <- df_players %>%
  mutate(gdp_per_capita_q1 = if_else(gdp_per_capita_ppp_logged > 10.988761, 1, 0))

df_players_q1 <- subset(df_players, gdp_per_capita_q1>=1)
df_players_q2 <- subset(df_players, gdp_per_capita_q2>=1)
df_players_q3 <- subset(df_players, gdp_per_capita_q3>=1)
df_players_q4 <- subset(df_players, gdp_per_capita_q4>=1)




model_q1 <- felm(player_accuracy ~ is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_q1 %>% filter(round>round_var))
model_q2 <- felm(player_accuracy ~ is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_q2 %>% filter(round>round_var))
model_q3 <- felm(player_accuracy ~ is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_q3 %>% filter(round>round_var))
model_q4 <- felm(player_accuracy ~ is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_q4 %>% filter(round>round_var))

summary(model_q1)
summary(model_q2)
summary(model_q3)
summary(model_q4)

keep_rows <- c(
  "is_white",
  "round:could_win_dynamic",
  "opponent_rating",
  "player_played_two",
  "second_tournament_per_day"
)

pretty_labels <- c(
  "White",
  "Round x Could Win",
  "Opponent Rating",
  "Played Two Games Same Day",
  "Second Tournament Per Day"
)


stargazer(
  model_q1, model_q2, model_q3, model_q4,
  type = "html",
  out = "chess_income_model_comparison.html",
  title = "Comparison of Player Accuracy Models",
  dep.var.labels = "Player Accuracy",
  column.labels = c("GDP per Capita (Q1)", "GDP per Capita (Q2)", "GDP per Capita (Q3)", "GDP per Capita (Q4)"),
  keep = keep_rows,
  order = keep_rows,
  covariate.labels = pretty_labels,
  digits = 3,
  no.space = FALSE,   # <–– allows spacing
  star.cutoffs = c(0.1, 0.05, 0.01),
  se = list(
    sqrt(diag(vcov(model_q1, cluster = ~ player_name))),
    sqrt(diag(vcov(model_q2, cluster = ~ player_name))),
    sqrt(diag(vcov(model_q3, cluster = ~ player_name))),
    sqrt(diag(vcov(model_q4, cluster = ~ player_name)))
  ),
  add.lines = list(
    c("Player FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("Date FE",   "Yes", "Yes", "Yes", "Yes", "Yes")
  ),
  notes = c(
    "SEs clustered by player.",
    "FE columns absorb player and date fixed effects where indicated."
  )
)



df_players_night <- df_players %>%
  filter(night == 1)

df_players_evening <- df_players %>%
  filter(evening == 1)

df_players_morning <- df_players %>%
  filter(morning == 1)

df_players_midday <- df_players %>%
  filter(morning == 0, evening == 0, night == 0)

countries_to_exclude <- c("USA", "Canada", "Russia")

df_players_filtered <- df_players %>%
  filter( !country_name %in% countries_to_exclude )


model_night <- felm(player_accuracy ~ player_rating + is_white + round*could_win_dynamic + opponent_rating  | player_name+date, data = df_players_night %>% filter(round>1))
model_evening <- felm(player_accuracy ~ player_rating + is_white + round*could_win_dynamic + opponent_rating  | player_name+date, data = df_players_evening %>% filter(round>1))
model_morning <- felm(player_accuracy ~ player_rating + is_white + round*could_win_dynamic + opponent_rating  | player_name+date, data = df_players_morning %>% filter(round>1))
model_midday <- felm(player_accuracy ~ player_rating + is_white + round*could_win_dynamic + opponent_rating  | player_name+date, data = df_players_midday %>% filter(round>1))


summary(model_night)
summary(model_evening)
summary(model_morning)
summary(model_midday)


keep_rows <- c(
  "player_rating",
  "is_white",
  "round:could_win_dynamic",
  "opponent_rating"
)

pretty_labels <- c(
  "Player Rating",
  "White",
  "Round x Could Win",
  "Opponent Rating")

stargazer(
  model_night, model_evening, model_morning, model_midday,
  type = "html",
  out = "chess_time_model_comparison.html",
  title = "Comparison of Player Accuracy Models",
  dep.var.labels = "Player Accuracy",
  column.labels = c("Played at Night", "Played at Evening", "Played at Morning", "Played at Midday"),
  keep = keep_rows,
  order = keep_rows,
  covariate.labels = pretty_labels,
  digits = 3,
  no.space = FALSE,   # <–– allows spacing
  star.cutoffs = c(0.1, 0.05, 0.01),
  se = list(
    sqrt(diag(vcov(model_night, cluster = ~ player_name))),
    sqrt(diag(vcov(model_evening, cluster = ~ player_name))),
    sqrt(diag(vcov(model_morning, cluster = ~ player_name))),
    sqrt(diag(vcov(model_midday, cluster = ~ player_name)))
  ),
  add.lines = list(
    c("Player FE", "Yes", "Yes", "Yes", "Yes"),
    c("Date FE",   "Yes", "Yes", "Yes", "Yes")
  ),
  notes = c(
    "SEs clustered by player.",
    "FE columns absorb player and date fixed effects where indicated."
  )
)

df_players_played_two <- df_players %>%
  filter(player_played_two=="True")

df_players_played_one <- df_players %>%
  filter(player_played_two=="False")

model10 <- feols(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_played_two %>% filter(round>1), se = "standard")
model11 <- feols(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_played_one %>% filter(round>1), se = "standard")

summary(model10)
summary(model11)

df_players_played_two_second <- df_players %>%
  filter(player_played_two=="True", second_tournament_per_day=="True")

df_players_played_two_first <- df_players %>%
  filter(player_played_two=="True", second_tournament_per_day=="False")

model12 <- feols(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_played_two_second %>% filter(round>1), se = "standard")
model13 <- feols(player_accuracy ~ player_rating + is_white + round*in_prizes + opponent_rating  | player_name+date, data = df_players_played_two_first %>% filter(round>1), se = "standard")

summary(model12)
summary(model13)


model14 <- feols(player_accuracy ~ player_rating + is_white + round*could_win_dynamic + opponent_rating + morning + evening + night + player_played_two + second_tournament_per_day | player_name+date, data = df_players %>% filter(round>1), se = "standard")

summary(model14)


all_effects <- getfe(model1)
all_effects

# Plot the distribution of player_name fixed effects
ggplot(all_effects %>% filter(fe == "player_name"), aes(x = effect)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
  labs(
    title = "Distribution of Player Fixed Effects (player_name)",
    x = "Fixed Effect Coefficient (Player-Specific Accuracy)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot the distribution of date fixed effects
ggplot(all_effects %>% filter(fe == "date"), aes(x = effect)) +
  geom_histogram(bins = 50, fill = "darkorange", alpha = 0.8) +
  labs(
    title = "Distribution of Date Fixed Effects (date)",
    x = "Fixed Effect Coefficient (Date-Specific Shock)",
    y = "Frequency"
  ) +
  theme_minimal()
