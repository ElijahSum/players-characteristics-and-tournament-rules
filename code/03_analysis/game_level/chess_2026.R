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
df_players <- read.csv('data/final_regression_data_tournaments_2024_2026.csv')


df_players <- df_players %>%
  mutate(
    date = as.Date(date),
    format_5_0 = if_else(date < as.Date("2025-09-01"), 0, 1)
  )

df_players <- df_players %>%
  filter(player_title != 'No Title')

df_players <- df_players %>%
  filter(player_accuracy != 0, player_accuracy != 100)

df_players$birthday <- as.integer(df_players$birthday)
df_players$age <- 2024 - df_players$birthday

df_players$age_under_35 <- ifelse(df_players$age <= 35, 1, 0)

df_players <- df_players %>%
  mutate(in_prizes = if_else(rank <= 6, 1, 0))

df_players <- df_players %>%
  mutate(bubble = if_else(rank <= 15 & rank >= 7, 1, 0))

df_players <- df_players %>%
  mutate(leader = if_else(rank == 1, 1, 0))



model1 <- felm(
  player_accuracy ~ player_rating + is_white + format_5_0*age + opponent_rating,
  data = df_players %>% filter(round>2)
)
summary(model1)



# analysis of the dataset before change of rules

df_players_before_sep2025 <- df_players %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2025-09-01"))

df_players_before_sep2025 <- df_players_before_sep2025 %>%
  filter(player_accuracy != 0, player_accuracy != 100)

model2 <- felm(
  player_accuracy ~ player_rating + is_white + bubble*round + opponent_rating | player_name + date,
  data = df_players_before_sep2025 %>% filter(round>2)
)
summary(model2)



#### tests
model_age_continuous <- feols(
  player_accuracy ~ 
    format_5_0:age +
    player_rating +
    opponent_rating +
    is_white +
    factor(round)
  | player_name + date,
  data = df_players,
  cluster = ~ player_name
)
summary(model_age_continuous)


###
model_rating <- feols(
  player_accuracy ~ 
    format_5_0:player_rating +
    opponent_rating +
    is_white +
    factor(round)
  | player_name + date,
  data = df_players,
  cluster = ~ player_name
)

summary(model_rating)

###
model_title <- feols(
  player_accuracy ~ 
    i(player_title, format_5_0, ref = "GM") +
    player_rating +
    opponent_rating +
    is_white +
    factor(round)
  | player_name + date,
  data = df_players,
  cluster = ~ player_name
)

summary(model_title)
iplot(model_title)


###
model_round <- feols(
  player_accuracy ~ 
    i(round, format_5_0, ref = 1) +
    player_rating +
    opponent_rating +
    is_white
  | player_name + date,
  data = df_players,
  cluster = ~ player_name
)

summary(model_round)
iplot(model_round)

###
df_players <- df_players %>%
  mutate(rating_diff = player_rating - opponent_rating)

model_result_rating <- feols(
  player_result ~ 
    format_5_0:rating_diff +
    player_rating +
    opponent_rating +
    is_white +
    factor(round)
  | player_name + date,
  data = df_players,
  cluster = ~ player_name
)

summary(model_result_rating)

###
library(lubridate)
df_players <- df_players %>%
  mutate(
    event_month = interval(as.Date("2025-09-01"), date) %/% months(1)
  )

df_event <- df_players %>%
  filter(event_month >= -8, event_month <= 8)

###


# graphics






