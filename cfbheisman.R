library(cfbfastR)
library(dplyr)
library(tidyverse)
library(gt)
library(webshot2)

Sys.setenv(CFBD_API_KEY = "qijnf0hISYc7A263Wk69ovHCrhd9hW9kxg8gmRFYcLwnBlVN608QwTHq5CykX386")

og_data <- load_cfb_pbp(2014:2023)
og_schedules <- load_cfb_schedules(2014:2023)

colnames(og_data)
confs <- og_schedules %>%
  distinct(season, home_team, home_conference)

og_data <- og_data %>% filter(year != 2015 & year != 2020)


pass_data <- og_data %>%
  filter(pass == 1, !is.na(passer_player_name)) %>%
  group_by(year, player = passer_player_name, team = pos_team) %>%
  summarize(pass_plays = n(), pass_yards = sum(yards_gained), pass_att = sum(pass_attempt), pass_ypa = pass_yards/pass_att, pass_comp = sum(completion), pass_comppct = pass_comp/pass_att, pass_td = sum(touchdown), pass_int = sum(int), pass_epa = sum(EPA, na.rm = TRUE), pass_epa_play = pass_epa/pass_plays) %>%
  filter(player != "- Team" & player != "TEAM", pass_plays >= 300)

rush_data <- og_data %>%
  filter(rush == 1, !is.na(rusher_player_name)) %>%
  group_by(year, player = rusher_player_name, team = pos_team) %>%
  summarize(rush_plays = n(), rush_yards = sum(yards_gained), rush_ypa = rush_yards/rush_plays, rush_td = sum(touchdown), rush_epa = sum(EPA, na.rm = TRUE), rush_epa_play = rush_epa/rush_plays) %>%
  filter(player != "- Team" & player != "TEAM")

data <- left_join(pass_data, rush_data, by = c("year", "player", "team"))

power_five <- confs %>%
  filter(home_conference == "Big 12" | home_conference == "Big Ten" | home_conference == "SEC" | home_conference == "ACC" | home_conference == "Pac-12")

filt_data <-inner_join(data, power_five, by = c("year"="season", "team"="home_team"))

wins <- og_schedules %>%
  mutate(winner_id = ifelse(home_points > away_points, home_id, ifelse(away_points > home_points, away_id, "NONE"))) %>%
  mutate(winner_name = ifelse(home_points > away_points, home_team, ifelse(away_points > home_points, away_team, "NONE"))) %>%
  filter(season_type == "regular") %>%
  group_by(season, winner_id) %>%
  summarize(winner_name, wins = n()) %>%
  distinct(season, winner_id, .keep_all = TRUE) %>%
  filter(!is.na(winner_id))

data_comp <- left_join(filt_data, wins, by = c("year"="season", "team"="winner_name")) %>%
  mutate(wins = ifelse(is.na(wins), 0, wins))

final_data <- data_comp %>%
  mutate(heisman = 0)

final_data$heisman[which(final_data$year == 2014 & final_data$player == "Marcus Mariota")] <- 1
final_data$heisman[which(final_data$year == 2016 & final_data$player == "Lamar Jackson")] <- 1
final_data$heisman[which(final_data$year == 2017 & final_data$player == "Baker Mayfield")] <- 1
final_data$heisman[which(final_data$year == 2018 & final_data$player == "Kyler Murray")] <- 1
final_data$heisman[which(final_data$year == 2019 & final_data$player == "Joe Burrow")] <- 1
final_data$heisman[which(final_data$year == 2021 & final_data$player == "Bryce Young")] <- 1
final_data$heisman[which(final_data$year == 2022 & final_data$player == "Caleb Williams")] <- 1
final_data$heisman[which(final_data$year == 2023 & final_data$player == "Jayden Daniels")] <- 1

final_data$home_conference <- as.factor(final_data$home_conference)

final_data_train <- final_data %>%
  filter(year != 2023)

write_csv(final_data_train, "final_data_train.csv")

final_data_test <- final_data %>%
  filter(year == 2023)

heisman_model <- glm(heisman ~ pass_epa_play + rush_epa_play + wins + as.factor(home_conference), data = final_data_train, family = "binomial")

final_data_train <- final_data_train %>%
  ungroup() %>%
  mutate(prediction = predict(heisman_model, final_data_train, type = "response")) %>%
  group_by(year) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(heisman == 1, "WON", "")) %>%
  ungroup() 

final_data_test <- final_data_test %>%
  ungroup() %>%
  mutate(prediction = predict(heisman_model, final_data_test, type = "response")) %>%
  group_by(year) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(heisman == 1, "WON", "")) %>%
  ungroup() 

final_data_top15_train <- final_data_train %>%
  group_by(year) %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(year, player, team, conf = home_conference, award_prob, award_won) %>%
  ungroup()

final_data_top15_2023 <- final_data_test %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(player, team, conf = home_conference, award_prob, award_won)

subfolder_path <- "heisman/"
dir.create(subfolder_path, showWarnings = FALSE)

szns <- unique(final_data_top15_train$year)

for (szn in szns) {
  per_year <- final_data_top15_train %>%
    filter(year == szn)
  table <- per_year %>% gt() %>% 
    cols_align(
      align = "center",
      columns = c(year, player, team, conf, award_prob, award_won)
    ) %>%
    data_color(
      columns = award_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    cols_label(
      year = md("**Year**"),
      player = md("**Player**"),
      team = md("**Team**"),
      conf = md("**Conference**"),
      award_prob = md("**Heisman Probability**"),
      award_won = md("**Heisman Result**")
    ) 
  filename <- paste0(szn, "heisman.png")
  gtsave(table, file.path(subfolder_path, filename))
}

table_2023 <- final_data_top15_2023 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, conf, award_prob, award_won)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    conf = md("**Conference**"),
    award_prob = md("**Heisman Probability**"),
    award_won = md("**Heisman Result**")
  ) %>%
  tab_header(
    title = md("**2023 QB Power 5 Heisman Probability**"),
    subtitle = "Based on Heisman CFB Data from 2014 - 2022 (Excluding Non-QB Winners)"
  )
gtsave(table_2023, "heisman/2023heisman.png")