# I want to look at Receivers and Running backs
# load all the libraries
# install.packages("package") and then
# library("package") to simplify how to install a package and use it for your library
library(tidyverse)
library(ggrepel)
library(ggimage)
library(gt)
library(ggpubr)
library(nflfastR)
nflreadr::.clear_cache()
# to start, we can load receiver and running back data for 2017-2021
# it's structured the same way to evaluate quarterbacks just using receiver_player_name instead
# receivers includes tight ends
# i also want to use the filter() function to only select some of the top receivers
re_21 <- load_pbp(2021) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
re_20 <- load_pbp(2020) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
re_19 <- load_pbp(2019) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
re_18 <- load_pbp(2018) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
re_17 <- load_pbp(2017) %>%
  group_by(receiver_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(passes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(passes > 100) %>%
  arrange(-avg_epa)
ru_21 <- load_pbp(2021) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(rushes > 100) %>%
  arrange(-avg_epa)
ru_20 <- load_pbp(2020) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(rushes > 100) %>%
  arrange(-avg_epa)
ru_19 <- load_pbp(2019) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(rushes > 100) %>%
  arrange(-avg_epa)
ru_18 <- load_pbp(2018) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(rushes > 100) %>%
  arrange(-avg_epa)
ru_17 <- load_pbp(2017) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            avg_epa = mean(epa),
            team_abbr = last(posteam)) %>%
  filter(rushes > 100) %>%
  arrange(-avg_epa)
# now we have dataframes for rushing and receiving data we can add in excel sheets for standings data
# the sheets have leading passer, however my goal is not to isolate teams by leading receiver or rusher
# i was having trouble downloading the sheets so i'm uncertain on how well they would work for anyone else
library(readxl)
standings_17 <- read_excel("2017 NFL Standings copy.xlsx")
standings_18 <- read_excel("2018 NFL Standings copy.xlsx")
standings_19 <- read_excel("2019 NFL Standings copy.xlsx")
standings_20 <- read_excel("2020 NFL Standings copy.xlsx")
standings_21 <- read_excel("2021 NFL Standings copy.xlsx")
# now that we have receiving, rushing and standings data we can join them together
re_21 <- re_21 %>%
  inner_join(standings_21, by = c(team_abbr = "Team Abbreviation"))
re_20 <- re_20 %>%
  inner_join(standings_20, by = c(team_abbr = "Team Abbreviation"))
re_19 <- re_19 %>%
  inner_join(standings_19, by = c(team_abbr = "Team Abbreviation"))
re_18 <- re_18 %>%
  inner_join(standings_18, by = c(team_abbr = "Team Abbreviation"))
re_17 <- re_17 %>%
  inner_join(standings_17, by = c(team_abbr = "Team Abbreviation"))
ru_21 <- ru_21 %>%
  inner_join(standings_21, by = c(team_abbr = "Team Abbreviation"))
ru_20 <- ru_20 %>%
  inner_join(standings_20, by = c(team_abbr = "Team Abbreviation"))
ru_19 <- ru_19 %>%
  inner_join(standings_19, by = c(team_abbr = "Team Abbreviation"))
ru_18 <- ru_18 %>%
  inner_join(standings_18, by = c(team_abbr = "Team Abbreviation"))
ru_17 <- ru_17 %>%
  inner_join(standings_17, by = c(team_abbr = "Team Abbreviation"))
# now that the rushing and receiving data is joined, we can join the data additionally to plot the data with team logos!
# left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
re_21 <- re_21 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
re_20 <- re_20 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
re_19 <- re_19 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
re_18 <- re_18 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
re_17 <- re_17 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
ru_21 <- ru_21 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
ru_20 <- ru_20 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
ru_19 <- ru_19 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
ru_18 <- ru_18 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
ru_17 <- ru_17 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
# now that we have joined all our data we can plot it
re_21 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Receiver EPA",
    y = "Team Win Percentage",
    title = "Receiver EPA and Team Win Percentage 2021",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
re_20 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Receiver EPA",
    y = "Team Win Percentage",
    title = "Receiver EPA and Team Win Percentage 2020",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))  
re_19 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Receiver EPA",
    y = "Team Win Percentage",
    title = "Receiver EPA and Team Win Percentage 2019",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
re_18 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Receiver EPA",
    y = "Team Win Percentage",
    title = "Receiver EPA and Team Win Percentage 2018",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
re_17 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Receiver EPA",
    y = "Team Win Percentage",
    title = "Receiver EPA and Team Win Percentage 2017",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# something really important of note:
# while higher rushers may have a higher correlation, a lot of the rush epa data is negative
# which is not good
# however, higher rushers can still be an advantage
ru_21 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Rusher EPA",
    y = "Team Win Percentage",
    title = "Rusher EPA and Team Win Percentage 2021",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ru_20 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Rusher EPA",
    y = "Team Win Percentage",
    title = "Rusher EPA and Team Win Percentage 2020",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ru_19 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Rusher EPA",
    y = "Team Win Percentage",
    title = "Rusher EPA and Team Win Percentage 2019",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ru_18 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Rusher EPA",
    y = "Team Win Percentage",
    title = "Rusher EPA and Team Win Percentage 2018",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
ru_17 %>%
  ggplot(aes(x=avg_epa, y=`Win Percentage`)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_smooth(se = FALSE, color = 'black', method = 'lm') +
  labs (
    x = "Rusher EPA",
    y = "Team Win Percentage",
    title = "Rusher EPA and Team Win Percentage 2017",
    caption = "Jarrett Markman | Syracuse University"
  ) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# now that the data is plotted, we can find the correlation coefficients for each year between win percentage and position epa
re_cor_21 <- cor(re_21$avg_epa, re_21$`Win Percentage`, use = "complete.obs")
re_cor_20 <- cor(re_20$avg_epa, re_20$`Win Percentage`, use = "complete.obs")
re_cor_19 <- cor(re_19$avg_epa, re_19$`Win Percentage`, use = "complete.obs")
re_cor_18 <- cor(re_18$avg_epa, re_18$`Win Percentage`, use = "complete.obs")
re_cor_17 <- cor(re_17$avg_epa, re_17$`Win Percentage`, use = "complete.obs")
ru_cor_21 <- cor(ru_21$avg_epa, ru_21$`Win Percentage`, use = "complete.obs")
ru_cor_20 <- cor(ru_20$avg_epa, ru_20$`Win Percentage`, use = "complete.obs")
ru_cor_19 <- cor(ru_19$avg_epa, ru_19$`Win Percentage`, use = "complete.obs")
ru_cor_18 <- cor(ru_18$avg_epa, ru_18$`Win Percentage`, use = "complete.obs")
ru_cor_17 <- cor(ru_17$avg_epa, ru_17$`Win Percentage`, use = "complete.obs")
avg_re_cor <- ((re_cor_17+ re_cor_18 + re_cor_19 + re_cor_20 + re_cor_21)/5)
avg_ru_cor <- ((ru_cor_17 + ru_cor_18 + ru_cor_19 + ru_cor_20 + ru_cor_21)/5)
# both have relatively small correlations of ~ .3,
# both of which are much lower than Quarterbacks and Defense to Win Percentage
