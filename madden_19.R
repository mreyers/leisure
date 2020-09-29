# Use this to quickly figure out how predictive Madden Ratings are for a given year
# Only have 2019 data to learn from
# Want to use this to set priors
library(tidyverse)

# Mappings are pretty much the same except for the 2020 rookies, which isnt a big deal
mappings <- readRDS("all_relevant.rds") %>%
  filter(years_exp > 0)
madden_last_year <- read_csv("M20 Launch Ratings.csv")

# Dont need team mapper, they used Abbreviations
  # Column name fix is disgusting, find a tidyR way
madden_ratings_updated_last_year <- madden_last_year %>%
  select(Name, Team, `jersey num`, contains("rating")) 

temp_names <- names(madden_ratings_updated_last_year)
temp_snake <- snakecase::to_snake_case(temp_names)
names(madden_ratings_updated_last_year) <- temp_snake

# Join with other data
mappings_with_last_year <- mappings %>%
  left_join(madden_ratings_updated_last_year %>% select(-jersey_num), by = c("player_name" = "name",
                                                                  "team"))

# Check for missing madden info
missing_frame_last_year <- mappings_with_last_year %>%
  filter(is.na(ovr_rating)) %>%
  select(player_name, primary_position, jersey_number, team, contains("id")) %>%
  select(-throw_accuracy_mid_rating)

# These players might be able to be fixed by just using jersey_number and team
missing_madden_last_year <- missing_frame_last_year %>%
  left_join(madden_ratings_updated_last_year %>% select(-name),
            by = c("team",
                   "jersey_number" = "jersey_num"))
# Much better
all_madden_last_year <- mappings_with_last_year %>%
  filter(!is.na(ovr_rating)) %>%
  bind_rows(missing_madden_last_year %>% filter(!is.na(ovr_rating)))

pbp_2019 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds")
)

# Calculate the number of targets, receptions, rushes, rush attempts, EPA per type
# for all players in the data
# Then map it back to the IDs and depth chart to see who has a chance of moving up
smaller_pbp_19 <- pbp_2019 %>%
  select(play_id, game_id, home_team, away_team, week, 
         yardline_100, game_seconds_remaining, game_half, qtr,
         down, goal_to_go, ydstogo, desc, play_type, yards_gained,
         total_home_score, total_away_score, complete_pass, ep, epa, wp, wpa,
         air_yards, air_epa,
         yards_after_catch, yac_epa, comp_air_epa, comp_yac_epa,
         fumble_lost, interception, two_point_conv_result, touchdown, pass_touchdown,
         receiver_player_id, rusher_player_id, passer_player_id,
         receiver_player_name, rusher_player_name, passer_player_name) %>%
  mutate(two_point_conv_result = if_else(two_point_conv_result %in% "success", 1, 0))

# This currently uses targets, not completions. Fix
pass_2019 <- smaller_pbp_19 %>%
  filter(play_type %in% "pass")
rush_2019 <- smaller_pbp_19 %>%
  filter(play_type %in% "run")
st_and_penalties <- smaller_pbp_19 %>%
  filter(!(play_type %in% c("pass", "run")))

# Neat
receiving_19 <- pass_2019 %>%
  select(game_id, play_id, week, yardline_100, down, wp, receiver_player_id, receiver_player_name,
         yards_gained, touchdown,
         epa, complete_pass, air_yards, air_epa, yards_after_catch, yac_epa) %>%
  mutate(fant_pts = 0.1 * yards_gained + 0.5 * complete_pass + 6 * touchdown) %>%
  filter(!is.na(receiver_player_id)) %>%
  group_by(receiver_player_id, week) %>%
  summarize(receiver_player_name = first(receiver_player_name),
            tot_targets = n(),
            tot_completions = sum(complete_pass, na.rm = TRUE),
            tot_epa = sum(epa, na.rm = TRUE),
            tot_air_epa = sum(air_epa, na.rm = TRUE),
            tot_air_yards = sum(air_yards, na.rm = TRUE),
            tot_yac_epa = sum(yac_epa, na.rm = TRUE),
            tot_yac = sum(yards_after_catch, na.rm = TRUE),
            tot_yards = sum(yards_gained, na.rm = TRUE),
            tot_fant_pts = sum(fant_pts, na.rm = TRUE)) %>%
  arrange(desc(tot_epa))

receivers_19 <- all_madden_last_year %>%
  filter(primary_position %in% c("WR", "TE", "RB", "FB", "HB")) %>%
  select(rec_pos = position, nflfastR_id, depth_chart_order, ovr_rating)

receiving_19 %>%
  left_join(receivers_19, by = c("receiver_player_id" = "nflfastR_id")) %>%
  mutate(starter = depth_chart_order == 1) %>%
  filter(rec_pos == "WR", !is.na(starter)) %>%
  ggplot(aes(x = tot_targets, y = tot_epa, col = starter, shape = starter)) +
  geom_point() +
  geom_smooth()

# Same idea, but summarize on whole season and then get some densities
curr_receiving_19 <- receiving_19 %>%
  select(-c(receiver_player_name, week)) %>%
  group_by(receiver_player_id) %>%
  summarize_all(sum)

receiving_plot_data <- curr_receiving_19 %>%
  left_join(receivers_19, by = c("receiver_player_id" = "nflfastR_id")) %>%
  filter(rec_pos == "WR") %>%
  mutate(starter = depth_chart_order == 1)

lm_check <- lm(tot_fant_pts ~ ovr_rating, data = receiving_plot_data)
summary(lm_check)

receiving_plot_data %>%
  ggplot(aes(x = tot_fant_pts, y = ovr_rating, col = ovr_rating, shape = starter)) +
  geom_point() +
  geom_smooth(method = "lm") 

receiving_plot_data %>%
  mutate(percentile = ovr_rating %/% 10 * 10,
         percentile = factor(percentile)) %>%
  ggplot(aes(x = tot_fant_pts, col = percentile, fill = percentile)) +
  geom_density() +
  facet_wrap(~ starter)

# So these are mixture distributions, even within percentiles
# I think these differences come from Injured / Not Injured
# I dont have injury data here so I would have to do some sort of mixture distribution

# How could I manage this?
# Perhaps it is informed by the previous season of data?
pbp_2018 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds")
  ) %>%
  select(play_id, game_id, home_team, away_team, week, 
         yardline_100, game_seconds_remaining, game_half, qtr,
         down, goal_to_go, ydstogo, desc, play_type, yards_gained,
         total_home_score, total_away_score, complete_pass, ep, epa, wp, wpa,
         air_yards, air_epa,
         yards_after_catch, yac_epa, comp_air_epa, comp_yac_epa,
         fumble_lost, interception, two_point_conv_result, touchdown, pass_touchdown,
         receiver_player_id, rusher_player_id, passer_player_id,
         receiver_player_name, rusher_player_name, passer_player_name) %>%
  mutate(two_point_conv_result = if_else(two_point_conv_result %in% "success", 1, 0))

pass_2018 <- pbp_2018 %>%
  filter(play_type %in% "pass")
rush_2018 <- pbp_2018 %>%
  filter(play_type %in% "run")
st_and_penalties_18 <- pbp_2018 %>%
  filter(!(play_type %in% c("pass", "run")))

# 2018 receiving stuff, lets see if we can figure out why they got their madden grades
receiving_18 <- pass_2018 %>%
  select(game_id, play_id, week, yardline_100, down, wp, receiver_player_id, receiver_player_name,
         yards_gained, touchdown,
         epa, complete_pass, air_yards, air_epa, yards_after_catch, yac_epa) %>%
  mutate(fant_pts = 0.1 * yards_gained + 0.5 * complete_pass + 6 * touchdown) %>%
  filter(!is.na(receiver_player_id)) %>%
  group_by(receiver_player_id, week) %>%
  summarize(receiver_player_name = first(receiver_player_name),
            tot_targets = n(),
            tot_completions = sum(complete_pass, na.rm = TRUE),
            tot_epa = sum(epa, na.rm = TRUE),
            tot_air_epa = sum(air_epa, na.rm = TRUE),
            tot_air_yards = sum(air_yards, na.rm = TRUE),
            tot_yac_epa = sum(yac_epa, na.rm = TRUE),
            tot_yac = sum(yards_after_catch, na.rm = TRUE),
            tot_yards = sum(yards_gained, na.rm = TRUE),
            tot_fant_pts = sum(fant_pts, na.rm = TRUE)) %>%
  arrange(desc(tot_epa))

full_send <- receiving_18 %>%
  mutate(season = 2018) %>%
  left_join(receivers_19, by = c("receiver_player_id" = "nflfastR_id")) %>%
  bind_rows(receiving_19 %>%
              left_join(receivers_19, by = c("receiver_player_id" = "nflfastR_id")) %>%
              mutate(season = 2019))

two_seasons <- full_send %>%
  group_by(receiver_player_id, season) %>%
  summarize(name = first(receiver_player_name),
            pos = first(rec_pos),
            madden_rating = first(ovr_rating),
            tot_targets_szn = sum(tot_targets),
            sd_targets = sd(tot_targets),
            tot_epa_szn = sum(tot_epa),
            sd_epa = sd(tot_epa),
            tot_air_yards_szn = sum(tot_air_yards),
            sd_air_yards = sd(tot_air_yards),
            tot_fant_pts_szn = sum(tot_fant_pts),
            sd_fant_pts = sd(tot_fant_pts))

two_seasons %>%
  filter( pos %in% "WR") %>%
  ggplot(aes(x = tot_fant_pts_szn, y = madden_rating, col = madden_rating)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ season)

# These must be some 0.7 R^2 type fits, after removing the one outlier
lm_2018 <- lm(madden_rating ~ tot_fant_pts_szn, data = two_seasons %>%
                filter(season == 2018, madden_rating > 60, pos %in% "WR")) # 0.8646
lm_2019 <- lm(madden_rating ~ tot_fant_pts_szn, data = two_seasons %>%
                filter(season == 2019, madden_rating > 60, pos %in% "WR")) # 0.5653

# So the only difference seems to be, again, the players that got hurt
# It does seem that for at least the first few games I could easily build posteriors
# that are heavily upweighted by the priors, where the priors are centred about
# the madden rating in some way. A starting point may just be to use the lm
# coef on tot_fant_pts / 16
per_game <- two_seasons %>%
  filter(season == 2018, madden_rating > 60, pos %in% "WR") %>%
  mutate(per_16_fant = tot_fant_pts_szn / 16)

prior_centres <- lm(per_16_fant ~ madden_rating, data = per_game)
# So madden_rating is related to per_game fantasy points at a clip of
# -35.5 + 0.543 * rating

# Now to simply build prior centres for each player then
real_prior_centres <- predict(prior_centres, per_game)

per_game_2018_centres <- per_game %>%
  ungroup() %>%
  mutate(centres = real_prior_centres)

# Now that I have the centres, can I build a Bayesian model to update off this as games go by?
# To be determined