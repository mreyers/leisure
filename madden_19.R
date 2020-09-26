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

curr_receiving_19 %>%
  left_join(receivers_19, by = c("receiver_player_id" = "nflfastR_id")) %>%
  filter(rec_pos == "WR") %>%
  mutate(starter = depth_chart_order == 1,
         percentile = ovr_rating %/% 10 * 10,
         percentile = factor(percentile)) %>%
  ggplot(aes(x = tot_fant_pts, col = percentile, fill = percentile)) +
  geom_density() +
  facet_wrap(~ starter)

# So these are mixture distributions, even within percentiles
# I think these differences come from Injured / Not Injured
# I dont have injury data here so I would have to do some sort of mixture distribution