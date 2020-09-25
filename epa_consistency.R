# Now to try and look at a few ideas, given that the id linkage is handled
# No need to bring the linkage in yet, that was just for others to use

library(tidyverse)
library(nflfastR)

# First opportunity to analyze fantasy opportunity would actually be to
# load in the linkage as it would require depth chart position
mappings <- readRDS("all_relevant.rds")

# 2020 pbp
pbp_2020 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
)

# Calculate the number of targets, receptions, rushes, rush attempts, EPA per type
# for all players in the data
# Then map it back to the IDs and depth chart to see who has a chance of moving up
smaller_pbp <- pbp_2020 %>%
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
pass_2020 <- smaller_pbp %>%
  filter(play_type %in% "pass")
rush_2020 <- smaller_pbp %>%
  filter(play_type %in% "run")
st_and_penalties <- smaller_pbp %>%
  filter(!(play_type %in% c("pass", "run")))

# Neat
receiving <- pass_2020 %>%
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

# Now join on id
receivers <- mappings %>%
  filter(primary_position %in% c("WR", "TE", "RB")) %>%
  select(rec_pos = position, nflfastR_id, depth_chart_order)

receiving %>%
  left_join(receivers, by = c("receiver_player_id" = "nflfastR_id")) %>%
  mutate(starter = depth_chart_order == 1) %>%
  filter(rec_pos == "WR", !is.na(starter)) %>%
  ggplot(aes(x = tot_targets, y = tot_epa, col = starter, shape = starter)) +
  geom_point() +
  geom_smooth()

receiving %>%
  left_join(receivers, by = c("receiver_player_id" = "nflfastR_id")) %>%
  mutate(starter = depth_chart_order == 1) %>%
  filter(rec_pos == "WR", !is.na(starter)) %>%
  ggplot(aes(x = tot_targets, y = tot_air_epa, col = starter, shape = starter)) +
  geom_point() +
  geom_smooth()

receiving %>%
  left_join(receivers, by = c("receiver_player_id" = "nflfastR_id")) %>%
  mutate(starter = depth_chart_order == 1) %>%
  filter(rec_pos == "WR", !is.na(starter)) %>%
  ggplot(aes(x = tot_targets, y = tot_yac_epa, col = starter, shape = starter)) +
  geom_point() +
  geom_smooth()

receiving %>%
  left_join(receivers, by = c("receiver_player_id" = "nflfastR_id")) %>%
  mutate(starter = depth_chart_order == 1) %>%
  filter(rec_pos == "WR", !is.na(starter)) %>%
  group_by(tot_targets) %>%
  arrange(desc(tot_epa)) %>%
  slice(1:6) %>%
  filter(tot_targets >= 4) %>%
  View()

# I wonder how informative Madden Ratings are
madden_ratings <- read_csv("M21 Launch Ratings - Sheet1.csv")

# Need to redo team mappings, incompatible (team name vs team abbr)
team_mapper <- read_csv("nfl_teams.csv") %>%
  mutate(reduced_name = str_extract(Name, "[0-9A-z]+$")) %>%
  select(Abbreviation, reduced_name)

madden_ratings_updated <- madden_ratings %>%
  left_join(team_mapper, by = c("team" = "reduced_name")) %>%
  mutate(Abbreviation = if_else(is.na(Abbreviation), "WAS", Abbreviation)) %>%
  select(Name, Abbreviation, jerseyNum, contains("rating"))

# Join with other data
mappings_with_madden <- mappings %>%
  left_join(madden_ratings_updated %>% select(-jerseyNum), by = c("player_name" = "Name",
                                           "team" = "Abbreviation"))

# Check for missing madden info
missing_frame <- mappings_with_madden %>%
  filter(is.na(overall_rating)) %>%
  select(player_name, primary_position, jersey_number, team, contains("id")) %>%
  select(-throwAccuracyMid_rating)

# These players might be able to be fixed by just using jersey_number and team
missing_madden <- missing_frame %>%
  left_join(madden_ratings_updated %>% select(-Name),
            by = c("team" = "Abbreviation",
                   "jersey_number" = "jerseyNum"))
# Much better
all_madden <- mappings_with_madden %>%
  filter(!is.na(overall_rating)) %>%
  bind_rows(missing_madden %>% filter(!is.na(overall_rating)))
  
# Now to join eligible receivers with some of their Madden evals
curr_receiving <- receiving %>%
  select(-receiver_player_name) %>%
  group_by(receiver_player_id) %>%
  summarize_all(sum)

# Players with NA Name, Abbreviation, and jerseyNum were fixed by the missing_madden update
all_madden %>%
  left_join(curr_receiving, by = c("nflfastR_id" = "receiver_player_id")) %>%
  left_join(receivers %>% select(-depth_chart_order), by = c("nflfastR_id")) %>%
  filter(primary_position %in% c("WR", "TE", "RB", "FB", "HB"),
         !is.na(tot_fant_pts)) %>%
  ggplot(aes(x = overall_rating, y = tot_fant_pts, col = (depth_chart_order == 1)  )) +
  geom_point()
