# What am I doing?
# Playing with nflfastR data
# Why am I doing it?
# Dont know, probably for fantasy football

# What question has been bothering me recently?
# The idea of play sequencing

# Why?
# I want to relate it to at bat outcomes in baseball in pitcher counts vs in batter counts

# Background
# Pitcher counts and batter counts are relatively easy to understand in baseball
# Typically 3-1 and 2-0 are considered batter counts (and occasionally 3-0)
# while any count with 2 strikes (excluding 3-2) are considered pitcher counts

# Batter: 2-0, 3-0, 3-1
# Pitcher: 0-2, 1-2, 2-2

# Question in football: What is an offense's count?
# What is a defense's count?

library(tidyverse)
library(tidymodels)
library(nflfastR)

# define which seasons shall be loaded
  # Load 2006 and onwards to get the more detailed pass info
seasons <- 2006:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp %>%
  select(contains("penalty"))

pbp %>%
  group_by(game_id, fixed_drive) %>%
  summarize(n_plays = n(),
            drive_n_plays = first(drive_play_count))

current_rel_cols <- pbp %>%
  filter(play_type %in% c("run", "pass")) %>%
  filter(!is.na(down)) %>% # Two point conversions
  select(game_id, old_game_id, play_id, sp, home_team, away_team, posteam, posteam_type, desc,
         yardline_100, half_seconds_remaining, game_seconds_remaining, drive, drive_play_count,
         fixed_drive, qtr, down, ydstogo, goal_to_go, play_type, posteam_timeouts_remaining,
         score_differential, wp, vegas_wp, fourth_down_converted, fourth_down_failed,
         fumble_lost, interception, incomplete_pass, complete_pass,
         pass_location, pass_length, air_yards, yards_after_catch,
         rush_attempt, pass_attempt,
         penalty, first_down_penalty, penalty_team, penalty_yards,
         roof, wind)

# Create set of downs
  # I think I group by game_id and run an incrementer for when the next down is 1
  # There is more logic necessary
  # It gets a little easieir by filtering out non- run/pass plays
with_downs <- current_rel_cols %>%
  group_by(game_id) %>%
  mutate(down_1_tracker = if_else(down == 1, 1, 0)) %>%
  mutate(down_set = cumsum(down_1_tracker))

# If this doesnt quite work I can always come back and nest to force data-scoping
# Note that by going all the way back to 2000 we dont have YAC separated from
# the yards gained on a passing play (air yards + yac)
# As such we cant partition easily into short, medium, long

# Now I have short and deep as options and left/middle/right
# Additionally have air yards and yards after catch

# Some preliminary exploratory things
# Over all years, what is the success rate of all sequences, regardless of down/distance
with_downs %>%
  ungroup() %>%
  select(game_id, posteam, sp, down_set, rush_attempt, pass_attempt, down, down_set) %>%
  filter(game_id == first(game_id)) %>%
  View()

# Use scoring play to help partition last plays
with_downs_nested <- with_downs %>%
  ungroup() %>%
  select(game_id, posteam, sp, down_set, rush_attempt, pass_attempt, down, down_set) %>%
  group_by(game_id, down_set) %>%
  mutate(sp_result = sum(sp)) %>%
  ungroup() %>%
  select(-sp) %>%
  nest(data = c(rush_attempt, pass_attempt, down)) %>%
  mutate(success = case_when(posteam == lead(posteam) ~ 1,
                             (posteam != lead(posteam)) & (sp_result == 1) ~ 1,
                             TRUE ~ 0)) 

# data holds tibble(rush_att, pass_att, down)
# Need to pull a sequence from this
sequence_generator <- function(attempts_tbl){
  attempts_tbl %>%
    arrange(down) %>%
    mutate(run_pass = if_else(rush_attempt == 1, "r", "p")) %>%
    pull(run_pass) %>%
    paste(collapse="")
}

sequenced_data <- with_downs_nested %>%
  mutate(play_seq = map_chr(data, sequence_generator)) %>%
  select(-data)

# Super simple box plot to start: Consider only the 3 term play sets
# Shorter sets are by definition more successful
sequenced_data %>%
  filter(nchar(play_seq) == 3) %>%
  group_by(play_seq) %>%
  summarize(prop_success = mean(success),
            n_seq = n()) %>%
  mutate(rush_start = substr(play_seq, 1, 1) == "r") %>%
  ggplot(aes(x = play_seq, y = prop_success, col = rush_start, size = n_seq)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# Think about how to improve sequence separation
# This seems unintuitive
# Perhaps by year?
sequenced_data %>%
  filter(nchar(play_seq) == 3) %>%
  mutate(year = substr(game_id, 1, 4)) %>%
  group_by(play_seq, year) %>%
  summarize(prop_success = mean(success),
            n_seq = n()) %>%
  mutate(rush_start = substr(play_seq, 1, 1) == "r") %>%
  ggplot(aes(x = play_seq, y = prop_success, col = rush_start, size = n_seq)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~year)

# Next thing would be to try and understand this from a pass-short, pass-long setup
# Continue tomorrow