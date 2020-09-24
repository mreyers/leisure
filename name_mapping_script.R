# Trying to load the new roster data
# Lets run a test file in first
library(tidyverse)
library(nflfastR)
library(httr)

one_game <- read_csv("nfl-rosters-master/2020/49ers-at-jets-2020-reg-2.csv")
one_game %>% View()

# Cool, this has 3 different ID (id, gsisId, esbId). Hopefully one pairs with sleeper
# At the very least the full names (firstName, lastName) and jersey number will be
# helpful in pairing

# Actually lets just try to do the record linkage now
base_url <- "https://api.sleeper.app/v1/league/604138427104952320/"
rosters_url <- paste0(base_url, "rosters")

rost_get <- GET(rosters_url)
rost_content <- content(rost_get)

extract_rosters <- function(lst){
  user_id <- lst$owner_id
  players <- lst$players
  plays <- rbind(players)
  starters <- lst$starters
  starts <- rbind(starters)
  
  return(tibble(user = user_id, starters = list(starters), players = list(players)))
}

rosters <- lapply(rost_content, extract_rosters)
rosters_df <- bind_rows(rosters) 

# Make a quick function that modifies players to be a df in which col1 is player, col2 is starter/not
fix_fn <- function(start, play){
  starters <- do.call(rbind.data.frame, start)
  names(starters) <- "starters"
  
  players <- do.call(rbind.data.frame, play)
  names(players) <- "players"
  
  players <- players %>%
    mutate(is_starter = players %in% starters$starters)
  
  return(players)
}

# Roster info with who is a starter
updated_rosters <- rosters_df %>%
  mutate(fix = map2(starters, players, fix_fn)) %>%
  select(-c(starters, players)) %>%
  unnest(fix)

# Now query the players
all_players_url <- "https://api.sleeper.app/v1/players/nfl"
all_players <- GET(all_players_url)
all_players_cont <- content(all_players)

# I can probably convert this to tibble
players_tibble <- tibble(data = all_players_cont) %>%
  unnest_wider(data) %>%
  unnest_wider(fantasy_positions)

# Rename positions
players_tibble <- players_tibble %>%
  rename("primary_position" = `...1`,
         "secondary_position" = `...2`,
         "tertiary_position" = `...3`)

# Rookie setup
rookies <- players_tibble %>%
  filter(years_exp == 0) 

# Modify a bit for full name, position, jersey number join
rookies_adj <- rookies %>%
  select(player_name = full_name, position, primary_position, jersey_number = number, team,
         contains("id"),
         contains("depth_chart")) %>%
  filter(!is.na(jersey_number), jersey_number > 0)

# From this, combine with the full roster info
one_game_joined <- one_game %>%
  mutate(player_name = paste0(firstName, " ", lastName),
         jerseyNumber = as.numeric(jerseyNumber)) %>%
  rename(jersey_number = jerseyNumber) %>%
  left_join(rookies_adj, by = c("player_name", "position", "jersey_number"))

# Hey this works wayyy better
# Will work even better when sleeper updates gsis_id in their platform for 2020
# until then, we do soft matching on these 3 identifiers.

# How to establish the whole data set
  # Load each game into environment
  # Join each game to the vets and to the rookies separately
  # Bind
vets_tibble <- players_tibble %>%
  filter(years_exp > 0) %>%
  select(player_name = full_name, position, primary_position, jersey_number = number, team,
         contains("id"),
         contains("depth_chart"))
  
test_proc <- read_csv("nfl-rosters-master/2020/49ers-at-jets-2020-reg-2.csv") %>%
  mutate(player_name = paste0(firstName, " ", lastName),
         jerseyNumber = as.numeric(jerseyNumber)) %>%
  rename(jersey_number = jerseyNumber, gsis_id = gsisId) %>%
  select(player_name, position, jersey_number, contains("id"))

vets_game <- vets_tibble %>%
  filter(!is.na(gsis_id)) %>%
  left_join(test_proc, by = c("gsis_id"))

# Style might make more sense with more games
# Lets create a full set of all players so far
files <- list.files(path = "nfl-rosters-master/2020/",
                    pattern = "*.csv")
files <- paste0("nfl-rosters-master/2020/", files)

myfiles <- lapply(files, read_csv) %>%
  bind_rows()

# Now do the same setup as above
games <- myfiles %>%
  mutate(player_name = paste0(firstName, " ", lastName),
         jerseyNumber = as.numeric(jerseyNumber)) %>%
  rename(jersey_number = jerseyNumber, gsis_id = gsisId) %>%
  select(player_name, position, jersey_number, contains("id"))

vets_game <- vets_tibble %>%
  filter(!is.na(gsis_id)) %>%
  left_join(games %>% select(-c(player_name, position, jersey_number)), by = c("gsis_id"))

rookies_game <- rookies_adj %>%
  filter(!is.na(team)) %>%
  select(-gsis_id) %>%
  left_join(games, by = c("player_name", "position", "jersey_number")) %>%
  filter(!is.na(id))

# All together
all_players <- vets_game %>%
  bind_rows(rookies_game)

# Relevant players
all_relevant <- all_players %>%
  filter(!is.na(id)) %>%
  group_by(player_name, position, jersey_number, team) %>%
  slice(1) %>%
  ungroup() %>%
  rename(nflfastR_id = id)

saveRDS(all_relevant, "all_relevant.rds")

