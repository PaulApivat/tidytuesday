library(tidyverse)

# read data
df <- read_csv("historical_RAPTOR_by_player.csv")

# quick replication
# replicate 538 RAPTOR seasons - exactly (i.e., 1000 mins played)
# note: add playoff WAR
# arrange descending by WAR
# within that, add playoff_WAR

df %>%
    select(player_name, season, mp, raptor_offense, raptor_defense, raptor_total, war_total, war_playoffs) %>%
    filter(mp > 1000) %>%
    arrange(desc(war_total)) %>%
    arrange(desc(war_playoffs)) %>%
    head(100) %>%
    view()
