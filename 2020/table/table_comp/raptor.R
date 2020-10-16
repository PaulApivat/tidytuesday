library(tidyverse)
library(reactable)

# read data
df <- read_csv("historical_RAPTOR_by_player.csv")

# quick replication
# replicate 538 RAPTOR seasons - exactly (i.e., 1000 mins played)
# note: add playoff WAR
# arrange descending by WAR
# within that, add playoff_WAR


# wrangle data with dplyr as much as possible
# change column name
# format numbers (decimal places)

table <- df %>%
    select(player_name, season, mp, raptor_offense, raptor_defense, raptor_total, war_total, war_playoffs) %>%
    filter(mp > 1000) %>%
    arrange(desc(war_total)) %>%
    rename(
        NAME = player_name,
        SEASON = season,
        `MIN. PLAYED` = mp,
        OFF. = raptor_offense,
        DEF. = raptor_defense,
        TOTAL = raptor_total,
        WAR = war_total,
        `PLAYOFF WAR` = war_playoffs
    ) %>%
    mutate_at(4:8, funs(round(., 1))) %>% 
    head(100) 

# reactable
reactable(table)








