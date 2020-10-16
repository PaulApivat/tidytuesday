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

raptor_table <- df %>%
    select(player_name, season, mp, raptor_offense, raptor_defense, raptor_total, war_total, war_playoffs) %>%
    filter(mp > 1000) %>%
    arrange(desc(war_total)) %>%
    rename(
        NAME = player_name,
        SEASON = season,
        MIN_PLAYED = mp,
        OFF = raptor_offense,
        DEF = raptor_defense,
        TOTAL = raptor_total,
        WAR = war_total,
        PLAYOFF_WAR = war_playoffs
    ) %>%
    mutate_at(4:8, funs(round(., 1))) %>% 
    head(100) 

# reactable
# can save some number format and prefixes for reactable

reactable(
    raptor_table,
    columns = list(
        SEASON = colDef(format = colFormat(digits = 0)),
        OFF = colDef(format = colFormat(prefix = "+", digits = 1)),
        DEF = colDef(format = colFormat(prefix = "+", digits = 1)),
        TOTAL = colDef(format = colFormat(prefix = "+", digits = 1)),
        WAR = colDef(format = colFormat(digits = 1)),
        PLAYOFF_WAR = colDef(format = colFormat(digits = 1))
    ),
    showSortIcon = TRUE,
    searchable = TRUE,
    minRows = 10,
    pagination = FALSE,
    showPageInfo = FALSE
)








