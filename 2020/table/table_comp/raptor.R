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

# conditional styling
orange_pal <- function(x) rgb(colorRamp(c("#ffefed", "#ff2c0f"))(x), maxColorValue = 255)

reactable(
    raptor_table,
    height = 600,
    defaultColDef = colDef(header = function(value) gsub("_", " ", value, fixed = TRUE),
                           footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))),
    columns = list(
        NAME = colDef(minWidth = 100),
        SEASON = colDef(format = colFormat(digits = 0), minWidth = 200),
        MIN_PLAYED = colDef(format = colFormat(separators = TRUE), minWidth = 200),
        OFF = colDef(format = colFormat(prefix = "+", digits = 1), 
                     style = function(value){
                         normalized <- (value - min(raptor_table$OFF)) / (max(raptor_table$OFF) - min(raptor_table$OFF))
                         color <- orange_pal(normalized)
                         list(background = color, fontWeight = "bold")
                     },
                     minWidth = 100,
                ),
        DEF = colDef(format = colFormat(prefix = "+", digits = 1),
                     style = function(value){
                         normalized <- (value - min(raptor_table$DEF)) / (max(raptor_table$DEF) - min(raptor_table$DEF))
                         color <- orange_pal(normalized)
                         list(background = color, fontWeight = "bold")
                     },
                     minWidth = 100,
                ),
        TOTAL = colDef(format = colFormat(prefix = "+", digits = 1), minWidth = 100),
        WAR = colDef(format = colFormat(digits = 1), 
                     minWidth = 100, 
                     ),
        PLAYOFF_WAR = colDef(format = colFormat(digits = 1), 
                             minWidth = 100,
                    )
        ),
    showSortIcon = TRUE,
    searchable = TRUE,
    minRows = 10,
    pagination = FALSE,
    showPageInfo = FALSE,
    highlight = TRUE,
    language = reactableLang(searchPlaceholder = "Search...", noData = "No matches")
)








