library(tidyverse)
library(reactable)
library(htmltools)

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
#orange_pal <- function(x) rgb(colorRamp(c("#ffefed", "#ff2c0f"))(x), maxColorValue = 255)

# note: color require two shade (blue & red), not different gradients of one shade (red only)
orange_pal <- function(x) rgb(colorRamp(c("#edfeff", "#ff2c0f"))(x), maxColorValue = 255)

bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
}



reactable(
    raptor_table,
    height = 600,
    defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = TRUE),
        headerStyle = list(fontFamily = "liberation mono", fontSize = 16)
        ),
    columns = list(
        NAME = colDef(
            minWidth = 200, 
            style = list(fontFamily = "liberation mono", fontSize = 14)
            ),
        SEASON = colDef(
                    format = colFormat(digits = 0), 
                    minWidth = 120,
                    style = list(fontFamily = "liberation mono", fontSize = 14),
                    align = 'left'
            ),
        MIN_PLAYED = colDef(
                    format = colFormat(separators = TRUE), 
                    minWidth = 120,
                    style = list(fontFamily = "liberation mono", fontSize = 14),
                    align = 'right'
            ),
        OFF = colDef(format = colFormat(prefix = "", digits = 1), 
                     style = function(value){
                         normalized <- (value - min(raptor_table$OFF)) / (max(raptor_table$OFF) - min(raptor_table$OFF))
                         color <- orange_pal(normalized)
                         list(background = color, fontWeight = "bold", fontFamily = "liberation mono", fontSize = 14)
                     },
                     minWidth = 100,
                     align = 'center'
                ),
        DEF = colDef(
                     format = colFormat(prefix = "", digits = 1),
                     name = "DEF",
                     style = function(value){
                         normalized <- (value - min(raptor_table$DEF)) / (max(raptor_table$DEF) - min(raptor_table$DEF))
                         color <- orange_pal(normalized)
                         list(background = color, fontWeight = "bold", fontFamily = "liberation mono", fontSize = 14)
                     },
                     minWidth = 100,
                     align = 'center'
                ),
        TOTAL = colDef(
                    format = colFormat(prefix = "+", digits = 1), 
                    minWidth = 100, 
                    style = list(backgroundColor = '#F5F5F5', fontFamily = "liberation mono", fontSize = 14),
                    align = 'center'
            ),
        WAR = colDef(
                     format = colFormat(digits = 1), 
                     minWidth = 100, 
                     style = list(fontFamily = "liberation mono", whiteSpace = "pre", fontSize = 14),
                     align = 'center'
                     ),
        PLAYOFF_WAR = colDef(
                    #format = colFormat(digits = 1), 
                    name = 'P/O WAR',
                    minWidth = 130,
                    style = list(fontFamily = "liberation mono", whiteSpace = "pre", fontSize = 14),
                    align = 'center',
                    # render bar chart
                    cell = function(value){
                        width <- paste0(value * 100 / max(raptor_table$PLAYOFF_WAR), "%")
                        value <- format(value, width = 9, justify = "right")
                        bar_chart(value, width = width, fill = "#3fc1c9")
                    }
                    )
        ),
    #columnGroups = list(
    #    colGroup(name = "RAPTOR", columns = c("OFF", "DEF", "TOTAL"))
    #),
    showSortIcon = TRUE,
    searchable = TRUE,
    minRows = 10,
    pagination = FALSE,
    showPageInfo = FALSE,
    highlight = TRUE,
    language = reactableLang(searchPlaceholder = "Search...", noData = "No matches"),
    compact = TRUE
)








