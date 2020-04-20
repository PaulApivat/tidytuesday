## Tutorial by Julia Silge
## source: https://juliasilge.com/blog/best-hip-hop/

## RData: rapartist

# load libraries
library(tidyverse)

# sample EDA
# scatter plot with geom_jitter (can also do geom_point())
rankings %>%
    ggplot(mapping = aes(x=year, y=points, color=gender)) 
    + geom_jitter(alpha=0.7) 
    + scale_y_log10() 
    + labs(y = "Critic's Ratings", x = NULL, color = NULL)


###### Spotify API ########
install.packages("spotifyr")
library(spotifyr)

## create function to find Spotify track identifier via search_spotify()
# note: .GlobalEnv
pull_id <- function(query){
    search_spotify(query, "track") %>% 
        arrange(-popularity) %>%
        filter(row_number() == 1) %>%
        pull(id)
}

## use purrr::map() to apply it to all songs in the dataset




