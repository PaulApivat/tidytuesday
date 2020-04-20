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
library(tidyverse)
library(knitr)

### FIRST
# Set up a Dev account with Spotify to access their Web API
# access Client ID and Client Secret

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()

### SECOND
# Sample usage of Spotify API
# What was The Beatles' favorite key?

library(tidyverse)
library(knitr)

beatles <- get_artist_audio_features('the beatles')
beatles %>%
    count(key_mode, sort = TRUE) %>%
    head(5) %>%
    kable()

#   Beatles favorite key is D major
#   |key_mode |  n|
#   |:--------|--:|
#   |D major  | 24|
#   |G major  | 21|
#   |A major  | 13|
#   |F major  | 12|
#   |C major  | 11|

### RESUME TUTORIAL

## create function to find Spotify track identifier via search_spotify()
# note: .GlobalEnv
pull_id <- function(query){
    search_spotify(query, "track") %>% 
        arrange(-popularity) %>%
        filter(row_number() == 1) %>%
        pull(id)
}

######

## use purrr::map() to apply it to all songs in the dataset




