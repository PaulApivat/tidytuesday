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
###### Get track identifier (id)

## use purrr::map() to apply it to all songs in the dataset
## takes rankings df
## add search_query and id column(s)

ranking_ids <- rankings %>%
  mutate(
    search_query = paste(title, artist),
    search_query = str_to_lower(search_query),
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))


## explainer

ranking_ids <- rankings %>%
  mutate(
    # select & paste title and artist column(s) from ranking
    search_query = paste(title, artist),
    # change all search queries to lower case
    search_query = str_to_lower(search_query),
    # remove any character that comes after ft - featuring
    # example: Stan Eminem ft. Dido --> stan eminem
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  # find id for each search query by map_chr()
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))

###### Get Audio Features of each track

# note: get_track_audio_features() only takes 100 tracks at most at once
# divide up tracks into smaller chunks, then map() through
ranking_features <- ranking_ids %>%
    mutate(id_group = row_number() %/% 80) %>%
    select(id_group, id) %>%
    nest(data = c(id)) %>%
    mutate(audio_features = map(data, ~ get_track_audio_features(.$id)))

# to see Tibble: 4 x 3
ranking_features

# View(ranking_features[[2]][[2]]) to see nested data

###### Put Audio Features together with Rankings 
### to create a dataframe for modeling

# join ranking_ids and ranking_features
ranking_df <- ranking_ids %>%
    bind_cols(ranking_features %>% 
        select(audio_features) %>% 
        unnest(audio_features)) %>%
    select(title, artist, points, year, danceability:tempo) %>%
    na.omit()

# to see Tibble: 293 x 15
ranking_df









