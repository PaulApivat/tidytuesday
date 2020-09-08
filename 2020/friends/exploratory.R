#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.5
sessionInfo()

# Library ----
library(tidyverse)

# Read data directly ----
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

# Exploratory ----
glimpse(friends)
glimpse(friends_emotions)
glimpse(friends_info)

# Exploratory Viz ----

# Explore user views across all season, including average views
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_views = mean(us_views_millions)
    ) %>%
    ggplot(aes(x = air_date, y = us_views_millions)) + 
    geom_line() +
    geom_hline(yintercept = 25.4, color = 'red')


# Explore imdb ratings across all seasons
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_rating = mean(imdb_rating)
    ) %>%
    ggplot(aes(x = air_date, y = imdb_rating)) +
    geom_line() +
    geom_hline(yintercept = 8.46, color = 'red')


# Is there a relationship between Views & Ratings?
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    ggplot(aes(x = us_views_millions, y = imdb_rating)) +
    geom_point() +
    geom_smooth(method = 'lm')

# Which season/episode contains the outlier in terms of Views?
# Season 2 in 1996
friends_info %>%
    select(season, episode, us_views_millions) %>%
    mutate(
        season_fct = as.factor(season)
    ) %>%
    group_by(season_fct) %>%
    ggplot(aes(x = season_fct, y = us_views_millions, color = episode)) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    geom_jitter()

# Let's filter for season 2 to see which episode was the outlier?
# Season 2, Episode 12, 13
friends_info %>%
    select(season, episode, us_views_millions) %>%
    filter(season==2) %>%
    mutate(
        episode_fct = as.factor(episode)
    ) %>%
    ggplot(aes(x = episode_fct, y = us_views_millions)) +
    geom_point()


# filter for all episode above 50 million views
# note: 
friends_info %>%
    select(season, episode, us_views_millions) %>%
    # filter for 50 million or 30 million
    filter(us_views_millions > 30) %>%
    view()



