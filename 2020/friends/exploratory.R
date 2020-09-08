#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.5
sessionInfo()

# Library
library(tidyverse)

# Read data directly
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

# Exploratory
glimpse(friends)
glimpse(friends_emotions)
glimpse(friends_info)

# Exploratory Viz

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






