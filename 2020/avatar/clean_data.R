# Session Info
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# Load Libraries
library(tidyverse)


# Read Data
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
glimpse(avatar)

scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')
glimpse(scene_description)

# EDA

str(scene_description)

# find major categories of scene_descriptions
scene_description %>%
    group_by(scene_description) %>% 
    tally(sort = TRUE) %>%
    view()
    
# remove special characters (non-alphanumeric) in scene_descriptions
scene_description %>%
    mutate(
        scene_description = gsub("\\[", "", scene_description),
        scene_description = gsub("\\.]", "", scene_description)
        ) %>% 
    group_by(scene_description) %>%
    tally(sort = TRUE) %>% view()

# Shorter way to remove all non-alphanumeric characters
scene_description %>%
    mutate(
        scene_description = gsub("[^[:alnum:]]", "", scene_description)
    ) %>% 
    group_by(scene_description) %>%
    tally(sort = TRUE) %>% view()


avatar %>%
    group_by(imdb_rating) %>%
    tally(sort = TRUE)

scene_description %>%
    left_join(avatar, by = 'id') %>%
    view()

####

View(str_remove_all(scene_description$scene_description, "\\["))








