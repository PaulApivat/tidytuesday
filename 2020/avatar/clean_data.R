# Session Info ----
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# Load Libraries ----
library(tidyverse)


# Read Data ----
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
glimpse(avatar)

scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')
glimpse(scene_description)

# EDA ----

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
    group_by(character) %>%
    tally(sort = TRUE)

# right join two data sets
scene_description %>%
    # use right_join because original avatar had 13385 obs., scene_description had 7626 obs
    # right_join leaves NA where there are no scene_descriptions (15416 obs)
    right_join(avatar, by = 'id') %>%
    mutate(
        scene_description = gsub("[^[:alnum:] ]", "", scene_description)
    ) %>% 
    # if drop_na then back to 7626 observations
    drop_na(scene_description) %>% 
    select(scene_description, book, imdb_rating) %>% view()

# NOTE: using left_join the same as right_join & drop_na




####

# remove all non-alphanumeric without using dplyr chain
View(str_remove_all(scene_description$scene_description, "[^[:alnum:] ]"))

# List all scene_descriptions that are emotion words ----

# Top 20 Emotions
Angrily 79 / Angry 10 / Angered 6
Sarcastically 65
Annoyed 42
Surprised 39
Shocked 36
Excitedly 30 / Excited 20
Happily 24
Laughs 23
Smiling 23 / Smiles 15
Confused 22
Sadly 20
Nervously 19
Calmly 18
Determined 13
Irritated 13
Amused 12
Cheerfully 12
Worried 12
Disappointed 10
Curiously 9

# Next 20 Emotions
Mockingly 9
Desperately 8
Smugly 8
Awkwardly 7
Dismayed 7
Downcast 7
Frantically 7
Horrified 7
Sheepishly 7
Thoughtfully 7
Worriedly 7
Amazed 6
Concerned 6
Furiously 6
Enraged 6
Proudly 6
Seriously 6
Confidently 5
Defensively 5
Sorrowfully 5

# Subset Data ----

subset_df <- scene_description %>%
    # use right_join because original avatar had 13385 obs., scene_description had 7626 obs
    # right_join leaves NA where there are no scene_descriptions (15416 obs)
    right_join(avatar, by = 'id') %>%
    mutate(
        scene_description = gsub("[^[:alnum:] ]", "", scene_description)
    ) %>% 
    # if drop_na then back to 7626 observations
    drop_na(scene_description) %>% 
    select(scene_description, book, imdb_rating)

glimpse(subset_df)


