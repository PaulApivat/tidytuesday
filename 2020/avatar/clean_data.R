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
    tally(sort = TRUE) %>%
    view()

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
    select(scene_description, book, imdb_rating, character)

glimpse(subset_df)

# Filter Column based on List of Words ----

top_emotions <- c('Angrily', 'Angry', 'Angered', 'Sarcastically', 'Annoyed', "Surprised", 
                  "Shocked", "Excitedly", "Excited", "Happily", "Laughs", "Smiling", "Smiles", 
                  "Confused", "Sadly", "Nervously", "Calmly", "Determined", "Irritated", "Amused", 
                  "Cheerfully", "Worried", "Disappointed", "Curiously")

top_characters <- c("Aang","Sokka","Katara","Zuko","Toph","Iroh","Azula","Jet","Suki","Zhao",
                    "Mai","Hakoda","Roku","Ty Lee","Ozai","Bumi","Yue","Hama","Warden","Long Feng")



# Heat Map Visualization ----

subset_df %>%
    filter(scene_description %in% top_emotions) %>%
    group_by(scene_description, book, character) %>%
    tally(sort = TRUE) %>% 
    ggplot(aes(x = scene_description, y = character)) +
    geom_tile(aes(fill = n)) +
    facet_wrap(~ book)

subset_df %>%
    filter(scene_description %in% top_emotions) %>%
    group_by(scene_description, book, imdb_rating) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x = scene_description, y = book)) +
    geom_tile(aes(fill = imdb_rating)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

subset_df %>%
    filter(scene_description %in% top_emotions) %>%
    filter(character %in% top_characters) %>%
    ggplot(aes(x = scene_description, y = character)) +
    geom_tile(aes(fill = imdb_rating)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()

subset_df %>%
    filter(scene_description %in% top_emotions) %>%
    filter(character %in% top_characters) %>% 
    ggplot(aes(x = character, y = scene_description)) +
    geom_tile(aes(fill = scene_description)) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = "grey")
        )

# Data Transformation (Emotion classification) ----

# Paul Ekman, universal emotions (and synonyms):

# Anger - annoy, furious, displease, exasperat(e), indignant, irrita(te)/irratably, rage/enrage/outrage, resentment
# Disgust - disgus(t), dislike
# Fear - fear, anxiety, concern, despair, dismay, doubt, horror, panic, scare, terror, worr(y, ies)
# Happiness
# Sadness
# Surprise

top_emotions <- c('Angrily', 'Angry', 'Angered', 'Sarcastically', 'Annoyed', "Surprised", 
                  "Shocked", "Excitedly", "Excited", "Happily", "Laughs", "Smiling", "Smiles", 
                  "Confused", "Sadly", "Nervously", "Calmly", "Determined", "Irritated", "Amused", 
                  "Cheerfully", "Worried", "Disappointed", "Curiously")

subset_df %>%
    mutate(rating_bin = ntile(imdb_rating, 3)) %>% view()

subset_df %>%
    filter(grepl("dislike", scene_description)) %>% view()

c("disgust", "dislike")

subset_df %>%
    filter(grepl(c("fear", "anxiety", "concern", "despair", "dismay", "doubt", "horror", "panic", "scare", "terror", "worr"), scene_description)) %>% view()

# Filtering a column, based on a vector of chracter strings


# Using base R to filter a specific column
subset_df$scene_description[Reduce(`|`, lapply(fear_words, grepl, x = subset_df$scene_description))] %>% view()

        
# Use with subset_df2 
subset_df2$scene_description[Reduce(`|`, lapply(fear_words, grepl, x = subset_df2$scene_description))] %>% view()

# see if this function produces a logical - TRUE/FALSE
Reduce(`|`, lapply(fear_words, grepl, x = subset_df2$scene_description))


top_characters <- toupper(c("Aang","Sokka","Katara","Zuko","Toph","Iroh","Azula","Jet","Suki","Zhao",
                    "Mai","Hakoda","Roku","Ty Lee","Ozai","Bumi","Yue","Hama","Warden","Long Feng"))





# Basic Emotion vectors
fear_words <- toupper(c("fear", "anxiety", "concern", "despair", "dismay", "doubt", "horror", "panic", "scare", "terror", "worr", "worried", "nervously", "horrified")) 
anger_words <- toupper(c("anger", "annoy", "annoyed", "furious", "displease", "exasperat", "indignant", "irrita", "rage", "resentment", "angrily", "angry", "angered", "irritated", "frantic"))
happiness_words <- toupper(c("bliss", "content", "delight", "exhil", "joy", "laugh", "opti", "peace", "happily", "smiling", "smiles", "amused", "cheerfully", "calmly"))
sadness_words <- toupper(c("hopeless", "misery", "sorrow", "sadly", "sad", "disappoint", "downcast"))
surprise_words <- toupper(c("amaze", "astonish", "awe", "bewild", "curious", "curiously", "shock", "wonder", "surprised", "excited", "excitedly"))
disgust_words <- toupper(c("disgust", "dislike"))


# Take data frame that's been converted to uppercase
# Near Final
subset_df2 %>%
    mutate(
        # create new column
        basic_emotions = "NA",
        # fill column based on condition
        # if scene_description contains a word from fear_words, label is "fear", otherwise keep column as is
        basic_emotions = if_else(Reduce(`|`, lapply(fear_words, grepl, x = scene_description)), "fear", basic_emotions),
        basic_emotions = if_else(Reduce(`|`, lapply(anger_words, grepl, x = scene_description)), "anger", basic_emotions),
        basic_emotions = if_else(Reduce(`|`, lapply(happiness_words, grepl, x = scene_description)), "happiness", basic_emotions),
        basic_emotions = if_else(Reduce(`|`, lapply(sadness_words, grepl, x = scene_description)), "sadness", basic_emotions),
        basic_emotions = if_else(Reduce(`|`, lapply(surprise_words, grepl, x = scene_description)), "surprise", basic_emotions),
        basic_emotions = if_else(Reduce(`|`, lapply(disgust_words, grepl, x = scene_description)), "disgust", basic_emotions)
    ) %>% 
    # group imdb_ratings into three bins - low(1), medium(2), high(3)
    mutate(
        rating_bin = ntile(imdb_rating, 10),
        rating_bin = as.factor(rating_bin)
        ) %>% 
    filter(rating_bin != 'NA') %>%
    filter(basic_emotions != 'NA') %>% 
    filter(character %in% top_characters) %>%
    ggplot(aes(x = character, y = basic_emotions, fill = rating_bin)) +
    geom_tile() +
    scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
    facet_wrap(~ book)
    

subset_df2 %>%
    mutate(imdb_rating = as.numeric(imdb_rating)) %>%
    summarize(
        min_rating = min(imdb_rating, na.rm = TRUE),
        max_rating = max(imdb_rating, na.rm = TRUE),
        mean_rating = mean(imdb_rating, na.rm = TRUE),
        median_rating = median(imdb_rating, na.rm = TRUE)
    )
   




