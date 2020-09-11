#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.5
sessionInfo()

# Library ----
library(tidyverse)
library(zoo) # calculate moving range

# Read data directly ----
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

# Exploratory ----
glimpse(friends)
glimpse(friends_emotions)
glimpse(friends_info)

# Exploratoring friends_info ----

# Explore user views across all season, including average views
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_views = mean(us_views_millions)
    ) %>%
    ggplot(aes(x = air_date, y = us_views_millions)) + 
    geom_line() +
    geom_hline(yintercept = 25.4, color = 'red')


# If 25.4 is the average, what's the standard deviation(s)?
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_views = mean(us_views_millions),
        one_sd_views = sd(us_views_millions),
        two_sd_views = one_sd_views * 2,
        three_sd_views = one_sd_views * 3
    ) %>%
    ggplot(aes(x = air_date, y = us_views_millions)) + 
    geom_line() +
    geom_hline(yintercept = 25.4, color = 'green') +
    geom_hline(yintercept = 25.4+10.5, color = 'orange') +
    geom_hline(yintercept = 25.4+5.23, color = 'yellow') +
    geom_hline(yintercept = 25.4-5.23, color = 'yellow') +
    geom_hline(yintercept = 25.4+15.7, color = 'red')


# What's the Business Process Chart for this?
library(zoo)

friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_views = mean(us_views_millions),
        # calculate lagging difference
        moving_range = diff(as.zoo(us_views_millions), na.pad=TRUE),
        # get absolute value of lagging difference
        moving_range = abs(moving_range),
        # Change NA to 0
        moving_range = ifelse(row_number()==1, 0, moving_range),
        avg_moving_range = mean(moving_range),
        lnpl = average_views - (2.66*avg_moving_range),
        lower_25 = average_views - (1.33*avg_moving_range),
        upper_25 = average_views + (1.33*avg_moving_range),
        unpl = average_views + (2.66*avg_moving_range)
    ) %>%
    ggplot(aes(x = air_date, y = us_views_millions)) + 
    geom_line() +
    geom_hline(yintercept = 25.4, color = 'green') +
    geom_hline(yintercept = 30.9, color = 'red') +
    geom_hline(yintercept = 19.9, color = 'red') +
    geom_hline(yintercept = 28.1, color = 'orange') +
    geom_hline(yintercept = 22.6, color = 'orange') 


# Filter for all episodes where us_views_million > 30.9
# n = 20
friends_info %>%
    filter(us_views_millions >= 30.9)

# Fitler for all episodes where us_views_million < 19.9
# n = 19
friends_info %>%
    filter(us_views_millions < 19.9)

# Filter for all episodes in upper 25% in us_views_millions
# n = 56 rows
friends_info %>%
    filter(us_views_millions >= 28.1)

# Filter for all episodes in lower 25% in us_views_millions
# n = 68 rows
friends_info %>%
    filter(us_views_millions <= 22.6)


# Explore imdb ratings across all seasons
friends_info %>%
    select(air_date, us_views_millions, imdb_rating) %>%
    mutate(
        average_rating = mean(imdb_rating)
    ) %>%
    ggplot(aes(x = air_date, y = imdb_rating)) +
    geom_line() +
    geom_hline(yintercept = 8.46, color = 'red')

# Divide imdb ratings into Statistical Process Control Segments
friends_info %>%
    select(air_date, imdb_rating) %>%
    mutate(
        average_rating = mean(imdb_rating),
        # lagging difference
        moving_range = diff(as.zoo(imdb_rating), na.pad=TRUE),
        # get absolute value of lagging difference
        moving_range = abs(moving_range),
        # Change NA to 0
        moving_range = ifelse(row_number()==1, 0, moving_range),
        avg_moving_range = mean(moving_range),
        lnpl = average_rating - (2.66*avg_moving_range),
        lower_25 = average_rating - (1.33*avg_moving_range),
        upper_25 = average_rating + (1.33*avg_moving_range),
        unpl = average_rating + (2.66*avg_moving_range)
    ) %>%
    ggplot(aes(x = air_date, y = imdb_rating)) + 
    geom_line() +
    geom_hline(yintercept = 8.46, color = 'green') +
    geom_hline(yintercept = 9.47, color = 'red') +
    geom_hline(yintercept = 7.45, color = 'red') +
    geom_hline(yintercept = 8.97, color = 'orange') +
    geom_hline(yintercept = 7.96, color = 'orange') 



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


# Exploratoring friends_emotion ----

friends_emotions %>%
    group_by(emotion) %>%
    tally(sort = TRUE)

# Exploratoring friends ----

friends %>%
    group_by(speaker) %>%
    tally(sort = TRUE) %>% view()
    
    
# Wrangling ----

# Create Column Binning for lower 25, lower natural process limit (lnpl), upper 25 and upper natural process limit (unpl)
# us_views_millions
friends_segmented_views <- friends_info %>%
    select(season, episode, us_views_millions) %>%
    mutate(
        spc = ifelse(us_views_millions > 30.9, 'unpl', 
                     ifelse(us_views_millions < 19.9, 'lnpl',
                            ifelse(us_views_millions > 28.1 & us_views_millions <= 30.9, 'upper25',
                                   ifelse(us_views_millions < 22.6 & us_views_millions >= 19.9, 'lower25', 'AVG'))))
    ) 

friends_segmented_views

# imdb_rating
friends_segmented_ratings <- friends_info %>%
    select(season, episode, imdb_rating) %>%
    mutate(
        spc = ifelse(imdb_rating > 9.47, 'unpl', 
                     ifelse(imdb_rating < 7.45, 'lnpl',
                            ifelse(imdb_rating > 8.97 & imdb_rating <= 9.47, 'upper25',
                                   ifelse(imdb_rating < 7.96 & imdb_rating >= 7.45, 'lower25', 'AVG'))))
    )


friends_segmented_ratings %>% view()



# Join Views & Emotions ----

# Join friends_segmented_views with friends_emotions
# filter by various segment
# group_by emotions

friends_segview_emo <- friends_segmented_views %>%
    left_join(friends_emotions, by = c('season', 'episode')) %>%
    select(season, episode, us_views_millions, spc, emotion)



# Lower Natural Process Limit, group by Emotions
friends_segview_emo %>%
    filter(spc=='lnpl') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)


# Lower 25(%), group by Emotions
friends_segview_emo %>%
    filter(spc=='lower25') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)


# Upper 25(%), group by Emotions
friends_segview_emo %>%
    filter(spc=='upper25') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)


# Upper Natural Process Limit, group by Emotions
friends_segview_emo %>%
    filter(spc=='unpl') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)



# Total Rows by each SPC Segment ----
# Counting Rows by SPC segment

# lnpl = 491 rows
friends_segview_emo %>%
    filter(spc=='lnpl')

# lower25 = 934 rows
friends_segview_emo %>%
    filter(spc=='lower25')

# AVG = 6,613 rows
friends_segview_emo %>%
    filter(spc=='AVG')

# upper25 = 2,881 rows
friends_segview_emo %>%
    filter(spc=='upper25')

# unpl = 1,796
friends_segview_emo %>%
    filter(spc=='unpl')

# No missing data except for 'emotion' (n = 139 missing)
friends_segview_emo %>%
    summarise_all(~ sum(is.na(.)))

# Visualize Views & Emotion by SPC facets ----
friends_segview_emo %>%
    ggplot(aes(x = emotion)) +
    geom_histogram(stat = 'count') +
    facet_wrap(~ spc)


# Join Ratings & Speaker ----

# Join friends_segmented_views with friends (speaker)
# filter by various segment
# group_by speaker

friends %>%
    group_by(speaker) %>%
    tally(sort = TRUE)


friends_segrate_speaker <- friends_segmented_ratings %>%
    left_join(friends, by = c('season', 'episode')) %>%
    select(season, episode, imdb_rating, spc, speaker)

# Visualize Rating & Speaker ----
friends_segrate_speaker %>%
    filter(speaker %in% c('	Monica Geller', 'Chandler Bing', 'Joey Tribbiani', 'Ross Geller', 'Rachel Green', 'Phoebe Buffay')) %>%
    filter(spc != 'AVG') %>%
    ggplot(aes(x = speaker)) +
    geom_histogram(stat = 'count') +
    facet_wrap(~ spc)

# JOIN Emotion & Speaker ----

friends %>%
    left_join(friends_emotions, by = c('season', 'episode', 'scene', 'utterance')) %>%
    view()


# join, then visualize emotions by speaker
friends %>%
    left_join(friends_emotions, by = c('season', 'episode', 'scene', 'utterance')) %>%
    filter(!is.na(emotion)) %>%
    filter(speaker %in% c('Monica Geller', 'Chandler Bing', 'Joey Tribbiani', 'Ross Geller', 'Rachel Green', 'Phoebe Buffay')) %>%
    ggplot(aes(x = speaker)) +
    geom_histogram(stat = 'count') +
    facet_wrap(~ emotion, scales = 'free_y')

# join, then visualize speaker by emotion
friends %>%
    left_join(friends_emotions, by = c('season', 'episode', 'scene', 'utterance')) %>%
    filter(!is.na(emotion)) %>%
    filter(speaker %in% c('Monica Geller', 'Chandler Bing', 'Joey Tribbiani', 'Ross Geller', 'Rachel Green', 'Phoebe Buffay')) %>%
    ggplot(aes(x = emotion)) +
    geom_histogram(stat = 'count') +
    facet_wrap(~ speaker)

