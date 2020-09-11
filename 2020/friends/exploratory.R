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
    
    
# Wrangling friends_info ----

# Create Column Binning for lower 25, lower natural process limit (lnpl), upper 25 and upper natural process limit (unpl)
friends_segmented_views <- friends_info %>%
    select(season, episode, us_views_millions) %>%
    mutate(
        spc = ifelse(us_views_millions > 30.9, 'unpl', 
                     ifelse(us_views_millions < 19.9, 'lnpl',
                            ifelse(us_views_millions > 28.1 & us_views_millions <= 30.9, 'upper25',
                                   ifelse(us_views_millions < 22.6 & us_views_millions >= 19.9, 'lower25', 'AVG'))))
    ) 

friends_segmented_views

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


# A tibble: 8 x 2
emotion      n
<chr>    <int>
1 Neutral    205
2 Joyful      99
3 Mad         60
4 Scared      46
5 Sad         26
6 Powerful    25
7 Peaceful    15
8 NA          15


# Lower 25(%), group by Emotions
friends_segview_emo %>%
    filter(spc=='lower25') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)


# A tibble: 8 x 2
emotion      n
<chr>    <int>
1 Neutral    269
2 Joyful     181
3 Scared     107
4 Mad         94
5 Powerful    90
6 Peaceful    88
7 Sad         65
8 NA          40

# Upper 25(%), group by Emotions
friends_segview_emo %>%
    filter(spc=='upper25') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)


# A tibble: 8 x 2
emotion      n
<chr>    <int>
1 Neutral    968
2 Joyful     567
3 Scared     367
4 Mad        311
5 Peaceful   261
6 Powerful   211
7 Sad        195
8 NA          11



# Upper Natural Process Limit, group by Emotions
friends_segview_emo %>%
    filter(spc=='unpl') %>%
    group_by(emotion) %>%
    tally(sort = TRUE)

# A tibble: 8 x 2
emotion      n
<chr>    <int>
1 Neutral    513
2 Joyful     379
3 Scared     272
4 Peaceful   190
5 Mad        187
6 Powerful   149
7 Sad        109
8 NA           7







