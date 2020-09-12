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
    geom_point() +
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

# JOIN ALL DATA ----

total_data <- friends %>%
    left_join(friends_emotions, by = c('season', 'episode', 'scene', 'utterance')) %>%
    left_join(friends_info, by = c('season', 'episode')) 


# 2d histogram
library(ggrepel)

ggplot(data = total_data, mapping = aes(x=us_views_millions, y=imdb_rating, label=title)) +
    geom_bin2d(bins = 60) +
    #scale_fill_continuous(type = 'viridis') +
    scale_fill_gradient(low = '#FFF580', high = '#FF4238') +
    # upper 25% of natural process limit (by views)
    #geom_vline(xintercept = 28.1, color = 'orange') +
    # upper 25% of natural process limit (by ratings)
    #geom_hline(yintercept = 8.97, color = 'orange') +
    geom_text(aes(label=ifelse(us_views_millions >= 28.1 & imdb_rating >= 8.97, season, "")), 
              nudge_y = 0.05,
              check_overlap = FALSE) +
    #geom_smooth(method = 'lm', se = FALSE, color = '#FF4238') +
    theme(
        plot.background = element_rect(fill = '#36454F'),
        panel.background = element_rect(fill = '#36454F'),
        legend.background = element_rect(fill = '#36454F'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = 'white'),
        axis.title.x = element_text(colour = 'white'),
        axis.text.y = element_text(colour = 'white'),
        axis.title.y = element_text(color = 'white')
    ) +
    labs(
        x = 'Views (Millions)',
        y = 'IMDB Rating'
    ) +
    #annotate("text", x = 50, y = 9.1, label = "r = 0.396", color = "#FF4238") +
    annotate("text", x = 15, y = 9.3, label = "S3E16:...the \n Morning After.", color = '#FF4238') +
    annotate("text", x = 19, y = 9.5, label = "S7E24:...Monica and \nChandler's wedding.", color = '#FFDC00') +
    annotate("text", x = 21, y = 9.8, label = "S6E25:...the Proposal.", color = '#42A2D6') +
    annotate("text", x = 25, y = 10, label = "S2E14:...the Prom Video.", color = '#FF4238') +
    annotate("text", x = 35, y = 8.3, label = "S2E7:...\nWhere Ross Finds Out.", color = '#FFF580') +
    annotate("text", x = 42, y = 8.8, label = "S4E24:...Ross' Wedding.", color = '#42A2D6') +
    annotate("text", x = 40, y = 8.6, label = "S8E2:...the Red Sweater.", color = '#FF4238') +
    geom_curve(x = 28.3, xend = 17, y = 9.1, yend = 9.2, color = '#FF4238', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30, xend = 23, y = 9.2, yend = 9.4, color = '#FFDC00', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30.7, xend = 24, y = 9.3, yend = 9.7, color = '#42A2D6', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 33.6, xend = 31, y = 9.4, yend = 9.9, color = '#FF4238', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30.5, xend = 33, y = 9, yend = 8.4, color = '#FFF580', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 31.6, xend = 38, y = 9.2, yend = 8.9, color = '#42A2D6', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30, xend = 35, y = 9.1, yend = 8.7, color = '#FF4238', curvature = 0.0, stat = 'identity')
    

# scale_fill_manual(values = c('#FF4238', '#FFDC00', '#42A2D6', '#9A0006', '#FFF580', '#00009E')) +

# Correlation between IMDB Rating & Views (r = 0.396)
cor(total_data$us_views_millions, total_data$imdb_rating)


# Filter total_data for highest views and ratings
total_data %>% 
    filter(us_views_millions >= 28.1) %>%
    filter(imdb_rating >= 8.97) %>%
    group_by(season, episode, title, us_views_millions, imdb_rating) %>%
    tally(sort = TRUE)




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


# Filter ----

# for Upper25 Rating
friends_segmented_ratings %>%
    filter(spc %in% c('upper25', 'unpl')) %>% 
    group_by(season, episode) %>%
    tally(sort = TRUE) %>% view()


# for Lower25 Rating
friends_segmented_ratings %>%
    filter(spc %in% c('lower25', 'lnpl')) %>% 
    group_by(season, episode) %>%
    tally(sort = TRUE) %>% view()

# TREEMAP ----
install.packages('treemap')
library(treemap)

friends_emo_tree <- total_data %>%
    select(speaker, emotion) %>%
    # filter out 'NA' emotions
    filter(!is.na(emotion)) %>%
    # filter out 'Neutral' emotions
    filter(emotion != 'Neutral') %>%
    # filter out all characters except the main
    filter(speaker %in% c('Monica Geller', 'Chandler Bing', 'Joey Tribbiani', 'Ross Geller', 'Rachel Green', 'Phoebe Buffay')) %>%
    group_by(speaker, emotion) %>%
    tally(sort = TRUE) %>% 
    arrange(speaker)


treemap(friends_emo_tree, index = c('speaker', 'emotion'), vSize = 'n', 
        type = 'index',
        palette = 'Set3'
        )

# TREEMAPIFY ----
install.packages('treemapify')
library(treemapify)

ggplot(friends_emo_tree, aes(area = n, fill = n, label = emotion, subgroup = speaker)) +
    geom_treemap() +
    geom_treemap_text(fontface = 'italic', color = 'white', place = 'centre', grow = TRUE)

# predominantly dodgerblue
ggplot(friends_emo_tree, aes(area = n, fill = n, label = speaker, subgroup = emotion)) +
    geom_treemap(aes(alpha = n), fill = 'dodgerblue') +
    geom_treemap_subgroup_border(color = 'white') +
    geom_treemap_subgroup_text(place = 'bottom', grow = T, alpha = 0.5, color = 'black',
                               fontface = 'italic', min.size = 0) +
    geom_treemap_text(color = 'white', place = 'centre', reflow = T, alpha = 0.5)


# color by emotion
ggplot(friends_emo_tree, aes(area = n, label = speaker, subgroup = emotion)) +
    geom_treemap(aes(fill = emotion, alpha = n)) +
    geom_treemap_subgroup_border(color = 'white') +
    geom_treemap_subgroup_text(place = 'bottom', grow = T, alpha = 0.3, color = 'black',
                                min.size = 0) +
    geom_treemap_text(color = 'white', fontface = 'italic', place = 'centre', reflow = T) +
    scale_fill_manual(values = c('#FF4238', '#FFDC00', '#42A2D6', '#9A0006', '#FFF580', '#00009E')) +
    theme(
        plot.background = element_rect(fill = '#36454F'),
        legend.position = 'none',
        title = element_text(colour = 'white')
        ) +
    labs(
        title = 'The One with the Dominant Emotions'
    )

# HEATMAP ----
ggplot(total_data, aes(x = season, y = episode, fill=imdb_rating)) +
    geom_tile() +
    #scale_fill_distiller(palette = 'RdPu') +
    scale_fill_gradient(low = '#FFF580', high = '#FF4238') +
    #scale_fill_viridis_b() +
    theme_classic()


ggplot(total_data, aes(x = season, y = episode, fill=us_views_millions)) +
    geom_tile() +
    #scale_fill_distiller(palette = 'RdPu') +
    scale_fill_gradient(low = '#FFF580', high = '#FF4238') +
    #scale_fill_viridis_b() +
    theme_classic()



