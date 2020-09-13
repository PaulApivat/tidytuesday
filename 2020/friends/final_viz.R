#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6
sessionInfo()

# Library ----
library(tidyverse)
library(zoo) # calculate moving range
library(treemapify)
library(patchwork)

# Read data directly ----
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

# Exploratory ----
glimpse(friends)
glimpse(friends_emotions)
glimpse(friends_info)

# Create total_data 
total_data <- friends %>%
    left_join(friends_emotions, by = c('season', 'episode', 'scene', 'utterance')) %>%
    left_join(friends_info, by = c('season', 'episode')) 

# 2D Density Plot
ggplot(data = total_data, mapping = aes(x=us_views_millions, y=imdb_rating, label=title)) +
    geom_bin2d(bins = 60) +
    scale_fill_gradient(low = '#FFF580', high = '#FF4238') +
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
        axis.title.y = element_text(color = 'white'),
        title = element_text(colour = 'white')
    ) +
    labs(
        x = 'Views (Millions)',
        y = 'IMDB Rating',
        title = 'The One with the Seminal Episodes'
    ) +
    annotate("text", x = 16, y = 9.3, label = "S3E16:...the \n Morning After.", color = '#FF4238') +
    annotate("text", x = 21, y = 9.5, label = "S7E24:...Monica and \nChandler's wedding.", color = '#FFDC00') +
    annotate("text", x = 22, y = 9.8, label = "S6E25:...the Proposal.", color = '#42A2D6') +
    annotate("text", x = 30, y = 10, label = "S2E14:...the Prom Video.", color = '#FF4238') +
    annotate("text", x = 34, y = 8.3, label = "S2E7:...\nWhere Ross Finds Out.", color = '#FFF580') +
    annotate("text", x = 40, y = 8.8, label = "S4E24:...Ross' Wedding.", color = '#42A2D6') +
    annotate("text", x = 38, y = 8.6, label = "S8E2:...the Red Sweater.", color = '#FF4238') +
    geom_curve(x = 28.3, xend = 17, y = 9.1, yend = 9.2, color = '#FF4238', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30, xend = 23, y = 9.2, yend = 9.4, color = '#FFDC00', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30.7, xend = 24, y = 9.3, yend = 9.7, color = '#42A2D6', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 33.6, xend = 31, y = 9.4, yend = 9.9, color = '#FF4238', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30.5, xend = 33, y = 9, yend = 8.4, color = '#FFF580', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 31.6, xend = 38, y = 9.2, yend = 8.9, color = '#42A2D6', curvature = 0.0, stat = 'identity') +
    geom_curve(x = 30, xend = 35, y = 9.1, yend = 8.7, color = '#FF4238', curvature = 0.0, stat = 'identity')


# Treemap

friends_emo_tree <- total_data %>%
    select(speaker, emotion) %>%
    # filter out 'NA' emotions
    filter(!is.na(emotion)) %>%
    # filter out 'Neutral' emotions
    filter(emotion != 'Neutral') %>%
    # filter out all characters except the main ones
    filter(speaker %in% c('Monica Geller', 'Chandler Bing', 'Joey Tribbiani', 'Ross Geller', 'Rachel Green', 'Phoebe Buffay')) %>%
    group_by(speaker, emotion) %>%
    tally(sort = TRUE) %>% 
    arrange(speaker)


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




