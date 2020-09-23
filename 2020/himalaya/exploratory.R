# session
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6
sessionInfo()

# load libraries
library(tidyverse)

# read in data 
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# Peaks ----

# How many unique peaks are there? (468)
peaks %>%
    group_by(peak_name) %>%
    tally(sort = TRUE)

peaks %>%
    summarize(
        unique_peaks = unique(peak_name)
    )

# Top 10 peaks, UNCLIMBED, in terms of height_metres (Everest #1) 
# all top-10 have been climbed
# Highest unclimbed peak = Yalung Kang West

peaks %>%
    select(peak_name, height_metres, climbing_status) %>%
    filter(climbing_status != 'Climbed') %>%
    arrange(desc(height_metres)) 


# Members ----

# How many unique individuals are represented in the data?
# 76,518 unique people

members %>%
    summarize(unique_individual = unique(member_id))
    
members %>%
    group_by(member_id) %>%
    tally(sort = TRUE)

# How many people have failed vs succeeded?
# 47320 fail (), 29199 success
members %>%
    group_by(success) %>%
    tally()

# Proportion of Success-to-Failure
# 47320 fail (62%), 29199 success (38%)
members %>%
    group_by(success) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n))

# Expedition ----

# When was the first expedition? Most recent?
# 1905 - 2019 (in over a century, 76,518 have risked their lives)
summary(expeditions$year)

# Table Idea ----

# Table of Peaks by Height 
# Number of people attempt
# % Success or Failed

# join members & peaks

table <- members %>%
    left_join(peaks, by = 'peak_id') %>%
    select(peak_id, peak_name.y, height_metres, climbing_status, member_id, success, died, injured) %>%
    rename(
        peak = peak_name.y,
        height = height_metres,
        status = climbing_status
    )


# See all peaks by success status
table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak, success) %>%
    tally()

# Get Success & Failure Percentages by Peak
# note: all failed attemps arranged by attempt & failure rate
table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak, success) %>%
    summarize(n = n()) %>%
    mutate(pct = n / sum(n)) %>%
    filter(success == FALSE) %>% 
    arrange(desc(n)) %>%
    arrange(desc(pct)) %>% view()


# Get number of attempts per peak
# get success/failure rate per peak















    
    
    

