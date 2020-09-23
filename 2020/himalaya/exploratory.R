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


    
    
    






    
    
    

