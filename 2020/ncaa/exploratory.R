# sessioninfo
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.6
sessionInfo()

# load libraries & data ----
library(tidyverse)

# load data manually
tournament <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

# Exploratory ----

# range of full_percent, full_w, full_l, filter 1990
tournament %>%
    filter(year==1990) %>%
    select(full_percent, full_w, full_l) %>%
    summarize(
        range_pct = range(full_percent),
        range_w = range(full_w),
        range_l = range(full_l)
    )

tournament %>%
    filter(year==1990) %>%
    arrange(desc(full_percent)) %>% 
    view()

# full_percent - median (74.2), mean (74.24)
tournament %>%
    select(year, school, full_percent) %>%
    group_by(school) %>%
    summary()


# quick visualizations ----
library(lubridate)
tournament %>%
    select(year, school, full_percent) %>%
    group_by(school) %>%
    tally(sort = TRUE) %>% 
    ungroup() %>%
    head(30) %>%
    pull(school) -> name_vector

# facet wrap with horizontal median line
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line(aes(color=school)) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'red')

# data wrangling ----
# create another column neg/pos
# calculate, anything above median is pos, anything below is neg
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    #geom_area(aes(fill=direction)) +
    geom_line() +
    geom_point() +
    #geom_line(aes(color=school)) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black')

 
# geom_ribbon - fill="direction"
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(x = year, ymin = 74.2, ymax=full_percent, fill="direction"), alpha = 0.15) +
    #geom_ribbon() +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black')






# select full_percent, year, program
# long-to-wide pivot_wider
# were there some years where schools were below 50%?

tournament %>%
    select(year, school, full_percent) %>%
    pivot_wider(names_from = year, values_from = full_percent) %>% 
    view()

library(data.table)
class(tournament)








