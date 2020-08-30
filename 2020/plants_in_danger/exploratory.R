# session info
# R version 4.0.2 (2020-06-22)
# RStudio Version 1.2.5042
sessionInfo()

# libraries & packages ----
library(tidyverse)

# read in data ----
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

# exploratory ----
glimpse(plants)
str(plants)

glimpse(actions)

glimpse(threats)

# data wrangling ----

# How many distinct groups of plans?            Ans: 6
# How many distinct binomial_name?              Ans: 500
# How many distinct year_last_seen ranges?      Ans: 8 (w/ NA)
# How many distinct red_list_category?          Ans: 2

plants %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(red_list_category) %>%
    summarize(
        #distinct_group = n_distinct(group),
        #distinct_binomial = n_distinct(binomial_name),
        #distinct_year_last = n_distinct(year_last_seen),
        distinct_red_list = n_distinct(red_list_category)
    )




















