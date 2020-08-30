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
# How many distinct threat_types?               Ans: 12
# How many distinct action_types?               Ans: 6

plants %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(red_list_category) %>%
    summarize(
        #distinct_group = n_distinct(group),
        #distinct_binomial = n_distinct(binomial_name),
        #distinct_year_last = n_distinct(year_last_seen),
        distinct_red_list = n_distinct(red_list_category)
    )

threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_group = n_distinct(binomial_name)
        #distinct_binomial = n_distinct(group),
        #distinct_year_last = n_distinct(year_last_seen),
        #distinct_red_list = n_distinct(red_list_category)
    )

actions %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(group) %>%
    summarize(
        #distinct_group = n_distinct(binomial_name),
        distinct_binomial = n_distinct(group)
        #distinct_year_last = n_distinct(year_last_seen),
        #distinct_red_list = n_distinct(red_list_category)
    )


# dataset: plants ----

# How many distinct binomial plants are in danger per each country or continent?
plants %>%
    select(binomial_name, country, continent) %>%
    group_by(country) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name)
    ) %>%
    view()



# dataset: threats ----
    
# How many distinct threat_types are there?
threats %>%
    group_by(threat_type) %>%
    summarize(distinct_threat_type = n_distinct(threat_type))


# How many extinction threat_type are associated with each binomial plant?
# Note: Not informative - there are 500 plants in each threat_type & 12 threat_types per plant; need to include "threatened"

threats %>%
    select(binomial_name, threat_type, threatened) %>%
    # only 899 out of 6000 meet this filter condition
    filter(threatened == 1) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_threat_type = n_distinct(threat_type)
    ) 

# How many binomial plants are associated with each extinction threat_type?
threats %>%
    select(binomial_name, threat_type, threatened) %>%
    # only 899 out of 6000 meet this filter condition
    filter(threatened == 1) %>%
    group_by(threat_type) %>%
    summarize(
        distinct_binomial_name = n_distinct(binomial_name)
    ) 






# Finding unique membership of distinct binomial plants within the 6 groups (threats)
# nested visuals
threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(group) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name)
    )

# number of plants & groups in each red_list_category 
# nested visuals
threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(red_list_category) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name),
        distinct_group = n_distinct(group)
    )


# dataset: actions ----

# How many distinct action_types are there?
actions %>%
    group_by(action_type) %>%
    summarize(distinct_action_type = n_distinct(action_type))
    

# How many action_types/action_taken are associated with each binomial plant?

actions %>%
    select(binomial_name, action_type, action_taken) %>%
    # only 899 out of 6000 meet this filter condition
    filter(action_taken == 1) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_action_type = n_distinct(action_type)
    ) %>%
    view()

# How many action_types/action_taken are associated with each plant group?

actions %>%
    select(group, action_type, action_taken) %>%
    # only 899 out of 6000 meet this filter condition
    filter(action_taken == 1) %>%
    group_by(group) %>%
    summarize(
        distinct_action_type = n_distinct(action_type)
    ) %>%
    view()







