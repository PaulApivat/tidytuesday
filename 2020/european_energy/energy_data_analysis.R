# Session Info
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# Libraries ----
library(tidyverse)

# Load Data ----
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

# Data Inspection ----

glimpse(energy_types)
glimpse(country_totals)

str(energy_types)
str(country_totals)

# Data Wrangling & Visualization ----

# Need to fill NA with country_name (United Kingdom?)

# Energy Types
energy_types %>%
    # drop 'UK' without a country_name
    drop_na(country_name) %>% 
    select(country_name, type, `2016`:`2018`) %>%
    gather(`2016`, `2017`, `2018`, key = 'year', value = 'energy_gwh') %>%
    
    group_by(country_name, type) %>%
    summarize(total_energy = sum(energy_gwh)) %>%
    ungroup() %>%
    
    arrange(desc(total_energy)) %>%
    
    # visualization
    ggplot(aes(x = reorder(country_name, total_energy), y = total_energy, fill = type)) +
    geom_bar(stat = 'identity', position = 'stack') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# COuntry Totals
country_totals %>%
    # drop 'UK' without a country_name
    drop_na(country_name) %>% 
    filter(type != 'Total net production') %>%
    select(country_name, type, `2016`:`2018`) %>%
    gather(`2016`, `2017`, `2018`, key = 'year', value = 'energy_gwh') %>%
    
    group_by(country_name, type) %>%
    summarize(total_energy = sum(energy_gwh)) %>%
    ungroup() %>%
    
    arrange(desc(total_energy)) %>%
    
    # visualization
    ggplot(aes(x = reorder(country_name, total_energy), y = total_energy, fill = type)) +
    geom_bar(stat = 'identity', position = 'stack') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Treemap in GGPLOT2 ----
library(treemapify)

# Data Transformation ----

# calculate Total gwh across three years and type_2 classification
energy_types_total <- energy_types %>%
    group_by(country_name, type) %>%
    mutate(total = `2016` + `2017` + `2018`) %>%
    # create Type_2 column
    mutate(type_2 = if_else(type != 'Conventional thermal', 'Clean electricity', type)) %>%
    mutate(type_2 = if_else(type == 'Nuclear', 'Nuclear', type_2)) %>%
    mutate(type_2 = if_else(type_2 == 'Clean electricity', 'Renewable', type_2)) %>%
    # always ungroup()
    ungroup()
    

# Total & Proportion of 2018 data
energy_types_total %>%
    select(country_name, country, `2018`, type_2) %>%
    group_by(country) %>%
    mutate(total_2018 = sum(`2018`)) %>%
    mutate(proportion_2018 = `2018`/total_2018) %>%
    mutate(type_2 = as.factor(type_2)) %>%
    ungroup() %>%
    
    ggplot(aes(x = reorder(country, `2018`), y = `2018`, fill = type_2)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.9) +
    facet_wrap(~ type_2)
    

# Find proportion of Clean energy + proportion of Conventional thermal
energy_types_proportion_2018 <- energy_types_total %>%
    select(country_name, country, `2018`, type_2) %>%
    group_by(country) %>%
    mutate(total_2018 = sum(`2018`)) %>%
    mutate(
        proportion_2018 = `2018`/total_2018
        ) %>%
    mutate(type_2 = as.factor(type_2)) %>%
    ungroup()
    

energy_types_proportion_2018 %>%
    group_by(country, type_2) %>%
    summarise(
        total_proportion_2018 = sum(proportion_2018)
    ) %>% 
    mutate(
        total_proportion_2018 = if_else(type_2=='Conventional thermal', -total_proportion_2018, total_proportion_2018)
    ) %>%
    ungroup() %>% 
    ggplot(aes(x = reorder(country, total_proportion_2018), y = total_proportion_2018, fill = type_2)) +
    geom_bar(stat = 'identity')




