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




