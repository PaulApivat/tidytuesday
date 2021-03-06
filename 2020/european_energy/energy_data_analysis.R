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
    
# Data Visualization ----

# Base Visualization
base_visualization_2018 <- energy_types_proportion_2018 %>%
    group_by(country, type_2) %>%
    summarise(
        total_proportion_2018 = sum(proportion_2018) 
    ) %>% 
    # key to one factor level be negative based on a condition
    # if 'Conventional thermal' add negative sign, else keep as is
    mutate(
        total_proportion_2018 = if_else(type_2=='Conventional thermal', -total_proportion_2018, total_proportion_2018)
    ) %>%
    ungroup() %>% 
    # order by total_proportion, descending
    ggplot(aes(x = reorder(country, desc(total_proportion_2018)), y = total_proportion_2018, fill = type_2)) +
    geom_hline(yintercept = 1, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = .75, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = .5, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = .25, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = -.25, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = -.5, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = -.75, color = '#98a2ab', linetype = 'longdash') +
    geom_hline(yintercept = -1, color = '#98a2ab', linetype = 'longdash') +
    geom_bar(stat = 'identity') 
    
# Add Theme
themed_visualization_2018 <- base_visualization_2018 +
    scale_y_continuous(labels = abs(c(-100,-75,-50,-25,25,50,75,100)), limits = c(-1,1.5), breaks = c(-1,-.75,-.5,-.25,.25,.5,.75,1)) +
    #scale_y_continuous(labels = scales::percent, limits = c(-1,1.5), breaks = c(-1,-.75,-.5,-.25,.25,.5,.75,1)) +
    labs(
        title = 'How european countries generated electricity in 2018',
        caption = 'author: @paulapivat | paulapivat.com',
        y = 'Proportion of Energy Sources (%)',
        x = 'Country Codes',
        fill = 'Energy Source'
    ) +
    theme_classic() +
    theme(
        line = element_blank(),
        axis.title.x = element_text(color = 'white', face = 'bold'),
        axis.title.y = element_text(color = 'white', face = 'bold'),
        axis.text.x = element_text(color = 'white'),
        axis.text.y = element_text(color = 'white'),
        panel.background = element_rect(fill = '#47535e'),
        plot.background = element_rect(fill = '#47535e'),
        plot.title = element_text(color = 'white', size = 25, face = 'bold'),
        plot.caption = element_text(color = 'white', face = 'italic'),
        legend.background = element_rect(fill = '#47535e'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        legend.position = 'bottom'
    ) +
    scale_fill_manual(values = c('grey', '#23a858', '#03fc66')) 
 
# Add Annotation and Arrows
annotated_visualization_2018 <- themed_visualization_2018 + 
    annotate(geom = 'text', x = 8, y = 1.3, color = 'white', label = 'Norway had an electricity production\n almost entirely made up of renewable\n energy (97%). It is the second largest\n producer of this type of energy.') +
    annotate('segment', x = 4.5, xend = 2, y = 1.3, yend = .99, color = 'white', size = .5, alpha = 0.5, arrow=arrow(angle = 30, length = unit(0.10, 'inches'), ends = 'last', type = 'open')) +
    
    annotate(geom = 'text', x = 8, y = -.75, color = 'white', label = 'France is, by far, the largest nuclear energy\n provider representing 71% of its production.') +
    annotate('segment', x = 4, xend = 4, y = -.75, yend = -.18, color = 'white', size = .5, alpha = 0.5, arrow=arrow(angle = 30, length = unit(0.10, 'inches'), ends = 'last', type = 'open')) +
    
    annotate(geom = 'text', x = 30, y = 1.0, color = 'white', label = 'Germany is the largest renewable and\n conventional thermal energy producer.') +
    annotate('segment', x = 26.5, xend = 23, y = .99, yend = .50, color = 'white', size = .5, alpha = 0.5, arrow=arrow(angle = 30, length = unit(0.10, 'inches'), ends = 'last', type = 'open')) +
    
    annotate(geom = 'text', x = 25, y = -.90, color = 'white', label = 'Most of Poland electricity production is\n from conventional thermal energy (90%).') +
    annotate('segment', x = 28.5, xend = 33.5, y = -.99, yend = -.92, color = 'white', size = .5, alpha = 0.5, arrow=arrow(angle = 30, length = unit(0.10, 'inches'), ends = 'last', type = 'open'))



annotated_visualization_2018





