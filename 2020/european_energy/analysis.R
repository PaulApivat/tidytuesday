# Session Info
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.5



# Cleaning Script (Excel) ----

# libraries
library(tidyverse)
library(readxl)

# unavailable package
install.packages('countrycodes')
library(countrycodes)

# read raw excel
raw_excel <- read_excel("./data/Electricity_generation_statistics_2019.xlsx", sheet = 3)
glimpse(raw_excel)

raw_excel %>% view()

# cleaning

# Raw Excel file lists GWh of various sources of energy from 2016 - 2018 by EU country
# This script takes the fourth column, which contains the country-code, 
# then puts the code in the newly created country column, mapping to all sources of energy

raw_excel %>%
    # remove all NA's in column 4
    filter(!is.na(...4)) %>% 
    mutate(country = str_remove_all(...4, "[:digit:]"), .before = ...1) %>% 
    mutate(country = if_else(
        str_length(country) > 1, country, NA_character_),
        country = str_extract(country, '[:alpha:]+')
    ) %>% 
    fill(country) %>% 
    select(-c(...1, ...2, ...14:...18)) %>% view()

# Go to Sheet 3 of Excel file, grab content from cells C48:C61 and turns them into individual words
# each representing various energy source categories. 
read_excel("./data/Electricity_generation_statistics_2019.xlsx",
           sheet = 3,
           # goes from a tibble (14 x 1) to individual words, get rid of the first 'of which'
           range = "C48:C61", col_names = FALSE)[[1]][c(1,3:14)] %>%
    str_remove('[:digit:]') %>%
    str_remove("of which: ") %>%
    str_remove("\\.") %>% str_trim()

