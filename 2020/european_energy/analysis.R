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

# Raw Excel ----
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

# Row Stat ----
# Go to Sheet 3 of Excel file, grab content from cells C48:C61 and turns them into individual words
# each representing various energy source categories. 
row_stat <- read_excel("./data/Electricity_generation_statistics_2019.xlsx",
           sheet = 3,
           # goes from a tibble (14 x 1) to individual words, get rid of the first 'of which'
           range = "C48:C61", col_names = FALSE)[[1]][c(1,3:14)] %>%
    str_remove('[:digit:]') %>%
    str_remove("of which: ") %>%
    str_remove("\\.") %>% str_trim()

# Country Range ----
# create a 13 x 2 tibble
country_range <- tibble(row_start = seq(from = 46, to = 454, by = 34),
       row_end = seq(from = 61, to = 469, by = 34)) %>%
    # create three columns
    mutate(col1 = 4, col2 = col1 + 5, col3 = col2 + 5) %>%
    # similar to gather() col1 to col3
    pivot_longer(cols = col1:col3, names_to = 'col_var', values_to = 'col_start') %>%
    mutate(col_end = col_start + 2) %>%
    # remove col_var
    select(-col_var) %>% 
    # cut the last two rows
    slice(-n(), -(n()-1)) %>% 
    # put words in row_stat into a Vector
    mutate(row_stat = list(row_stat)) 


# all columns from country_range tibble are parameters to this function
get_country_stats <- function(row_start, row_end, col_start, col_end, row_stat){
    
    # # pull the row_stat names
    # row_stat <- row_stat
    
    # create the range programatically
    col_range <- glue::glue("{LETTERS[col_start]}{row_start}:{LETTERS[col_end]}{row_end}")
    
    # read in the data section quietly
    raw_data <- suppressMessages(
        read_excel("./data/Electricity_generation_statistics_2019.xlsx", 
                   sheet = 3,
                   col_names = FALSE,
                   range = col_range))
    
    
    country_data <-  raw_data %>% 
        # set appropriate names
        set_names(nm = c(2016:2018)) %>% 
        # drop the year ranges
        filter(!is.na(`2016`), `2016` != "2016") %>% 
        # get the country into a column rather than a header
        mutate(country = if_else(
            is.na(`2017`), 
            `2016`, 
            NA_character_), 
            .before = `2016`) %>% 
        # fill country down
        fill(country) %>% 
        # drop old country header
        filter(!is.na(`2017`)) %>% 
        # add row stat in
        mutate(type = row_stat, 
               .after = country, 
               # add levels of the stats
               level = c("Total", "Level 1", "Level 1", "Level 1", "Level 2", 
                         "Level 1", "Level 1", "Level 1", "Level 1", "Total", 
                         "Total", "Total", "Total")) %>% 
        # format as double
        # NOTE: dplyr 1.0.0 - my version dplyr 0.8.5
        mutate(across(c(`2016`:`2018`), as.double))
    
    # return data
    country_data
}

# pick up here, outdated dplyr hits error with new across() function #

all_countries <- country_range %>% 
    pmap_dfr(get_country_stats) %>% 
    left_join(raw_code, by = "country") %>% 
    select(country, country_name, everything())



