# load libraries
library(tidyverse)

# load data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# exploratory ----
str(shelters)
dim(shelters) # 115916 rows, 13 columns

# Filter (Toronto City) ----

# create occupancy rate 
# occupancy / capacity
# filter for City of Toronto / Toronto city
toronto_shelters <- shelters %>%
    mutate(
        occupancy_rate = occupancy/capacity
    ) %>% 
    # filter for city, leaves 108,054 rows
    filter(shelter_city=="Toronto") %>%
    # filter for organization, leaves 30,163 rows
    filter(organization_name=="City of Toronto") 


# Exploring (Toronto Shelters) ----

dim(toronto_shelters) # 30163 rows, 14 columns

# find best way to organize

# shelter_name
# 11 categories
toronto_shelters %>%
    group_by(shelter_name) %>%
    tally(sort = TRUE)

# facility_name
# 30 categories
toronto_shelters %>%
    group_by(facility_name) %>%
    tally(sort = TRUE)


# priogram name
# 43 categories
toronto_shelters %>%
    group_by(program_name) %>%
    tally(sort = TRUE)

# count & tally by multiple columns
toronto_shelters %>%
    count(shelter_name, facility_name, program_name, sort = TRUE) 

# Count & Tally of shelter types (sector) for each shelter_name
toronto_shelters %>%
    count(shelter_name, sector, sort = TRUE)


# handling missing values, NaN and Inf ----
toronto_shelters %>%
    select(shelter_name, sector:occupancy_rate) %>%
    drop_na() %>%
    filter(!capacity==0)
    
    
    
    group_by(shelter_name, sector) %>% 
    summarize(
        sum_occupancy = sum(occupancy),
        sum_capacity = sum(capacity) 
    ) %>%
    # drop all cap
    filter(!is.na(sum_capacity))
    

toronto_shelters %>%
    filter(shelter_name=="Birkdale Residence") %>% view()

