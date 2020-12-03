# load libraries
library(tidyverse)
library(lubridate)

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

# NOTE: include occupancy date
# show changes in occupancy rate over time for shelters in the City of Toronto
# something off: hypothesis: dttm format, need to turn this into discrete figure
# in one day, there could be multiple entries
# challenge: split dttm into year, month, day
# note: some shelter_name will have multiple entries for the same year_month (must group across facility_name)


toronto_shelters %>%
    # handling date first
    mutate(
        date = occupancy_date %>% ymd(),
        year = date %>% year(),
        month = date %>% month(),
        day = date %>% day(),
        year_month = make_datetime(year, month) # make year-month
    ) %>% 
    select(year_month, shelter_name, sector:capacity) %>%
    drop_na() %>%               #29,155 rows
    filter(!capacity==0) %>% 
    # group_by shelter_name, then sum_occupancy, sum_capacity
    group_by(year_month, shelter_name, sector) %>% 
    summarize(
        sum_occupancy = sum(occupancy),
        sum_capacity = sum(capacity) 
    ) %>%
    ggplot()+
    geom_line(aes(x=year_month, y=sum_occupancy, color=sector))+
    geom_line(aes(x=year_month, y=sum_capacity, color=sector))+
    facet_wrap(~shelter_name)
    
    
    
    
    
    
    
    
    group_by(year_month, shelter_name, sector) %>%
    ggplot(aes(x=year_month, y=occupancy, color=sector)) +    
    geom_line() +
    facet_wrap(~shelter_name)



# pick up here
toronto_shelters %>%
    select(occupancy_date, shelter_name, sector:capacity) %>%
    drop_na() %>%               #29,155 rows
    filter(!capacity==0) %>%    #29,051 rows
    group_by(shelter_name, sector) %>%
    ggplot(aes(x=occupancy_date, y=occupancy, color=sector)) +
    geom_line() +
    facet_wrap(~shelter_name)





   
toronto_shelters %>%
    filter(shelter_name=="Seaton House") %>%
    select(occupancy_date, shelter_name, sector:capacity)
    group_by(occupancy_date, shelter_name, sector) %>% 
    summarize(
        sum_occupancy = sum(occupancy),
        sum_capacity = sum(capacity) 
    ) %>%
    mutate(
        occupancy_rate = sum_occupancy/sum_capacity
    ) %>%
    ungroup() %>%
    view()
   
    



