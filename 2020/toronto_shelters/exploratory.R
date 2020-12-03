# load libraries
library(tidyverse)

# load data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# exploratory
str(shelters)
dim(shelters) # 115916 rows, 13 columns

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


# basic viz of Shelters in Toronto landscape

dim(toronto_shelters) #












