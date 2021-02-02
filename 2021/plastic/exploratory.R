# Library
library(tidyverse)

# Data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# EXPLORATORY ----

# How many countries are there? Ans: 69
plastics %>%
    group_by(country) %>%
    count() %>%
    arrange(desc(n))

# How many parent_companies are there? Ans: 10,813
plastics %>%
    group_by(parent_company) %>%
    count() %>%
    arrange(desc(n))

# How many countries is The Coca-Cola Company in? 88 (not counting empty)
plastics %>%
    filter(parent_company == 'The Coca-Cola Company') %>%
    filter(country != 'EMPTY') %>%
    # where does Coca-Cola Product the most plastic?
    arrange(desc(grand_total))

# Which country had the most volunteer? : Taiwan?
plastics %>%
    arrange(desc(volunteers)) %>%
    view()

# NOTE: in parent_company, need to remove 'Grant Total' and 'Unbranded'

# Which parent company products the most harmful plastic Code 1?
plastics %>%
    arrange(desc(pet)) %>%
    filter(parent_company != 'Grand Total' & parent_company != 'null' & parent_company != 'Unbranded')

# Dive into Coca-Cola ----

# How many countries is The Coca-Cola Company in? 88 (not counting empty)
plastics %>%
    filter(parent_company == 'The Coca-Cola Company') %>%
    filter(country != 'EMPTY') %>%
    # where does Coca-Cola Produce the most plastic?
    arrange(desc(grand_total))




# What does num_events mean?
plastics %>%
    filter(parent_company == 'The Coca-Cola Company') %>%
    arrange(country) %>% view()



# Maps of Coca-Cola ----

# install libraries
install.packages('ggmap')
install.packages('maps')
install.packages('mapdata')

library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(tidyverse)

world_map <- map_data('world')
ggplot() + 
    geom_polygon(data = world_map, aes(x=long, y=lat, group=group)) + 
    coord_fixed(1.3)

# Join world_map region w/ plastics country

# Anti-join patterns ----
library(dplyr)

world_map1 <- world_map %>%
    mutate(id = region)


plastics1 <- plastics %>%
    filter(parent_company == 'The Coca-Cola Company') %>%
    arrange(country) %>%
    mutate(id = country)


# anti_join
anti_join(world_map1, plastics1, by ="id") %>%
    View(title = 'world')

anti_join(plastics1, world_map1, by ="id") %>%
    View()


#strategy 1
world_map1[!world_map1$id %in% plastics1$id, ]

plastics1[!plastics1$id %in% world_map1$id, ]

# NOTE
# delete ecuador (missing data)
# delete empty (missing data)
# delete hong kong (missing data)
# turn Korea to South Korea
# delete nigeria (missing data)
# delete United Kingdom of Great Britain & Northern Ireland (missing data)
# delete United States of America (missing data)





