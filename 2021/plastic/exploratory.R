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
# change ECUADOR (to normal case)
# change NIGERIA
# change Korea to South Korea

# delete empty (missing data)
# delete hong kong (missing data)
# delete United Kingdom of Great Britain & Northern Ireland (missing data)
# delete United States of America (missing data)

# Change or Delete Values ----
world_map1 %>% View(title = 'world_map1')
world_map1 %>%
    mutate(id1 = row_number()) %>%
    View(title = 'world_map1')




# Change values in plastic1 ----

# ECUADOR -> Ecuador
plastics1$country[22] <- 'Ecuador'
plastics1$country[22]
plastics1 %>% View(title = 'plastics1')

# NIGERIA -> Nigeria
plastics1$country[55] <- 'Nigeria'
plastics1$country[55]
plastics1 %>% View(title = 'plastics1')

# Korea -> South Korea
plastics1$country[42] <- 'South Korea'
plastics1$country[42]
plastics1 %>% View(title = 'plastics1')

# Delete values in plastic1 ----

# Delete Row 24 w/ country == 'EMPTY'
# Delete Row 20 w/ country == 'Hong Kong'
# delete United Kingdom of Great Britain & Northern Ireland
# delete United States of America (missing data)

plastics1 %>% View(title = 'plastics1')


plastics2 <- plastics1[-c(24, 30, 85, 86, 87),] 
    
# Re-check anti_join pattern ----

# make sure id column is updated
plastics2 <- plastics2 %>%
    mutate(id = country)

# confirm all column values match and ready for join
anti_join(plastics2, world_map1, by ="id") %>%
    View()

plastics3 <- plastics2 %>%
    left_join(world_map1, by = 'id')

# Plot Map ----

# base plot
base <- 
    
plastics3 %>%
    arrange(desc(grand_total)) %>%
    View()

    
ggplot(data = plastics3) + 
    coord_fixed(1.3) +
    geom_polygon(aes(x=long, y=lat, group=group, fill = grand_total)) +
    scale_fill_viridis_c()

# divide grand_total into 7 levels (binning)
# business data science notes (binning)

plastics3$grand_total_1 <- cut(plastics3$grand_total, breaks = 7, labels = c("level1", "level2", "level3", "level4", "level5", "level6", "level7"))


ggplot(data = plastics3) + 
    coord_fixed(1.3) +
    geom_polygon(aes(x=long, y=lat, group=group, fill = grand_total_1)) +
    scale_fill_viridis_d()

# Base Plot with Map & Binning
plastics3 %>%
    filter(year == 2020) %>%
    ggplot() +
    coord_fixed(1.3) +
    geom_polygon(aes(x=long, y=lat, group=group, fill = grand_total_1)) +
    scale_fill_viridis_d() +
    theme_minimal()


# make sure that all plastic types add up to grand_total
plastics2 %>%
    filter(year == 2020) %>%
    arrange(desc(grand_total)) %>%
    mutate(
        # sum across columns
        calc_total = select(., empty:pvc) %>% rowSums(na.rm = TRUE)
    ) %>% view()



plastics3 %>%
    filter(year == 2020) %>%
    ggplot() +
    geom_bar(aes(x=grand_total_1, fill = grand_total_1))


plastics2 %>%
    group_by(country) %>%
    arrange(desc(grand_total)) %>%
    view()


# create binning for smaller data frame plastics2




