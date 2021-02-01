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
install.packages(c("RgoogleMaps", "ggmap", "mapproj", "sf",
                   "dplyr", "OpenStreetMap", "devtools", "DT"))




install.packages('sf')
library(sf)
install.packages('rnaturalearth')
library(rnaturalearth)
install.packages('rnaturalearthdata')
library(rnaturalearthdata)
install.packages('rgeos')
library(rgeos)
install.packages('ggspatial')
library(ggspatial)



# load data
world <- ne_countries(scale = 110, type = "countries", continent = NULL,
                      country = NULL, geounit = NULL, sovereignty = NULL,
                      returnclass = c("sp", "sf"))

# gene world map
ggplot(data = world) +
    geom_sf(colour = 'white')


ggplot(data = world) +
    geom_sf() +
    labs( x = "Longitude", y = "Latitude") +
    coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw()
