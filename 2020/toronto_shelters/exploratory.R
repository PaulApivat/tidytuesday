# load libraries
library(tidyverse)
library(lubridate) # handling dates
library(magick) # add logo to plot


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

# all_toronto ----
all_toronto <- shelters %>%
    # filter for city, leaves 108,054 rows
    filter(shelter_city=="Toronto") 



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

# city_of_toronto ----
# note: read in logo png below


city_of_toronto <- toronto_shelters %>%
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
    #geom_line(aes(x=year_month, y=sum_occupancy, color=sector))+
    geom_col(aes(x=year_month, y=sum_occupancy, fill=sector))+
    facet_wrap(~shelter_name) +
    scale_y_continuous(labels = scales::comma, position = "right") +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold"),
        plot.background = element_rect(fill = '#d1e0e0'),
    ) +
    labs(
        title = "Organization: City of Toronto",
        subtitle = "Total Occupancy by Homeless Shelters: 2017 - 2020",
        caption = "Data: open.toronto.ca | Visualization: @paulapivat",
        fill = "Type",
        y = "",
        x = ""
    ) +
    scale_fill_manual(values = c("#17295B", "#0B488F", "#ffffff", "#E92515"))
    

# read in open_data_toronto.png
open_data_toronto <- image_read("open_data_toronto.png")

city_of_toronto
grid::grid.raster(open_data_toronto, x = 0.95, y = 0.1, just = c('right', 'bottom'), width = unit(2, 'inches'))


# all_toronto (plot) ----

all_toronto %>%
    mutate(
        date = occupancy_date %>% ymd(),
        year = date %>% year(),
        month = date %>% month(),
        day = date %>% day(),
        year_month = make_datetime(year, month) # make year-month
    ) %>% 
    select(year_month, organization_name, sector:capacity) %>%
    drop_na() %>%                # drop NA values
    filter(!capacity==0) %>%     # filter out capacity==0 (to prevent Nan from occupancy/capacity)
    #filter(organization_name != "City of Toronto") %>%
    #filter(organization_name != "COSTI Immigrant Services") %>%
    group_by(year_month, organization_name, sector) %>% 
    summarize(
        sum_occupancy = sum(occupancy),
        sum_capacity = sum(capacity) 
    ) %>%
    ggplot()+
    geom_bar(aes(x=year_month, y=sum_occupancy, fill=sector), position = "fill", stat = "identity")+
    facet_wrap(~organization_name) +
    scale_y_continuous(labels = scales::comma, position = "right") +
    theme_minimal()+
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold"),
        plot.background = element_rect(fill = '#d1e0e0'),
    )+
    labs(
        title = "Homeless in Toronto: 2017 - 2020",
        subtitle = "Proportional occupancy across organizations",
        caption = "Data: open.toronto.ca | Visualization: @paulapivat",
        fill = "Type",
        y = "",
        x = ""
    ) +
    scale_fill_manual(values = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0"))
    #scale_fill_manual(values = c("#17295B", "#0B488F", "#ffffff", "#E92515", "#F6A7A1"))

