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
    filter(!is.na(grand_total_1)) %>%
    ggplot() +
    coord_fixed(1.3) +
    geom_polygon(aes(x=long, y=lat, group=group, fill = grand_total_1)) +
    theme_minimal() +
    scale_fill_manual(values = c("#fff8ff","#ba432e","#6f1712", "#360103")) +
    theme(
        panel.background = element_rect(fill = "light gray"),
        legend.position = "bottom"
    ) +
    labs(
        x = "Longitude",
        y = "Latitude",
        fill = "Plastic Output",
        title = "Top 5 Countries in Plastic Waste",
        subtitle = "The Coca-Cola Company: 2020"
    )


# make sure that all plastic types add up to grand_total
plastics2 %>%
    filter(year == 2020) %>%
    arrange(desc(grand_total)) %>%
    mutate(
        # sum across columns
        calc_total = select(., empty:pvc) %>% rowSums(na.rm = TRUE)
    ) %>% view()

plastics2 %>%
    filter(year == 2020) %>%
    select(country:grand_total) %>%
    arrange(desc(grand_total)) %>%
    slice(1:5)

# Other Visuals ----

# Standard Bar Plot
plastics2 %>%
    filter(!is.na(grand_total)) %>%
    filter(year == 2020) %>%
    ggplot(aes(x=reorder(country, grand_total), y=grand_total)) +
    geom_bar(stat = 'identity', fill=alpha("#6f1712", 0.8)) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
    ) +
    labs(
        x = "",
        y = "Total Output (Plastic)",
        title = "Recipient of the Most Plastic in 2020",
        subtitle = "From the Coca-Cola Company"
    )

# Parts of Whole
plastics2 %>%
    filter(country == 'Nigeria') %>%
    filter(year == 2020) %>%
    select(country, empty:pvc) %>%
    pivot_longer(cols = c(hdpe:pvc), names_to = "plastic", values_to = "amount") %>%
    filter(amount > 0) %>%
    ggplot(aes(x="", y=amount, fill=plastic)) +
    geom_bar(stat = 'identity', width = 1) +
    coord_polar("y", start = 0) +
    labs(
        fill = "Plastic Type",
        x = "",
        y = "",
        title = "Plastic Waste in Nigeria by Type",
        subtitle = "Producer: The Coca-Cola Company 2020"
    )




plastics3 %>%
    filter(year == 2020) %>%
    ggplot() +
    geom_bar(aes(x=grand_total_1, fill = grand_total_1))


plastics2 %>%
    group_by(country) %>%
    arrange(desc(grand_total)) %>%
    view()




# Circular Bar Plot (Coca-Cola) ----
# NOTE: start with plastics2

# Order data
tmp <- plastics2 %>%
    filter(year==2020) %>%
    select(country, grand_total) %>%
    filter(!is.na(grand_total)) %>%
    arrange(desc(grand_total)) %>%
    mutate(country=factor(country, country))

# Set a number of 'empty bar' (why 10?)
empty_bar = 10

# Add lines to the initial tmp dataframe (why?)
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

# get the name and the y position of each label
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle = 90 - 360 * (label_tmp$id-0.5) /number_of_bar  # subtract 0.5 so letters are at the center of the bar
label_tmp$hjust <- ifelse(angle < -90, 1, 0)
label_tmp$angle <- ifelse(angle < -90, angle+180, angle)
label_tmp$country <- paste(label_tmp$country, " (", label_tmp$grand_total,")", sep = "")


# Make the Circular Bar Plot
ggplot(tmp, aes(x=as.factor(id), y=grand_total)) +
    geom_bar(stat = "identity", fill=alpha("#6f1712", 0.8)) +
    ylim(-5000, 5000) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar(start = 0)+
    geom_text(data = label_tmp, aes(x=id, y=grand_total+200, label=country), color="black", fontface="bold", alpha=0.6, 
              size=2.5, angle = label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE) +
    geom_text(aes(x=24, y=4500, label="Where Did Coca-Cola Dump the Most Plastic in 20202?"), color="black", inherit.aes = FALSE)


# Circular Bar Plot (Multiple) ----

plastics %>%
    filter(year==2020) %>%
    group_by(parent_company) %>%
    summarise(sum = sum(grand_total)) %>%
    arrange(desc(sum)) %>%
    view()

# Top 20 Plastic Producing Brands
top_brands = c("Universal Robina Corporation", 
               "Colgate-Palmolive", 
               "Mayora Indah", 
               "Tamil Nadu Co-operative Milk Producers' Federation Ltd", 
               "Procter & Gamble", 
               "Philip Morris International", 
               "Blow-Chem Industries", 
               "Rite Foods Limited", 
               "Danone", 
               "Monde Nissin Corporation", 
               "Peerless Products Manufactoring Inc", 
               "Mondelez International", 
               "Master Chef", 
               "Keurig Dr. Pepper", 
               "Liwayway Holdings Company Limited", 
               "Nutri-Asia Inc.", 
               "Voltic Ghana Limited", 
               "Britannia", 
               "Bakhresa Group", 
               "jasmine")

# filter Plastics by Top 20 Brands
plastics %>%
    filter(parent_company %in% top_brands)




