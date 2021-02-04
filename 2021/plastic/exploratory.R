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

# filter Plastics by Top 20 Plastic Producing Brands in 2020
# filter by 2020
# save as circ
circ <- plastics %>%
    filter(parent_company %in% top_brands) %>%
    filter(year==2020) 

# 46 countries, 20 brands
circ %>%
    group_by(parent_company) %>%
    tally(sort = TRUE)

# Quick Stacked Bar Chart of top 5 brands
circ %>%
    group_by(parent_company) %>%
    arrange(desc(grand_total)) %>% 
    view()


top_five = c("Universal Robina Corporation", 
             "Colgate-Palmolive", 
             "Mayora Indah", 
             "Tamil Nadu Co-operative Milk Producers' Federation Ltd", 
             "Procter & Gamble")

# filter by top_five plastic producing brand
circ %>%
    filter(parent_company %in% top_five) %>%
    arrange(desc(grand_total)) %>%
    view()


# Better Group By Parent Company (top_five) manageable
# Group By country is 34
# already filter 2020
circ <- circ %>%
    filter(parent_company %in% top_five) %>%
    select(country, parent_company, hdpe:pvc) %>%
    #pivot_longer (tidy format)
    pivot_longer(cols = c(hdpe:pvc), names_to = "plastic_type", values_to = "amount")

# Set "empty bar" at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(circ$plastic_type))
to_add <- data.frame(matrix(NA, empty_bar*nlevels(circ$parent_company)*nObsType, ncol(circ)))  #initially empty
colnames(to_add) <- colnames(circ)
to_add$parent_company <- rep(levels(circ$parent_company), each=empty_bar*nObsType)
circ <- rbind(circ, to_add)

circ <- circ %>% 
    arrange(parent_company, country)

circ$id <- rep(seq(1, nrow(circ)/nObsType), each=nObsType)

# Get the name and y position of each label
label_data <- circ %>% 
    group_by(id, country) %>%
    summarize(total_amount=sum(amount))

number_of_bar <- nrow(label_data)

# put text at center of bar
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar 

label_data$hjust <- ifelse(angle < -90, 1, 0)

label_data$angle <- ifelse(angle < -90, angle+180, angle)


# prepare a data frame for base lines
base_data <- circ %>%
    group_by(parent_company) %>%
    summarize(start=min(id), 
              end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))
    
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(circ) +
    # add stacked bar
    geom_bar(aes(x=as.factor(id), y=amount, fill=plastic_type), stat = "identity", alpha = 0.5) +
    scale_fill_viridis_d() +
    
    # Add a val=100/75/50/25 lines, to make sure barplots overlay
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing value of each 100/75/50/25 lines
    ggplot2::annotate("text", x = rep(max(circ$id), 5), y = c(0,50,100,150,200), 
                      label=c("0", "50", "100", "150", "200"), color="grey", size=6, angle=0, fontface="bold", hjust=1) +
    
    ylim(-5000, max(label_data$total_amount, na.rm = TRUE)) +
    theme_minimal()+
    theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() +
    
    # Add labels on each bar
    geom_text(data=label_data, aes(x=id, y=total_amount+10, label=country, hjust=hjust),
              color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), 
                 colour = "black", alpha = 0.8, size=0.6 , inherit.aes = FALSE) +
    geom_text(data=base_data, aes(x = title, y = -18, label=parent_company),
              hjust=c(1,1,0,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)






# EXAMPLE ----

# individual = country
# group = parent_company
# observation = plastic_type
# value = amount

sample_circ_data <- data.frame(
    individual=paste( "Mister ", seq(1,60), sep=""),
    group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
    value1=sample( seq(10,100), 60, replace=T),
    value2=sample( seq(10,100), 60, replace=T),
    value3=sample( seq(10,100), 60, replace=T)
)

sample_circ_data <- sample_circ_data %>% gather(key = "observation", value="value", -c(1,2)) 






