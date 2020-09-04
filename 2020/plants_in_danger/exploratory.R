# session info
# R version 4.0.2 (2020-06-22)
# RStudio Version 1.2.5042
sessionInfo()

# libraries & packages ----
library(tidyverse)

# read in data ----
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

# exploratory ----
glimpse(plants)
str(plants)

glimpse(actions)

glimpse(threats)

# data wrangling ----

# How many distinct groups of plans?            Ans: 6
# How many distinct binomial_name?              Ans: 500
# How many distinct year_last_seen ranges?      Ans: 8 (w/ NA)
# How many distinct red_list_category?          Ans: 2
# How many distinct threat_types?               Ans: 12
# How many distinct action_types?               Ans: 6

plants %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(red_list_category) %>%
    summarize(
        #distinct_group = n_distinct(group),
        #distinct_binomial = n_distinct(binomial_name),
        #distinct_year_last = n_distinct(year_last_seen),
        distinct_red_list = n_distinct(red_list_category)
    )

threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_group = n_distinct(binomial_name)
        #distinct_binomial = n_distinct(group),
        #distinct_year_last = n_distinct(year_last_seen),
        #distinct_red_list = n_distinct(red_list_category)
    )

actions %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(group) %>%
    summarize(
        #distinct_group = n_distinct(binomial_name),
        distinct_binomial = n_distinct(group)
        #distinct_year_last = n_distinct(year_last_seen),
        #distinct_red_list = n_distinct(red_list_category)
    )


# dataset: plants ----

# How many distinct binomial plants are in danger per each country or continent?
plants %>%
    select(binomial_name, country, continent) %>%
    group_by(country) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name)
    ) %>%
    view()



# dataset: threats ----
    
# How many distinct threat_types are there?
threats %>%
    group_by(threat_type) %>%
    summarize(distinct_threat_type = n_distinct(threat_type))


# How many extinction threat_type are associated with each binomial plant?
# Note: Not informative - there are 500 plants in each threat_type & 12 threat_types per plant; need to include "threatened"

threats %>%
    select(binomial_name, threat_type, threatened) %>%
    # only 899 out of 6000 meet this filter condition
    filter(threatened == 1) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_threat_type = n_distinct(threat_type)
    ) 

# How many binomial plants are associated with each extinction threat_type?
threats %>%
    select(binomial_name, threat_type, threatened) %>%
    # only 899 out of 6000 meet this filter condition
    filter(threatened == 1) %>%
    group_by(threat_type) %>%
    summarize(
        distinct_binomial_name = n_distinct(binomial_name)
    ) 






# Finding unique membership of distinct binomial plants within the 6 groups (threats)
# nested visuals
threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(group) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name)
    )

# number of plants & groups in each red_list_category 
# nested visuals
threats %>%
    select(binomial_name, group, year_last_seen, red_list_category) %>%
    group_by(red_list_category) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name),
        distinct_group = n_distinct(group)
    )


# dataset: actions ----

# How many distinct action_types are there?
actions %>%
    group_by(action_type) %>%
    summarize(distinct_action_type = n_distinct(action_type))
    

# How many action_types/action_taken are associated with each binomial plant?

actions %>%
    select(binomial_name, action_type, action_taken) %>%
    # only 899 out of 6000 meet this filter condition
    filter(action_taken == 1) %>%
    group_by(binomial_name) %>%
    summarize(
        distinct_action_type = n_distinct(action_type)
    ) %>%
    view()

# How many action_types/action_taken are associated with each plant group?

actions %>%
    select(group, action_type, action_taken) %>%
    # only 899 out of 6000 meet this filter condition
    filter(action_taken == 1) %>%
    group_by(group) %>%
    summarize(
        distinct_action_type = n_distinct(action_type)
    ) %>%
    view()

# Data Visualization ----

# ggplot: geom_tile heatmap

# Heatmap: Threats & Action Per Binomial Plant
plants %>%
    select(binomial_name, threat_AA:action_NA) %>%
    pivot_longer(cols = threat_AA:action_NA, names_to = "action", values_to = "count") %>%
    ggplot(aes(x = binomial_name, y = action, fill = count)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Heatmap: Threats & Action Per Group (of Plants)
plants %>%
    select(binomial_name, group, threat_AA:action_NA) %>%
    pivot_longer(cols = threat_AA:action_NA, names_to = "action", values_to = "count") %>%
    ggplot(aes(x = group, y = action, fill = count)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Distinct Binomial Plants by Quantile in Country/Continent
plants %>%
    select(binomial_name, country, continent) %>%
    group_by(continent, country) %>%
    summarize(
        distinct_binomial = n_distinct(binomial_name)
    ) %>%
    mutate(
        distinct_binomial_bin = as.factor(ntile(distinct_binomial, 4))
    ) %>% 
    ggplot(aes(x = continent, y = reorder(country, distinct_binomial))) +
    # can also add position = 'jitter'
    geom_point(aes(color = distinct_binomial_bin, size = distinct_binomial), alpha = 0.8) +
    labs(title = "Distinct Binomial Plants by Quantile")


 
# EXAMPLE: Dendrogram from a nested dataframe ----
# source: https://www.r-graph-gallery.com/334-basic-dendrogram-with-ggraph.html

install.packages("ggraph")
install.packages("igraph")
library(ggraph)
library(igraph)

# create a data frame
data <- data.frame(
    level1="CEO",
    level2=c( rep("boss1",4), rep("boss2",4)),
    level3=paste0("mister_", letters[1:8])
)

data

# transform it to an edge list
edges_level1_2 <- data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

# plot
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
    geom_edge_diagonal() +
    geom_node_point() +
    theme_void()


# Plant Dendogram ----

plants_data <- plants %>%
    select(group, binomial_name) %>%
    group_by(group) %>%
    arrange(group) %>% 
    mutate(
        level1 = 'center',
        level2 = group,
        level3 = binomial_name
    ) %>%
    # important to ungroup here
    ungroup() %>%
    select(level1:level3) 

plants_data

# transform it to an edge list
plants_edges_level1_2 <- plants_data %>% 
    select(level1, level2) %>% 
    unique %>% 
    rename(from=level1, to=level2)

plants_edges_level2_3 <- plants_data %>% 
    select(level2, level3) %>% 
    unique %>% 
    rename(from=level2, to=level3)

plants_edge_list=rbind(plants_edges_level1_2, plants_edges_level2_3)

plants_edge_list

# plot plant dendogram
plantgraph <- graph_from_data_frame(plants_edge_list)

ggraph(plantgraph, layout = "dendrogram", circular = TRUE) +
    geom_edge_diagonal() +
    geom_node_point() +
    theme_void()


# add text & color(leaf)
ggraph(plantgraph, layout = "dendrogram", circular = TRUE) +
    geom_edge_diagonal(edge_colour = 'orange') +
    geom_node_text(aes(label = name, filter=leaf, color="red"), hjust = 1, size = 3) +
    geom_node_point() +
    theme(
        plot.background = element_rect(fill = '#343d46'),
        panel.background = element_rect(fill = '#343d46'),
        legend.position = 'none',
        plot.title = element_text(colour = 'orange'),
        plot.caption = element_text(color = 'orange')
    ) +
    labs(
        title = 'Fire Gray',
        caption = '@paulapivat'
    )

?geom_node_text
?geom_edge_diagonal

# Flowering Plants far out number other groups
plants %>%
    group_by(group) %>%
    tally(sort = TRUE)

# Dendogram: Flowering Plant (only) by Threats

threats %>%
    select(group, threat_type) %>%
    filter(group=='Flowering Plant') %>%
    group_by(threat_type) %>%
    tally(sort = TRUE)
    

# Dendogram: Flowering Plants by Continents + Binomial name ----

# 6 distinct continents and 6 groups
plants %>%
    summarize(
        distinct_continents = n_distinct(continent),
        distinct_groups = n_distinct(group)
        )


# plant continent
plants_per_continent <- plants %>%
    select(continent, binomial_name, group) %>%
    filter(group=='Flowering Plant') %>%
    arrange(continent) %>% 
    select(continent, binomial_name) %>%
    mutate(
        level1 = 'center',
        level2 = continent,
        level3 = binomial_name
    ) %>%
    select(level1:level3) 

plants_per_continent


# transform it to an edge list
ppc_edges_level1_2 <- plants_per_continent %>% 
    select(level1, level2) %>% 
    unique %>% 
    rename(from=level1, to=level2)

ppc_edges_level2_3 <- plants_per_continent %>% 
    select(level2, level3) %>% 
    unique %>% 
    rename(from=level2, to=level3)

ppc_edge_list=rbind(ppc_edges_level1_2, ppc_edges_level2_3)

ppc_edge_list

# plot plant dendogram
ppc_graph <- graph_from_data_frame(ppc_edge_list)

ppc_graph

# Distinct Continent Names
plants %>%
    group_by(continent) %>%
    tally(sort = TRUE)



# ggraph
ggraph(ppc_graph, layout = "dendrogram", circular = TRUE) +
    geom_edge_diagonal(aes(edge_colour = ppc_edge_list$from)) +
    geom_node_text(aes(label = name, filter=leaf, color="red"), hjust = 1, size = 3) +
    geom_node_point() +
    theme(
        plot.background = element_rect(fill = '#343d46'),
        panel.background = element_rect(fill = '#343d46'),
        legend.position = 'none',
        plot.title = element_text(colour = 'orange', face = 'bold'),
        plot.caption = element_text(color = 'orange', face = 'italic')
    ) +
    labs(
        title = 'Disco Fire',
        caption = '@paulapivat'
    )

?element_text

## Dendrogram Individual Continent x Plants x Threat Type ----

# distinct threat
threats %>%
    group_by(threat_type) %>%
    tally(sort = TRUE)


plants %>%
    # select all 'known' threats (exclude: threat_NA)
    select(continent, binomial_name, threat_AA:threat_GE) %>%
    pivot_longer(cols = threat_AA:threat_GE, names_to = 'threats') %>%
    filter(value==1) %>%
    select(threats, binomial_name, continent) 
    

# OCEANIA ----

# data wrangling
oceania_threat <- plants %>%
    # select all 'known' threats (exclude: threat_NA)
    select(continent, binomial_name, threat_AA:threat_GE) %>%
    pivot_longer(cols = threat_AA:threat_GE, names_to = 'threats') %>%
    filter(value==1) %>%
    select(continent, threats, binomial_name) %>%
    filter(continent=='Oceania') %>%
    arrange(threats) %>%
    mutate(
        level1 = continent,
        level2 = threats,
        level3 = binomial_name
    ) %>%
    select(level1:level3) 

oceania_threat

# transform it to an edge list
oceania_level1_2 <- oceania_threat %>% 
    select(level1, level2) %>% 
    unique %>% 
    rename(from=level1, to=level2)

oceania_level2_3 <- oceania_threat %>% 
    select(level2, level3) %>% 
    unique %>% 
    rename(from=level2, to=level3)

oceania_edge_list=rbind(oceania_level1_2, oceania_level2_3)

oceania_edge_list


# plot plant dendogram
oceania_graph <- graph_from_data_frame(oceania_edge_list)

oceania_graph

# ggraph
ggraph(oceania_graph, layout = "dendrogram", circular = FALSE) +
    geom_edge_diagonal(aes(edge_colour = oceania_edge_list$from)) +
    #geom_edge_diagonal(aes(edge_colour = oceania_edge_list$from, label = oceania_edge_list$from, angle = 45)) +
    geom_node_text(aes(label = name, filter=leaf, color='red'), hjust = 1.1, size = 3) +
    geom_node_point(color = 'whitesmoke') +
    theme(
        plot.background = element_rect(fill = '#343d46'),
        panel.background = element_rect(fill = '#343d46'),
        legend.position = 'none',
        plot.title = element_text(colour = 'whitesmoke', face = 'bold'),
        plot.subtitle = element_text(colour = 'whitesmoke'),
        plot.caption = element_text(color = 'whitesmoke', face = 'italic')
    ) +
    labs(
        title = 'OCEANIA',
        subtitle = 'Contributors to Plant Extinction',
        caption = '@paulapivat'
    ) +
    expand_limits(x = c(-1.1, 1.1), y = c(-0.5, 0.5)) +
    coord_flip() +
    annotate("text", x = 4, y = 1, label = "Agriculture & Aquaculture", color = '#DE8C00') +
    annotate("text", x = 13, y = 1, label = "Biological Resource Use", color = "#B79F00") +
    annotate("text", x = 18, y = 1, label = "Climate Change", color = "#7CAE00") +
    annotate("text", x = 23, y = 1, label = "Energy Production & Mining", color = "#00BA38") +
    annotate("text", x = 27, y = 1, label = "Geological Events", color = "#00C08B") +
    annotate("text", x = 30, y = 1, label = "Human Intrusion", color = "#00BFC4") +
    annotate("text", x = 33, y = 1, label = "Invasive Species", color = "#00B4F0") +
    annotate("text", x = 41, y = 1, label = "Natural System Modification", color = "#619CFF") +
    annotate("text", x = 47, y = 1, label = "Polution", color = "#C77CFF") +
    annotate("text", x = 50, y = 1, label = "Commercial Development", color = "#F564E3") +
    annotate("text", x = 54, y = 1, label = "Transportation Cooridor", color = "#FF64B0") +
    annotate("text", x = 29, y = 2, label = "Oceania", color = "#F8766D")

threats %>%
    filter(continent=='Oceania') %>%
    group_by(threat_type) %>%
    tally(sort = TRUE)

?scale_fill_hue

# Extract Default Color Palette
install.packages('scales')
library(scales)


data <- threats %>%
    group_by(threat_type) %>%
    tally(sort = T)

n1 <- nrow(data)
hue_pal()(n1)

plants %>%
    group_by(continent) %>%
    tally(sort = TRUE)
    
# ASIA ----

# data wrangling
asia_threat <- plants %>%
    # select all 'known' threats (exclude: threat_NA)
    select(continent, binomial_name, threat_AA:threat_GE) %>%
    pivot_longer(cols = threat_AA:threat_GE, names_to = 'threats') %>%
    filter(value==1) %>%
    select(continent, threats, binomial_name) %>%
    filter(continent=='Asia') %>%
    arrange(threats) %>%
    mutate(
        level1 = continent,
        level2 = threats,
        level3 = binomial_name
    ) %>%
    select(level1:level3) 


# transform it to an edge list
asia_level1_2 <- asia_threat %>% 
    select(level1, level2) %>% 
    unique %>% 
    rename(from=level1, to=level2)

asia_level2_3 <- asia_threat %>% 
    select(level2, level3) %>% 
    unique %>% 
    rename(from=level2, to=level3)

asia_edge_list=rbind(asia_level1_2, asia_level2_3)

asia_edge_list
    
# create graph_from_data_frame
asia_graph <- graph_from_data_frame(asia_edge_list)

asia_graph
    
# plot Asia ggraph

ggraph(asia_graph, layout = "dendrogram", circular = FALSE) +
    geom_edge_diagonal(aes(edge_colour = asia_edge_list$from)) +
    #geom_edge_diagonal(aes(edge_colour = oceania_edge_list$from, label = oceania_edge_list$from, angle = 45)) +
    geom_node_text(aes(label = name, filter=leaf, color='red'), hjust = 1.1, size = 3) +
    geom_node_point(color = 'whitesmoke') +
    theme(
        plot.background = element_rect(fill = '#343d46'),
        panel.background = element_rect(fill = '#343d46'),
        legend.position = 'none',
        plot.title = element_text(colour = 'whitesmoke', face = 'bold'),
        plot.subtitle = element_text(colour = 'whitesmoke'),
        plot.caption = element_text(color = 'whitesmoke', face = 'italic')
    ) +
    labs(
        title = 'ASIA',
        subtitle = 'Contributors to Plant Extinction',
        caption = '@paulapivat'
    ) +
    expand_limits(x = c(-1.1, 1.1), y = c(-0.5, 0.5)) +
    coord_flip() +
    annotate("text", x = 12, y = 1, label = "Agriculture & Aquaculture", color = '#DE8C00') +
    annotate("text", x = 34, y = 1, label = "Biological Resource Use", color = "#B79F00") +
    annotate("text", x = 45, y = 1, label = "Climate Change", color = "#7CAE00") +
    annotate("text", x = 51, y = 1, label = "Energy Production & Mining", color = "#00BA38") +
    annotate("text", x = 55, y = 1, label = "Geological Events", color = "#00C08B") +
    annotate("text", x = 57, y = 1, label = "Human Intrusion", color = "#00BFC4") +
    annotate("text", x = 61, y = 1, label = "Invasive Species", color = "#00B4F0") +
    annotate("text", x = 65, y = 1, label = "Natural System Modification", color = "#619CFF") +
    annotate("text", x = 70, y = 1, label = "Polution", color = "#C77CFF") +
    annotate("text", x = 80, y = 1, label = "Commercial Development", color = "#F564E3") +
    annotate("text", x = 88, y = 1, label = "Transportation Cooridor", color = "#FF64B0") +
    annotate("text", x = 48, y = 2, label = "Asia", color = "#F8766D")

# NORTH AMERICA ----

# data wrangling
namerica_threat <- plants %>%
    # select all 'known' threats (exclude: threat_NA)
    select(continent, binomial_name, threat_AA:threat_GE) %>%
    pivot_longer(cols = threat_AA:threat_GE, names_to = 'threats') %>%
    filter(value==1) %>%
    select(continent, threats, binomial_name) %>%
    filter(continent=='North America') %>%
    arrange(threats) %>%
    mutate(
        level1 = continent,
        level2 = threats,
        level3 = binomial_name
    ) %>%
    select(level1:level3) 

# transform it to an edge list
namerica_level1_2 <- namerica_threat %>% 
    select(level1, level2) %>% 
    unique %>% 
    rename(from=level1, to=level2)

namerica_level2_3 <- namerica_threat %>% 
    select(level2, level3) %>% 
    unique %>% 
    rename(from=level2, to=level3)

namerica_edge_list=rbind(namerica_level1_2, namerica_level2_3)

namerica_edge_list

# create graph_from_data_frame
namerica_graph <- graph_from_data_frame(namerica_edge_list)

namerica_graph

# plot Asia ggraph

ggraph(namerica_graph, layout = "dendrogram", circular = FALSE) +
    geom_edge_diagonal(aes(edge_colour = namerica_edge_list$from)) +
    #geom_edge_diagonal(aes(edge_colour = oceania_edge_list$from, label = oceania_edge_list$from, angle = 45)) +
    geom_node_text(aes(label = name, filter=leaf, color='red'), hjust = 1.1, size = 2.2) +
    geom_node_point(color = 'whitesmoke') +
    theme(
        plot.background = element_rect(fill = '#343d46'),
        panel.background = element_rect(fill = '#343d46'),
        legend.position = 'none',
        plot.title = element_text(colour = 'whitesmoke', face = 'bold'),
        plot.subtitle = element_text(colour = 'whitesmoke'),
        plot.caption = element_text(color = 'whitesmoke', face = 'italic')
    ) +
    labs(
        title = 'NORTH AMERICA',
        subtitle = 'Contributors to Plant Extinction',
        caption = '@paulapivat'
    ) +
    expand_limits(x = c(-1.5, 1.5), y = c(-0.5, 0.5)) +
    coord_flip() +
    annotate("text", x = 11, y = 1, label = "Agriculture & Aquaculture", color = '#DE8C00') +
    annotate("text", x = 23, y = 1, label = "Biological Resource Use", color = "#B79F00") +
    annotate("text", x = 38, y = 1, label = "Climate Change", color = "#7CAE00") +
    #annotate("text", x = 51, y = 1, label = "Energy Production & Mining", color = "#00BA38") +
    annotate("text", x = 53, y = 1, label = "Geological Events", color = "#00C08B") +
    annotate("text", x = 60, y = 1, label = "Human Intrusion", color = "#00BFC4") +
    annotate("text", x = 86, y = 1, label = "Invasive Species", color = "#00B4F0") +
    annotate("text", x = 119, y = 1, label = "Natural System Modification", color = "#619CFF") +
    annotate("text", x = 127, y = 1, label = "Polution", color = "#C77CFF") +
    annotate("text", x = 134, y = 1, label = "Commercial Development", color = "#F564E3") +
    annotate("text", x = 141, y = 1, label = "Transportation Cooridor", color = "#FF64B0") +
    annotate("text", x = 68, y = 2, label = "N. America", color = "#F8766D")









