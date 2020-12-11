# Library
library(tidyverse)




# rworldmap 
install.packages('rworldmap')
library(rworldmap)
vignette('rworldmap')

# Load Data Manually
women <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Filter by Category
women %>%
    filter(category=='Knowledge') %>%
    view()


# Fitler by Country
women %>%
    count(country, sort = TRUE) %>%
    view()

# Group By Role (All Activism)
women %>%
    count(role, sort = TRUE) %>%
    filter(str_detect(role, 'act'))

# EDA

women %>%
    group_by(category, country) %>%
    tally(sort = TRUE) %>%
    arrange(category) %>%
    view()

# ggraph sample ----
library(ggraph)
library(igraph)

# data frame with hierarchical structure
edges <- flare$edges

# information about each node
vertices <- flare$vertices

# make graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices)

# plot
ggraph(mygraph, layout = 'circlepack')+
    geom_node_circle()+
    theme_void()









