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


# Map Color to Hierarchy Depth ----

# Left: color depends on depth
p <- ggraph(mygraph, layout = 'circlepack')+
    geom_node_circle(aes(fill = depth))+
    theme_void()+
    theme(legend.position = 'FALSE')

# Adjust color palette: Viridis
p + scale_fill_viridis()

# Adjust color palette: colorBrewer
p + scale_fill_distiller(palette = "RdPu")

# Circular Packing w/ Labels ----

# Create a subset of the dataset (remove 1 level)
edges2 <- flare$edges %>%
    filter(to %in% from) %>%
    droplevels()

vertices2 <- flare$vertices %>%
    filter(name %in% c(edges$from, edges$to)) %>%
    droplevels()

vertices2$size <- runif(nrow(vertices))

# Rebuild the graph object
mygraph2 <- graph_from_data_frame(edges2, vertices = vertices2)

# circular packing with labels
ggraph(mygraph2, layout = 'circlepack')+
    geom_node_circle(aes(fill=depth))+
    geom_node_text(aes(label=shortName, filter=leaf, fill=depth, size=size))+
    theme_void()+
    theme(legend.position = 'FALSE')+
    scale_fill_viridis()

# Hiding Levels ----
# source: https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing.html

edges3 <- flare$edges
vertices3 <- flare$vertices
mygraph3 <- graph_from_data_frame(edges3, vertices=vertices3)

# hide first level (right)
ggraph(mygraph3, layout = 'circlepack') +
    geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth)))+
    scale_fill_manual(values=c("0" = "white", "1" = "red", "2" = "green", "3" = "yellow", "4"= "blue"))+
    scale_color_manual(values = c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black"))+
    theme_void()+
    theme(legend.position = "FALSE")











