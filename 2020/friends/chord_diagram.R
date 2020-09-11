# Basic Chord Diagram
# source: https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html

# Chord Diagram from Adjacency Matrix ----

# a list of connections between 20 origin nodes, 5 destination nodes:
numbers <- sample(c(1:1000), 100, replace = T)
data <- matrix( numbers, ncol=5)
rownames(data) <- paste0("orig-", seq(1,20))
colnames(data) <- paste0("dest-", seq(1,5))

# Load the circlize library
install.packages('circlize')
library(circlize)

# Make the circular plot
chordDiagram(data, transparency = 0.5)





# Chord Diagram from Edge List ----

# Create Edge List
origin <- paste0("orig ", sample(c(1:10), 20, replace = T))
destination <- paste0("dest ", sample(c(1:10), 20, replace = T))
data2 <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data2, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)

# 2D Density Plot ----
library(tidyverse)

# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data3 <- rbind(a,b,c)

# Basic scatterplot
ggplot(data = data3, mapping = aes(x=x, y=y)) +
    geom_point()

# 2D Histogram with default option
ggplot(data3, aes(x=x, y=y)) +
    geom_bin2d()

# Bin size control + viridis color
ggplot(data3, aes(x=x, y=y)) +
    geom_bin2d(bins = 70) +
    scale_fill_continuous(type = 'viridis')
