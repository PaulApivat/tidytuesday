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

