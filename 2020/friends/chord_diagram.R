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

# Treemap ----

install.packages('treemap')
library(treemap)

# Create data
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data4 <- data.frame(group,subgroup,value)


# Custom labels:
treemap(data4, index=c("group","subgroup"),     vSize="value", type="index",
        
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
            c("center", "center"), 
            c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
        
)
