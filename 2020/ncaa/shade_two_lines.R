# Experimental: Shading between Two Lines
# source: https://www.r-bloggers.com/2014/06/shading-between-two-lines-ggplot/

# load data
library(ggplot2)
library(RCurl)
library(gridExtra)
library(plyr)
theme_set(theme_bw())

dat <-read.csv(url("https://raw.githubusercontent.com/nzcoops/datasets/master/shading_two_lines"))
head(dat)

# this block is run within each person essentially it creates a duplicate of
# all rows bar the first and last two and adds a grouping variable to the
# end that way every 4 rows are will be the coords for a polygon

mperson <-function(x) {
    x <-x[order(x$time), ]
    y <-x[-c(1, 2, nrow(x) -1, nrow(x)), ]
    x <-rbind(x, y)
    x <-x[order(x$time), ]
    x$group <-rep(letters[1:(nrow(x)/4)], each = 4)
    return(x)
}

dat2 <-ddply(dat, .(id), mperson)
head(dat2)

# this block is run within each person and 'block (group)' of 4 rows (each
# polygon) essentially this is to get the rows in the correct order, so that
# the geom_polygon function can work clockwise to construct the polygons the
# correct way

mgroup <-function(x) {
    x <-x[order(x$bgl), ]
    left <-x[x$time ==min(x$time), ]
    right <-x[x$time ==max(x$time), ]
    if (all(left$order ==right$order)) {
        left <-left[order(left$bgl, decreasing = T), ]
        right <-right[order(right$bgl, decreasing = F), ]
        return(rbind(left, right))
    } else {
        return(x[order(x$time), ])
    }
}

dat2 <-ddply(dat2, .(id, group), mgroup)
head(dat)



# plot
ggplot(dat, aes(x = time, y = bgl, group = order)) + 
    geom_line(aes(colour = factor(order))) +
    geom_point(aes(colour = factor(order))) +
    geom_polygon(data = dat2, aes(y = bgl, group = group), alpha = 0.3) +
    facet_wrap(~id)





