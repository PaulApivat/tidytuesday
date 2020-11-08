# sessioninfo
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6

# load libraries
library(tidyverse)

# load data
datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

# summary statistics
datasaurus %>%
    summary()


# single coordinate plotting 

# histogram ----
datasaurus %>%
    ggplot(aes(x=x)) +
    geom_histogram()

datasaurus %>%
    ggplot(aes(x=y)) +
    geom_histogram()

datasaurus %>%
    ggplot() +
    geom_histogram(aes(x=x, fill='red'), alpha = 0.5) +
    geom_histogram(aes(x=y, fill='green'), alpha = 0.5)


datasaurus %>%
    ggplot() +
    geom_density(aes(x=x, fill='purple'), alpha = 0.5) +
    geom_density(aes(x=y, fill='orange'), alpha = 0.5)

# boxplot ----
datasaurus %>%
    ggplot(aes(x=x, y=x)) +
    geom_boxplot() 

datasaurus %>%
    ggplot(aes(x=y, y=y)) +
    geom_boxplot() 

# boxplot for both variables
datasaurus %>%
    pivot_longer(cols = c(x,y), names_to = "name", values_to = "value") %>%
    ggplot(aes(x=name, y=value, color=name)) +
    geom_boxplot() +
    geom_jitter(alpha=0.2)


# scatter plot ----

# correlation btwn x & y: -0.06601891
cor(datasaurus$x, datasaurus$y, use = "everything", method = c("pearson"))

# 95 CI: -0.11130666 -0.02045753
cor.test(datasaurus$x, datasaurus$y, use = "everything", method = c("pearson"))

# basic scatterplot
datasaurus %>%
    ggplot(aes(x=x, y=y)) +
    geom_point()

# 13 categories in the dataset ----

# categories include: 
# away, bullseye, circle, dino, dots, h_lines, high_lines
# slant_down, slant_up, star, v_lines, wide_lines, x_shape

# scatter plot - facet_wrap
datasaurus %>%
    ggplot(aes(x=y, y=x, color=dataset)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~dataset) +
    theme_classic() +
    theme(legend.position = "none")

# dino
datasaurus %>%
    filter(dataset=="dino") %>%
    ggplot(aes(x=x, y=y)) +
    geom_point()


datasaurus %>%
    filter(dataset=="x_shape") %>%
    ggplot(aes(x=x, y=y)) +
    geom_point()

# data wrangling ----

# all categories have n = 142 rows
datasaurus %>%
    group_by(dataset) %>%
    tally(sort = TRUE)

# pivot_wider w/ x
datasaurus %>%
    pivot_wider(names_from = dataset, values_from = x) %>%
    view()

# pivot_wider w/ y
datasaurus %>%
    pivot_wider(names_from = dataset, values_from = y) %>%
    view()

# Correlogram ----
install.packages("corrgram")
install.packages("GGally")
library(GGally)


# sample half-matrix correlelogram
corrgram(mtcars, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Car Milage Data (unsorted)")

corrgram(datasaurus, order = NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt)


mtcars %>% view()




# GGally ----

# external example - c reate data
sample <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1))
sample$v4 = sample$var1 ** 2
sample$v5 = -(sample$var1 ** 2)

# Nice visualization of correlations
ggcorr(sample, method = c("everything", "pearson"))

# split datasaurus into just X value ----

widesaurus_x <- datasaurus %>%
    pivot_wider(names_from = dataset, values_from = x) %>%
    select(-y)


# away - 142
away <- widesaurus_x %>%
    select(away) %>%
    drop_na()

# dino - 103
dino <- widesaurus_x %>%
    select(dino) %>%
    drop_na()

# h_lines - 142
hlines <- widesaurus_x %>%
    select(h_lines) %>%
    drop_na()

# v_lines - 142
vlines <- widesaurus_x %>%
    select(v_lines) %>%
    drop_na()

# x_shape - 142
xshape <- widesaurus_x %>%
    select(x_shape) %>%
    drop_na()

# star - 142
star <- widesaurus_x %>%
    select(star) %>%
    drop_na()

# high_lines - 142
highlines <- widesaurus_x %>%
    select(high_lines) %>%
    drop_na()

# dots - 142
dots <- widesaurus_x %>%
    select(dots) %>%
    drop_na()

# circle - 142
circle <- widesaurus_x %>%
    select(circle) %>%
    drop_na()

# bullseye - 142
bullseye <- widesaurus_x %>%
    select(bullseye) %>%
    drop_na()

# slant_up - 142
slant_up <- widesaurus_x %>%
    select(slant_up) %>%
    drop_na()

# slant_down - 142
slant_down <- widesaurus_x %>%
    select(slant_down) %>%
    drop_na()

# wide_lines - 142
wide_lines <- widesaurus_x %>%
    select(wide_lines) %>%
    drop_na()

# bind_cols, exclude: dino (just x values) ----

# note: omit_dino is a data.frame, however each column is a LIST of numbers (need to unlist)
# unlisting changes the column names to something else
omit_dino <- bind_cols(unlist(away), unlist(bullseye), unlist(circle), unlist(dots), unlist(highlines), unlist(hlines), unlist(slant_down), unlist(slant_up), unlist(star), unlist(vlines), unlist(wide_lines), unlist(xshape))


omit_dino2 <- omit_dino %>%
    rename(
        away = ...1,
        bullseye = ...2,
        circle = ...3,
        dots = ...4,
        highlines = ...5,
        hlines = ...6,
        slant_down = ...7,
        slant_up = ...8,
        star = ...9,
        vlines = ...10,
        wide_lines = ...11,
        xshape = ...12
    )

# GGally correlelogram
ggcorr(omit_dino2, method = c("everything", "pearson"))

# Final

# 2D Histogram
datasaurus %>%
    filter(dataset=='dino') %>%
    ggplot(aes(x=x, y=y)) +
    geom_bin2d(bins=70) +
    scale_fill_continuous(type = 'viridis') +
    theme_bw()
    
# 2D histogram
datasaurus %>%
    filter(dataset=='dino') %>%
    ggplot(aes(x=x, y=y)) +
    geom_bin2d() +
    theme_bw()

# Hexbin
datasaurus %>%
    filter(dataset=='dino') %>%
    ggplot(aes(x=x, y=y)) +
    geom_hex() +
    scale_fill_continuous(type = "viridis") +
    theme_bw()

# show contour geom_density
datasaurus %>%
    filter(dataset=='dino') %>%
    ggplot(aes(x=x, y=y)) +
    geom_density2d()



# show area geom_density ----

# contour only 
datasaurus %>%
    #filter(dataset=='bullseye') %>%
    ggplot(aes(x=x, y=y)) +
    geom_density_2d() +
    theme_classic() +
    facet_wrap(~dataset)


# area only 
datasaurus %>%
    #filter(dataset=='bullseye') %>%
    ggplot(aes(x=x, y=y)) +
    stat_density_2d(aes(fill=y),geom = "polygon") +
    theme_classic() +
    facet_wrap(~dataset)
    
    
# area + contour
datasaurus %>%
    #filter(dataset=='bullseye') %>%
    ggplot(aes(x=x, y=y)) +
    stat_density_2d(aes(fill=y),geom = "polygon", colour="white") +
    theme_classic() +
    facet_wrap(~dataset)


# using raster
datasaurus %>%
    #filter(dataset=='bullseye') %>%
    ggplot(aes(x=x, y=y)) +
    stat_density_2d(aes(fill=y), geom = "raster", colour=FALSE) +
    theme_classic() +
    facet_wrap(~dataset)




    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(legend.position = 'none')



# facet wrap
datasaurus %>%
    ggplot(aes(x=y, y=x, color=dataset)) +
    geom_bin2d(bins = 25) +
    scale_fill_continuous(type = "viridis") +
    facet_wrap(~dataset)
    
    
    

