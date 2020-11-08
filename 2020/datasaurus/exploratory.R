# sessioninfo
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.6

# load libraries
library(tidyverse)

# load data
datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

# summary statistics
datasaurus %>%
    summary()


# single coordinate plotting 

# histogram
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

# boxplot
datasaurus %>%
    ggplot(aes(x=x, y=x)) +
    geom_boxplot() 

datasaurus %>%
    ggplot(aes(x=y, y=y)) +
    geom_boxplot() 

# boxplot for both variables
datasaurus %>%
    pivot_longer(cols = c(x,y), names_to = "name", values_to = "value") %>%
    ggplot(aes(x=name, y=value)) +
    geom_boxplot()


