# library

library(tidyverse)

# get data

bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

# explore Big Mac in USD

names(bigmac)

bigmac %>%
    select(date, name, dollar_price) %>%
    filter(name=="United States" | name=="Euro area" | name=="Japan" | name=="China") %>%
    ggplot(aes(x = date, y = dollar_price, color = name)) +
    geom_line()



