# library

library(tidyverse)

# get data

bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

# explore Big Mac in USD

names(bigmac)

# title = "Dollar Price of Big Macs: China, EU, Japan, US and Thailand"
bigmac %>%
    select(date, name, dollar_price) %>%
    filter(name=="United States" | name=="Euro area" | name=="Japan" | name=="China" | name=="Thailand") %>%
    ggplot(aes(x = date, y = dollar_price, color = name)) +
    geom_line(size = 1.5) +
    theme_minimal() +
    labs(
        title = "Dollar Price of Big Macs: China, EU, Japan, US, Thailand",
        subtitle = "2000 - 2020"
    ) +
    theme(legend.position = 'bottom')


# title = "Local Currency Unit Per Dollar: China, Thailand, Japan, US"
bigmac %>%
    select(date, name, dollar_ex) %>%
    filter(name=="United States" | name=="Euro area" | name=="Japan" | name=="China" | name=="Thailand") %>%
    ggplot(aes(x = date, y = dollar_ex, color = name)) +
    geom_line(size = 1.5) +
    theme_minimal() +
    labs(
        title = "Local Currency Unit Per Dollar: China, Thailand, Japan, US",
        subtitle = "2000 - 2020"
    ) +
    theme(legend.position = 'bottom')


bigmac %>%
    select(date, name, usd_raw) %>%
    filter(name=="United States" | name=="Euro area" | name=="Japan" | name=="China" | name=="Thailand") %>%
    ggplot(aes(x = date, y = usd_raw, color = name)) +
    geom_line(size = 1.5) +
    theme_minimal() +
    labs(
        title = "Raw Index, relative to US dollar: China, Thailand, Japan, US",
        subtitle = "2000 - 2020"
    ) +
    theme(legend.position = 'bottom')


bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(aes(x = date, y = usd_raw, color = ifelse(usd_raw > 0, name, NA))) +
    geom_line(size = 1.5, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    geom_hline(yintercept = 0, color = 'black', size = 2) +
    scale_color_discrete(na.value='white')

