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

# title = "Raw Index, relative to US dollar: China, Thailand, Japan, US"
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


# title = "Raw Index, relative to US dollar: All countries above the US
bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(aes(x = date, y = usd_raw, color = ifelse(usd_raw > 0, name, NA))) +
    geom_line(size = 1.5, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    geom_hline(yintercept = 0, color = 'black', size = 2) +
    scale_color_discrete(na.value='white') +
    labs(
        title = "Raw Index, relative to US dollar: All countries above the US"
    )

# title = "Raw Index: Undervalued and Overvalued compared to US"
# July 2020
bigmac %>%
    select(date, name, usd_raw) %>%
    filter(date=='2020-07-01') %>%
    ggplot(aes(x = usd_raw, y = name, fill = name)) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = 0, color = 'red') +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(
        title = "Raw Index: Undervalued and Overvalued compared to US",
        subtitle = "July, 2020"
    )
    
# title = "Raw Index: Undervalued and Overvalued compared to US"
bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(aes(x = usd_raw, y = name)) +
    # golden yellow
    geom_point(size = 1.5, alpha = 0.8, color = '#FFDF00') +
    geom_vline(xintercept = 0, color = 'red') +
    theme_minimal() +
    theme(
        legend.position = 'none',
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
    labs(
        title = "Raw Index: Undervalued and Overvalued compared to US",
        subtitle = "2000 - 2020"
    ) +
    scale_x_continuous(breaks = seq(-1.0, 1.5, by = 0.25)) +
    coord_flip()




