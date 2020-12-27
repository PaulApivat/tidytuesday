# library
install.packages('gghighlight')
library(gghighlight)
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
# subtitle = "July, 2020"
# approximate squishing of scatterplot
bigmac %>%
    select(date, name, usd_raw) %>%
    filter(date=='2020-07-01') %>%
    ggplot(aes(x = usd_raw, y = date)) +
    geom_point(
               size = 1.5, 
               position = 'jitter'
               ) +
    geom_vline(xintercept = 0, color = 'red') +
    theme_minimal() +
    theme(
        legend.position = 'none',
        # expand distance between title and plot
        plot.title = element_text(margin = margin(0,0,10,0)),
        plot.subtitle = element_text(margin = margin(0,0,180,0)),
        # y-axis tick redundant with subtitle
        axis.text.y = element_blank()
        ) +
    labs(
        title = "Raw Index: Undervalued and Overvalued compared to US",
        subtitle = "July, 2020",
        y = ""
    ) + 
    # squish the plot
    # > 1 squish from sides
    # < 1 squish from top
    coord_fixed(ratio = 0.08) +
    scale_x_continuous(labels = scales::percent) +
    gghighlight(name=='Britain')
    
    


## FINAL ----
# title = "Raw Index: Undervalued and Overvalued compared to US"
bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(aes(x = usd_raw, y = name)) +
    # golden yellow
    geom_point(size = 4, alpha = 0.7, color = ifelse(bigmac$usd_raw > 0.0, '#FFDF00', 'whitesmoke')) +
    #geom_point(size = 1.5, alpha = 0.8, color = '#FFDF00') +
    geom_vline(xintercept = 0, color = '#900000') +
    theme(
        legend.position = 'none',
        # background options: patel blue gree #42ABC5 or 8FD0CA   light gray: #858b97
        panel.background = element_rect(fill = '#8FD0CA'),
        panel.grid.major = element_line(color = '#8FD0CA'),
        panel.grid.minor = element_line(color = '#8FD0CA'),
        plot.background = element_rect(fill = '#8FD0CA'),
        plot.title = element_text(face = 'bold', family = 'sans'),
        plot.subtitle = element_text(margin = margin(0,0,30,0), face = 'italic'),
        axis.text.x = element_text(face = 'bold', margin = margin(0,0,20,0)),
        axis.title.x = element_text(hjust = 0.35)
        ) +
    labs(
        title = "The Big Mac index",
        subtitle = "Using 'burgernomics' to show the exchange rate between each \ncountry's currency relative to the US dollar from 2000 - 2020.",
        x = "Raw index, relative to the US dollar",
        y = "",
        caption = "Data: The Economist | Visual: @paulapivat"
    ) +
    scale_x_continuous(labels = scales::percent)
    #coord_flip() 


names(pdfFonts())

install.packages('gghighlight')
library(gghighlight)


bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(mapping = aes(x = date, y = usd_raw)) +
    geom_point() +
    geom_line(aes(group = ifelse(name=='Britain', name, FALSE), 
                  color = ifelse(name=='Britain', 'black', FALSE))) +
    theme(
        panel.background = element_rect(fill = 'white'),
        legend.position = 'none'
    )

bigmac %>%
    select(date, name, usd_raw) %>%
    ggplot(mapping = aes(x = date, y = usd_raw, color = name)) +
    geom_line() +
    gghighlight(usd_raw > 0.0) +
    theme(
        panel.background = element_rect(fill = 'white'),
        legend.position = 'none'
    ) +
    facet_wrap(~name)


