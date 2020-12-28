# library
library(tidyverse)

# data
bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

# FINAL
# highlighting United States specifically
final <- bigmac %>%
    select(date, name, usd_raw) %>%
    # re-order names based on usd_raw, better readability
    ggplot(aes(x = usd_raw, y = reorder(name, usd_raw))) +
    # golden yellow
    geom_point(size = 4, alpha = 0.7, color = ifelse(bigmac$usd_raw > 0.0, '#FFDF00', 
                                                     ifelse(bigmac$name=='United States', '#900000', 'whitesmoke'))) +
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


final

