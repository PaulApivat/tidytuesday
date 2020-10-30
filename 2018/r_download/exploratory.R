# library
library(tidyverse)

# read data
# source: TidyTuesday: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-10-30
df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-30/r_downloads_year.csv")


# Dates: 2017-10-23    2018-10-20
# A Year's worth of R language downloads
df %>%
    select(X1, date, country) %>%
    group_by(date) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x=date, y=n)) + 
    geom_line() +
    labs(
        y = "Daily Downloads (R)",
        x = "Date",
        title = "R Language Daily Downloads from RStudio CRAN Mirror",
        subtitle = "Oct 20, 2017 - Oct 20, 2018"
    )

df %>%
    select(X1, date, country) %>%
    group_by(country) %>%
    tally(sort = TRUE) %>%
    filter(country != 'US') %>% 
    head(30) %>%
    # note: reorder AND desc 
    ggplot(aes(x = reorder(country, desc(n)), y = n, fill=n)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
        x = "Countries",
        y = "Total Downloads",
        title = "Total R Language Downloads by Country",
        subtitle = "Top-30 non-US, October, 2017-18"
    )



