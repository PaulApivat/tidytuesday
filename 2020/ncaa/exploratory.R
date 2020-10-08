# sessioninfo
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.6
sessionInfo()

# load libraries & data ----
library(tidyverse)

# load data manually
tournament <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

# Exploratory ----

# range of full_percent, full_w, full_l, filter 1990
tournament %>%
    filter(year==1990) %>%
    select(full_percent, full_w, full_l) %>%
    summarize(
        range_pct = range(full_percent),
        range_w = range(full_w),
        range_l = range(full_l)
    )

tournament %>%
    filter(year==1990) %>%
    arrange(desc(full_percent)) %>% 
    view()
