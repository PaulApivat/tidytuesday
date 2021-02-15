library(tidyverse)

# load data ----
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

bach <- read_csv("bachelors_or_higher_degree.csv")

# quick explore ----
bach %>%
    select(1:2) %>%
    ggplot(aes(x = total, y = total_percent_of_all_persons_age_25_and_over)) +
    geom_line()

