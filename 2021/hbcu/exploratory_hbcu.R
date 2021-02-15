library(tidyverse)

# load data ----
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

bach <- read_csv("bachelors_or_higher_degree.csv")

# quick explore ----
bach %>%
    select(1:2, 4, 6, 8, 10, 12, 14, american_indian_alaska_native) %>% 
    mutate(
        white1 = as.numeric(white1),
        black1 = as.numeric(black1),
        hispanic = as.numeric(hispanic),
        total_asian_pacific_islander = as.numeric(total_asian_pacific_islander),
        asian = as.numeric(asian_pacific_islander_asian),
        pacific_islander = as.numeric(asian_pacific_islander_pacific_islander),
        american_indian_alaska_native = as.numeric(american_indian_alaska_native)
    ) %>%
    ggplot(aes(x = total, y = total_percent_of_all_persons_age_25_and_over)) +
    geom_line() +
    geom_line(aes(x = total, y = white1), color = "red") +
    geom_line(aes(x = total, y = black1), color = "green") +
    geom_line(aes(x = total, y = hispanic), color = "blue") +
    geom_line(aes(x = total, y = total_asian_pacific_islander), color = "orange") +
    geom_line(aes(x = total, y = asian), color = "yellow") + 
    geom_line(aes(x = total, y = pacific_islander), color = "brown") +
    geom_line(aes(x = total, y = american_indian_alaska_native), color = "purple")


