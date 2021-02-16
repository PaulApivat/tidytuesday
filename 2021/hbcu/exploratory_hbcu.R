library(tidyverse)

# load data ----
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

# The percentage of students broken down by race/ethnicity, aged 25 and over who have graduated HS.
bach <- read_csv("bachelors_or_higher_degree.csv")

# quick explore ----

# wide approach
# layer on multiple geom_lines
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


# pivot_longer approach
bach %>%
    select(total, white1, black1, hispanic, asian_pacific_islander_asian, asian_pacific_islander_pacific_islander, american_indian_alaska_native) %>% 
    mutate(
        white1 = as.numeric(white1),
        black1 = as.numeric(black1),
        hispanic = as.numeric(hispanic),
        asian_pacific_islander_asian = as.numeric(asian_pacific_islander_asian),
        asian_pacific_islander_pacific_islander = as.numeric(asian_pacific_islander_pacific_islander),
        american_indian_alaska_native = as.numeric(american_indian_alaska_native)
    ) %>% 
    rename(
        asian = asian_pacific_islander_asian,
        pacific_islander = asian_pacific_islander_pacific_islander,
        alaska_native = american_indian_alaska_native
    ) %>% 
    pivot_longer(!total, names_to = "ethnicity", values_to = "percent") %>% 
    ggplot(aes(x = total, y = percent, color = ethnicity)) +
    geom_line()

# save all ethnicity (1910 - 2016) as one variable ----
all_ethnicities <- bach %>%
    select(total, white1, black1, hispanic, asian_pacific_islander_asian, asian_pacific_islander_pacific_islander, american_indian_alaska_native) %>% 
    mutate(
        white1 = as.numeric(white1),
        black1 = as.numeric(black1),
        hispanic = as.numeric(hispanic),
        asian_pacific_islander_asian = as.numeric(asian_pacific_islander_asian),
        asian_pacific_islander_pacific_islander = as.numeric(asian_pacific_islander_pacific_islander),
        american_indian_alaska_native = as.numeric(american_indian_alaska_native)
    ) %>% 
    rename(
        asian = asian_pacific_islander_asian,
        pacific_islander = asian_pacific_islander_pacific_islander,
        alaska_native = american_indian_alaska_native
    ) %>%
    pivot_longer(!total, names_to = "ethnicity", values_to = "percent")

# show only from year 2003 - 2016 (data availability) 
all_ethnicities %>%
    filter(total > 2002) %>%
    ggplot(aes(x = total, y = percent, color = ethnicity)) +
    geom_line()
    

# show Standard Error ----
# needs work, no insight added
bach %>%
    select(total, white1, standard_errors_white1, black1, standard_errors_black1) %>%
    mutate(
        white1 = suppressWarnings(as.numeric(white1)),
        standard_errors_white1 = suppressWarnings(as.numeric(standard_errors_white1)),
        black1 = suppressWarnings(as.numeric(black1)),
        standard_errors_black1 = suppressWarnings(as.numeric(standard_errors_black1))
    ) %>% 
    filter(total > 1975) %>%
    ggplot() +
    geom_bar(aes(x = total, y = white1), stat = 'identity', fill = 'orange', position = position_dodge()) +
    geom_point(aes(x = total, y = white1 + standard_errors_white1))
    


# Boxplot ----
all_ethnicities %>%
    filter(total > 2002) %>%
    ggplot(aes(x = reorder(ethnicity, percent), y = percent, color = ethnicity)) +
    geom_boxplot() +
    geom_point(aes(x = ethnicity, y = percent)) +
    coord_flip()




