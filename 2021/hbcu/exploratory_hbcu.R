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
    geom_line(aes(x = total, y = american_indian_alaska_native), color = "purple") +
    geom_ribbon(aes(ymin=black1, ymax=white1), fill='blue') +
    geom_ribbon(aes(ymin=hispanic, ymax=black1), fill='green')


# pivot_longer approach ----
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
        Asian = asian_pacific_islander_asian,
        `Pacific Islander` = asian_pacific_islander_pacific_islander,
        `Alaska Native` = american_indian_alaska_native,
        White = white1,
        Black = black1,
        Hispanic = hispanic
    ) %>% 
    filter(total > 1939) %>%
    pivot_longer(!total, names_to = "Ethnicity", values_to = "Percent") %>% 
    ggplot(aes(x = total, y = Percent, color = Ethnicity)) +
    geom_line(size = 1.5) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
    ) +
    labs(
        x = "",
        title = "Bachelor's Degree Attainment",
        subtitle = "Among Persons Aged 25 and Older",
        caption = "Data: National Center for Education Statistics | Graphic: @paulapivat"
    ) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"))


# Geom_line White, Black, Hispanic ----
# with geom_ribbon - must use pivot_wider approach
install.packages('ggtext')
library(ggtext)




bach %>%
    select(total, white1, black1, hispanic) %>% 
    mutate(
        white1 = suppressWarnings(as.numeric(white1)),
        black1 = suppressWarnings(as.numeric(black1)),
        hispanic = suppressWarnings(as.numeric(hispanic))
    ) %>% 
    filter(total > 1939) %>%
    ggplot(aes(x = total)) +
    geom_line(aes(x = total, y = white1), color = "#e6ab02", size = 1.5) +
    geom_line(aes(x = total, y = black1), color = "#7570b3", size = 1.5) +
    geom_line(aes(x = total, y = hispanic), color = "#e7298a", size = 1.5) +
    geom_ribbon(aes(ymin = black1, ymax = white1), fill = 'grey', alpha = 0.5) +
    geom_ribbon(aes(ymin = hispanic, ymax = black1), fill = 'pink', alpha = 0.5) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(1930, 2025, by = 10)) +
    xlim(1930, 2025) +
    theme(
        panel.grid.minor = element_blank(),
        plot.caption = element_text(margin = margin(30,20,0,0))
    ) +
    labs(
        x = NULL,
        y = "Percent",
        caption = 'Data: National Center for Education Statistics | Graphic: @paulapivat'
    ) +
    annotate("text", x = 1940, y = 35, label = 'Widening Gap', hjust = 0, vjust = 1, size = 8, fontface = "bold") +
    annotate("text", x = 1940, y = 33, label = "Bachelors' Degree Attainment, 1940 - 2016", hjust = 0, vjust = 1, size = 6) +
    annotate("text", x = 1938, y = 3, label = "3.6%") +
    annotate("text", x = 2017, y = 38, label = "White\nAmerican\n(37.3%)", hjust = 0) +
    annotate("text", x = 2017, y = 24, label = "Black\nAmerican\n(23.5%)", hjust = 0) +
    annotate("text", x = 2017, y = 17, label = "Hispanic\nAmerican\n(16.4%)", hjust = 0)
    




# Geom_line Asian, Pacific and Alaskan
bach %>%
    select(total, asian_pacific_islander_asian, asian_pacific_islander_pacific_islander, american_indian_alaska_native) %>% 
    mutate(
        asian_pacific_islander_asian = as.numeric(asian_pacific_islander_asian),
        asian_pacific_islander_pacific_islander = as.numeric(asian_pacific_islander_pacific_islander),
        american_indian_alaska_native = as.numeric(american_indian_alaska_native)
    ) %>% 
    rename(
        Asian = asian_pacific_islander_asian,
        `Pac_Islander` = asian_pacific_islander_pacific_islander,
        `Alaskan` = american_indian_alaska_native,
    ) %>% 
    filter(total > 2002) %>%
    ggplot(aes(x=total)) +
    geom_line(aes(x = total, y = Asian), color = '#d95f02', size = 1.5) +
    geom_line(aes(x = total, y = Pac_Islander), color = '#66a61e', size = 1.5) +
    geom_line(aes(x = total, y = Alaskan), color = '#1b9e77', size = 1.5) +
    geom_ribbon(aes(ymin = Pac_Islander, ymax = Asian), fill = 'grey', alpha = 0.5) +
    geom_ribbon(aes(ymin = Alaskan, ymax = Pac_Islander), fill = 'orange', alpha = 0.5) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2003, 2020, by = 1)) +
    theme(
        panel.grid.minor = element_blank(),
        plot.caption = element_text(margin = margin(30,20,0,0))
    ) +
    labs(
        x = NULL,
        y = "Percent",
        caption = 'Data: National Center for Education Statistics | Graphic: @paulapivat'
    ) +
    annotate("text", x = 2003, y = 60, label = 'Persistent Gap', hjust = 0, vjust = 1, size = 8, fontface = "bold") +
    annotate("text", x = 2003, y = 58, label = "Bachelors' Degree Attainment, 2003 - 2016", hjust = 0, vjust = 1, size = 6) +
    annotate("text", x = 2016.8, y = 57, label = "Asian (56.4%)") +
    annotate("text", x = 2016.5, y = 28, label = "Pacific\nIslander\n(27.5%)") +
    annotate("text", x = 2016.5, y = 17, label = "Alaska\nNative\n(16.8%)")




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
    geom_smooth(method = 'loess', formula = 'y ~ x') +
    facet_wrap(~ethnicity)
    

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




