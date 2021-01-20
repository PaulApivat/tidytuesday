library(tidyverse)

df <- read_csv("V-Dem-CY-Core-v10.csv")

# High-Level Democracy Indices ----

# Electoral Democracy Index (D) ----
# select columns - 
df %>%
    select(country_name, year, v2x_polyarchy) %>%
    filter(year == 2019) %>%
    arrange(desc(v2x_polyarchy)) %>%
    ggplot(aes(x = reorder(country_name, v2x_polyarchy), y = v2x_polyarchy)) +
    geom_bar(stat = 'identity') +
    theme(
        axis.text.x = element_text(angle = 90)
    ) +
    coord_flip()

# United States
df %>%
    select(country_name, year, v2x_polyarchy) %>%
    filter(country_name == 'United States of America') %>% 
    ggplot(aes(x = year, y = v2x_polyarchy)) +
    geom_bar(stat = 'identity', fill = 'white') +
    theme(
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        panel.grid.major.y = element_line(color = 'grey'),
        plot.title = element_text(colour = 'white'),
        plot.subtitle = element_text(colour = 'white'),
        plot.caption = element_text(colour = 'white'),
        axis.text.x = element_text(color = 'white'),
        axis.text.y = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white')
    ) +
    labs(
        title = "Electoral Democracy Index (0.0 - 1.0)",
        subtitle = "United States of America: 1789 - 2019",
        y = "Index",
        x = "",
        caption = "Data: University of Gothenburg, V-Dem Institute | Graphic: @paulapivat"
    )


# Country Comparison
df %>%
    select(country_name, year, v2x_polyarchy) %>%
    filter(country_name %in% countries) %>% 
    ggplot(aes(x = year, y = v2x_polyarchy, fill = country_name)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~country_name) +
    theme_minimal() +
    theme(
        legend.position = 'None',
        plot.background = element_rect(fill = '#D3D3D3'),
        #panel.grid.minor = element_line(colour = 'grey'),
        panel.grid.major = element_line(color = '#D3D3D3'),
        panel.grid.major.x = element_line(color = '#D3D3D3'),
        panel.grid.minor.x = element_line(color = '#D3D3D3'),
        plot.title = element_text(face = 'bold', size = 15)
    ) +
    labs(
        title = "Electoral Democracy Index (0.0 - 1.0)",
        subtitle = "Years: 1789 - 2019",
        y = "Index",
        x= "",
        fill = "Country Name",
        caption = "Data: University of Gothenburg, V-Dem Institute | Graphic: @paulapivat"
    )+
    scale_fill_brewer(palette = "BuGn")


    

# Cean Elections Index
# min: 0, max: 0.98
df %>%
    select(country_name, year, v2xel_frefair) %>%
    filter(country_name == 'Thailand' | country_name == 'United States of America') %>%
    ggplot(aes(x = year, y = v2xel_frefair, fill = country_name)) +
    geom_bar(stat = 'identity')


# Liberal Democracy ----
countries = c("Thailand", "United States of America", "Canada", "China", "France", "Venezuela", "Zimbabwe", "India", "Hong Kong")

# Summary Liberal Democracy
df %>%
    select(country_name, year, v2x_libdem) %>%
    filter(country_name %in% countries) %>%
    summary()

# Facet Wrap: Country Comparison
df %>%
    select(country_name, year, v2x_libdem) %>%
    filter(country_name %in% countries) %>% 
    ggplot(aes(x = year, y = v2x_libdem, fill = country_name)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~country_name) +
    theme_minimal() +
    theme(
        legend.position = 'None',
        plot.background = element_rect(fill = '#D3D3D3'),
        #panel.grid.minor = element_line(colour = 'grey'),
        panel.grid.major = element_line(color = '#D3D3D3'),
        panel.grid.major.x = element_line(color = '#D3D3D3'),
        panel.grid.minor.x = element_line(color = '#D3D3D3'),
        plot.title = element_text(face = 'bold', size = 15)
    ) +
    labs(
        title = "Liberal Democracy Index (0.0 - 1.0)",
        subtitle = "Years: 1789 - 2019",
        y = "Index",
        x= "",
        fill = "Country Name",
        caption = "Data: University of Gothenburg, V-Dem Institute | Graphic: @paulapivat"
    )+
    scale_fill_brewer(palette = "BuGn")

# United States
df %>%
    select(country_name, year, v2x_libdem) %>%
    filter(country_name == 'United States of America') %>% 
    ggplot(aes(x = year, y = v2x_libdem)) +
    geom_bar(stat = 'identity', fill = 'white') +
    theme(
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        panel.grid.major.y = element_line(color = 'grey'),
        plot.title = element_text(colour = 'white'),
        plot.subtitle = element_text(colour = 'white'),
        plot.caption = element_text(colour = 'white'),
        axis.text.x = element_text(color = 'white'),
        axis.text.y = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white')
    ) +
    labs(
        title = "Liberal Democracy Index (0.0 - 1.0)",
        subtitle = "United States of America: 1789 - 2019",
        y = "Index",
        x = "",
        caption = "Data: University of Gothenburg, V-Dem Institute | Graphic: @paulapivat"
    )


# Summary Electoral Democracy 

df %>%
    select(country_name, year, v2x_polyarchy) %>%
    filter(country_name %in% countries) %>% 
    ggplot(aes(x = year, y = v2x_polyarchy, fill = country_name)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~country_name)
