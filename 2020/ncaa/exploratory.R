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

# full_percent - median (74.2), mean (74.24)
tournament %>%
    select(year, school, full_percent) %>%
    group_by(school) %>%
    summary()



library(lubridate)

# name_vector ----
name_vector <- tournament %>%
    select(year, school, full_percent) %>%
    group_by(school) %>%
    tally(sort = TRUE) %>% 
    ungroup() %>%
    head(30) %>%
    pull(school)

# quick visualizations ----

# facet wrap with horizontal median line
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line(aes(color=school)) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'red')

# data wrangling ----
# create another column neg/pos
# calculate, anything above median is pos, anything below is neg
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    #geom_area(aes(fill=direction)) +
    geom_line() +
    geom_point() +
    #geom_line(aes(color=school)) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black')

 
# geom_ribbon - fill="direction"
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(x = year, ymin = 74.2, ymax=full_percent, fill="direction"), alpha = 0.15) +
    #geom_ribbon() +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black')


# two shade ribbon
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(x = year, ymin = 74.2, ymax=full_percent, fill="red"), alpha = 0.15) +
    geom_ribbon(aes(x = year, ymin = full_percent, ymax = 74.2, fill="blue"), alpha = 0.15) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black')

# ifelse ribbon
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
    ggplot(aes(x=year, y=full_percent)) +
    #geom_line() +
    #geom_point() +
    geom_ribbon(aes(x = year, ymin = 74, ymax = full_percent, fill=ifelse(direction=='up', 'red', 'blue')), alpha = 0.15) +
    geom_ribbon(aes(x = year, ymin = full_percent, ymax = 74, fill=ifelse(direction=='up', 'red', 'blue')), alpha = 0.15) +
    facet_wrap(~school) +
    geom_hline(yintercept = 74, color = 'black') +
    geom_line()




# select full_percent, year, program
# long-to-wide pivot_wider
# were there some years where schools were below 50%?

tournament %>%
    select(year, school, full_percent) %>%
    pivot_wider(names_from = year, values_from = full_percent) %>% 
    view()

library(data.table)
class(tournament)


# Data Wrangling to fit geom_ribbon ----

# one color
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(
        median_percent = median(full_percent),
        subtract = full_percent - median_percent,
        positive = ifelse(subtract > 0, subtract, NA),
        negative = ifelse(subtract < 0, subtract, NA)
        ) %>%
    ggplot(aes(x=year, y=median_percent)) +
    geom_ribbon(aes(x=year, ymax=full_percent, ymin=median_percent), fill='blue', alpha=.5) +
    #geom_ribbon(aes(x=year, ymax=median_percent, ymin=full_percent), fill='red', alpha=.5) +
    facet_wrap(~school)


# two color, two lines
tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(
        median_percent = median(full_percent),
        # key to having two shades between two lines
        z = ifelse(full_percent > median_percent, full_percent, median_percent)
    ) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_ribbon(aes(ymin=median_percent, ymax=full_percent), fill='red', color='red') +
    # using z as ymax is the key to having two shaded colors
    geom_ribbon(aes(ymin=median_percent, ymax=z), fill='blue', color='blue') +
    geom_hline(yintercept = 76.7) +
    facet_wrap(~school)

# polish two lines two color plot

# final plot
tournament %>%
    select(year, school, full_percent) %>%
    # use name_vector to filter for programs with most data availability
    filter(school %in% name_vector) %>%
    mutate(
        median_percent = 74.2,
        # key to having two shades between two lines
        z = ifelse(full_percent > median_percent, full_percent, median_percent)
    ) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_ribbon(aes(ymin=median_percent, ymax=full_percent), fill='#46edc8') +
    # using z as ymax is the key to having two shaded colors
    geom_ribbon(aes(ymin=median_percent, ymax=z), fill='#374d7c') +
    geom_hline(yintercept = 74.2) +
    facet_wrap(~school) +
    theme(
        strip.text.x = element_text(face = 'bold', size = 10, family = 'Lato'),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = 'whitesmoke'),
        panel.background = element_rect(fill = 'whitesmoke'),
        plot.title = element_text(face = 'bold', family = 'Lato', size = 20)
    ) +
    labs(
        x = 'Years',
        y = 'Total Sum Win / Loss Percent (%)',
        caption = "Visualization: @paulapivat, Data: FiveThirtyEight,\n TidyTuesday 2020-10-06",
        title = "Sustained Excellence among NCAA Women's College Basketball Programs",
        subtitle = "These 30 programs were chosen based on data availability. They are benchmarked against the median win / loss percentages (%)\namong all programs. UConn is the gold standard for sustained excellence as is Stanford, Tennesee and Louisiana Tech.\nThese programs are consistently above the median, in some cases achieving high win/loss percentages over decades.\n"
    )
    
# Suggested Edits
# facet_wrap(scales = 'free_x')
library(lubridate)

tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(
        median_percent = 74.2,
        z = ifelse(full_percent > median_percent, full_percent, median_percent)
    ) %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_ribbon(aes(ymin=median_percent, ymax=full_percent), fill='#46edc8') +
    geom_ribbon(aes(ymin=median_percent, ymax=z), fill='#374d7c') +
    geom_hline(yintercept = 74.2) +
    facet_wrap(~school, scales = 'free_x') +
    #facet_wrap(~school, scales = 'free_x', labeller = label_bquote(.(school)-.(range(year)))) +
    theme(
        strip.text.x = element_text(face = 'bold', size = 10, family = 'Lato'),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = 'whitesmoke'),
        panel.background = element_rect(fill = 'whitesmoke'),
        plot.title = element_text(face = 'bold', family = 'Lato', size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    labs(
        x = 'Years',
        y = 'Total Sum Win / Loss Percent (%)',
        caption = "Visualization: @paulapivat, Data: FiveThirtyEight,\n TidyTuesday 2020-10-06",
        title = "Sustained Excellence among NCAA Women's College Basketball Programs",
        subtitle = "These 30 programs were chosen based on data availability. They are benchmarked against the median win / loss percentages (%)\namong all programs. UConn is the gold standard for sustained excellence as is Stanford, Tennesee and Louisiana Tech.\nThese programs are consistently above the median, in some cases achieving high win/loss percentages over decades.\n"
    )



# calculate min and max years (year_range)
year_range <- tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    group_by(school) %>%
    summarize(
        min_year = min(year), 
        max_year = max(year)
    ) %>%
    ungroup() 

# create a dataframe of selected schools to join with year_range
selected_school <- tournament %>%
    select(year, school, full_percent) %>%
    filter(school %in% name_vector) %>%
    mutate(
        median_percent = 74.2,
        z = ifelse(full_percent > median_percent, full_percent, median_percent)
    )

# join year_range and selected_school
tournamemt2 <- selected_school %>%
    left_join(year_range, by = 'school')

# Adjusted panel label ----

# rename the factors of the variable used for faceting
tournamemt2 
tournamemt2$school <- as.factor(tournamemt2$school)
levels(tournamemt2$school) <- c("Auburn, 1982-2017", "DePaul, 1990-2018", "Duke, 1987-2018", "George W., 1991-2018",
                                "Georgia, 1982-2018", "Green Bay, 1994-2018", "Iowa, 1986-2018", "Louisiana T., 1982-2011",
                                "Louisville, 1983-2018", "LSU, 1984-2018", "Maryland, 1982-2018", "Montana, 1983-2015",
                                "NC State, 1982-2018", "North Carolina, 1983-2015", "Notre Dame, 1992-2018", "Ohio St., 1982-2018",
                                "Oklahoma, 1986-2018", "Old Dominion, 1982-2008", "Penn St., 1982-2014", "Purdue, 1989-2017",
                                "Rutgers, 1986-2015", "Stanford, 1982-2018", "Tennessee, 1982-2018", "Texas, 1983-2018",
                                "Texas Tech, 1984-2013", "UConn, 1989-2018", "Vanderbilt, 1986-2014", "Virginia, 1984-2018",
                                "Washington, 1985-2017", "Western Ky., 1985-2018")
levels(tournamemt2$school)


# join year_range and selected_school
tournamemt2 %>%
    ggplot(aes(x=year, y=full_percent)) +
    geom_line() +
    geom_ribbon(aes(ymin=median_percent, ymax=full_percent), fill='#46edc8') +
    geom_ribbon(aes(ymin=median_percent, ymax=z), fill='#374d7c') +
    geom_hline(yintercept = 74.2) +
    #facet_wrap(~school, scales = 'free_x') +
    facet_wrap(~school, scales = 'free_x') +
    theme(
        strip.text.x = element_text(face = 'bold', size = 10, family = 'Lato'),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = 'whitesmoke'),
        panel.background = element_rect(fill = 'whitesmoke'),
        plot.title = element_text(face = 'bold', family = 'Lato', size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    labs(
        x = 'Years',
        y = 'Total Sum Win / Loss Percent (%)',
        caption = "Visualization: @paulapivat, Data: FiveThirtyEight,\n TidyTuesday 2020-10-06",
        title = "Sustained Excellence among NCAA Women's College Basketball Programs",
        subtitle = "These 30 programs were chosen based on data availability. They are benchmarked against the median win / loss percentages (%)\namong all programs. UConn is the gold standard for sustained excellence as is Stanford, Tennesee and Louisiana Tech.\nThese programs are consistently above the median, in some cases achieving high win/loss percentages over decades.\n"
    )













