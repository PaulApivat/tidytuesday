library(tidyverse)

# Load Data ----
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')


bach <- read_csv("bachelors_or_higher_degree.csv")

# Merge bach + student_debt ----

# data availability in student_debt (since 1989) 
# (create bach1)
bach1 <- bach %>%
    select(total, white1, black1, hispanic) %>% 
    slice(14:41)

# pivot_wider loan_debt, so white, black, hispanic get their own columns
# pivot_wider loan_debt_pct
loan_debt <- student_debt %>%
    select(1:3) %>%
    pivot_wider(names_from = race, values_from = loan_debt) %>%
    arrange(year)


loan_debt_pct <- student_debt %>%
    select(1:2, 4) %>%
    pivot_wider(names_from = race, values_from = loan_debt_pct) %>%
    arrange(year)


# select every 3rd row in bach1 to match student_debt
bach1 <- bach1[seq(1, nrow(bach1), 3),]

# combine bach1 and loan_debt by columns
loan_debt_bach <- bach1 %>%
    rename(
       year = total
    ) %>%
    left_join(loan_debt, by = "year") %>%
    mutate(
        white1 = as.numeric(white1),
        black1 = as.numeric(black1),
        hispanic = as.numeric(hispanic)
    )

# combine bach1 and loan_debt_pct by columns
loan_debt_pct_bach <- bach1 %>%
    rename(
        year = total
    ) %>%
    left_join(loan_debt_pct, by = "year") %>%
    mutate(
        white1 = as.numeric(white1),
        black1 = as.numeric(black1),
        hispanic = as.numeric(hispanic)
    )

# create new columns ----

diff_columns <- loan_debt_pct_bach %>%
    mutate(
        w_minus_b = white1 - black1,
        b_minus_h = black1 - hispanic,
        w_minus_h = white1 - hispanic
    )

# calculate correlation
cor.test(diff_columns$w_minus_b, diff_columns$Black)     # 0.85

cor.test(diff_columns$b_minus_h, diff_columns$Hispanic)  # 0.66

cor.test(diff_columns$w_minus_h, diff_columns$Hispanic)  # 0.68

# scatterplots ----

# Gap btwn white1 - black1 scatterplot with Black (share families with student debt)
# positive trend
loan_debt_pct_bach %>%
    mutate(
        w_minus_b = white1 - black1,
        b_minus_h = black1 - hispanic,
        w_minus_h = white1 - hispanic
    ) %>%
    ggplot(aes(x = w_minus_b, y = Black)) +
    geom_point(size = 13, color = "white") +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "#7570b3"),
        plot.background = element_rect(fill = "#7570b3"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(colour = "white", margin = margin(20,0,0,0)),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white"),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white", margin = margin(10,0,0,0)),
    ) +
    labs(
        x = "College Attainment Gap (White - Black Americans, %)",
        y = "Share of Black Families with Student Debt Loans",
        title = "Student Debt Burden and College Attainment Gap (r = 0.85)",
        subtitle = "African American Families",
        caption = "Data: Urban Institute | National Center for Education Statistics\nGraphic: @paulapivat"
    )
    



# Gap btwn black1-hispanic scatterplot with Hispanic (share families w student debt)
loan_debt_pct_bach %>%
    mutate(
        w_minus_b = white1 - black1,
        b_minus_h = black1 - hispanic,
        w_minus_h = white1 - hispanic
    ) %>%
    ggplot(aes(x = b_minus_h, y = Hispanic)) +
    geom_point(size = 13, color = "white") +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "#e7298a"),
        plot.background = element_rect(fill = "#e7298a"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(colour = "white", margin = margin(20,0,0,0)),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white"),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white", margin = margin(10,0,0,0)),
    ) +
    labs(
        x = "College Attainment Gap (Black - Hispanic Americans, %)",
        y = "Share of Hispanic Families with Student Debt Loans",
        title = "Student Debt Burden and College Attainment Gap (r = 0.66)",
        subtitle = "Hispanic American Families",
        caption = "Data: Urban Institute | National Center for Education Statistics\nGraphic: @paulapivat"
    )




# Gap btwn white1-hispanic scatterplot with Hispanic (share families w student debt)
loan_debt_pct_bach %>%
    mutate(
        w_minus_b = white1 - black1,
        b_minus_h = black1 - hispanic,
        w_minus_h = white1 - hispanic
    ) %>%
    ggplot(aes(x = w_minus_h, y = Hispanic)) +
    geom_point() +
    geom_smooth(method = "loess", formula = "y ~ x")

# straight forward loan debt percentage ----
# Share of families with student loan debt
student_debt %>%
    ggplot(aes(x = year, y = loan_debt_pct, fill = race)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = round(loan_debt_pct, digits = 2)), hjust = 0.5, vjust = 3, position = "stack") +
    scale_fill_manual(values = c("#7570b3", "#e7298a", "#e6ab02")) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.caption = element_text(margin = margin(30,20,0,0))
    ) +
    labs(
        x = NULL,
        y = "% of families with student loan debt",
        fill = "Race",
        title = "Share of Families with Student Loan Debt (Ages 25 - 55)",
        subtitle = "1989 - 2016",
        caption = "Data: Urban Institute | Graphic: @paulapivat"
    )



