#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6
sessionInfo()

# libraries ----
install.packages('gt')
library(gt)
library(tidyverse)
library(readxl)


# load data ----
data <- read_xlsx("sdg_goal_4.4.xlsx")

# pivot wider ----

# Examine by Sub-ICT-Skill

# apply gt
data %>%
    #filter(GeoAreaName=="Morocco" | GeoAreaName=="Qatar") %>% 
    select(GeoAreaName, TimePeriod, Sex, `Type of skill`, Value) %>%
    #group_by(GeoAreaName, TimePeriod, Sex, `Type of skill`, Value) %>%
    #ungroup() %>%
    pivot_wider(names_from = `Type of skill`, values_from = Value) %>%
    filter(Sex=='MALE') %>%
    filter(TimePeriod==2015) %>%
    # mutate COPA:CMFL from chr to num
    mutate(
        COPA = as.numeric(COPA),
        EMAIL = as.numeric(EMAIL),
        SOFT = as.numeric(SOFT),
        EPRS = as.numeric(EPRS),
        PCPR = as.numeric(PCPR),
        INST = as.numeric(INST),
        ARSP = as.numeric(ARSP),
        TRAF = as.numeric(TRAF),
        CMFL = as.numeric(CMFL)
    ) %>%
    # Add Groups to Table
    group_by(GeoAreaName) %>%
    gt(rowname_col = "men") %>%
    tab_header(
        title = 'Specific ICT Skills',
        subtitle = 'By Country from 2014 - 2019'
    ) %>%
    fmt_number(
        columns = 4:12,
        decimals = 2
    ) %>%
    # unable to generate summary_rows
    summary_rows(
        groups = TRUE,
        columns = vars(`COPA`, `EMAIL`, `SOFT`, `EPRS`, `PCPR`, `INST`, `ARSP`, `TRAF`, `CMFL`),
        fns = list(
            avg = ~mean(.),
            sd = ~sd(.)
        )
    ) 
    



# Exploratory ----
data %>%
    summarize(
        unique_timeperiod = unique(TimePeriod)
    )

str(data$TimePeriod)
