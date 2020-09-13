#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6
sessionInfo()

# libraries ----
library(tidyverse)
library(readxl)

# load data ----
data <- read_xlsx("sdg_goal_4.4.xlsx")

# pivot wider ----

# Examine by Sub-ICT-Skill
data %>%
    #filter(GeoAreaName=="Morocco" | GeoAreaName=="Qatar") %>% 
    select(GeoAreaName, TimePeriod, Sex, `Type of skill`, Value) %>%
    #group_by(GeoAreaName, TimePeriod, Sex, `Type of skill`, Value) %>%
    #ungroup() %>%
    pivot_wider(names_from = `Type of skill`, values_from = Value) 