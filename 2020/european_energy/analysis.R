# Session Info
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.5



# Cleaning Script (Excel) ----

# libraries
library(tidyverse)
library(readxl)

# unavailable package
install.packages('countrycodes')
library(countrycodes)

# read raw excel
raw_excel <- read_excel("./data/Electricity_generation_statistics_2019.xlsx", sheet = 3)
glimpse(raw_excel)

raw_excel %>% view()
