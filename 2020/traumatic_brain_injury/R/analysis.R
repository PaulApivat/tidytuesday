# load library
library(tidyverse)

# load library for scrapping from PDF
library(rvest)

# read data from CSV
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

### EDA tbi_age

# check class
class(tbi_age$age_group)
# convert age_group as character to factor
tbi_age$age_group <- as.factor(tbi_age$age_group)


# see all levels [1] "0-17"  "0-4"   "15-24" "25-34" "35-44" "45-54" "5-14"  "55-64" "65-74" "75+"   "Total"
tbi_age %>% sapply(levels)
# reorder factor levels for age_group
tbi_age$age_group = factor(tbi_age$age_group, levels(tbi_age$age_group)[c(2, 7, 3, 4, 5, 6, 8, 9, 10, 11, 1)])

## Plot
# disable scientific notation
options(scipen = 999)

# Unintentional Falls vs Everything else, across age groups
tbi_age %>% 
filter(age_group != 'Total') %>% 
ggplot(aes(x=age_group, y=number_est, fill = ifelse(grepl("Unintentional Falls", injury_mechanism), "red", "black"))) 
+ geom_bar(stat = "identity")
+ theme_classic()

# have each injury mechanism represent on bar geom_bar


# read data from PDF
