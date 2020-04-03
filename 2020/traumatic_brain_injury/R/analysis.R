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
filter(age_group != 'Total' & age_group != '0-17') %>% 
ggplot(aes(x=age_group, y=number_est, fill = ifelse(grepl("Unintentional Falls", injury_mechanism), "red", "black"))) 
+ geom_bar(stat = "identity")
+ theme_classic()
+ labs(fill = "Injury Mechanism", y = "Numbers", x = "Age")

# Comparison of Injury Mechanism across Life Span, 
# have each injury mechanism represent on bar geom_bar, Position = Dodge
tbi_age %>% 
filter(age_group != 'Total' & age_group != '0-17' & injury_mechanism != 'Other or no mechanism specified') %>% 
ggplot(aes(x=age_group, y=number_est, fill = injury_mechanism)) 
+ geom_bar(stat = "identity", position = "dodge") 
+ theme_classic() 
+ labs(fill = "Injury Mechanism", y = "Numbers", x = "Age")

# convert injury_mechanism as character to factor
tbi_age$injury_mechanism <- as.factor(tbi_age$injury_mechanism)

# Comparing Injury Mechanisms across Life Span,
# Highlight diverging trend in brain trauma from Unintentional Falls
# manually change colors only for Factors
tbi_age %>% 
filter(age_group != 'Total' & age_group != '0-17' & injury_mechanism != 'Other or no mechanism specified') %>% 
ggplot(aes(x=age_group, y=number_est, fill = injury_mechanism)) 
+ geom_bar(stat = "identity", position = "dodge") 
+ theme_classic() 
+ labs(fill = "Injury Mechanism", y = "Numbers", x = "Age") 
+ scale_fill_manual(values = c("#276419", "#4d9221", "#7fbc41", "#b8e186", "#de77ae", "#e6f5d0"))

# Two levels of diversion (cool vs warm colors); all lighter shades
# highlight Unintentional Falls; only dark shade
injury_across_age_group <- tbi_age %>% 
filter(age_group != 'Total' & age_group != '0-17' & injury_mechanism != 'Other or no mechanism specified') %>% 
ggplot(aes(x=age_group, y=number_est, fill = injury_mechanism)) 
+ geom_bar(stat = "identity", position = "dodge") 
+ theme_classic() 
+ labs(title = "Injuries Across Life-Span", subtitle = "Data from 2006 - 2014", fill = "Injury Mechanism", y = "Numbers", x = "Age") 
+ scale_fill_manual(values = c("#a6cee3", "#b2df8a", "#cab2d6", "#fb9a99", "#e31a1c", "#fdbf6f"))

injury_across_age_group


##----- tbi_year ------##
# change injury_mechanism to factor
class(tbi_year$injury_mechanism)
tbi_year$injury_mechanism <- as.factor(tbi_year$injury_mechanism)


# display all years separately on x-axis



# plot basic barchart




## creation of 'temp' data frame, does NOT make sense
## figure out why

# select only type and year
tbi_year %>%
+ select(type, year) -> temp

# join temp with tbi_age
temp <- tbi_age %>%
+ inner_join(temp, by = "type")

# re-create previous plot using 'temp' data frame
# save to new data frame
injury_across_age_group2 <- temp %>% 
filter(age_group != 'Total' & age_group != '0-17' & injury_mechanism != 'Other or no mechanism specified') %>% 
ggplot(aes(x=age_group, y=number_est, fill = injury_mechanism)) 
+ geom_bar(stat = "identity", position = "dodge") 
+ theme_classic() 
+ labs(title = "Injuries Across Life-Span", subtitle = "Data from 2006 - 2014", fill = "Injury Mechanism", y = "Numbers", x = "Age") 
+ scale_fill_manual(values = c("#a6cee3", "#b2df8a", "#cab2d6", "#fb9a99", "#e31a1c", "#fdbf6f"))

# facet_wrap using years
injury_across_age_group2 + facet_wrap(~year, ncol = 2, dir = "v")


# read data from PDF
