# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# libraries
library(tidyverse)

# get data from TidyTuesday
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
# get data from Kaggle
tdf_stages <- read_csv("stages_TDF.csv")

#### Exploratory Data Analysis

# scatter plot showing decreasing overall time & distance throughout the years
ggplot(data = tdf_winners, mapping = aes(x=start_date, y=time_overall)) + geom_point()
ggplot(data = tdf_winners, mapping = aes(x=start_date, y=distance)) + geom_point()

# lollipop chart of winner_names (arranged alphabetically) and distance travelled
tdf_winners %>% 
    arrange(distance) %>% 
    ggplot(aes(x=winner_name, y=distance)) 
    + geom_segment(aes(xend=winner_name, yend=0)) 
    + geom_point(size=4, color='orange') 
    + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## re-ordering distance
tdf_winners %>% 
    arrange(distance) %>% 
    # reorder winner_name by distance
    ggplot(aes(x=reorder(winner_name, distance), y=distance)) 
    + geom_segment(aes(xend=winner_name, yend=0)) 
    + geom_point(size=4, color='orange') 
    + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## re-ordering winners by distance traveled (arranged in descending left-to-right)
tdf_winners %>% 
    arrange(distance) %>% 
    ggplot(aes(x=reorder(winner_name, desc(distance)), y=distance)) 
    + geom_segment(aes(xend=winner_name, yend=0)) 
    + geom_point(size=4, color='orange') 
    + theme(axis.text.x = element_text(angle = 90, hjust = 1))


## group_by birth_country of winner (France: 36, Belgium: 19, Spain: 12, Italy: 11, USA: 10)
tdf_winners %>%
+ group_by(birth_country) %>%
+ tally(sort = TRUE)

## change color by 'number' of tour de france(s) won
# step 1: group by winner_name, tally
tdf_winners %>%
    group_by(winner_name) %>%
    tally(sort = TRUE)


# what *could* be combined? distance + time_overall

