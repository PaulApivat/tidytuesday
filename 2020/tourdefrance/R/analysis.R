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
    tally(sort = TRUE) -> multi_win

colnames(multi_win)[2] <- 'num_wins'

# join to add new column 'num_wins'
tdf_winners2 <- tdf_winners %>%
inner_join(multi_win, by = 'winner_name')

# two colors, >1 red, 1 orange
tdf_winners2 %>% 
    arrange(distance) %>% 
    ggplot(aes(x=reorder(winner_name, desc(distance)), y=distance)) 
    + geom_segment(aes(xend=winner_name, yend=0)) 
    # conditionally change color
    + geom_point(size=4, color = if_else(tdf_winners2$num_wins > 1, 'red', 'orange')) 
    + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# NOTE: given num_wins goes from 1-7, doing 7 nested if_else() statement is unwieldy
# convert num_wins from int to factor otherwise get "Error: Continuous value supplied to discrete scale"
tdf_winners3 <- tdf_winners2
tdf_winners3$num_wins <- as.factor(tdf_winners3$num_wins)


# match color of lollipop to x-axis winner_name

# step 1 create color column - nested ifelse()
# NOTE: the factor level for 'color' should be "1" "2" "3" "4" "5" "7"
tdf_winners3 <- tdf_winners3 %>%
+ mutate(color = ifelse(num_wins==1, '#d9f0a3', 
                ifelse(num_wins==2, '#addd8e', 
                ifelse(num_wins==3, '#78c679', 
                ifelse(num_wins==4, '#41ab5d', 
                ifelse(num_wins==5, '#238443', 
                ifelse(num_wins==6, '#006837', '#004529')))))))


# lollipop chart, color by factor level of num_wins, arranged in descending order of Distance
ggplot(data = tdf_winners3, aes(x=reorder(winner_name, desc(distance)), y=distance, color = num_wins)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4) 
# colour = tdf_winners3$color2 doesn't line up quite right because 
# with axis.test.x...we're manually assigning colors to *specific axis labels*,
# therefore colors need to be exactly in the right order
# source: https://stackoverflow.com/questions/47934253/coloring-ggplot2-axis-tick-labels-based-on-data-displayed-at-axis-tick-positions
+ theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = tdf_winners3$color2), 
    panel.background = element_rect(fill = 'black'), 
    panel.grid.major = element_line(colour = 'black'), 
    panel.grid.minor = element_line(colour = 'black'), 
    plot.background = element_rect(fill = 'black'),) 
+ scale_color_manual(values = c('#addd8e', '#78c679', '#41ab5d', '#238443', '#006837', '#004529'))



# what *could* be combined? distance + time_overall

