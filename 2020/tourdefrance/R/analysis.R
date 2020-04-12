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

# step 2 subset tdf_winners3 with edition, winner_name, distance, num_wins, color

tdf_winners3 %>%
    select(edition, winner_name, distance, num_wins, color) -> tdf_winners4

# step 2a group_by winner_name
# goal is to match color with winner_name ONCE (only) AND in descending order of MEAN distance

# distance arrange in descending order 
# use this view to see what color is associated with each winner_name to manually create vector 
# specifically for y-axis labels
# need 63 labels (not 106) because multi-time winners count as one
View(tdf_winners4 %>% arrange(desc(distance)))


# step 3
# Create 'colors' vector to manually assign colors to *specific axis labels*
# note: need to reverse to fit horizontal y-axis of names in descending order of distance
# multi-time winners, 4x, 5x or 7x are in orange shade
colors <- c("#d9f0a3", "#d9f0a3", "#addd8e", "#addd8e", "#addd8e", "#78c679", "#d9f0a3", "#d9f0a3", 
"#d9f0a3", "#d9f0a3", "#addd8e", "#addd8e", "#addd8e", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", 
"#addd8e", "#d9f0a3", "#d9f0a3", "#78c679", "#d9f0a3", "#d9f0a3", "#addd8e", "#d9f0a3", "#d9f0a3", 
"#feb24c", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#addd8e", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", 
"#d9f0a3", "#d9f0a3", "#addd8e", "#d9f0a3", "#feb24c", "#d9f0a3", "#addd8e", "#d9f0a3", "#feb24c", 
"#d9f0a3", "#feb24c", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#78c679", "#d9f0a3", "#addd8e", 
"#f03b20", "#d9f0a3", "#fed976", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3", "#d9f0a3")

# step 4 
# plot horizontal bar chart, space grey background
ggplot(data = tdf_winners3, aes(x=reorder(winner_name, distance), y=distance, color = num_wins)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4) 
# use reverse of 'colors' vector manually created above - rev() to match descending order
+ theme(axis.text.y = element_text(hjust = 1, colour = rev(colors)), 
    axis.text.x = element_text(colour = "white"), 
    # space gray background
    panel.background = element_rect(fill = '#4f5b66'), 
    panel.grid.major = element_line(colour = '#4f5b66'), 
    panel.grid.minor = element_line(colour = '#4f5b66'), 
    plot.background = element_rect(fill = '#4f5b66'),) 
    # 4x, 5x and 7x multi-time winners in orange shade to contrast against space gray background
+ scale_color_manual(values = c('#addd8e', '#78c679', '#41ab5d', '#fed976', '#feb24c', '#f03b20')) 
+ coord_flip()


#### Final Plot 

final_lolli <- ggplot(data = tdf_winners3, aes(x=reorder(winner_name, distance), y=distance, color = num_wins)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4) 
+ theme(axis.text.y = element_text(hjust = 1, colour = rev(colors)), 
    axis.text.x = element_text(colour = "white"), 
    panel.background = element_rect(fill = '#4f5b66'), 
    panel.grid.major = element_line(colour = '#4f5b66'), 
    panel.grid.minor = element_line(colour = '#4f5b66'), 
    plot.background = element_rect(fill = '#4f5b66'), 
    legend.position = "none", plot.title = element_text(color = "white"), 
    plot.subtitle = element_text(color = "white"), 
    axis.title.x = element_text(color = "white"), 
    axis.title.y = element_text(color = "white")) 
+ scale_color_manual(values = c('#addd8e', '#78c679', '#41ab5d', '#fed976', '#feb24c', '#f03b20')) 
+ coord_flip() 
+ labs(y = "Distance (KM)", x = "Names", title = "Tour De France Winners by Distance", subtitle = "1903 - 2019")



### Appendix ###

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

# Horizontal Bar Chart
ggplot(data = tdf_winners3, aes(x=reorder(winner_name, distance), y=distance, color = num_wins)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4) 
# axis.text.y instead of axis.text.x when using coord_flip()
+ theme(axis.text.y = element_text(hjust = 1, colour = tdf_winners3$color2), 
        # this axis becomes distance
        axis.text.x = element_text(colour = "white"), 
        panel.background = element_rect(fill = 'black'), 
        panel.grid.major = element_line(colour = 'black'), 
        panel.grid.minor = element_line(colour = 'black'), 
        plot.background = element_rect(fill = 'black'),) 
+ scale_color_manual(values = c('#addd8e', '#78c679', '#41ab5d', '#238443', '#006837', '#004529')) 
+ coord_flip()



# what *could* be combined? distance + time_overall

# basic plot with time_margin as y-axis variable
# note: observations of p1 + p2 side-by-side
# 1. time_margin appears to follow power law distribution
# 2. the people who had the highest time_margin victory appears to also have the shortest distances
p1 <- ggplot(data = tdf_winners3, mapping = aes(x=reorder(winner_name, time_margin), y=time_margin)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4) 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


p2 <- ggplot(data = tdf_winners3, mapping = aes(x=reorder(winner_name, desc(distance)), y=distance)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4, color = 'orange') 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

p3 <- ggplot(data = tdf_winners3, mapping = aes(x=reorder(winner_name, time_overall), y=time_overall)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4, color = 'purple') 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(patchwork)
p1 + p2 + p3

# correlation between time_margin and distance
library(moderndive)

# r = 0.167
get_correlation(data = tdf_winners3, distance ~ time_margin, na.rm = TRUE)

# conversely, higher correlation between distance and time_overall
# r = 0.93
get_correlation(data = tdf_winners3, distance ~ time_overall, na.rm = TRUE)

##### Bar chart of time_margin, but arranged in order of descending, distance
# x-axis arranged in order of distance
ggplot(data = tdf_winners3, aes(x=reorder(winner_name, desc(distance)))) 
# y-axis time_margin
+ geom_bar(aes(y=time_margin), stat = 'identity', alpha = 0.5) 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Lolli chart of time_margin, arranged in order of distance
# only those with time_margin >1 highlighted
p4 <- ggplot(data = tdf_winners3, mapping = aes(x=reorder(winner_name, distance), y=time_margin)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4, color = ifelse(tdf_winners3$time_margin > 1, 'lightblue', 'orange')) 
+ theme(axis.text.x = element_text(hjust = 1, color = 'white'), 
    axis.text.y = element_text(colour = "white"), 
    panel.background = element_rect(fill = '#4f5b66'), 
    panel.grid.major = element_line(colour = '#4f5b66'), 
    panel.grid.minor = element_line(colour = '#4f5b66'), 
    plot.background = element_rect(fill = '#4f5b66'), 
    plot.title = element_text(color = 'white'), 
    plot.subtitle = element_text(color = 'white'), 
    axis.title.x = element_text(color = 'white'), 
    axis.title.y = element_text(color = 'white')) 
+ coord_flip() 
# flips the bars and puts the y-axis on the left
+ scale_y_reverse() 
+ scale_x_discrete(position = "left") 
+ labs(y = "Time Margin", x = "Names", title = "Tour de France Winners by Time", subtitle = "Margin of Victory in Minutes")

## ranked ordered in descending order by Distance
## sample as final_lolli with one color scheme
p5 <- ggplot(data = tdf_winners3, mapping = aes(x=reorder(winner_name, distance), y=distance)) 
+ geom_segment(aes(xend=winner_name, yend=0)) 
+ geom_point(size=4, color = 'orange') 
+ theme(axis.text.x = element_text(hjust = 1, color = 'white'), 
    axis.text.y = element_text(colour = "white"), 
    panel.background = element_rect(fill = '#4f5b66'), 
    panel.grid.major = element_line(colour = '#4f5b66'), 
    panel.grid.minor = element_line(colour = '#4f5b66'), 
    plot.background = element_rect(fill = '#4f5b66'), 
    plot.title = element_text(color = 'white', hjust = 1), 
    plot.subtitle = element_text(color = 'white', hjust = 1), 
    axis.title.x = element_text(color = 'white'), 
    axis.title.y = element_text(color = 'white')) 
+ coord_flip() 
+ labs(y = "Distance (KM)", x = "Names", title = "Tour De France Winners by Distance", subtitle = "1903 - 2019")

# patchwork
library(patchwork)
p4 + p5