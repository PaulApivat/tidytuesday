# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"

# load data
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# load library
library(tidyverse)

# quick exploration
# What year range is represented in this data set? Ans: 1979 - 2019
str(polls)
summary(polls)

#####----- Exploratory Data Analysis------ ######

# What was the #1 song in each year from 1979 - 2019
polls %>% 
    filter(rank==1) %>%
    arrange(year)

# Which song got the most #1 vote? (see also rankings df)
polls %>% filter(rank==1) %>% arrange(year) -> num_one

num_one %>%
    group_by(title) %>%
    tally(sort = TRUE)

# Which artist got the most #1 vote?
num_one %>%
    group_by(artist) %>%
    tally(sort = TRUE)

# What's gender breakdown of artists with songs voted #1?
num_one %>%
    group_by(gender) %>%
    tally(sort = TRUE)

# What countries are represented in these votes?
num_one %>%
    group_by(critic_country) %>%
    tally(sort = TRUE)

polls %>%
    group_by(critic_country) %>%
    tally(sort = TRUE)

# What media channels are represented? (see data dictionary: critic_rols)
polls %>%
    group_by(critic_rols) %>%
    tally(sort = TRUE)

####-------- Basic Plots -------#####
ggplot(data = num_one, mapping = aes(x=year, y=artist)) + geom_point()

# want x-axis to list ALL years
library(zoo)

# beginning of year
# year is not a 'date' technically
# end of year would be as.Date(as.yearmon(num_one$year) + 11/12, frac = 1)
num_one$year1 <- as.Date(as.yearmon(num_one$year))

# basic geom_point plot all artists with #1 songs, by individual year
ggplot(data = num_one, mapping = aes(x=year1, y=artist)) 
+ geom_point() 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# arrange y-axis, artist, not by alphabetical, but by year
ggplot(data = num_one, mapping = aes(x=year1, y=reorder(artist, year1))) 
+ geom_point() 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Golden Age of Rap
# two red lines 1990 - 1999
ggplot(data = num_one, mapping = aes(x=year1, y=reorder(artist, year1))) 
+ geom_point() 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ geom_vline(xintercept = as.numeric(as.Date(c("1990-01-01", "1999-01-01"))), linetype=4, color="red")

