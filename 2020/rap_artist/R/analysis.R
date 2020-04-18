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

# heat map
# difficult to tell
ggplot(data = num_one, aes(x=year1, y=artist)) 
+ geom_bin2d(bins = 70) 
+ scale_fill_continuous(type = "viridis") + theme_bw()


# basic bar plot
# tally all number 1 votes
num_one %>% group_by(artist) %>% tally(sort = TRUE) -> z
colnames(z)[2] <- 'num_critics'

ggplot(data = z, mapping = aes(x=reorder(artist, num_critics), y=num_critics)) 
+ geom_bar(stat = 'identity') 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# geom_point
# golden age of rap + num_critics as bubble size
num_one_2 <- num_one %>%
    inner_join(z, by = 'artist')

ggplot(data = num_one_2, mapping = aes(x=year1, y=reorder(artist, year1), size = num_critics)) 
+ geom_point(alpha = 0.7) 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ geom_vline(xintercept = as.numeric(as.Date(c("1990-01-01", "1999-01-01"))), linetype=4, color="red")

# geom_point
# golden age of hip hop (1985 - 1995), points as bubble size
ggplot(data = num_one_3, mapping = aes(x=year1, y=reorder(artist, year1), size = points)) 
+ geom_point(alpha = 0.3) 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ geom_vline(xintercept = as.numeric(as.Date(c("1985-01-01", "1996-01-01"))), linetype=4, color="red")

# add color to bubble size
# golden age of hip hop (1985 - 1995), points as bubble size

# change num_critics to factor
num_one_3$num_critics <- as.factor(num_one_3$num_critics)

ggplot(data = num_one_3, mapping = aes(x=year1, y=reorder(artist, year1), size = points, color = num_critics)) 
+ geom_point(alpha = 0.3) 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ geom_vline(xintercept = as.numeric(as.Date(c("1985-01-01", "1996-01-01"))), linetype=4, color="red") 
+ scale_size(range = c(1, 20), name="Points Awarded")

# annotate for golden age period
ggplot(data = num_one_3, mapping = aes(x=year1, y=reorder(artist, year1), size = points, color = num_critics)) 
+ geom_point(alpha = 0.3) 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ geom_vline(xintercept = as.numeric(as.Date(c("1985-01-01", "1996-01-01"))), linetype=1, color="white") 
+ scale_size(range = c(1, 20), name="Points Awarded") 
# gray shaded area to highlight golden age of rap
+ annotate("rect", xmin = as.Date("1978-01-01"), xmax = as.Date("1981-01-01"), ymin = 0, ymax = Inf, alpha = 0.2) 
+ annotate("rect", xmin = as.Date("1996-01-01"), xmax = as.Date("2017-01-01"), ymin = 0, ymax = Inf, alpha = 0.2)


# add image to background
install.packages("png")
library(png)
library(grid)

# downloaded a biggie.png and saved working directory
img <- png::readPNG('biggie.png')
img1 <- png::readPNG("biggie_bw.png")

# biggie background with no golden age shade
ggplot(data = num_one_3, mapping = aes(x=year1, y=reorder(artist, year1), size = points, color = num_critics)) 
+ annotation_custom(rasterGrob(img1, width = unit(1, "npc"), height = unit(1, "npc"))) 
+ geom_point(alpha = 0.3) 
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ scale_size(range = c(1, 20), name="Points Awarded")

# Final Version 1.0
img1 <- png::readPNG("biggie_bw.png")

ggplot(data = num_one_3, mapping = aes(x=year1, y=reorder(artist, year1), size = points, color = num_critics))
# load background image 
+ annotation_custom(rasterGrob(img1, width = unit(1, "npc"), height = unit(1, "npc"))) 
+ geom_point(alpha = 0.8) + scale_x_date(date_labels = "%Y", date_breaks = "1 year") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'gold'), 
    axis.text.y = element_text(color = 'gold'),
    panel.background = element_rect(fill = "#777572"), 
    panel.grid.major = element_line(color = '#777572'), 
    panel.grid.minor = element_line(color = '#777572'), 
    plot.background = element_rect(fill = '#777572',color = 'gold'), 
    plot.title = element_text(color = 'gold'), 
    plot.subtitle = element_text(color = 'gold'),
    axis.title.x = element_text(color = 'gold'), 
    axis.title.y = element_text(color = "gold"), 
    legend.position = "none") 
+ scale_size(range = c(1, 20), name="Points Awarded") 
+ labs(title = 'In the Golden Age of Hip-Hop, One Artist and One Song Stands Out', x = '', y='')

