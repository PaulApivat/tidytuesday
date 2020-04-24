# RStudio Version 1.2.5042
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# note: update R and RStudio

# load library (first)
library(tidyverse)
library(ggrepel)    #for legible text annotations

# load data from TidyTuesday
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

### Exploratory Data Analysis

# descending order by imdb_rating and total_votes
# NOTE: shouldn't a true rating be imdb_rating x total_votes ?
View(office_ratings %>% arrange(desc(imdb_rating)))
View(office_ratings %>% arrange(desc(total_votes)))



# What was the highest rated season?
library(ggridges)

# change season to as.factor()
df <- office_ratings
df$season <- as.factor(df$season)

# basic density plot total_votes
# note: number of votes doesn't mean highest rated
ggplot(data = df, mapping = aes(x = total_votes, y = season, fill = season)) 
+ geom_density_ridges() 
+ theme_ridges()

# basic density plot imdb_rating
ggplot(data = df, mapping = aes(x = imdb_rating, y = season, fill = season)) 
+ geom_density_ridges() 
+ theme_ridges()

# scatter plot of both total_votes and imdb_ratings
# effective to identify if episode had highest imdb_rating AND total_votes
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter()

# add geom_text label to each geom_point()
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_text(aes(label=title), hjust=1, vjust=1)

# Conditionally add geom_text label only if total_votes > 4000
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_text(aes(label=ifelse(total_votes > 4000, title, '')), hjust=1, vjust=1)

# Conditionally add geom_text label only if imdb_ratings > 9.0
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_text(aes(label=ifelse(imdb_rating > 9.0, title, '')), hjust=1, vjust=1)

###### ----- Prevent Text Overlap ggrepel ------#######
library(ggrepel)

# same conditional add geom_text_repel - much more legible 
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_text_repel(aes(label=ifelse(imdb_rating > 9.0, title, '')))

###### MOST legible
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_label_repel(aes(label=ifelse(imdb_rating > 9.0, title, '')))

