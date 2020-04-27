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

###### MOST legible - geom_label_repel

# highest rated episodes
# Finale, Goodbye Michael, Stress Relief, Dinner Party, Casino Night
# Niagara part 1+2, Threat Level Midnight, Goodbye, Toby, Beach Games, Broke, Garage Sale
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_label_repel(aes(label=ifelse(imdb_rating > 9.0, title, '')))


# lowest rated episodes
# The Banker, Get the Girl
ggplot(data = df, mapping = aes(x=total_votes, y=imdb_rating, color = season)) 
+ geom_point() 
+ geom_jitter() 
+ geom_label_repel(aes(label=ifelse(imdb_rating < 7.0, title, '')))


### Next Steps:
install.packages('schrute')
library(schrute)

# load data
mydata <- schrute::theoffice

# subset data frame in mydata (imdb_rating > 9.0) - highest rated ep
# subset data frame in df (imdb_rating > 9.0) - highest rated ep
mydata %>% filter(imdb_rating > 9.0) -> mydata_high
df %>% filter(imdb_rating > 9.0) -> df_high

# singling out one high rated episode "Dinner Party"
mydata %>% filter(episode_name=='Dinner Party') -> dinner_party

# create data frame to represent lines of text (across all 10 seasons)
mydata %>%
    group_by(character) %>%
    tally(sort = TRUE) -> lines_text

###### better idea, visualize lines_of_text by EACH season
###### Create stacked area chart by season examining lines of each character
mydata %>% filter(season==1) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s1

# change n to num_lines
colnames(lines_text_s1)[2] <- 'num_lines'

# add column 'season', assign 1
lines_text_s1$season <- 1

# change num to int (so can join with mydata, where 'season' is int)
lines_text_s1$season <- as.integer(lines_text_s1$season)

# join lines_text_s1 with mydata by season and character
mydata2 <- dplyr::left_join(mydata, lines_text_s1, by=c("season" = "season", "character" = "character"))

## need to make sure season 2 can be done without erasing season 1
## NOTE: did NOT work, adding season 2 erased season 1
mydata %>% filter(season==2) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s2
colnames(lines_text_s2)[2] <- 'num_lines'
lines_text_s2$season <- 2
lines_text_s2$season <- as.integer(lines_text_s2$season)
mydata2 <- dplyr::left_join(mydata, lines_text_s2, by=c("season" = "season", "character" = "character"))

## rbind lines_text_s1 and lines_text_s2, then left_join with mydata
## this works.
lines_text_temp <- rbind(lines_text_s1, lines_text_s2)
mydata2 <- dplyr::left_join(mydata, lines_text_temp, by=c("season" = "season", "character" = "character"))

## Next steps: create lines_text_s3-10, then rbind(), then left_join()



# 2. read tidytext sentiment analysis
# 3. re-create sentiment by character https://pudding.cool/2017/08/the-office/
# 4. find most common positive / negative words in select episodes
# 5. visualize