# RStudio Version 1.2.5042
# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# note: update R and RStudio

# load library (first)
library(tidyverse)
library(ggrepel)    #for legible text annotations
library(schrute)    #the office text

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

# note - must add all seasons at once, not individually
mydata2 <- dplyr::left_join(mydata, lines_text_s2, by=c("season" = "season", "character" = "character"))


## Next steps: create lines_text_s3-9, then rbind(), then left_join()

# season 3
mydata %>% filter(season==3) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s3
colnames(lines_text_s3)[2] <- 'num_lines'
lines_text_s3$season <- 3
lines_text_s3$season <- as.integer(lines_text_s3$season)

# season 4
mydata %>% filter(season==4) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s4
colnames(lines_text_s4)[2] <- 'num_lines'
lines_text_s4$season <- as.integer(4)

# season 5
mydata %>% filter(season==5) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s5
colnames(lines_text_s5)[2] <- 'num_lines'
lines_text_s5$season <- as.integer(5)

# season 6
mydata %>% filter(season==6) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s6
colnames(lines_text_s6)[2] <- 'num_lines'
lines_text_s6$season <- as.integer(6)

# season 7
mydata %>% filter(season==7) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s7
colnames(lines_text_s7)[2] <- 'num_lines'
lines_text_s7$season <- as.integer(7)

# season 8
mydata %>% filter(season==8) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s8
colnames(lines_text_s8)[2] <- 'num_lines'
lines_text_s8$season <- as.integer(8)

# season 9
mydata %>% filter(season==9) %>% group_by(character) %>% tally(sort = TRUE) -> lines_text_s9
colnames(lines_text_s9)[2] <- 'num_lines'
lines_text_s9$season <- as.integer(9)


## rbind lines_text_s1 through lines_text_s9, then left_join with mydata
## this works.
lines_text_temp <- rbind(lines_text_s1, lines_text_s2, lines_text_s3, lines_text_s4, lines_text_s5, lines_text_s6, lines_text_s7, lines_text_s8, lines_text_s9)

mydata2 <- dplyr::left_join(mydata, lines_text_temp, by=c("season" = "season", "character" = "character"))

# first attemp stacked area chart (NOT working)
mydata2 %>%
+ filter(num_lines > 500) %>%
+ ggplot(aes(x=season, y=num_lines, fill=character)) + geom_area()

# need to make 'character' a factor; season as numeric, num_lines as numeric
mydata2$character <- as.factor(mydata2$character)
mydata2$season <- as.numeric(mydata2$season)
mydata2$num_lines <- as.numeric(mydata2$num_lines)




####### create example stacked area chart from scratch  ########
time <- as.numeric(rep(seq(1,7), each=7))   # X Axis
value <- runif(49, 10, 100)                 # Y Axis
group <- rep(LETTERS[1:7], times=7)         # group, one shape per group
data <- data.frame(time, value, group)

# sample stacked area chart works
ggplot(data = data, aes(x=time, y=value, fill=group)) + geom_area()

#################################################################


lines_text_temp$character <- as.factor(lines_text_temp$character)
lines_text_temp$num_lines <- as.numeric(lines_text_temp$num_lines)
lines_text_temp$season <- as.numeric(lines_text_temp$season)

# office_factor_levels
office_levels <- c('Michael', 'Jim', 'Pam', 'Dwight', 'Andy', 'Meredith', 'Toby', 'Ryan', 'Kevin', 'Oscar', 'Angela', 'Phyllis', 'Stanley')

# subset dataframe of lines_text_main (main characters only)
# Idea: apply filter for mydata2 as well
lines_text_temp %>% filter(character %in% office_levels) -> lines_text_main

# basic stacked area chart for main characters (working)
ggplot(data = lines_text_main, mapping = aes(x=season, y=num_lines, fill=character)) 
+ geom_area()
+ scale_x_discrete(limits=1:9)

# back stacked area chart (by percentage) for main characters 
ggplot(data = lines_text_main, mapping = aes(x=season, y=num_lines, fill=character)) 
# automatic transform to percentage
+ geom_area(position = 'fill') 
+ scale_x_discrete(limits=1:9)


# reorder factor levels in descending order by sum of all lines (season 1-9)
lines_text_main %>% group_by(character) %>% summarize(sum_lines = sum(num_lines)) %>% arrange(desc(sum_lines))

# A tibble: 13 x 2 -- use this table to guide re-ordering of factor levels
   character sum_lines
   <fct>         <dbl>
 1 Michael       10921
 2 Dwight         6847
 3 Jim            6303
 4 Pam            5031
 5 Andy           3754
 6 Angela         1569
 7 Kevin          1564
 8 Oscar          1368
 9 Ryan           1198
10 Phyllis         970
11 Toby            818
12 Stanley         678
13 Meredith        559

# first drop UNUSED factor levels in character (from 773 -> 13)
lines_text_main$character <- droplevels(lines_text_main$character)







# 2. read tidytext sentiment analysis
# 3. re-create sentiment by character https://pudding.cool/2017/08/the-office/
# 4. find most common positive / negative words in select episodes
# 5. visualize