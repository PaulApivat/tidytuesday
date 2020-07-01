# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.5
# Version 1.2.5042

# load library and packages ----
library(tidyverse)
remotes::install_github("malcolmbarrett/claremontrun")
library(claremontrun)

# check data frames ----
character_visualization %>% view()
characters %>% glimpse()
comic_bechdel %>% glimpse()
covers %>% glimpse()
issue_collaborators %>% glimpse()
locations %>% glimpse()
xmen_bechdel %>% glimpse()

# initial explorations ----

# character_visualization ----

# look through Uncanny X-Men issue #97-278
# see 25 characters by frequency of speech, thought, narrative
# arrange in descending by either speech, thought, narrative, depicted
character_visualization %>%
    group_by(character) %>%
    summarize(
        speech_sum = sum(speech),
        thought_sum = sum(thought),
        narrative_sum = sum(narrative),
        depicted_sum = sum(depicted)
    ) %>%
    arrange(desc(speech_sum)) %>% 
    arrange(desc(thought_sum)) %>% 
    arrange(desc(narrative_sum)) %>% 
    arrange(desc(depicted_sum)) %>% view()

# characters ----

# look through Uncanny X-Men issue #97-278
# see 23 characters by frequency of actions - captured, declared_dead, initiates physical conflict

# get column names
names(characters)

# missing data
# grab first two columns
# columns 14 - 28 had significant missing data
characters %>%
    select(1:2, 14:28) %>%
    summarize_all(~ sum(is.na(.))) %>%
    view()

# proportion of missing data
# grab first two columns
# most missing data range from 85% - 100%
characters %>%
    select(1:2, 14:28) %>%
    summarize_all(~ sum(is.na(.)) / length(.)) %>%
    view()

# dividing characters dataset into two
options(scipen = 999)

# outlier? jean grey killing 5 billion?
characters %>% 
    arrange(desc(number_of_kills_non_humans)) %>%
    view()

#### character  by Count of action (filter out issue 135 outlier; Jean Gray)
# filter only issues #97 - 278
# Storm sees the most action
characters %>% 
    select(1:13) %>% 
    gather(key = Actions, 3:13, value = Count) %>% 
    filter(issue != 135 & issue != 279:280) %>%
    arrange(desc(Count)) %>% 
    ggplot(aes(x=Count, y=reorder(character, Count), fill=Actions)) + geom_bar(stat = "identity")

# check to see if filter out 135, 279, 280
characters %>%
    filter(issue == 135 | issue== 279:280) %>% view()

#### analyze to see character-to-character interactions
characters %>%
    select(1:2, 14:34) %>% view()



# select all columns that are double()
characters %>%
    select_if(is.character) %>% view()

characters %>%
    select_if(is.double)

# covers ----

covers %>% view()

# issue_collaborators ----

issue_collaborators %>% view()

# locations ----

locations %>% view()

# comic bechdel ----

comic_bechdel %>% view()

# xmen bechdel ----

xmen_bechdel %>% view()
