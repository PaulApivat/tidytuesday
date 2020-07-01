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
    
    