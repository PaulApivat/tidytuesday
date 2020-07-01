# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.5
# Version 1.2.5042

# load library and packages
library(tidyverse)
remotes::install_github("malcolmbarrett/claremontrun")
library(claremontrun)

# check data frames
character_visualization %>% view()
characters %>% glimpse()
comic_bechdel %>% glimpse()
covers %>% glimpse()
issue_collaborators %>% glimpse()
locations %>% glimpse()
xmen_bechdel %>% glimpse()

# initial explorations

