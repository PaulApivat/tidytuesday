# sessioninfo
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.6

sessionInfo()

# libraries
library(jsonlite)
library(tidyverse)
library(reactable)

# read json file

jump <- fromJSON("jumpdata.json", flatten = TRUE)

# explore
str(jump)

# change date
jump <- jump %>%
    mutate(date = c("2018-11-01")) %>%
    mutate(date = as.Date(date))


# basic react table
reactable(jump, 
          filterable = TRUE, 
          searchable = TRUE, 
          minRows = 10, 
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15,
          columns = list(
              date = colDef(format = colFormat(date = TRUE, locales = 'en-GB'))
          ))


# option: GroupBy 'driver
reactable(jump, 
          groupBy = "group", columns = list(
              score = colDef(aggregate = "mean"),
              alert = colDef(aggregate = "max"),
              group = colDef(aggregate = "unique"),
              respondents = colDef(aggregate = "frequency"),
              flagged = colDef(aggregate = "frequency")
          ),
          filterable = TRUE, 
          searchable = TRUE, 
          minRows = 10, 
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15)