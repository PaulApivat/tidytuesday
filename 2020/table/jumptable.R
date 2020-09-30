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

# basic react table
reactable(jump, 
          filterable = TRUE, 
          searchable = TRUE, 
          minRows = 10, 
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15)


# option: GroupBy 'driver
reactable(jump, 
          groupBy = "driver", columns = list(
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