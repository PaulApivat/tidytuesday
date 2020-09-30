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
# change datatypes
jump <- jump %>%
    mutate(date = c("2018-11-01")) %>%
    mutate(
        date = as.Date(date),
        score = as.numeric(score),
        alert = as.numeric(alert),
        respondents = as.numeric(respondents)
    ) %>%
    mutate(respondents = (jump$respondents)/26) %>%
    select(driver, score, alert, group, respondents, date)

# orange palette
orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)

# basic react table
reactable(jump, 
          borderless = TRUE,
          outlined = TRUE,
          searchable = TRUE, 
          minRows = 10, 
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15,
          #rowStyle = function(index){
          #    if(jump[index, "score"] < 50){
          #        list(background = "rgba(0,0,0,0.05)")
          #    }
          #},
          theme = reactableTheme(
              borderColor = "#dfe2e5",
              stripedColor = "#f6f8fa",
              highlightColor = "#f0f5f9",
              cellPadding = "8px 12px",
              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
              searchInputStyle = list(width = "100%")
          ),
          columns = list(
              date = colDef(format = colFormat(date = TRUE, locales = 'en-GB')),
              score = colDef(style = function(value){
                  if (value > 50){
                      color <- "#008000"
                  } else if (value < 50){
                      color <- "#e00000"
                  } else {
                      color <- "#777"
                  }
                  list(color = color, fontWeight = "bold")
              }),
              alert = colDef(
                  style = function(value){
                      normalized <- (value - min(jump$alert)) / (max(jump$alert) - min(jump$alert))
                      color <- orange_pal(normalized)
                      list(background = color)
                  }
              ),
              respondents = colDef(format = colFormat(percent = TRUE, digits = 0))
          ))



# basic react table
# with htmlwidget at bottom
reactable(jump, 
          filterable = TRUE, 
          searchable = TRUE, 
          minRows = 10, 
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 15,
          defaultColDef = colDef(footer = function(values){
              if(!is.numeric(values)) return()
              sparkline(values, type = 'box', width = 100, height = 30)
          }),
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


# option: GroupBy 'driver'
install.packages("sparkline")
library(sparkline)


# htmlwidget: sparklilne
reactable(jump, 
          groupBy = "driver", columns = list(
              #score = colDef(aggregate = "mean"),
              score = colDef(cell = function(value, index){
                 sparkline(jump$score[[index]])
              }),
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



# htmlwidgets (only barchart)

data <- jump %>%
    group_by(driver) %>%
    summarize(score = list(score)) 
    

reactable(data, columns = list(
    score = colDef(cell = function(values){
        sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(jump$score))
    })
))









