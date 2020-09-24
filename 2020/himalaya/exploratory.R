# session
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Catalina 10.15.6
sessionInfo()

# load libraries
library(tidyverse)
library(reactable)
library(htmltools)

# read in data 
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# Exploratory ----

# Peaks 

# How many unique peaks are there? (468)
peaks %>%
    group_by(peak_name) %>%
    tally(sort = TRUE)

peaks %>%
    summarize(
        unique_peaks = unique(peak_name)
    )

# Top 10 peaks, UNCLIMBED, in terms of height_metres (Everest #1) 
# all top-10 have been climbed
# Highest unclimbed peak = Yalung Kang West

peaks %>%
    select(peak_name, height_metres, climbing_status) %>%
    filter(climbing_status != 'Climbed') %>%
    arrange(desc(height_metres)) 


# Members 

# How many unique individuals are represented in the data?
# 76,518 unique people

members %>%
    summarize(unique_individual = unique(member_id))
    
members %>%
    group_by(member_id) %>%
    tally(sort = TRUE)

# How many people have failed vs succeeded?
# 47320 fail (), 29199 success
members %>%
    group_by(success) %>%
    tally()

# Proportion of Success-to-Failure
# 47320 fail (62%), 29199 success (38%)
members %>%
    group_by(success) %>%
    summarize(n = n()) %>%
    mutate(freq = n / sum(n))

# Expedition 

# When was the first expedition? Most recent?
# 1905 - 2019 (in over a century, 76,518 have risked their lives)
summary(expeditions$year)

# Table Idea ----

# Table of Peaks by Height 
# Number of people attempt
# % Success or Failed

# join members & peaks

# table ----
table <- members %>%
    left_join(peaks, by = 'peak_id') %>%
    select(peak_id, peak_name.y, height_metres, climbing_status, member_id, success, died, injured) %>%
    rename(
        peak = peak_name.y,
        height = height_metres,
        status = climbing_status
    )


# See all peaks by success status
table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak, success) %>%
    tally()

# Get Success & Failure Percentages by Peak
# note: all failed attemps arranged by attempt & failure rate
table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak, success) %>%
    summarize(n = n()) %>%
    mutate(pct = n / sum(n)) %>%
    filter(success == FALSE) %>% 
    arrange(desc(n)) %>%
    arrange(desc(pct)) %>% view()


# Get number of attempts per peak
# get success/failure rate per peak
# note: Everest number of attempt 21813

# attempts ----
attempts <- table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak) %>%
    summarize(number_of_attempt = n()) %>%
    arrange(desc(number_of_attempt)) %>%
    ungroup() 

# fail_pct ----

# note: Everest success (10036), failure (11777), total (21813)
fail_pct <- table %>%
    select(peak, height, status, success, died, injured) %>%
    group_by(peak, success) %>%
    summarize(number_of_attempt = n()) %>%
    mutate(pct = number_of_attempt / sum(number_of_attempt)) %>% 
    filter(success==FALSE) %>%
    select(peak, pct) %>%
    rename(
        failure_rate = pct
    ) %>%
    ungroup()


# df (join) ----

# join attemps and fail_pct; filter by top 20 by number of attemps
df <- attempts %>%
    left_join(fail_pct, by = 'peak') %>%
    mutate(across(is.numeric, round, 3)) %>%
    rename(
        attempts = number_of_attempt,
        fail_rate = failure_rate
    ) %>%
    head(20)


# reactable tutorial ----

install.packages('reactable')
library(reactable)

# Step 1: create a basic table ----
reactable(df)


# Step 2: Customize table heading ----

# Change column Names
# Set default column to sort
# Set order to descending
# Change number format for both columns
reactable(
    df,
    defaultSorted = "attempts",
    columns = list(
        peak = colDef(
            name = "Peak"
        ),
        attempts = colDef(
            name = "Attempts (#)",
            defaultSortOrder = "desc",
            format = colFormat(separators = TRUE)
        ),
        fail_rate = colDef(
            name = "Fail (%)",
            defaultSortOrder = "desc",
            format = colFormat(percent = TRUE, digits = 1)
        )
    )
)

# Step 3: Add Bar Charts ----
# also: change font to "monospace" and change numeric column width

# note: fail(%) makes sense to have a background color gray, while attemps(#) does not


library(htmltools)

bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL){
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
}



reactable(
    df,
    defaultSorted = "attempts",
    columns = list(
        peak = colDef(
            name = "Peaks"
        ),
        attempts = colDef(
            name = "Attempts (#)",
            defaultSortOrder = "desc",
            #format = colFormat(separators = TRUE),
            
            # Render Bar charts using a custom cell render function
            cell = function(value){
                width <- paste0(value * 100 / max(df$attempts), "%")
                # Add thousands separators
                value <- format(value, big.mark = ",")
                # Fix each label using the width of the widest number (incl. thousands separators)
                value <- format(value, width = 9, justify = 'right')
                bar_chart(value, width = width, fill = "#3fc1c9")
            },
            # And left-align the columns
            align = "left",
            # Use the operating system's default monospace font, and
            # preserve the white space to prevent it from being collapsed by default
            style = list(fontFamily = "monospace", whiteSpace = "pre")
        ),
        fail_rate = colDef(
            name = "Fail (%)",
            defaultSortOrder = "desc",
            #format = colFormat(percent = TRUE, digits = 1)
            
            # Render Bar charts using a custom cell render function
            cell = function(value){
                # Format as percentage with 1 decimal place
                value <- paste0(format(value * 100, nsmall = 1), "%")
                # Fix width here to align single and double-digit percentages
                value <- format(value, width = 5, justify = "right")
                bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
            },
            # And left-align the columns
            align = "left",
            style = list(fontFamily = "monospace", whiteSpace = "pre")
        )
    )
)


# Step 4: Dynamic Formatting & Finishing ----
# NOTE: Dynamic formatting requires switching to javascript render function to 
# access client-side (browser) state of the table, to know which row is first after sorting
# add pagination = FALSE to view all rows at once
# add compact = TRUE to reduce white space


reactable(
    df,
    pagination = FALSE,
    defaultSorted = "attempts",
    columns = list(
        peak = colDef(
            name = "Peaks"
        ),
        attempts = colDef(
            name = "Attempts (#)",
            defaultSortOrder = "desc",
            cell = function(value){
                width <- paste0(value * 100 / max(df$attempts), "%")
                value <- format(value, big.mark = ",")
                value <- format(value, width = 9, justify = 'right')
                bar_chart(value, width = width, fill = "#3fc1c9")
            },
            align = "left",
            style = list(fontFamily = "monospace", whiteSpace = "pre")
        ),
        fail_rate = colDef(
            name = "Fail (%)",
            defaultSortOrder = "desc",
            # Format and render the cell with a javascript render function
            cell = JS("function(cellInfo) {
                const pct = (cellInfo.value * 100).toFixed(1) + '%'
                let value = pct.padStart(5)
                if (cellInfo.viewIndex > 0){
                    value = value.replace('%', ' ')
                }
                return(
                '<div style=\"display: flex; align-items: center;\">' +
                    '<span style=\"font-family: monospace; white-space: pre;\">' + value + '</span>' +
                    '<div style=\"flex-grow: 1; margin-left: 6px; height: 14px; background-color: #e1e1e1\">' +
                        '<div style=\"height: 100%; width: ' + pct + '; background-color: #fc5185\"></div>' +
                    '</div>'
                )
            }"),
            
            # --- replace previous R code with Javascript (above) --- #
            
            #cell = function(value){
            #    value <- paste0(format(value * 100, nsmall = 1), "%")
            #    value <- format(value, width = 5, justify = "right")
            #    bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
            #},
            
            # ------------------------------- #
            
            # Render this column as HTML
            html = TRUE,
            align = "left",
            #style = list(fontFamily = "monospace", whiteSpace = "pre")
        )
    ),
    compact = TRUE
)











    
    
    

