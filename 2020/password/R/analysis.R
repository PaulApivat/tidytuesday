library(tidyverse)
library(moderndive)
library(ggridges)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# create column changing all time_unit to seconds (redundant w offline_crack_sec)
# save in new data frame pass_in_sec
pass_in_sec <- passwords %>% 
mutate(value_in_sec = if_else(time_unit=="years", value*3.154e+7,
                if_else(time_unit=="months", value*2.628e+6,
                if_else(time_unit=="weeks", value*604800,
                if_else(time_unit=="days", value*86400,
                if_else(time_unit=="hours", value*3600,
                if_else(time_unit=="minutes", value*60,
                if_else(time_unit=="seconds", value*1, value))))))))

# save length of password to new column
pass_in_sec <- pass_in_sec %>%
mutate(password_length = nchar(pass_in_sec$password))

# count number of passwords with Uppercase (none)
sapply(regmatches(pass_in_sec$password, gregexpr("[A-Z]", pass_in_sec$password, perl = TRUE)), length)

# check for special characters in password (all FALSE)
grepl("[[:punct:]]", pass_in_sec$password, perl = TRUE)

# count and save how many of the passwords had a lowercase AND number to new column
pass_in_sec$lower_and_num <- grepl("(?=.*[a-z])(?=.*[0-9])", pass_in_sec$password, perl = TRUE)

# delete last six rows (missing data across all columns, NA)
> pass_in_sec <- pass_in_sec[-(501:507),]


add <- function(x,y){
    x + y
}

print(add(10,20))