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



#-----------RIDGELINE PLOT-------------#

# Ridgeline plot emphasizing outlier that happens to have both alphabet & numbers

plot1 <- ggplot(data = pass_in_sec, mapping = aes(x = strength, y = category, fill = lower_and_num)) 
+ geom_density_ridges(scale = 1.5, alpha = 0.8) + theme_classic() 
+ scale_fill_manual(values = c("#7b3294", "#008837")) 
+ labs(x = "Password Strength", y = "Password Category", fill = "Contains lowercase letters  AND numbers")

# Ridgeline plot emphasizing categories

plot2 <- ggplot(data = pass_in_sec, mapping = aes(x = strength, y = category, fill = category)) 
+ geom_density_ridges(scale = 1.5, alpha = 0.8) 
+ xlim(0,15) + theme_classic() 
+ labs(x = "Password Strength", y = "Password Category")

# Ridgline plot, cateogries
# compare with quantile lines (1st, median, 3rd), vline_color & scale_discrete_manual
# change plot colors scale_fill_manual

plot3 <- ggplot(data = pass_in_sec, mapping = aes(x = strength, y = category, fill = category, vline_color = ..quantile..)) 
+ geom_density_ridges(scale = 1.5, alpha = 0.8, quantile_lines = TRUE) 
+ scale_discrete_manual("vline_color", values = c("blue", "black", "blue", "#39FF14"), breaks = c(1,2,3), labels = c("1st", "median", "3rd"), name = NULL) 
+ theme_classic() 
+ labs(x = "Password Strength", y = "Password Category") 
+ scale_y_discrete(expand = c(0.01, 0)) 
+ theme_ridges(grid = FALSE, center = TRUE) + xlim(0,15) 
+ scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"))

# Ridgline plot, categories
# median only

plot4 <- ggplot(data = pass_in_sec, mapping = aes(x = strength, y = category, fill = category)) 
+ geom_density_ridges(scale = 1.5, alpha = 0.8, quantile_lines = TRUE, quantiles = 2) 
+ theme_classic() 
+ labs(x = "Password Strength", y = "Password Category", title = "Password Category Median Strength") 
+ scale_y_discrete(expand = c(0.05, 0)) 
+ theme_ridges(grid = FALSE, center = TRUE) 
+ xlim(0,15) 
+ scale_fill_manual(values = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")) 
+ geom_text(data = pass_in_sec %>% group_by(category) %>% summarize(strength = median(strength)), aes(label=sprintf("%1.1f", strength)), position = position_nudge(y = -0.1), color = "red") 
+ theme(legend.position = "none")



#-----------CIRCULAR BARPLOT-------------#

#preparing label data for CIRCULAR BARPLOT

label_data <- pass_in_sec

# calculate ANGLE of labels (note: rank is id)
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$rank-0.5)/number_of_bar

# calculate alignment of labels: right or left
# labels on the left side of the plot have angles < -90 
label_data$hjust <- if_else(angle < -90, 1, 0)

# flip angle to make them readable
label_data$angle <- if_else(angle < -90, angle+180, angle)

# circle bar plot
# rank is id, necessary to order the words
circle_bar1 <- ggplot(data = label_data, mapping = aes(x=as.factor(rank), y=strength)) 

# adds blue bars for 'strength' of password
+ geom_bar(stat = "identity", fill = alpha("blue", 0.3)) 

# limits of plot: more negative (left) makes circle bigger; more positive (right number) makes circle smaller
+ ylim(-200, 120) 
+ theme_minimal() 
+ theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(), 
        plot.margin = unit(rep(0,4), "cm")) 

# coordinate polar instead of cartesian
+ coord_polar(start = 0) 

# hjust and angle ensure words don't overlap (to extent possible)
+ geom_text(data = label_data, aes(x=rank, y=strength+60, label=password, hjust=hjust), 
        color = "black", 
        fontface="bold", 
        alpha=0.6, size=1.5, 
        angle=label_data$angle, 
        inherit.aes = FALSE)



