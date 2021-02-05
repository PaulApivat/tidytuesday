# Library
library(tidyverse)

# Data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')


plastics %>%
    filter(year==2020) %>%
    group_by(parent_company) %>%
    summarise(sum = sum(grand_total)) %>%
    arrange(desc(sum)) %>%
    view()


# Top 20 Plastic Producing Brands
top_brands = c("Universal Robina Corporation", 
               "Colgate-Palmolive", 
               "Mayora Indah", 
               "Tamil Nadu Co-operative Milk Producers' Federation Ltd", 
               "Procter & Gamble", 
               "Philip Morris International", 
               "Blow-Chem Industries", 
               "Rite Foods Limited", 
               "Danone", 
               "Monde Nissin Corporation", 
               "Peerless Products Manufactoring Inc", 
               "Mondelez International", 
               "Master Chef", 
               "Keurig Dr. Pepper", 
               "Liwayway Holdings Company Limited", 
               "Nutri-Asia Inc.", 
               "Voltic Ghana Limited", 
               "Britannia", 
               "Bakhresa Group", 
               "jasmine")


# filter by Top 20 Plastic Producing Brands in 2020
# filter by 2020
# save as circ
circ <- plastics %>%
    filter(parent_company %in% top_brands) %>%
    filter(year==2020) 



# Order data
tmp <- circ %>%
    select(country, grand_total) %>%
    filter(!is.na(grand_total)) %>%
    arrange(desc(grand_total)) %>%
    mutate(country = as.factor(country))

# Set a number of 'empty bar' (why 10?)
empty_bar = 10

# Add lines to the initial tmp dataframe (why?)
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))


# get the name and the y position of each label
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle = 90 - 360 * (label_tmp$id-0.5) /number_of_bar  # subtract 0.5 so letters are at the center of the bar
label_tmp$hjust <- ifelse(angle < -90, 1, 0)
label_tmp$angle <- ifelse(angle < -90, angle+180, angle)
label_tmp$country <- paste(label_tmp$country, " (", label_tmp$grand_total,")", sep = "")

# Make the Circular Bar Plot
ggplot(tmp, aes(x=as.factor(id), y=grand_total)) +
    geom_bar(stat = "identity", fill=alpha("#6f1712", 0.8)) +
    ylim(-5000, 5000) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar(start = 0)+
    geom_text(data = label_tmp, aes(x=id, y=grand_total+200, label=country), color="black", fontface="bold", alpha=0.6, 
              size=2.5, angle = label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE)

    #geom_text(aes(x=24, y=4500, label="Where Did Coca-Cola Dump the Most Plastic in 20202?"), color="black", inherit.aes = FALSE)


# FACET ----

# Facet Wrap Bar Chart
circ %>%
    select(country, parent_company, grand_total) %>%
    filter(!is.na(grand_total)) %>%
    arrange(desc(grand_total)) %>%
    mutate(country = as.factor(country)) %>%
    ggplot(aes(x=reorder(country, grand_total), y=grand_total)) + 
    geom_bar(stat = "identity") +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    facet_wrap(~parent_company)

# Problem: Insufficient colors in various palettes
# Problem: Need enough colors for each country

# Define number of colors you want\
library(RColorBrewer)
nb.cols <- 46
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)


# Box Plot + Scatter Points
# NOTE: moved geom_label before geom_point
# change size of geom_point based on grand_total
circ %>%
    select(country, parent_company, grand_total) %>%
    filter(!is.na(grand_total)) %>%
    arrange(desc(grand_total)) %>%
    mutate(country = as.factor(country)) %>%
    ggplot(aes(x=reorder(parent_company, grand_total), grand_total)) +
    geom_boxplot(fill = "#b5dedc") +
    geom_label(
        data = circ %>% filter(grand_total > 2000), 
        nudge_x = 0.5,
        aes(label=country), 
        colour = "white",
        fill = "#429c8b",
        fontface = "italic",
        label.size = NA,
        size = 3.5
    ) +
    geom_point(aes(color=factor(country)), size = 10, alpha = 0.7, position = position_dodge(width = 0.5)) +     #size = (grand_total*10)
    scale_color_manual(values = mycolors) +
    theme(
        legend.position = 'none',
        panel.background = element_rect(fill = '#429c8b'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = '#429c8b'),
        axis.text.x = element_text(colour = "#f3e29f"),
        axis.text.y = element_text(colour = "#f3e29f", size = 12),
        axis.title.x = element_text(colour = "#f3e29f", margin = margin(10,0,0,0)),
        plot.title = element_text(colour = "white", margin = margin(0,0,5,0), size = 16, face = "bold"),
        plot.subtitle = element_text(colour = "white", margin = margin(0,0,30,0), face = "bold"),
        plot.caption = element_text(colour = "white", margin = margin(30,0,0,30))
    ) +
    coord_flip() +
    ylim(0, 6500) +
    labs(
        y = "Total Count",
        x = "",
        title = "Top 20 Plastic Producing Companies in 2020",
        subtitle = "& Five Outlier Locations",
        caption = "Data: BreakFreeFromPlastic.org | Graphic: @paulapivat"
    )



circ %>%
    select(country, parent_company, grand_total) %>%
    arrange(desc(grand_total))










