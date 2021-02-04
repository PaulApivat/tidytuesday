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


# filter Plastics by Top 20 Plastic Producing Brands in 2020
# filter by 2020
# save as circ
circ <- plastics %>%
    filter(parent_company %in% top_brands) %>%
    filter(year==2020) 
