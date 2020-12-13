# Library
library(tidyverse)




# rworldmap 
install.packages('rworldmap')
library(rworldmap)
vignette('rworldmap')

# Load Data Manually
women <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Filter by Category
women %>%
    filter(category=='Knowledge') %>%
    view()


# Fitler by Country
women %>%
    count(country, sort = TRUE) %>%
    view()

# Group By Role (All Activism)
women %>%
    count(role, sort = TRUE) %>%
    filter(str_detect(role, 'act'))

# EDA

women %>%
    group_by(category, country) %>%
    tally(sort = TRUE) %>%
    arrange(category) %>%
    view()

# ggraph sample ----
library(ggraph)
library(igraph)

# data frame with hierarchical structure
edges <- flare$edges

# information about each node
vertices <- flare$vertices

# make graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices)

# plot
ggraph(mygraph, layout = 'circlepack')+
    geom_node_circle()+
    theme_void()


# Map Color to Hierarchy Depth ----

# Left: color depends on depth
p <- ggraph(mygraph, layout = 'circlepack')+
    geom_node_circle(aes(fill = depth))+
    theme_void()+
    theme(legend.position = 'FALSE')

# Adjust color palette: Viridis
p + scale_fill_viridis()

# Adjust color palette: colorBrewer
p + scale_fill_distiller(palette = "RdPu")

# Circular Packing w/ Labels ----

# Create a subset of the dataset (remove 1 level)
edges2 <- flare$edges %>%
    filter(to %in% from) %>%
    droplevels()

vertices2 <- flare$vertices %>%
    filter(name %in% c(edges$from, edges$to)) %>%
    droplevels()

vertices2$size <- runif(nrow(vertices))

# Rebuild the graph object
mygraph2 <- graph_from_data_frame(edges2, vertices = vertices2)

# circular packing with labels
ggraph(mygraph2, layout = 'circlepack')+
    geom_node_circle(aes(fill=depth))+
    geom_node_text(aes(label=shortName, filter=leaf, fill=depth, size=size))+
    theme_void()+
    theme(legend.position = 'FALSE')+
    scale_fill_viridis()

# Hiding Levels ----
# source: https://www.r-graph-gallery.com/315-hide-first-level-in-circle-packing.html

edges3 <- flare$edges
vertices3 <- flare$vertices
mygraph3 <- graph_from_data_frame(edges3, vertices=vertices3)

# hide first level (right)
ggraph(mygraph3, layout = 'circlepack') +
    geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth)))+
    scale_fill_manual(values=c("0" = "white", "1" = "red", "2" = "green", "3" = "yellow", "4"= "blue"))+
    scale_color_manual(values = c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black"))+
    theme_void()+
    theme(legend.position = "FALSE")

# Add Labels to Specific Level of the Hierarchy ----
install.packages("data.tree")
library(data.tree)

# Rebuild the data
edges4 <- flare$edges
vertices4 <- flare$vertices

# Transform into tree format
tree <- FromDataFrameNetwork(edges4)

# Easily get level of each node, add to initial dataframe
mylevels <- data.frame(name=tree$Get('name'), level=tree$Get('level'))

vertices4 <- vertices4 %>%
    left_join(., mylevels, by=c("name"="name"))

# Now add label for level1 and 2 only:
vertices4 <- vertices4 %>%
    mutate(new_label=ifelse(level==2, shortName, NA))

mygraph4 <- graph_from_data_frame(edges4, vertices = vertices4)

# plot graph
ggraph(mygraph4, layout = 'circlepack')+
    geom_node_circle(aes(fill=as.factor(depth), color=as.factor(depth)))+
    # level 1 = green, level 2 = blue, level 3 = purple, level 4 = grey
    scale_fill_manual(values = c("0"="white", "1" = "green", "2"= "blue", "3"="purple", "4"="gray"))+
    scale_color_manual(values = c("0"="white", "1" = "black", "2"="black", "3"="black", "4"="black"))+
    geom_node_label(aes(label=new_label), size=4)+
    theme_void()+
    theme(legend.position = "FALSE", plot.margin = unit(rep(0,4), "cm"))



# BBC Women Draft (2 levels)----
edges5 <- women %>% 
    select(category, role, name) %>%
    arrange(category) %>% 
    select(category, role) %>%
    rename(
        from = category,
        to = role
    ) %>%
    slice(2:100) 
    

vertices5 <- women %>% 
    select(category, role, name) %>%
    arrange(category) %>% 
    select(role, name) %>%
    rename(
        name = role,
        shortName = name
    ) %>%
    slice(2:100) 

# Duplicate Vertex Name
# will add one extra row to vertices5
temp_vert <- data.frame("women", "women")
names(temp_vert) <- c("name", "shortName")
vertices5 <- rbind(vertices5, temp_vert)

# Detect duplicate values
unique(edges5, incomparables = FALSE) %>% view()

# Add index value to column in vertices5/edges5
edges5 <- rowid_to_column(edges5, "index")
vertices5 <- rowid_to_column(vertices5, "index")

edges5$index <- as.character(edges5$index)
str(edges5)

vertices5$index <- as.character(vertices5$index)
str(vertices5)

edges5$from <- with(edges5, paste0(index, from))
edges5$to <- with(edges5, paste0(index, to))

vertices5$name <- with(vertices5, paste0(index, name))

# remove row 100 
vertices5 <- vertices5 %>%
    slice(1:99) 

# remove index now that it's purpose is served
edges5 <- edges5[,2:3]
vertices5 <- vertices5[,2:3]

edges5[which(!edges5$to %in% vertices5$name),]

# Try removing white spaces between words
edges5$to <- gsub("\\s+","",edges5$to)
vertices5$name <- gsub("\\s+","",vertices5$name)


edges5a <- edges5 <- edges5 %>%
    slice(1:38)

vertices5a <- vertices5 %>%
    slice(1:38) 

mygraph5 <- graph_from_data_frame(edges5a, vertices = vertices5a)

# Try another example (error) ----
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))


relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))



graph_from_data_frame(actors, vertices = relations)

# Pivot to Sunburst ----
install.packages('sunburstR')
library(sunburstR)


df <- data.frame(
    date = seq.Date(
        as.Date('2014-01-01'),
        as.Date('2016-12-31'),
        by = "days"
    ),
    stringsAsFactors = FALSE
)

df$year = format(df$date, "%Y")
df$quarter = paste0("Q", ceiling(as.numeric(format(df$date,"%m"))/3))
df$month = format(df$date, "%b")
df$path = paste(df$year, df$quarter, df$month, sep="-")
df$count = rep(1, nrow(df))

print(head(data.frame(xtabs(count~path,df))))

sunburst(
    data.frame(xtabs(count~path,df)),
    # added a degree of difficulty by providing
    #  not easily sortable names
    sortFunction = htmlwidgets::JS(
        "
    function(a,b){
    abb = {
    2014:-7,
    2015:-6,
    2016:-5,
    Q1:-4,
    Q2:-3,
    Q3:-2,
    Q4:-1,
    Jan:1,
    Feb:2,
    Mar:3,
    Apr:4,
    May:5,
    Jun:6,
    Jul:7,
    Aug:8,
    Sep:9,
    Oct:10,
    Nov:11,
    Dec:12
    }
    return abb[a.data.name] - abb[b.data.name];
    }
    "
    )
)

sunburst(data.frame(xtabs(count~path,df)))

# data wrangling (women into sunburst)

women_sun <- women %>%
    select(category, country, role) %>%
    mutate(
        path = paste(category, country, role, sep="-"),
        count = 1
    ) %>%
    slice(2:100) %>%
    arrange(category) 

sunburst(data.frame(xtabs(count~path,women_sun)))

sunburst(data = data.frame(xtabs(count~path, women_sun)), colors = c("red", "green", "blue", "yellow", "purple"), percent = FALSE)
sunburst(data = data.frame(xtabs(count~path, women_sun)), colors = c("#d9d9d9","#bc80bd","#ccebc5", "#ffed6f"), percent = FALSE)

# women into sunburt (version 2: country first)

women_sun2 <- women %>%
    select(country, category, role) %>%
    mutate(
        path = paste(country, category, role, sep = "-"),
        count = 1
    ) %>%
    slice(2:100) %>%
    arrange(country)

sunburst(data.frame(xtabs(count~path, women_sun2)))


# add continent column to women
women %>%
    arrange(country) %>%
    view()

asia <-  c('Afghanistan', 'Bangladesh', 'China', 'Exiled Uighur from Ghulja (in Chinese, Yining)', 'Hong Kong', 'India', 'Indonesia', 'Iran', 'Iraq/UK', 'Japan', 'Kyrgyzstan',
           'Lebanon', 'Malaysia', 'Myanmar', 'Nepal', 'Pakistan', 'Singapore', 'South Korea', 'Syria', 'Thailand', 'UAE', 'Vietnam', 'Yemen')
south_america <- c('Argentina', 'Brazil', 'Colombia', 'Ecuador', 'Peru', 'Venezuela')
oceania <- c('Australia')
europe <- c('Belarus', 'Finland', 'France', 'Germany', 'Italy', 'Netherlands', 'Northern Ireland', 'Norway', 'Republic of Ireland', 'Russia', 'Turkey', 'UK', 'Ukraine', 'Wales, UK')
africa <- c('Benin', 'DR Congo', 'Egypt', 'Ethiopia', 'Kenya', 'Morocco', 'Mozambique', 'Nigeria', 'Sierra Leone', 'Somalia', 'Somaliland', 'South Africa', 'Tanzania', 'Uganda', 'Zambia',
            'Zimbabwe')
north_america <- c('El Salvador', 'Jamaica', 'Mexico', 'US')


women2 <- women %>%
    mutate(continent = NA) 
    
women2$continent <- ifelse(women2$country %in% asia, 'Asia', women2$continent)
women2$continent <- ifelse(women2$country %in% south_america, 'South America', women2$continent)
women2$continent <- ifelse(women2$country %in% oceania, 'Oceania', women2$continent)
women2$continent <- ifelse(women2$country %in% europe, 'Europe', women2$continent)
women2$continent <- ifelse(women2$country %in% africa, 'Africa', women2$continent)
women2$continent <- ifelse(women2$country %in% north_america, 'North America', women2$continent)



