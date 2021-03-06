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

# plot sunburst with continent (continent, country) (women_sun3)
women_sun3 <- women2 %>%
    select(continent, country, category, role) %>%
    mutate(
        path = paste(continent, country, category, role, sep = "-"),
        count = 1
    ) %>%
    slice(2:100) %>%
    arrange(continent)

# women_sun4
women_sun4 <- women2 %>%
    select(continent, category, country, role) %>%
    mutate(
        path = paste(continent, category, country, role, sep = "-"),
        count = 1
    ) %>%
    slice(2:100) %>%
    arrange(continent)

# women_sun5
women_sun5 <- women2 %>%
    select(category, continent, country, role) %>%
    mutate(
        path = paste(category, continent, country, role, sep = "-"),
        count = 1
    ) %>%
    slice(2:100) %>%
    arrange(category) %>%
    view()


    
sunburst(data = data.frame(xtabs(count~path, women_sun3)), 
         colors = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33"))



sunburst(data = data.frame(xtabs(count~path, women_sun4)), 
                #red(switch)        #blue      #green    #purple     #orange   #yellow
         colors = c("#00441b", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
                #pink(switch)     #light-blue  #light-green   #light-purple
                    "#e41a1c", "#b3cde3", "#ccebc5", "#decbe4",
            #dark-green(switch) - sequential greens                                                                  - then purple
                    "#fbb4ae", "#006d2c", "#238b45", "#41ae76", "#66c2a4", "#99d8c9", "#ccece6", "#e5f5f9", "#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c", "#4d004b"))


# NOTE: Try Continent + Countries being the same shade (implies cateogry, continent, country, role)
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, qualitative, 4-class Set1
         colors = c("#7570b3", "#377eb8", "#4daf4a", "#984ea3",
        # continent: (switch) 6-class Dark2
                    "#e41a1c", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
# Asia (#7570b3) x 34 (switch)    
                    "#1b9e77", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3",
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3",
#Africa(#d95f02) x 21         
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02",
# Europe (#e7298a) x 22
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a",
# S.Amer (#e6ab02) x 8 
                    "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02",
# N.Amer (#66a61e) x 12
                    "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e",
                    "#66a61e", "#66a61e", 
# Oceania ("#1b9e77") x 2
                    "#1b9e77", "#1b9e77",
# There's actually 99 roles          
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC",
                    "#DCDCDC", "#DCDCDC", "#DCDCDC", "#DCDCDC"), legend = FALSE)





# Final attempt for color alignment ----
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, qualitative, 4-class Set1
         colors = c("#7570b3", "#377eb8", "#4daf4a", "#984ea3",
                    # continent: (switch) 6-class Dark2
                    "#e41a1c", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
                    # Asia (#7570b3) x 70 (switch)    
                    "#1b9e77", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3",
                    "#1b9e77", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3",
                    "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", "#7570b3", 
                    #Africa(#d95f02) x 45         
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    "#d95f02", "#d95f02", "#d95f02", "#d95f02", "#d95f02",
                    # Europe (#e7298a) x 44
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    "#e7298a", "#e7298a", "#e7298a", "#e7298a",
                    # S.Amer (#e6ab02) x 16 
                    "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02",
                    "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", "#e6ab02", 
                    # N.Amer (#66a61e) x 26
                    "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e",
                    "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e",
                    "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e", "#66a61e",
                    # Oceania ("#1b9e77") x 4
                    "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77"), legend = FALSE)



 
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, qualitative, 4-class Set1
         colors = c("#8c510a", "#377eb8", "#4daf4a", "#984ea3",
                    # continent: (switch) 6-class BrBG (diverging)
                    "#e41a1c", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"), legend = FALSE)


# see sunburstR package
# create women_sun5 from women2
# women2 = manually create continents
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, qualitative, 4-class Set1
         colors = c("#8c510a", "#377eb8", "#4daf4a", "#984ea3",
        # continent: (switch) 11-class BrBG (diverging)
                    "#e41a1c", "#bf812d", "#dfc27d", "#80cdc1", "#35978f", "#01665e"), legend = FALSE)


# Two Sequential Tracks Version - Grey and Reds
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, sequential, single-hue 9-class Grey
         colors = c("#969696", "#737373", "#525252", "#252525",
        # continent: (switch) sequential, single-hue, 6-class Reds
                    "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"), legend = FALSE)



# Two Sequential Tracks Version - YlGnBu and Reds
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: colorbrewer, sequential, multi-hue 9-class YlGnBu
         colors = c("#41b6c4", "#1d91c0", "#225ea8", "#253494",
        # continent: (switch) sequential, single-hue, 6-class Reds
                    "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"), legend = FALSE)


# Two Sequential Tracks Version - Mint Green and Royal Red
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: mint green
         colors = c("#ffcce7", "#6ce0b1", "#2dd290", "#1f9365",
                    # continent: (switch) royal red
                    "#abedd3", "#ff99cf", "#ff66b8", "#ff33a0", "#ff0088", "#cc006d"), legend = FALSE)

# FINAL ----
# see sunburstR package
# create women_sun5 from women2
# women2 = manually create continents

# Two Sequential Tracks Version - Mint Green and Black/Grey
sunburst(data = data.frame(xtabs(count~path, women_sun5)), 
         # category: mint green
         colors = c("#e6e6e6", "#6ce0b1", "#2dd290", "#1f9365",
        # continent: (switch) Black/Gray shades
                    "#abedd3", "#bfbfbf", "#999999", "#737373", "#4d4d4d", "#262626"), legend = FALSE)

women2 %>%
    #filter(continent=='Africa') %>%
    count(country, sort = TRUE)

# count number of country shapes
women2 %>%
    group_by(continent) %>%
    count(country, sort = TRUE) %>%
    summarize(
        sum_n = sum(n)
    )

# count number of role shapes
women2 %>%
    group_by(continent, role) %>%
    count(role, sort = TRUE) %>%
    summarize(
        sum_n = sum(n)
    ) %>%
    summarize(
        sum_role = sum(sum_n)
    )

women2 %>%
    group_by(continent, country) %>%
    count(role, sort = TRUE) %>%
    arrange(continent) %>%
    view()
    
# another s/o example ----
leafs <- c("base","base-child1-grandchild1","base-child1-grandchild2","base-child1","base-child2-grandchild3","base-child2","base-child3-grandchild4","base-child3-grandchild5","base-child3-grandchild6","base-child3-grandchild7","base-child3")
values <- c(200,15,10,20,55,10,120,30,30,20,10)  

# colors
colors <- c("#c994c7","#6a51a3","#807dba","#bcbddc","#74c476","#c7e9c0","#fcbba1","#fc9272","#ef3b2c","#cb181d","#99000d")
# match those colors to leaf names, matched by index
labels <- c("base","grandchild1","grandchild2","child1","child2","grandchild3","grandchild4","grandchild5","grandchild6","grandchild7","child3")

df = data.frame(v1=leafs, v2=values);

sunburst(df, 
         colors = list(range = colors, domain = labels))




