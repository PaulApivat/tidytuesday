## Tutorial by Julia Silge
## source: https://juliasilge.com/blog/best-hip-hop/

## RData: rapartist

# load libraries
library(tidyverse)

# sample EDA
# scatter plot with geom_jitter (can also do geom_point())
rankings %>%
    ggplot(mapping = aes(x=year, y=points, color=gender)) 
    + geom_jitter(alpha=0.7) 
    + scale_y_log10() 
    + labs(y = "Critic's Ratings", x = NULL, color = NULL)


###### Spotify API ########
install.packages("spotifyr")
library(spotifyr)
library(tidyverse)
library(knitr)

### FIRST
# Set up a Dev account with Spotify to access their Web API
# access Client ID and Client Secret

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()

### SECOND
# Sample usage of Spotify API
# What was The Beatles' favorite key?

library(tidyverse)
library(knitr)

beatles <- get_artist_audio_features('the beatles')
beatles %>%
    count(key_mode, sort = TRUE) %>%
    head(5) %>%
    kable()

#   Beatles favorite key is D major
#   |key_mode |  n|
#   |:--------|--:|
#   |D major  | 24|
#   |G major  | 21|
#   |A major  | 13|
#   |F major  | 12|
#   |C major  | 11|

### RESUME TUTORIAL

## create function to find Spotify track identifier via search_spotify()
# note: .GlobalEnv
pull_id <- function(query){
    search_spotify(query, "track") %>% 
        arrange(-popularity) %>%
        filter(row_number() == 1) %>%
        pull(id)
}

######
###### Get track identifier (id)

## use purrr::map() to apply it to all songs in the dataset
## takes rankings df
## add search_query and id column(s)

ranking_ids <- rankings %>%
  mutate(
    search_query = paste(title, artist),
    search_query = str_to_lower(search_query),
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))


## explainer

ranking_ids <- rankings %>%
  mutate(
    # select & paste title and artist column(s) from ranking
    search_query = paste(title, artist),
    # change all search queries to lower case
    search_query = str_to_lower(search_query),
    # remove any character that comes after ft - featuring
    # example: Stan Eminem ft. Dido --> stan eminem
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  # find id for each search query by map_chr()
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))

###### Get Audio Features of each track

# note: get_track_audio_features() only takes 100 tracks at most at once
# divide up tracks into smaller chunks, then map() through
ranking_features <- ranking_ids %>%
    mutate(id_group = row_number() %/% 80) %>%
    select(id_group, id) %>%
    nest(data = c(id)) %>%
    mutate(audio_features = map(data, ~ get_track_audio_features(.$id)))

# to see Tibble: 4 x 3
ranking_features

# View(ranking_features[[2]][[2]]) to see nested data

###### Put Audio Features together with Rankings 
### to create a dataframe for modeling

# join ranking_ids and ranking_features
ranking_df <- ranking_ids %>%
    bind_cols(ranking_features %>% 
        select(audio_features) %>% 
        unnest(audio_features)) %>%
    select(title, artist, points, year, danceability:tempo) %>%
    na.omit()

# to see Tibble: 293 x 15
ranking_df

###### Find out how musical attributes are correlated with each other
install.packages("corrr")
library(corrr)

ranking_df %>%
    select(year:tempo) %>%
    correlate() %>%
    rearrange() %>%
    shave() %>%
    rplot(shape = 15, colour = c("darkorange", "white", "darkcyan")) 

## Findings: loudness positively correlated with energy
## danceability negatively correlated with year - older songs more danceable
## valence negatively correlated with year - older songs more happy

###### Train a Linear Model on these Audio Features

ranking_lm <- ranking_df %>%
    select(-title, -artist) %>%
    lm(log(points) ~ ., data = .)

summary(ranking_lm)

## note  'year' was only significant coefficient in the model
## model does *NOT* explain critic's rating well 
## Adjusted R-squared: 0.05653

##### Recap: Data Frames created ######
# function pull_id
# data frame: 
ranking_ids
ranking_features
ranking_df

# linear model
ranking_lm


#######------- Principal Components Analysis--------#######

# question: why do PCA when entire linear model has weak explanatory power?
# answer: PCA - dimensionality reduction - clusters the variables together to potentially improve explanatory power

library(tidymodels)

# tell recipe() what the model is going to be
ranking_rec <- recipe(points ~ ., data = ranking_df) %>%
    # update role for title, artist because these variables we want to keep around for convenience 
    # as identifiers for rows but not a predictor or outcome
    update_role(title, artist, new_role = 'id') %>%
    # take the log of the outcome (points - critic's ratings)
    step_log(points) %>%
    # center and scale the numeric predictors as precursor to implementing PCA
    step_normalize(all_predictors()) %>%
    # implement principal component analysis
    step_pca(all_predictors())

# this steps actually runs the PCA
ranking_prep <- prep(ranking_rec)

ranking_prep

#### Explore PCA

# tidy() recipe step 3 - the PCA step
# NOTE: You can inspect the 12 components in a data frame
tidied_pca <- tidy(ranking_prep, 3)

# NOTE: COULD NOT GET TO WORK
tidied_pca %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component) +
  labs(y = NULL)

##### Zoom in on First Four Components (PC1 - PC4)
library(tidytext)

# Plot NOT showing up correctly
tidied_pca %>%
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
  group_by(component) %>%
  top_n(6, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )


########## Pause PCA ############
########## SINGLE RADAR CHART ##########
install.packages('fmsb')
library(fmsb)

### Create sample data frame to visually see 
### what the data for Radar chart must look like
# source: https://www.r-graph-gallery.com/142-basic-radar-chart.html

# create sample data
# sample(2:20, 10, replace = T) - random sample and permutations; 
# you'll get a different Radar Chart everytime re-run
sample_data <- as.data.frame(matrix(sample(2:20, 10, replace = T), ncol = 10))
colnames(sample_data) <- c("math", "english", "biology", "music", "R-coding", "data-viz", "french", "physics", "statistics", "sports")

# add two lines to dataframe - Max and Min of each topic to show on the plot
# Max -> rep(20,10) "20, ten times"
# Min -> rep(0,10) "0, ten times"
sample_data <- rbind(rep(20,10), rep(0,10), sample_data)

# default Radar Chart
radarchart(sample_data)

##### RADAR CHART: Biggie vs Nas
library(fmsb)

# subset ranking_df for six attributes
ranking_df %>%
    select(danceability, energy, speechiness, acousticness, liveness, valence) -> temp

# subset first row from temp - The Notorious BIG, Juicy
# subset ninth row from temp - Nas, NY State of Mind
# subset fifth row from temp - Dre, Nuthin' But A 'G' Thang
biggie <- temp[1,]
nas <- temp[9,]
dre <- temp[5,]

# Add Min and Max to biggie and nas and dre
biggie <- rbind(rep(1,6), rep(0,6), biggie)
nas <- rbind(rep(1,6), rep(0,6), nas)
dre <- rbind(rep(1,6), rep(0,6), dre)

# default Radar Chart - biggie and nas and dre
radarchart(biggie)
radarchart(nas)
radarchart(dre)


########## MULTI RADAR CHART ##########
install.packages('fmsb')
library(fmsb)

### Create sample data frame to visually see 
### what the data for Radar chart must look like
# source: https://www.r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html

# Create data: note in High school for several students
set.seed(99)
multi_data <- as.data.frame(matrix(sample(0:20, 15, replace = F), ncol = 5))
colnames(multi_data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(multi_data) <- paste("mister", letters[1:3], sep = "-")

# Add Max and Min
multi_data <- rbind(rep(20,5), rep(0,5), multi_data)

# View(multi_data) to see

# Color Vectors (to distinguish the three Radar Charts)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# basic Multi Radar plot with 
    # default options
radarchart(multi_data, axistype = 1, 
    # custom polygon
    pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1, 
    # custom grid
    cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8, 
    # custom labels
    vlcex = 0.8)

# add legend
legend(x=0.7, y=1, legend = rownames(multi_data[-c(1,2),]), 
        bty = "n", 
        pch = 20, 
        col = colors_in, 
        text.col = 'grey', 
        cex = 1.2, 
        pt.cex = 3)

##### Multi-Radar Chart for Biggie, Nas & Dre

# copy Biggie, Dre and Nas into one data frame
# temp[1,] , temp[5,] , temp[9,]
# NOTE: must add ALL three at once
multi_rapper <- temp[c(1,5,9),]

# Add Max and Min - two rows
multi_rapper <- rbind(rep(1,6), rep(0,6), multi_rapper)

# set row names
rownames(multi_rapper)[3] <- "Juicy, Biggie"
rownames(multi_rapper)[4] <- "Nuthin But.., Dr Dre"
rownames(multi_rapper)[5] <- "NY State of Mind, Nas"

# Color Vectors (to distinguish three Radar Charts)
# re-use one from example above
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


# re-create basic Multi Radar plot  
radarchart(multi_rapper, axistype = 1, 
    pcol = colors_border, 
    pfcol = colors_in, 
    plwd = 4, 
    plty = 1, 
    cglcol = 'grey', 
    cglty = 1, 
    axislabcol = 'grey', 
    # interpretation: seq(least, most, space-between)
    caxislabels = seq(0,1,.25), 
    cglwd = 0.8, 
    vlcex = 0.8)

# add legend

    # position: lower-left; 
    # NOTE: x=0, y=0 is dead center of spider-chart
legend(x=-1.25, y=-1, 
        # NOTE: this works better than rownames(multi_rapper[-c(1,2),])
        legend = rownames(multi_rapper)[3:5], 
        bty = "n", 
        pch = 20, 
        col = colors_in, 
        text.col = 'grey', 
        cex = 1.2, 
        pt.cex = 3)

#### Customize Multi_Rapper Radar Chart

# customize color - neon pink (#ff14af), green (#39ff14), purple (#6514ff)
# note: MUST set maxColorValue = 255 to use rgb 0-255
# note: alpha sets transparency level, 0 = completely transparent, 255 = solid
colors_border1=c( rgb(255,20,175, maxColorValue = 255), rgb(57,255,20, maxColorValue = 255) , rgb(101,20,255, maxColorValue = 255) )
colors_in1=c( rgb(255,20,175, alpha = 75, maxColorValue = 255), rgb(57,255,20, alpha = 75, maxColorValue = 255) , rgb(101,20,255, alpha = 75, maxColorValue = 255) )

radarchart(multi_rapper, axistype = 1, 
# set custom colors
pcol = colors_border1, 
pfcol = colors_in1, 
plwd = 4, 
plty = 1, cglcol = 'grey', cglty = 1, axislabcol = 'grey', caxislabels = seq(0,1,.25), cglwd = 0.8, vlcex = 0.8)


# set custom color in legend
legend(x=-1.25, y=-1, legend = rownames(multi_rapper)[3:5], bty = "n", pch = 20, col = colors_border1, text.col = 'grey', cex = 1.2, pt.cex = 3)

#### Final Customize Multi_Rapper Radar Chart #####
#### This is Base R, more difficult ##############

# set background color
par(bg = '#4f5b66')

# custom function just to change the text-label color
# source: https://stackoverflow.com/questions/54185029/change-labels-colors-in-r-radarchart

radarchart2 <- function (df, axistype = 1, seg = 4, pty = 16, pcol = colors_border1, plty = 1, 
+                          plwd = 1, pdensity = NULL, pangle = 45, pfcol = colors_in1, cglty = 1, 
+                          cglwd = 0.8, cglcol = "white", axislabcol = "white", vlabcol = "white", title = "", 
+                          maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
+                          vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
+                          palcex = NULL, ...) 
+ {
+     if (!is.data.frame(df)) {
+         cat("The data must be given as dataframe.\n")
+         return()
+     }
+     if ((n <- length(df)) < 3) {
+         cat("The number of variables must be 3 or more.\n")
+         return()
+     }
+     if (maxmin == FALSE) {
+         dfmax <- apply(df, 2, max)
+         dfmin <- apply(df, 2, min)
+         df <- rbind(dfmax, dfmin, df)
+     }
+     plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
+          axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
+          ...)
+     theta <- seq(90, 450, length = n + 1) * pi/180
+     theta <- theta[1:n]
+     xx <- cos(theta)
+     yy <- sin(theta)
+     CGap <- ifelse(centerzero, 0, 1)
+     for (i in 0:seg) {
+         polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
+                                                                    CGap), lty = cglty, lwd = cglwd, border = cglcol)
+         if (axistype == 1 | axistype == 3) 
+             CAXISLABELS <- paste(i/seg * 100, "(%)")
+         if (axistype == 4 | axistype == 5) 
+             CAXISLABELS <- sprintf("%3.2f", i/seg)
+         if (!is.null(caxislabels) & (i < length(caxislabels))) 
+             CAXISLABELS <- caxislabels[i + 1]
+         if (axistype == 1 | axistype == 3 | axistype == 4 | 
+             axistype == 5) {
+             if (is.null(calcex)) 
+                 text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
+                      col = axislabcol)
+             else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
+                       col = axislabcol, cex = calcex)
+         }
+     }
+     if (centerzero) {
+         arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
+                length = 0, col = cglcol)
+     }
+     else {
+         arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
+                    1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
+     }
+     PAXISLABELS <- df[1, 1:n]
+     if (!is.null(paxislabels)) 
+         PAXISLABELS <- paxislabels
+     if (axistype == 2 | axistype == 3 | axistype == 5) {
+         if (is.null(palcex)) 
+             text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
+         else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
+                   cex = palcex)
+     }
+     VLABELS <- colnames(df)
+     if (!is.null(vlabels)) 
+         VLABELS <- vlabels
+     if (is.null(vlcex)) 
+         text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol)
+     else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol)
+     series <- length(df[[1]])
+     SX <- series - 2
+     if (length(pty) < SX) {
+         ptys <- rep(pty, SX)
+     }
+     else {
+         ptys <- pty
+     }
+     if (length(pcol) < SX) {
+         pcols <- rep(pcol, SX)
+     }
+     else {
+         pcols <- pcol
+     }
+     if (length(plty) < SX) {
+         pltys <- rep(plty, SX)
+     }
+     else {
+         pltys <- plty
+     }
+     if (length(plwd) < SX) {
+         plwds <- rep(plwd, SX)
+     }
+     else {
+         plwds <- plwd
+     }
+     if (length(pdensity) < SX) {
+         pdensities <- rep(pdensity, SX)
+     }
+     else {
+         pdensities <- pdensity
+     }
+     if (length(pangle) < SX) {
+         pangles <- rep(pangle, SX)
+     }
+     else {
+         pangles <- pangle
+     }
+     if (length(pfcol) < SX) {
+         pfcols <- rep(pfcol, SX)
+     }
+     else {
+         pfcols <- pfcol
+     }
+     for (i in 3:series) {
+         xxs <- xx
+         yys <- yy
+         scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
+                                                              ] - df[2, ]) * seg/(seg + CGap)
+         if (sum(!is.na(df[i, ])) < 3) {
+             cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
+                         df[i, ]))
+         }
+         else {
+             for (j in 1:n) {
+                 if (is.na(df[i, j])) {
+                     if (na.itp) {
+                         left <- ifelse(j > 1, j - 1, n)
+                         while (is.na(df[i, left])) {
+                             left <- ifelse(left > 1, left - 1, n)
+                         }
+                         right <- ifelse(j < n, j + 1, 1)
+                         while (is.na(df[i, right])) {
+                             right <- ifelse(right < n, right + 1, 
+                                             1)
+                         }
+                         xxleft <- xx[left] * CGap/(seg + CGap) + 
+                             xx[left] * (df[i, left] - df[2, left])/(df[1, 
+                                                                        left] - df[2, left]) * seg/(seg + CGap)
+                         yyleft <- yy[left] * CGap/(seg + CGap) + 
+                             yy[left] * (df[i, left] - df[2, left])/(df[1, 
+                                                                        left] - df[2, left]) * seg/(seg + CGap)
+                         xxright <- xx[right] * CGap/(seg + CGap) + 
+                             xx[right] * (df[i, right] - df[2, right])/(df[1, 
+                                                                           right] - df[2, right]) * seg/(seg + 
+                                                                                                             CGap)
+                         yyright <- yy[right] * CGap/(seg + CGap) + 
+                             yy[right] * (df[i, right] - df[2, right])/(df[1, 
+                                                                           right] - df[2, right]) * seg/(seg + 
+                                                                                                             CGap)
+                         if (xxleft > xxright) {
+                             xxtmp <- xxleft
+                             yytmp <- yyleft
+                             xxleft <- xxright
+                             yyleft <- yyright
+                             xxright <- xxtmp
+                             yyright <- yytmp
+                         }
+                         xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
+                                                xxleft)/(yy[j] * (xxright - xxleft) - 
+                                                             xx[j] * (yyright - yyleft))
+                         yys[j] <- (yy[j]/xx[j]) * xxs[j]
+                     }
+                     else {
+                         xxs[j] <- 0
+                         yys[j] <- 0
+                     }
+                 }
+                 else {
+                     xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
+                         (df[i, j] - df[2, j])/(df[1, j] - df[2, 
+                                                              j]) * seg/(seg + CGap)
+                     yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
+                         (df[i, j] - df[2, j])/(df[1, j] - df[2, 
+                                                              j]) * seg/(seg + CGap)
+                 }
+             }
+             if (is.null(pdensities)) {
+                 polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
+                                                                       2], border = pcols[i - 2], col = pfcols[i - 
+                                                                                                                   2])
+             }
+             else {
+                 polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
+                                                                       2], border = pcols[i - 2], density = pdensities[i - 
+                                                                                                                           2], angle = pangles[i - 2], col = pfcols[i - 
+                                                                                                                                                                        2])
+             }
+             points(xx * scale, yy * scale, pch = ptys[i - 2], 
+                    col = pcols[i - 2])
+         }
+     }
+ }


# call radarchart2() function, specify label color
radarchart2(multi_rapper, vlabcol = "white")

# legend with custom colors to match

# position (lower left) of legend
# NOTE: x=0, y=0 is dead center of spider-chart
legend(x=-1.25, y=-1, 
    # NOTE: this works better than rownames(multi_rapper[-c(1,2),])
    legend = rownames(multi_rapper)[3:5], 
    bty = "n", 
    pch = 20, 
    col = colors_border1, 
    text.col = 'white', 
    cex = 1.2, 
    pt.cex = 3)

