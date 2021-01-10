library(ggplot2)
library(dplyr)
library(magick)
library(ggridges)

# read in image, convert to grayscale
img <- image_read("milin2.jpg") %>% image_convert(colorspace = 'gray')

img2 <- image_read("marvin2.jpg") %>% image_convert(colorspace = 'gray')

# get dimensions (milin)
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# get dimensions (marvin)
img2_w <- image_info(img2)$width
img2_h <- image_info(img2)$height
img2_ratio <- img2_w / img2_h



# resize longest dimension to 160 pixels (milin)
if (img_w >= img_h){
    img <- image_resize(img, "160")
} else {
    img <- image_resize(img, ("x160"))
}

# resize longest dimension to 160 pixels (marvin)
if (img2_w >= img2_h){
    img2 <- image_resize(img2, "160")
} else {
    img2 <- image_resize(img2, ("x160"))
}



##---- img[[1]] is 120x160 bitmap array ----###


# create array and number rows and columns (milin)
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# create array and number rows and columns (marvin)
img2_array <- drop(as.integer(img2[[1]]))
rownames(img2_array) <- 1:nrow(img2_array)
colnames(img2_array) <- 1:ncol(img2_array)


# create data frame from array and rename columns (milin)
img_df <- as.data.frame.table(img_array) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
        across(everything(), as.numeric),
        n = row_number()
    ) %>%
    filter(n %% 2 == 0)

# create data frame from array and rename columns (marvin)
img2_df <- as.data.frame.table(img2_array) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
        across(everything(), as.numeric),
        n = row_number()
    ) %>%
    filter(n %% 2 == 0)


# Colors, fill and background
col_fill <- "black"
col_bg <- "white"

# MILIN RIDGELINES
ggplot(img_df) +
    # can vary fill by any column y,x,b, or n
    geom_ridgeline_gradient(aes(x, y, height = b/75, group = y, fill = b), color = "grey30", size = 0.10) +  
    scale_y_reverse() +
    #scale_fill_viridis_c(option = "magma") + other options: "inferno", "plasma" or "viridis"
    scale_fill_viridis_c(option = "magma")+
    coord_cartesian(expand = FALSE) +
    theme_void() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = col_fill, color = NA)
    )  +
    ggsave(("milin_ridges2.png"), dpi = 320, width = 7, height = 7 / img_ratio)


# stat_density_2d...geom 'polygon'
ggplot(img_df, aes(x = b, y = n)) +
    stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(
        legend.position = 'none'
    )
    
# Using geom_tile
ggplot(img_df, aes(x = x, y = y, fill = b)) +
    geom_tile() +
    scale_y_reverse() +
    scale_fill_viridis_c(option = "magma")


# MILIN GEOM TILE (continuous color)
ggplot(img_df) +
    # height = b/80,
    geom_tile(aes(x = x, y = y,  fill = b, height = b/85)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_y_reverse() +
    scale_fill_viridis_c(option = "A") +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        plot.background = element_rect(fill = 'black'),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = 'white', size = 30, face = 'bold', margin = margin(t = 20, r = 0, b = 0, l = 0, unit = 'pt'))
    ) +
    labs(
        title = "Milin"
    )



# MARVIN GEOM TILE (continuous color)
ggplot(img2_df) +
    # height = b/80,
    geom_tile(aes(x = x, y = y,  fill = b, height = b/90)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_y_reverse() +
    scale_fill_viridis_c(option = "A") +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        plot.background = element_rect(fill = 'black'),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = 'white', size = 30, face = 'bold', margin = margin(t = 20, r = 0, b = 0, l = 0, unit = 'pt'))
    ) +
    labs(
        title = "Marvin"
    )


# Error: Insufficient values in manual scale. 235 needed but only 3 provided.
number <- 235

# repeat the given colors enough times
#palette <- rep(c("#7fcdbb", "#41b6c4", "#1d91c0"), length.out = number)

palette <- rep(c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), length.out = number)

# MARVIN GEOM TILE (discrete scale color)
# random distribution
set.seed(23)
#palette <- rep(c("#fff5f0", "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a"), number, replace = TRUE)
#palette <- rep(c("#f6e8c3", "#f5f5f5", "#c7eae5"), number, replace = TRUE)
#palette <- rep(c("#b1efe0", "#adeadb", "#ace2d3"), number, replace = TRUE)  # MINT PASTEL COLOR PALETTE
palette <- rep(c("#b1d3c8", "#ace2d3", "#b1efe0"), number, replace = TRUE)  # MINT PASTEL COLOR PALETTE

ggplot(img2_df) +
    # height = b/80,
    geom_tile(aes(x = x, y = y,  fill = as.factor(b), height = b/100)) +
    scale_y_reverse() +
    # fill must be factor
    #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9')) +
    scale_fill_manual(values=palette) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        plot.background = element_rect(fill = 'black'),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = 'white', size = 30, face = 'bold', margin = margin(t = 20, r = 0, b = 0, l = 0, unit = 'pt'))
    ) +
    labs(
        title = "Marvin"
    )



