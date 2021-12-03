# <!---------------------------------------------------------- Notes ----------------------------------------------------------->
#
#
#
# <!---------------------------------------------------------------------------------------------------------------------------->

# This script contains additional material:
# - Some visualizations of the WCS data
# 

# Packages
library(colorspace)
library(maps)
library(rvest)
library(scatterplot3d)
library(viridis)
library(xml2)
library(tidymodels)
library(tidyverse)

read_from_db

# Visualization **************************************************************************************************************** #


# Age/sex frequency distribution ----------------------------------------------------------------------------------------------- #

# Plot the distribution of age and sex per age group
dl$speaker %>%
  mutate(speaker_age = num_speaker_age(speaker_age)) %>%
  ggplot(aes(x = cut_width(speaker_age,10, boundary = 0), fill = speaker_sex)) +
  geom_bar(alpha = 0.8) + 
  labs(x = "Age Groups", y = "Frequency", fill = "Sex") +
  scale_fill_manual(values=c("#4271AE","#FF6347"), na.value = "grey60") +
  coord_fixed(ratio = 1/200) +
  theme(
    text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  ) 

# Speaker per language -------------------------------------------------------------------------------------------------------- #

# Plots the amount of speaker per language and the amount of terms per language
left_join(dl$speaker %>% group_by(lang_nr) %>% count(),
          dl$dict %>% group_by(lang_nr) %>% count() %>% rename(m = n)) %>%
  ggplot(aes(x = lang_nr, y = n)) +
  geom_point(aes(size = m, colour = cut_width(m, width = 10, boundary = 0)), alpha = 0.8) +
  #coord_flip() +
  scale_colour_viridis_d(direction = -1) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
  geom_hline(yintercept = mean(23.81), color="red", linetype = "dashed") +
  labs(x = "Language", y = "Frequency", colour = "Terms per \n language") + 
  coord_fixed(ratio = 1.5/1) +
  theme(
    text = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm")
  ) +
  guides(size = FALSE)

# Geographical distribution of the languages ----------------------------------------------------------------------------------- #

# We need geospatial data (Latitude and Longitude coordinates), to assign each country a position on the coordinate system
# Therefore we download a table which contains a list of all countries in the world and those coordinate values.
wcs_iso_codes <- function() {  
  url3 <- "https://www1.icsi.berkeley.edu/wcs/WCS_SIL_codes.html"
  file <- xml2::read_html(url3)
  tables <- rvest::html_nodes(file, "table")
  
  codes <- 
    tibble::as_tibble(rvest::html_table(tables[[1]], fill = TRUE)) %>%
    dplyr::rename(lang_nr = Index, 
                  lang_name = Language, 
                  iso_693 = `ISO 639-3 Code`, 
                  family = Family, 
                  lang_country = `Country Where`) %>%
    dplyr::mutate(lang_country = dplyr::recode(lang_country, 
                                               `Mexico|` = "Mexico",
                                               `Columbia` = "Colombia",
                                               `Indonesia (Irian Jaya)` = "Indonesia",
                                               `Peru, Brazil` = "Peru",
                                               `USA, Mexico` = "Mexico",
                                               `Nigeria, Cameroon` = "Nigeria")
    )
  return(codes)
}

get_lat_long <- function() {
  url4 <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
  file <- read_html(url4)
  tables <- html_nodes(file, "table")
  lat_long <- as_tibble(html_table(tables[[1]], fill = TRUE)) %>%
    rename(lang_country = name,
           lat = latitude,
           long = longitude) %>%
    select(-country) %>%
    mutate(lang_country = recode(lang_country, `Côte d'Ivoire` = "Ivory Coast",
                                 `Suriname` = "Surinam",
                                 `United States` = "USA"))
}

# We take the data from the language dictionary and merge them with their corresponding coordinate values
wcs_iso_codes <- left_join(wcs_iso_codes(), 
                           get_lat_long(), 
                           by = "lang_country")

# Plot the world map, which shows in which countries the survey took place and how many speakers there are per country
left_join(wcs_iso_codes, 
          dl$speaker) %>% 
  group_by(lang_country, lat, long) %>% 
  count() %>%
  ggplot(aes(y = lat, x = long, colour = n, size = n)) +
  geom_point(alpha = 0.65) +
  borders("world") +
  coord_quickmap() +
  scale_colour_viridis_c(name = "Freq") +
  scale_x_continuous(expand=c(0,0), 
                     name = element_blank()) + 
  scale_y_continuous(expand=c(0,0), 
                     name = element_blank()) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed(1.15/1) + 
  guides(size = FALSE)

# Lab color space -------------------------------------------------------------------------------------------------------------- #

# We take the Lab values for all color chips and make a special object out of them with LAB funtion
lab_values <- dl$mun_2_lab %>% select(7:9) 
names(lab_values) <- c("L", "a", "b")

lab_colorspace <- 
  colorspace::LAB(lab_values[[1]],
                  lab_values[[2]], 
                  lab_values[[3]])

# A 2D plot of the Lab color space for the chips
ggplot(lab_values, aes(x = a, y = b)) +
  geom_point(size = 3, aes(color = hex(lab_colorspace, fixup = TRUE))) +
  scale_color_identity() +
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(size=12)) +
  theme_bw()

# A 3D plot of the Lab color space for the chips
scatterplot3d(lab_values[[3]], 
              lab_values[[2]], 
              lab_values[[1]], 
              color = hex(lab_colorspace, fixup = TRUE), 
              pch = 19,
              angle =100, 
              scale.y=0.4, 
              xlab = "a", ylab = "b", zlab = "L")

# Additional Lab plot with further perspectives
plot(b)

# Frequency distribution of color chips in the foci task ------------------------------------------------------------------------ #

# Contour plot of the freq dist as in (Kay & Regier, 2003)
foci_task %>%
  mutate(grid_row = as.numeric(grid_row)) %>%
  filter(chip_nr != 141 & chip_nr != 89) %>%
  ggplot(aes(x = grid_col, y = grid_row, z = chip_nr)) +
  geom_density_2d_filled(alpha = 0.9, contour_var = "count") + 
  scale_y_reverse(expand=c(0,0), breaks = 1:9, labels = paste(LETTERS[1:9])) + 
  theme(legend.position="none",
        panel.background = element_rect(fill = NA),
        panel.grid.minor.x = element_line(colour="white", size=0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(colour="white", size=0.2),
        panel.ontop = TRUE) + 
  scale_x_continuous(expand=c(0,0), breaks = seq(0,40, 5), minor_breaks = seq(0, 40, 1)) + 
  coord_fixed(ratio=2/1) 

# Different way of plotting it (Tile plot)
foci_task %>%
  mutate(grid_row = as.numeric(grid_row)) %>%
  filter(chip_nr != 141 & chip_nr != 89) %>%
  group_by(grid_col, grid_row) %>% 
  count(chip_nr) %>%
  ggplot() +
  geom_tile(aes(x = grid_col, y = grid_row, fill = n)) +
  scale_y_reverse(expand=c(0,0), breaks = 1:9, labels = paste(LETTERS[1:9])) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        panel.grid.minor.x = element_line(colour="white", size=0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(colour="white", size=0.2),
        panel.ontop = TRUE) + 
  scale_x_continuous(expand=c(0,0), breaks = seq(0,40, 5), minor_breaks = seq(0, 40, 1)) + 
  coord_fixed(ratio=2/1) +
  scale_fill_viridis_c()

# Plotting the frequency distribution of the color chips
chips_plot_freq_dist <- function() {
  # select data of concern: chip nrs, their frequency and corresponding lab values
  df <- foci_task %>%
    group_by(L_star, a_star, b_star) %>%
    count(chip_nr) %>% 
    arrange(-n) %>%
    ungroup()
  
  # select lab_values and create colorspace object
  lab_cs <- function(df) {
    lv <- df %>% 
      select(1:3) 
    cs <- LAB(lv[[1]],
              lv[[2]],
              lv[[3]])
  }
  cs <- lab_cs(df)
  
  # plot
  df %>%
    ggplot(aes(x = reorder(chip_nr, -n), y = n)) + 
    geom_bar(stat = "identity", fill = hex(cs, fixup = TRUE)) +
    scale_x_discrete(breaks = df$chip_nr[seq(1,330, 15)], 
                     expand = c(0.02,0), 
                     name = "chip_nr") +
    scale_y_log10(breaks = c(10, 50, 200, 1000),
                  name = "Freq") +
    theme(panel.background = element_rect(fill = "grey50", colour = NA),
          #panel.border =      theme_rect(fill = NA, colour="grey50"), 
          panel.grid.major =  element_blank(),
          panel.grid.minor =  element_blank() #,
          # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          # plot.background = element_rect(fill = "grey60")
    ) + 
    coord_fixed(ratio=20/1)
}

chips_plot_freq_dist()
