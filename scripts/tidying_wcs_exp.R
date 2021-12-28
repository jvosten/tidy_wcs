
# <!---------------------------------------------------------- Notes ----------------------------------------------------------->
#
#
#
# <!---------------------------------------------------------------------------------------------------------------------------->

# Packages
library(colorspace)
library(maps)
library(rvest)
library(scatterplot3d)
library(skimr)
library(viridis)
library(xml2)
library(tidymodels)
library(tidyverse)


# Read in data ***************************************************************************************************************** #

read_fkt <- function() { # to do: den variable naming teil in die read_web fkt ziehen; local fkt schreiben, in der files lokal ausgelesen werden (aus dem package)
  # Version 1: web --------------------------------------------------------------------------------------------------------------- #
  
  read_web <- function(){
    # Urls for all files of interes
    url <-"https://www1.icsi.berkeley.edu/wcs/data/20041016/WCS-Data-20110316.zip"
    url2 <- "https://www1.icsi.berkeley.edu/wcs/data/cnum-maps/cnum-vhcm-lab-new.txt"
    
    # Creating temporary files
    temp <- tempfile()
    temp2 <- tempfile()
    
    # Store and unzip 
    download.file(url, temp)
    unzip(zipfile = temp, exdir = temp2)
    
    # Create a path list to the data
    list_of_files <- list.files(path = temp2, recursive = TRUE,
                                pattern = "\\.txt$",
                                full.names = TRUE)
    
    # Read in data into a list of data frames (dfl)
    custom_reader <- function(x){
      if(endsWith(x, "foci-exp.txt")){
        readr::read_table(x, col_names = FALSE)
      }else{
        readr::read_tsv(x, col_names = FALSE)
      }
    }
    
    # Read in data into a list of data frames (dfl)
    dl <- list_of_files %>%
      purrr::set_names(.) %>%
      purrr::map(., custom_reader)
    
    # Remove temp files 
    unlink(c(temp, temp2), recursive = TRUE)
    
    # Move foci data in its own object and force it into global env
    foci <<- dl[[4]]
    dl <- dl[-4]
    
    # Read in the last txt file and add it to dfl
    dl <- c(dl[1], 
            list("cnum-vhcm-lab-new.txt" = readr::read_tsv(url2, col_names = FALSE)), 
            dl[2:6])
    # return a list object entailling df and foci
    # list(dl, foci)
  }
  
  dl <- read_web() # kommt weg
  
  read_local <- function() {
    # Version 2: local ------------------------------------------------------------------------------------------------------------- #
    
    # # Create a path list to the data: add the path to directory you unpacked the zip in and where you stored cnum-vhcm-lab-new.txt
    # list_of_files <- list.files(path = "./WCS_data", recursive = TRUE,
    #                             pattern = "\\.txt$", 
    #                             full.names = TRUE)
    # 
    # # Removes the readme of cnum-vhcm-lab-new.txt, if it is in the directory
    # list_of_files <- subset(list_of_files, !endsWith(list_of_files, "README.txt"))
    # 
    # # Read in data into a list of data frames (dfl)
    # dl <- list_of_files %>%
    #   #set_names(.) %>%
    #   map(read_tsv, col_names = FALSE) 
    # 
    # # Move foci data in its own object
    # foci <- dl[[5]]
    # dl <- dl[-5]
    #
    # return a list object entailling df and foci
    # list(dl, foci)
  }
  
  # Name all Variables of each df ------------------------------------------------------------------------------------------------ #
  
  # Name each df in dfl
  names(dl) <- c("chip", "mun_2_lab", 
                 "dict", "foci_exp", 
                 "lang", "speaker", 
                 "term") 
  
  # set all variable names
  colnames(dl$chip) <- c("chip_nr", "grid_row", "grid_col", 
                         "grid_coord")
  colnames(dl$mun_2_lab) <- c("chip_nr", "wcs_mv", "wcs_mh", 
                              "mun_chroma", "mun_hue", "mun_value", 
                              "L_star", "a_star", "b_star")
  colnames(dl$dict) <- c("lang_nr", "term_nr", "term", 
                         "term_abb")
  colnames(dl$foci_exp) <- c("lang_nr", "speaker_nr", "focus_response", 
                             "term_abb", "grid_coord")
  colnames(dl$lang) <- c("lang_nr", "lang_name", "lang_country", 
                         "field_worker", "field_worker_2", "field_worker_3", 
                         "Orig_file", "File_type")
  colnames(dl$speaker) <- c("lang_nr", "speaker_nr", "speaker_age", 
                            "speaker_sex") 
  colnames(dl$term) <- c("lang_nr", "speaker_nr", "chip_nr", 
                         "term_abb")
  
  # colnames for the additional foci file
  colnames(foci) <- c("lang_nr", "speaker_nr", "focus_response", 
                             "term_abb", "grid_coord")
  
  # Two data frames had column names, which turned into the first row now; we delete them
  dl$mun_2_lab <- dl$mun_2_lab[-1,]
  dl$dict <- dl$dict[-1,]
  
  # Empty list for plots
  plots <- list()
  #####
  # if(local = TRUE) {
  #   read_local() } else {
  #     read_web()
  #   }
}

#c(dl, foci) %<-% read_fkt() 
# Overview on variable description --------------------------------------------------------------------------------------------- #

vars <- tibble::tibble("WCS Variable Name" = c("WCS Chip Number", "WCS Grid Row", "WCS Grid Columns", "Concatenation of fields", #1
                                       "V", "H", "C", "MunH", "MunV", "L*", "a*", "b*", #2
                                       "WCS Language Number", "Term Number", "Term", "Term Abbreviation", #3
                                       "WCS Speaker Number", "WCS Focus Response", #4
                                       "WCS Language Name", "WCS Language Geographic Location", "Field Worker", #5
                                       "WCS Speaker Age", "WCS Speaker Sex" #6
                                       ),
                "Our Variable Name" = c("chip_nr", "grid_row", "grid_col", "grid_coord", #1
                                        "wcs_mv", "wcs_mh", "mun_chroma", "mun_hue", "mun_value", 
                                        "L_star", "a_star", "b_star", #2
                                        "lang_nr", "term_nr", "term", "term_abb", #3
                                        "speaker_nr", "focus_response", #4
                                        "lang_name", "lang_country", "field_worker", #5
                                        "speaker_age", "speaker_sex" #6
                                         ),
                "Description" = c("Values from 1 to 330", "Values from A to J", "Values from 0 to 40", 
                                  "Concatenation of grid_row and grid_col",#1
                                  "WCS code for Munsell Value", "WCS code for Munsell Hue", "Munsell Chroma", "Munsell Hue", 
                                  "Munsell Value", "CIEL*a*b* `L*` reference value", "CIEL*a*b* `a` reference value", 
                                  "CIEL*a*b* `b*` reference value", #2
                                  "Values from 1 to 110", "Sequential numbering of terms per language", 
                                  "Transcription of the color term", "A unique abbreviation of the term", #3
                                  "Unique number of speakers per language", "Sequential enumeration of focus responses", #4
                                  "Name of each language", "Country", "Field worker", #5
                                  "Age, in years", "Sex, male or female" #6
                                  )
  )


# Tidy ************************************************************************************************************************* #


# Overview about all row and column length-------------------------------------------------------------------------------------- #

ov <- dl %>%
  purrr::map_df(ncol)
ov[2,] <- dl %>%
  purrr::map_dfr(nrow)

# Function for Stat summary----------------------------------------------------------------------------------------------------- #

my_skim <- skimr::skim_with(
  numeric = skimr::sfl(complete_rate = NULL, 
                p25 = NULL, 
                p50 = NULL, 
                p75 = NULL, 
                hist = NULL),
  character = skimr::sfl(empty = NULL,
                  whitespace = NULL)
)

# Print function for markdown
skim_print <- function(df){
  m <- dplyr::if_else(all(purrr::map_lgl(df, is.character)), 7, 11)
  
  my_skim(df) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename_with(., ~ gsub("character", "char", .x, fixed = TRUE), dplyr::starts_with("char")) %>% 
    dplyr::rename_with(., ~ gsub("numeric", "num", .x, fixed = TRUE), dplyr::starts_with("num")) %>% 
    dplyr::select(variable = skim_variable, data_type = skim_type, 3, all_of(5:m)) %>%
    dplyr::mutate(dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 1)),
                  dplyr::across(dplyr::everything(), as.character)) %>% 
    replace(is.na(.), "-")  # Noch die digits begrenzen
}


#### Tidy every single data frame ---------------------------------------------------------------------------------------------- # 


### dl$chip -------------------------------------------------------------------------------------------------------------------- # 

skim_print(dl$chip)

# Change vars in factors
dl$chip <- dl$chip %>%
  dplyr::mutate(dplyr::across(c(grid_row, grid_coord), as.factor))

### dl$mun_2_lab --------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$mun_2_lab)
# Looks good; since nrow = 330 

# Make all vars numeric or factor
dl$mun_2_lab <- dl$mun_2_lab %>%
  dplyr::mutate(dplyr::across(!c(wcs_mv, mun_hue), as.numeric),
                dplyr::across(c(wcs_mv, mun_hue), as.factor))

# Test, if factor variables have the right amount of levels
length(levels(dl$mun_2_lab$wcs_mv))

### dl$lang -------------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$lang)
# The summary statistic looks good; it contains all 110 language name. But if we look at the actual data frame, we see, that there 
# are a lot of missing values decoded by "*". We will transform them in NA values and we will further drop the author and file 
# variables, as they are sort of meta data.

dl$lang <- dl$lang %>% 
  dplyr::select(lang_nr, lang_name, lang_country) %>%
  dplyr::mutate(lang_country = na_if(lang_country, "*"))

# At this point we drop the variables author, file and file status; if there is an interest in those files, the above selection
# function can be replaced wit the following:
# dl$lang <- dl$lang %>% 
#   unite(Author, Author1:Author3, sep = " ") %>%
#   mutate(lang_country = na_if(lang_country, "*"))

# Since half of the country names are missing and some languages contain special characters which cause problems when displaying
# the data, we use the "ISO 639-3 codes" file from the "Primary WCS Data" section of the archive and substitute lang_name and
# lang_country with values from that file by substituting the old dl$lang data frame with the newly created one

# Downloading the additional table, transforming the html table into R data frame 
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

# Substitute the data frames
dl$lang <- wcs_iso_codes() %>%
  dplyr::select(lang_nr, lang_name, lang_country)

### dl$speaker ----------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$speaker)

# Investigating speaker_sex ---------------------------------------------------------------------------------------------------- #

# filter, which other values then M and F are contained in speaker_sex
dl$speaker %>% 
  dplyr::filter(!speaker_sex %in% c("M","F")) %>%
  dplyr::distinct(speaker_sex)

# Frequency distribution for all values of speaker_sex
dl$speaker %>% 
  dplyr::select(speaker_sex) %>% 
  table()

# Frequency distribution of non male/female values in speaker sex 
dl$speaker %>% 
  dplyr::select(speaker_sex) %>% 
  table() %>% 
  tibble::as_tibble() %>%
  dplyr::rename(term = ".") %>%
  dplyr::rename(Freq = "n") %>%
  dplyr::mutate(term = as.factor(term)) %>% 
  ggplot2::ggplot(aes(x = term, y = Freq)) +
  ggplot2::geom_col(fill = "#4271AE", width = 0.6, alpha = 0.8, color = "#4271AE") + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggplot2::coord_fixed(1/500)

# Recode values either to M,F or missing
dl$speaker <- dl$speaker %>%
  dplyr::mutate(speaker_sex = dplyr::case_when(speaker_sex == "m" ~ "M",
                                 speaker_sex == "FA" ~ "F",
                                 speaker_sex == "f" ~ "F",
                                 speaker_sex == "?" ~ NA_character_,
                                 speaker_sex == "*" ~ NA_character_,
                                 TRUE ~ speaker_sex) %>%
           as.factor()) 

# Investigating speaker_age ---------------------------------------------------------------------------------------------------- #

# The frequency table for non-numeric values in speaker_age
dl$speaker%>% 
  dplyr::filter(!speaker_age %in% 1:99) %>% 
  dplyr::select(speaker_age) %>% 
  table()

# and the plot of this table
dl$speaker %>% 
  dplyr::filter(!speaker_age %in% 1:99) %>% 
  dplyr::select(speaker_age) %>% 
  table() %>% 
  tibble::as_tibble() %>%
  dplyr::rename(term = ".") %>%
  dplyr::rename(Freq = "n") %>%
  dplyr::mutate(term = as.factor(term)) %>% 
  ggplot2::ggplot(aes(x = term, y = Freq)) +
  ggplot2::geom_col(fill = "#4271AE", width = 0.6) + 
  ggplot2::scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggplot2::coord_fixed(1/10) +
  ggplot2::theme_bw()

# Change all non-number values in missing
dl$speaker <- dl$speaker %>%
  dplyr::mutate(speaker_age = dplyr::case_when(speaker_age == "?" ~ NA_character_,
                                 speaker_age == "??" ~ NA_character_,
                                 speaker_age == "*" ~ NA_character_,
                                 speaker_age == "0" ~ NA_character_,
                                 speaker_age == "M" ~ NA_character_,
                                 speaker_age == "X" ~ NA_character_,
                                 TRUE ~ speaker_age)) 

### dl$dict -------------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$dict)
# The summary statistic points out three issues with this data frame: 
# 1. numeric variables language number (lang_nr) and term number (term_nr) are formated as strings; this is easily changed. 
# 2. the term variable only has 2252 unique values, the entire dictionary file has 2363 rows though. From this difference we 
# state the hypothesis that there are 111 cases in which the same term for a colour appears in two different languages or more. 
# 3. The term abbreviation variable (term_abb) contains a few missing cases. 
# 4. Furthermore for term_abb is the amount of unique values by far lower then the amount of terms in the dictionary. From this we infer that 
# the same abbreviaton is used for different terms in different languages

# Issue I ---------------------------------------------------------------------------------------------------------------------- #

# The first issue is solved easily:
dl$dict <- dl$dict %>%
  dplyr::mutate(dplyr::across(c(lang_nr, term_nr), as.numeric))

# Issue II & III ---------------------------------------------------------------------------------------------------------------- #

# Second and third issue are related, as we can easyly show by looking at the missing values of term_abb:
dl$dict %>%
  dplyr::filter(is.na(term))# %>% print(n = Inf)

# From this table we can draw 2 conclusions:
# 1. The missing term_abb values are relatively even spread over all languages, there is no clustering for a single language.
# This indicates, that the missing values are distributed randonmly.
# 2. We can divide the missing value issue in three cases:
# i) 'term'is missing (symbolized by 'blank' or '??') and term_abb is missing (symbolized by 'NA') 
# ii) term_abb is missing, but there is a value for term
# iii) term is missing, but there is a value for term_abb

# The following function gives a solution to all three cases. For case i) there is nothing to do. ii) is solved by taking the 
# initial two characters of term to substitute the missing value in term_abb (if the abbreviation would be 'NA' another combination
# of letters was choosen as 'NA' decodes missing values). iii) nothing can be done here either. 

# Before we correct the values for term_abb and term we store the lang_nr which have missing values for term_abb in a vector, 
# because the term_abb problem will recur later and we'll use this vector for comparison
na_lang <- function(){
  dl$dict %>%
  dplyr::filter(is.na(term_abb)) %>%
  dplyr::select(lang_nr) %>%
  pull()
}

# We can then reduce the missing term_abb to 4 values and missing term to 9, with an intersection of 4
dl$dict <- dl$dict %>%  
  dplyr::mutate(term_abb = dplyr::case_when(term == "'ndaa" ~ "ND", # This recodes the missing term_abb which have a corresponding term
                              term == "namonsitihante" ~ "NM",
                              term == "naatuca" ~ "NT",
                              term == "anaranjada/naranjada" ~ "AN",
                              term == "naranjana" ~ "NR",
                              term == "néng2/niáng2" ~ "NE",
                              term == "naranjado" ~ "NR",
                              term == "najerona" ~ "NJ",
                              term == "narane" ~ "NR",
                              term == "nilea" ~ "NI",
                              term == "naranjada" ~ "NR",
                              term == "cana" ~ "CA",
                              term == "naraja" ~ "NR",
                              term == "ñagla" ~ "NG",
                              term == "namaal" ~ "NM",
                              term == "ñiro" ~ "NI",
                              TRUE ~ term_abb),
         # This cleans the terms, which are missing
         term = dplyr::case_when(term == "??" ~ NA_character_,
                                 term == "blank" ~ NA_character_,
                                 TRUE ~ term),
         # this cleans the two term_abb, which are '??'
         term_abb = dplyr::case_when(term == is.na(term) ~ NA_character_,
                                     term == "welee" ~ "WE",
                                     term_abb == "??" ~ NA_character_,
                                     TRUE ~ term_abb)
  )       

# For issue two and three we look at the frequency distribution of term and term_abb variables which appear more then once

# Function to filter duplicates of a variable within a single language; if option l = TRUE, the function returns the language 
# number instead of the values of the inserted variable
non_u_fun <- function(var, l = FALSE) {
  df <- dl$dict %>% 
    dplyr::group_by(lang_nr, {{var}}) %>%
    dplyr::mutate(duplicate.flag = n() > 1) %>%
    dplyr::filter(duplicate.flag == TRUE) 
  
  if(l == TRUE){
    df %>% 
      dplyr::filter(duplicate.flag == TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(lang_nr)
  }else{
    df %>% 
      dplyr::select({{var}}, duplicate.flag) %>% 
      unique() %>% 
      pull({{var}})
  }
}

dict_plots <- function() {
  # Plot for the frequency distribution of term
  plot1 <- function() {
    dl$dict %>% 
    group_by(term) %>% 
    mutate(duplicate.flag = n() > 1) %>% 
    filter(duplicate.flag) %>%
    arrange(term) %>%
    select(term) %>% 
    table() %>%
    as_tibble() %>%
    rename(term = ".") %>%
    rename(freq = "n") %>%
    mutate(term = as.factor(term),
           non_u = as_factor(if_else(term %in% non_u_fun(term), "Non-unique", "Unique"))) %>% 
    ggplot(aes(x = term, y = freq, colour = non_u)) + 
    geom_point() +
    scale_x_discrete(expand=c(0,0),breaks=c("ami", "cafe", "gihobuna", "lal", "pink", "solferino")) +
    scale_colour_manual(values = c("#4271AE","#FF6347"))
  }
  
  # Plot for the frequency distribution of term_abb
 
  plot2 <- function() {
    br <- dl$dict %>%
      select(term_abb) %>%
      distinct() %>%
      arrange(term_abb) 
    breaks <- function() br[as.integer(seq(1, nrow(br), nrow(br)/10)),] %>% pull()
    
    dl$dict %>% 
    group_by(term_abb) %>%                   
    mutate(duplicate.flag = n() > 1) %>% 
    filter(duplicate.flag) %>%
    arrange(term) %>%
    select(term_abb) %>% 
    table() %>%
    as_tibble() %>%
    rename(term_abb = ".") %>%
    rename(freq = "n") %>%
    mutate(term_abb = as.factor(term_abb), 
           non_u_lang = as_factor(if_else(term_abb %in% non_u_fun(term_abb), 
                                          "Non-unique", "Unique"))
           ) %>%
    ggplot(aes(x = term_abb, y = freq, colour = non_u_lang)) + 
    geom_point() +
    scale_x_discrete(expand=c(0,0),breaks=breaks()) + 
    scale_colour_manual(values = c("#4271AE","#FF6347"))
  }
  
  list(
    dict_freq_dist_term = plot1,
    dict_freq_dist_term_abb = plot2
  )
}

plots <- append(plots, dict_plots())

# c[["d"]] <- a$dict_freq_dist_term
# plots$b <- a$dict_freq_dist_term_abb

#
# plapp <- function(plist, plots) {
#   plts <- plots 
#   map(plts)
#}

# Duplicates of term and term_abb in language 12
dl$dict %>% group_by(lang_nr, term) %>%
  dplyr::filter(lang_nr == 12) %>%
  dplyr::mutate(duplicate.flag = n() > 1) %>%
  dplyr::filter(duplicate.flag == T) %>%
  head(n=4)

### dl$foci_exp ---------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$foci_exp)

# Problems: 
# I) `grid_coord`, despite being formated as character, contains 410 unique values. This means there are theoretically 80 
# additional chips, as the Munsell chart only counts 330.
# II) `term_abb` has a few ($0.004\%$) missing values. Furthermore the data frame seems quite large; as we can see in Tab. 
# ov, it has $110784$ rows. Given the mean value for number of rows of `focus_response` multiplied by the total amount of 
# speakers, yields about $8,500$. But since a single `focus_response` can appear several times for a single speaker, the mean 
# of `focus_response` in the summary statistic does not show the mean of focus response per speaker for each language. This mean 
# is easily calculated  and multiplied by the amount of speakers our data frame should have about 18,500 rows (7.05 \times 2619)

# Function to calculate the mean of focus_response per speaker for each language
grouped_mean <- function(var){
  dl$foci_exp %>% 
    dplyr::group_by(lang_nr, speaker_nr) %>% 
    dplyr::distinct({{var}}) %>% 
    count() %>% 
    dplyr::group_by(lang_nr) %>%
    dplyr::summarise(mean_lang = mean(n),
                     sd = sd(n))
}

# Check the mean for term_abb
grouped_mean(term_abb)

# See highest standard deviation
grouped_mean(focus_response) %>% 
  dplyr::arrange(sd) %>% 
  tail()

# I Grid_coord Problem --------------------------------------------------------------------------------------------------------- #

# Creates a vector with the grid coordinates of the Munsell chart
grid_coordinates <- function() {
  LETTERS[2:9] %>%
    purrr::map(paste0, 0:40) %>% 
    purrr::flatten_chr() %>% 
    c("A0", ., "J0")
}

# Filter all values in foci_exp of grid_coord, which are not in the above vector
dl$foci_exp %>%
  dplyr::filter(!grid_coord %in% grid_coordinates()) %>% 
  dplyr::select(grid_coord) %>% 
  unique() %>% 
  print(n = Inf)

# Plot for the distribution of the excess grid coordinate values 
foci_plot1 <- function() {  
  dat <- function()
    dl$foci_exp %>% # graph noch optimieren
    filter(!grid_coord %in% grid_coordinates()) %>%
    select(grid_coord, lang_nr) %>%
    mutate(lang_nr = as.factor(lang_nr))
  
  br <- (unique(dat()$lang_nr)[seq(1, 77, 5)])
  
  ggplot(dat(), aes(grid_coord, fill = lang_nr)) +
    geom_bar() +
    scale_y_continuous(expand=c(0,0), name = "Freq") +
    scale_x_discrete(expand=c(0,0),breaks=c("A10", "A20", "A30", "A40",
                                            "J10", "J20", "J30", "J40")) +
    scale_fill_viridis_d(breaks = br) +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      #legend.position = "bottom",
      legend.key.size = unit(0.5, "cm")
    ) #+
    #guides(fill = guide_legend(nrow = 2))
}
  
plots$foci_dist_exec_gc <- foci_plot1

# Row  numbers of original foci file
nrow(foci)

# Filter foci for additional grid coordinates
foci %>%
  dplyr::filter(!grid_coord %in% grid_coordinates()) %>% 
  dplyr::select(grid_coord) %>% 
  unique() %>%
  print(n = 100)

# Finding excess coordinates "A0..40" and "J0..40"
foci %>%
  dplyr::filter(!grid_coord %in% grid_coordinates()) %>% 
  dplyr::select(grid_coord) %>% 
  unique() %>%
  dplyr::slice(51:52)

# Figuring out, how frequent additional A values appear
foci %>% 
  dplyr::filter(grid_coord == "A0..40")%>% # grid_coord == "J0..40"
  dplyr::select(lang_nr) %>% 
  unique() 

# 
foci %>% 
  dplyr::filter(grid_coord == "F10..13G10..13")

# Discard excess coordinates from df
dl$foci_exp <- 
  dl$foci_exp %>% 
  dplyr::filter(grid_coord %in% grid_coordinates()) %>%
  dplyr::mutate(grid_coord = as.factor(grid_coord))

# Check row number
nrow(dl$foci_exp) 

# II Missing Value Problem ----------------------------------------------------------------------------------------------------- #

# Frequency distribution for missing values
freq_NA_term_abb <- function(data){  
  textcol <- "grey40"
  data %>%
    group_by(lang_nr) %>%
    mutate(flag = as.numeric(any(is.na(term_abb)))) %>% 
    filter(flag == TRUE) %>%
    group_by(lang_nr, speaker_nr) %>%
    mutate(Freq = as.factor(any(is.na(term_abb))),
           lang_nr = as.factor(lang_nr),
           speaker_nr = as.factor(speaker_nr)) %>%
    select(lang_nr, speaker_nr, Freq) %>%
    count(Freq) %>% 
    ungroup() %>%
    tidyr::complete(speaker_nr, nesting(lang_nr)) %>% 
    ggplot(., aes(x = speaker_nr, y = lang_nr, fill = Freq)) + 
    geom_tile(color = "gray", size = 0.3, alpha = 0.95) +
    scale_y_discrete(expand=c(0,0))+
    scale_fill_manual(values=c("#4271AE","#FF6347"), na.value = "grey60", name = "Contains \nmissing \nvalue")+  
    scale_x_discrete(expand=c(0,0),breaks=c("5", "10", "15", "20", "25", "30")) +
    theme(axis.text.x=element_text(size=10,colour=textcol),
          axis.text.y=element_text(vjust=0.2,colour=textcol),
          axis.ticks=element_line(size=0.4),
          plot.background=element_blank(),
          panel.border=element_blank(),
          plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
          plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))
}

freq_NA_term_abb(dl$foci_exp)

# Languages in foci-exp containing missing values for term_abb
na_foci <- function() {
  dl$foci_exp %>%
    dplyr::filter(is.na(term_abb)) %>% 
    dplyr::select(lang_nr) %>% 
    dplyr::distinct()
}

# Compare dict and foci-exp for languages containing missing values for term_abb
dplyr::setdiff(na_foci(), na_lang())
dplyr::setdiff(na_lang(), na_foci())
dplyr::intersect(na_foci(), na_lang())

### dl$term -------------------------------------------------------------------------------------------------------------------- # 

# Summary statistic
my_skim(dl$term)

# The data set is comparably large (~860,000 observations); the amount of observations of term_task should be approx the amount 
# of speakers times amount of color chips, as every client names all 300 color chips:
nrow(dl$term) - (length(dl$speaker$speaker_nr) * length(unique(dl$term$chip_nr)))

# The deviation is quite small (+990) and it has the follwoing reason:

# Find the additional observations --------------------------------------------------------------------------------------------- #

# Helper fun for set difference
spls <- function(ln){
  dl$speaker %>% 
    dplyr::filter(lang_nr == ln) %>% 
    dplyr::select(speaker_nr) %>% 
    count() %>% 
    pull()
}

# Helper fun for set difference
splt <- function(ln){
  dl$term %>% 
    dplyr::filter(lang_nr == ln & chip_nr == 330) %>% 
    dplyr::distinct(speaker_nr) %>% 
    count() %>% 
    pull()
}

# Taking the set differences for distinct speakers in the "term.txt" and "speaker.txt" file.
dplyr::full_join(dplyr::setdiff(dl$term %>% distinct(lang_nr, speaker_nr), 
                                dl$speaker %>% distinct(lang_nr, speaker_nr)) %>%
                   dplyr::mutate(diff = "T \\ S"),
                 dplyr::setdiff(dl$speaker %>% distinct(lang_nr, speaker_nr),
                                dl$term %>% distinct(lang_nr, speaker_nr)) %>% 
                   dplyr::mutate(diff = "S \\ T")) %>%
  dplyr::mutate(dig = purrr::map_int(.x = lang_nr, ~ splt(.x)),
         dag = purrr::map_int(.x = lang_nr, ~ spls(.x)))

# Investigate "T \\ S" cases
dl$term %>% 
  dplyr::filter(lang_nr == 95, speaker_nr == 26)
splt(95)

dl$speaker %>% 
  dplyr::filter(lang_nr == 95, speaker_nr == 26)
spls(95)

dl$term %>% 
  dplyr::filter(lang_nr == 97, speaker_nr == 13)
splt(97)

dl$speaker %>% 
  dplyr::filter(lang_nr == 97, speaker_nr == 13)
spls(97)

# Locating the double value
dl$speaker %>%
  dplyr::filter(lang_nr == 97 & speaker_nr %in% c(10:15))

# Correct the misspelled value
dl$speaker <- dl$speaker %>% 
  dplyr::mutate(speaker_nr = if_else(lang_nr == 97 & speaker_nr == 12 & speaker_age == 19, 13, speaker_nr))

# In the summary stat all values seem in their range, except term_abb has missing values again; we can easily prove, that the
# same problem as for foci_tas occurs:

# Missing term_abb ------------------------------------------------------------------------------------------------------------- #

na_term <- function() {
  dl$term %>%
  filter(is.na(term_abb)) %>% 
  select(lang_nr) %>% 
  distinct() %>%
    pull()
}

intersect(na_term(), na_lang())
setdiff(na_term(), na_lang())
setdiff(na_lang(), na_term())

purrr::map2(na_term, na_lang, c(intersect))

# Frequency distribution for missing values
freq_NA_term_abb(dl$term)

# Check term_abb for missing values other then NA
dl$term %>% 
  dplyr::select(term_abb) %>%
  table()

dl$term %>% 
  dplyr::filter(term_abb == "*") %>%
  count()

# Which languages are affected
dl$term %>% 
  dplyr::filter(term_abb == "*") %>%
  dplyr::select(lang_nr) %>%
  distinct()

# Recoding NA values
dl$term <- dl$term %>%
  dplyr::mutate(term_abb = na_if(term_abb, "*"),
                term_abb = na_if(term_abb, "?"))

# Merge ************************************************************************************************************************ #


# Overview for join ------------------------------------------------------------------------------------------------------------ #

dl %>%
  purrr:map_df(~`%in%`(table = colnames(.), x = vars$New)) %>%
  dplyr::mutate(across(where(is.logical), as.factor)) %>%
  purrr::map_df(~ recode(., "TRUE" = "\U2713", "FALSE" = "-")) %>% 
  add_column("Variable" = vars$New) %>%
  column_to_rownames(var = "Variable")

# Merge chip and mun2lab ------------------------------------------------------------------------------------------------------- #

chart <- function() {
  dl$mun_2_lab %>% # grid row & col aus chip sind möglw unnötig
    dplyr::mutate(chip_nr = as.numeric(chip_nr)) %>%
    dplyr::left_join(dl$chip, by = "chip_nr") %>%
    dplyr::select(c(1, 12, 2:11)) %>%
    dplyr::arrange(chip_nr)
}

# Merge term and all dictionary files ------------------------------------------------------------------------------------------ #

term_task <- dl$term %>%
  dplyr::left_join(dl$dict, by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  dplyr::left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>%
  dplyr::left_join(chart, by = "chip_nr") %>%
  dplyr::left_join(dl$lang, by = "lang_nr")

# Helper function for visualization
sp25 <- function(){term_task %>% 
    dplyr::group_by(lang_nr) %>%
    dplyr::distinct(speaker_nr) %>% 
    count() %>% 
    dplyr::filter(n == 25) %>%
    pull(lang_nr)
}

# Subset relevant data
v <- term_task %>% 
  dplyr::group_by(lang_nr) %>%
  count() %>%
  dplyr::filter(lang_nr %in% sp25()) %>%
  dplyr::mutate(cfill = as.factor(if_else(n > 8250, FALSE, TRUE)))

# Plot the Amount of chips per language with 25 speakers
ggplot(v, aes(x = lang_nr, y = n, colour = cfill)) +  
  geom_point(alpha = 0.75, size = 2.5) + 
  scale_colour_manual(values = c("#4271AE","#FF6347"), labels = c("s > 8250", "s = 8250")) +
  theme(legend.title = element_blank()) +
  coord_fixed(1/500)

# Compare the set of exceeding languages with those languages filtered for duplicates of term_abb
# Shows, that the first set is a proper subset of the latter 
intersect(v  %>% filter(n > 8250) %>% select(lang_nr), non_u_fun(term_abb, l = TRUE))
setdiff(non_u_fun(term_abb, l = TRUE), v  %>% filter(n > 8250) %>% select(lang_nr))
setdiff(v  %>% filter(n > 8250) %>% select(lang_nr), non_u_fun(term_abb, l = TRUE))

# Function, to substitute duplicate values in term_abb by na values
rm_duplicates <- function(lang_dict){
  lang_dict %>%
    group_by(lang_nr, term_abb) %>%
    mutate(duplicate.flag = n() > 1,
           term_abb = if_else(duplicate.flag == TRUE, NA_character_, term_abb),
           term_abb = as.factor(term_abb)) %>% 
    ungroup() %>%
    select(-duplicate.flag)
}

# Changes due to the substitution function:
dl$dict %>%
  dplyr::group_by(lang_nr, term_abb) %>%
  dplyr::mutate(duplicate.flag = n() > 1,
         term_abb = if_else(duplicate.flag == TRUE, NA_character_, term_abb)) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(lang_nr %in% c(1,42,76)) %>% 
  print(n=Inf)

# Merge term with the dictionary data frames
term_task <- function() {
  dl$term %>%
    dplyr::left_join(rm_duplicates(dl$dict), 
                     by = c("lang_nr", "term_abb"),
                     na_matches = "never") %>%
    dplyr::left_join(dl$speaker, 
                     by = c("lang_nr", "speaker_nr")) %>%
    dplyr::left_join(chart(), 
                     by = "chip_nr") %>%
    dplyr::left_join(dl$lang, 
                     by = "lang_nr")
} 

# Merge foci-exp and all dictionary files -------------------------------------------------------------------------------------- #

# Same merging operation for foci-exp
foci_task <- function() {
  dl$foci_exp %>%
    dplyr::left_join(rm_duplicates(dl$dict), 
                     by = c("lang_nr", "term_abb"), 
                     na_matches = "never") %>%
    dplyr::left_join(dl$speaker, 
                     by = c("lang_nr", "speaker_nr")) %>% 
    dplyr::left_join(chart(), 
                     by = "grid_coord") %>%
    dplyr::left_join(dl$lang, 
                     by = "lang_nr")
}

# Check the length of the new and the old foci dfs
nrow(foci_task) - nrow(dl$foci_exp)

# Write task dfs as csv --------------------------------------------------------------------------------------------------------- #

# Helper fun for writing
make_csv <- function(df_list, df_names, path = getwd()) { # Umbennen
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  
  setwd(path)
  output_csv <- function(data, names){ 
    folder_path <- getwd()
    
    write_csv(data, paste0(names, ".csv"))
  }
  list(data = df_list,
       names = df_names) %>%
    pwalk(output_csv)
}

# Write term and foci task as csv
make_csv(list(term_task, foci_task),c("term_task", "foci_task"))

# Write each df from dl csv ----------------------------------------------------------------------------------------------------- #
make_csv(dl, names(dl), "./data/new")


# ML functions ***************************************************************************************************************** #

# General preprocessing -------------------------------------------------------------------------------------------------------- #

# Function for cleaning speaker_age
num_speaker_age <- function(speaker_age){
  case_when(speaker_age == "15-16" ~ "15",
            speaker_age == "16?" ~ "16",
            speaker_age == "16(approx)" ~ "16",
            speaker_age == "20-22" ~ "21",
            speaker_age == "22?" ~ "22",
            speaker_age == "25?" ~ "25",
            speaker_age == "26-28?" ~ "27",
            speaker_age == "30-40" ~ "35",
            speaker_age == "30?" ~ "30",
            speaker_age == "32+" ~ "32",
            speaker_age == "33-34?" ~ "33",
            speaker_age == "35?" ~ "35",
            speaker_age == "40-45" ~ "42",
            speaker_age == "40-50" ~ "45",
            speaker_age == "40+" ~ "40",
            speaker_age == "50-60" ~ "55",
            speaker_age == "50?" ~ "50",
            speaker_age == "50+" ~ "50",
            speaker_age == "53?" ~ "53",
            speaker_age == "55?" ~ "55",
            speaker_age == "70+" ~ "70",
            speaker_age == "about 40" ~ "40",
            speaker_age == "over 60" ~ "60",
            #speaker_age == is.na(speaker_age) ~ NA_real_,
            TRUE ~ speaker_age) %>%
    as.numeric(speaker_age)
}

# Removes of all observations which contain missing values, cleans speaker_age and kicks out dictionary information and duplicates
model_data <- function(task) {
  task %>%
    mutate(speaker_age = num_speaker_age(speaker_age)) %>%
    select(-c(term, term_nr, grid_row, grid_col, lang_name, lang_country)) %>%
    filter(across(everything(), ~ !is.na(.)))
}

# Model recipe ----------------------------------------------------------------------------------------------------------------- #

set.seed(123)

# split the n = 1000 sample data
splits <- term_task %>%
  slice_sample(n = 1000) %>%
  model_data() %>%
  initial_split() #kleines sample nhemen

term_other <- training(splits)
term_test  <- testing(splits)

# We’ll use the validation_split() function to allocate 20% of data to the validation set 
val_set <- validation_split(term_other, 
                            prop = 0.80)

# We write a sort of recipe for preparing training data
term_recipe <- 
  recipe(speaker_sex ~ ., data = term_other) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors()) %>%
  prep(training = term_other, retain = TRUE)

# Check the processed train data
juice(term_recipe)

# Data frame list (CNN) -------------------------------------------------------------------------------------------------------- #

# Function to create a list of dfs, in which each data frame all data for a speaker for the chosen task;
# It has the options to include only Munsell color space data, Lab data or both.
df_list <- function(task, 
                    mun = TRUE, 
                    ciel = TRUE) {
  mod_task <- model_data(task) 
  if(mun == FALSE){
    mod_task %>%
      select(-c(wcs_mv:mun_value)) %>%
      group_split(lang_nr, speaker_nr)  #Leave out Munsell and select L*a*b
  } else if(ciel == FALSE){
    mod_task %>%
      select(-c(`L_star`,`a_star`,`b_star`)) %>%
      group_split(lang_nr, speaker_nr)# Leave out L_stara*b and select Munsell 
  } else {
    mod_task %>% 
      group_split(lang_nr, speaker_nr)
  }
}

# Apply for term_task
term_task_list <- df_list(term_task, mun = FALSE)

# Name each element
# names(b) <- 1:length(b) %>% map(., ~paste("Speaker", ., sep = " "))








