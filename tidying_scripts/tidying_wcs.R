# <!---------------------------------------------------------- Notes ----------------------------------------------------------->
#
#
#
# <!---------------------------------------------------------------------------------------------------------------------------->

# Packages
library(xml2)
library(rvest)
library(skimr)
library(tidyverse)

# setwd("./WCS_Data")
# library(here)


# Read in data ***************************************************************************************************************** #


# Version 1 -------------------------------------------------------------------------------------------------------------------- #

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
dl <- list_of_files %>%
  purrr::map(readr::read_tsv, col_names = FALSE)

# Remove temp files 
unlink(c(temp, temp2))

# Move foci data in its own object
foci <- dl[[4]]
dl <- dl[-4]

# Read in the last txt file and add it to dfl
dl <- c(dl[1], list("cnum-vhcm-lab-new.txt" = readr::read_tsv(url2, col_names = FALSE)), dl[2:6])

# Version 2 -------------------------------------------------------------------------------------------------------------------- #

# # Create a path list to the data: add the path to directory you unpacked the zip in and where you stored cnum-vhcm-lab-new.txt
# list_of_files <- list.files(path = "./wcs_data", recursive = TRUE,
#                             pattern = "\\.txt$",
#                             full.names = TRUE)
# 
# # Removes the readme of cnum-vhcm-lab-new.txt, if it is in the directory
# list_of_files <- subset(list_of_files, !endsWith(list_of_files, "README.txt"))
# 
# # Read in data into a list of data frames (dfl)
# dl <- list_of_files %>%
#   #set_names(.) %>%
#   purrr::map(read_tsv, col_names = FALSE)
# 
# # Move foci data in its own object
# foci <- dl[[5]]
# dl <- dl[-5]


# Name all Variables of each df-------------------------------------------------------------------------------------------------- #

# Name each df in dfl
names(dl) <- c("chip", "mun_2_lab", 
               "dict", "foci_exp", 
               "lang", "speaker", 
               "term") 

colnames(dl$chip) <- c("chip_nr", "grid_row", "grid_col", "grid_coord")
colnames(dl$mun_2_lab) <- c("chip_nr", "wcs_mv", "wcs_mh", "mun_chroma", 
                            "mun_hue", "mun_value", "L_star", "a_star", "b_star")
colnames(dl$dict) <- c("lang_nr", "term_nr", "term", "term_abb")
colnames(dl$foci_exp) <- c("lang_nr", "speaker_nr", "focus_response", "term_abb", "grid_coord")
colnames(dl$lang) <- c("lang_nr", "lang_name", "lang_country", "field_worker", "field_worker_2", "field_worker_3", 
                       "Orig_file", "File_type")
colnames(dl$speaker) <- c("lang_nr", "speaker_nr", "speaker_age", "speaker_sex") 
colnames(dl$term) <- c("lang_nr", "speaker_nr", "chip_nr", "term_abb")

dl$mun_2_lab <- dl$mun_2_lab[-1,]
dl$dict <- dl$dict[-1,]

# Overview variable description ------------------------------------------------------------------------------------------------- #

vars <- tibble::tibble("WCS Variable Name" = c("WCS Chip Number", "WCS Grid Row", "WCS Grid Columns", "Concatenation of fields", #1
                                       "V", "H", "C", "MunH", "MunV", "L*", "a*", "b*", #2
                                       "WCS Language Number", "Term Number", "Term", "Term Abbreviation", #3
                                       "WCS Speaker Number", "WCS Focus Response", #4
                                       "WCS Language Name", "WCS Language Geographic Location", "Field Worker", #5
                                       "WCS Speaker Age", "WCS Speaker Sex" #6
                                        ),
                "New Variable Name" = c("chip_nr", "grid_row", "grid_col", "grid_coord", #1
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

# Stat summary for each df ------------------------------------------------------------------------------------------------------ #

my_skim <- skimr::skim_with(
  numeric = sfl(complete_rate = NULL, 
                p25 = NULL, 
                p50 = NULL, 
                p75 = NULL, 
                hist = NULL),
  character = sfl(empty = NULL,
                  whitespace = NULL)
)

purrr::map(dl, my_skim)

# Tidy ************************************************************************************************************************** #

## dl$chip ---------------------------------------------------------------------------------------------------------------------- # 

# Change vars in factors
dl$chip <- dl$chip %>%
  dplyr::mutate(across(c(grid_row, grid_coord), as.factor))

## dl$mun_2_lab ----------------------------------------------------------------------------------------------------------------- #

# Make all vars numeric or factor
dl$mun_2_lab <- dl$mun_2_lab %>%
  dplyr::mutate(across(!c(wcs_mv, mun_hue), as.numeric),
                across(c(wcs_mv, mun_hue), as.factor))

## dl$lang ---------------------------------------------------------------------------------------------------------------------- # 

# transform "*" in NA values and drop the author and file variables, as they are meta data
dl$lang <- dl$lang %>% 
  dplyr::select(lang_nr, lang_name, lang_country) %>%
  dplyr::mutate(lang_country = na_if(lang_country, "*"))

# Since half of the country names are missing and some languages contain special characters which cause problems when displaying
# the data, we use the "ISO 639-3 codes" file from the "Primary WCS Data" section of the archive and substitute lang_name and
# lang_country with values from that file.

# Downloading the additional table, transforming the html table into R data frame 
url3 <- "https://www1.icsi.berkeley.edu/wcs/WCS_SIL_codes.html"
file <- xml2::read_html(url3)
tables <- rvest::html_nodes(file, "table")
wcs_iso_codes <- as_tibble(rvest::html_table(tables[[1]], fill = TRUE)) %>%
  dplyr::rename(lang_nr = Index, 
                lang_name = Language, 
                iso_693 = `ISO 639-3 Code`, 
                family = Family, 
                lang_country = `Country Where`) %>%
  dplyr::mutate(lang_country = recode(lang_country, `Mexico|` = "Mexico",
                                      `Columbia` = "Colombia",
                                      `Indonesia (Irian Jaya)` = "Indonesia",
                                      `Peru, Brazil` = "Peru",
                                      `USA, Mexico` = "Mexico",
                                      `Nigeria, Cameroon` = "Nigeria"))

# Substitute the variables
dl$lang <- wcs_iso_codes %>%
  dplyr::select(lang_nr, lang_name, lang_country)

## dl$speaker ------------------------------------------------------------------------------------------------------------------- # 

# Recode values of speaker_sex to M,F or missing
dl$speaker <- dl$speaker %>%
  dplyr::mutate(speaker_sex = case_when(speaker_sex == "m" ~ "M",
                                 speaker_sex == "FA" ~ "F",
                                 speaker_sex == "f" ~ "F",
                                 speaker_sex == "?" ~ NA_character_,
                                 speaker_sex == "*" ~ NA_character_,
                                 TRUE ~ speaker_sex) %>%
                  as.factor()) 

# Change all non-number values in missing
dl$speaker <- dl$speaker %>%
  dplyr::mutate(speaker_age = case_when(speaker_age == "?" ~ NA_character_,
                                 speaker_age == "??" ~ NA_character_,
                                 speaker_age == "*" ~ NA_character_,
                                 speaker_age == "0" ~ NA_character_,
                                 speaker_age == "M" ~ NA_character_,
                                 speaker_age == "X" ~ NA_character_,
                                 TRUE ~ speaker_age))

# There is a spelling mistake in lnag 97; there exists a speaker 12 twice but no speaker 13:
dl$speaker %>%
  dplyr::filter(lang_nr == 97 & speaker_nr %in% c(10:15))

# Correct the misspelled value
dl$speaker <- dl$speaker %>% 
  dplyr::mutate(speaker_nr = if_else(lang_nr == 97 & speaker_nr == 12 & speaker_age == 19, 13, speaker_nr))

## dl$dict ---------------------------------------------------------------------------------------------------------------------- #

# Transform char vars to numeric
dl$dict <- dl$dict %>%
  dplyr::mutate(across(c(lang_nr, term_nr), as.numeric))

# Deal with missing values and falsely named vars
dl$dict <- dl$dict %>%  
  dplyr::mutate(term_abb = case_when( term == "'ndaa" ~ "ND", # This recodes the missing term_abb which have a corresponding term
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
                 term = case_when(term == "??" ~ NA_character_,
                                  term == "blank" ~ NA_character_,
                                  TRUE ~ term),
                 # this cleans the two term_abb, which are '??'
                 term_abb = case_when(term == is.na(term) ~ NA_character_,
                                      term == "welee" ~ "WE",
                                      term_abb == "??" ~ NA_character_,
                                      TRUE ~ term_abb)
          )   

## dl$foci_exp ------------------------------------------------------------------------------------------------------------------ # 

# Creates a vector with the grid coordinates of the Munsell chart
grid_coordinates <- LETTERS[2:9] %>%
  purrr::map(paste0, 0:40) %>% 
  purrr::flatten_chr() %>% 
  c("A0", ., "J0")

# Discard excess coordinates from foci_exp
dl$foci_exp <- dl$foci_exp %>% 
  dplyr::filter(grid_coord %in% grid_coordinates) %>%
  dplyr::mutate(grid_coord = as.factor(grid_coord))

## dl$term ---------------------------------------------------------------------------------------------------------------------- # 

# Recoding NA values for term_abb
dl$term <- dl$term %>%
  dplyr::mutate(term_abb = na_if(term_abb, "*"),
                term_abb = na_if(term_abb, "?"))

# Merge ************************************************************************************************************************* #

# Overview for join ------------------------------------------------------------------------------------------------------------ #

dl %>%
  purrr::map_df(~`%in%`(table = colnames(.), x = vars$New)) %>%
  dplyr::mutate(across(where(is.logical), as.factor)) %>%
  purrr::map_df(~ recode(., "TRUE" = "\U2713", "FALSE" = "-")) %>% 
  tibble::add_column("Variable" = vars$New) %>%
  tibble::column_to_rownames(var = "Variable")

# Merge chip and mun2lab ------------------------------------------------------------------------------------------------------- #

chart <- dl$mun_2_lab %>% # grid row & col aus chip sind möglw unnötig
  dplyr::mutate(chip_nr = as.numeric(chip_nr)) %>%
  dplyr::left_join(dl$chip, by = "chip_nr") %>%
  dplyr::select(c(1, 12, 2:11)) %>%
  dplyr::arrange(chip_nr)

# Merge term and all dictionary files ------------------------------------------------------------------------------------------- #

# Function, to substitute duplicate values in term_abb by na values
rm_duplicates <- function(lang_dict){
  lang_dict %>%
    dplyr::group_by(lang_nr, term_abb) %>%
    dplyr::mutate(duplicate.flag = n() > 1,
                  term_abb = if_else(duplicate.flag == TRUE, NA_character_, term_abb),
                  term_abb = as.factor(term_abb)) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-duplicate.flag)
}

# Merge term with the dictionary data frames
term_task <- dl$term %>%
  dplyr::left_join(rm_duplicates(dl$dict), by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  dplyr::left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>%
  dplyr::left_join(chart, by = "chip_nr") %>%
  dplyr::left_join(dl$lang, by = "lang_nr")

# Same merging operation for foci-exp
foci_task <- dl$foci_exp %>%
  dplyr::left_join(rm_duplicates(dl$dict), by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  dplyr::left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>% 
  dplyr::left_join(chart, by = "grid_coord") %>%
  dplyr::left_join(dl$lang, by = "lang_nr")

# Write task dfs as csv --------------------------------------------------------------------------------------------------------- #

# Helper fun
make_csv <- function(df_list, df_names, path = getwd()) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  
  setwd(path)
  output_csv <- function(data, names){ 
    folder_path <- getwd()
    
    readr::write_csv(data, paste0(names, ".csv"))
  }
  list(data = df_list,
       names = df_names) %>%
    purrr::pwalk(output_csv)
}

# Write term and foci task as csv
make_csv(list(term_task, foci_task), c("term_task", "foci_task"))

# Write each df from dl csv ----------------------------------------------------------------------------------------------------- #

make_csv(dl, names(dl), "data/data_orig")


