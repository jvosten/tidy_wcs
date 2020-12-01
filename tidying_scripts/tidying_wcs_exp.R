
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
  set_names(.) %>%
  map(read_tsv, col_names = FALSE)

# Remove temp files 
unlink(c(temp, temp2))

# Move foci data in its own object
foci <- dl[[4]]
dl <- dl[-4]

# Read in the last txt file and add it to dfl
dl <- c(dl[1], list("cnum-vhcm-lab-new.txt" = read_tsv(url2, col_names = FALSE)), dl[2:6])

# Name each df in dfl
names(dl) <- c("chip",
               "dict", "foci_exp",
               "lang", "speaker",
               "term", "mun_2_lab")

# Version 2 -------------------------------------------------------------------------------------------------------------------- #

# Create a path list to the data: add the path to directory you unpacked the zip in and where you stored cnum-vhcm-lab-new.txt
list_of_files <- list.files(path = "./WCS_data", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

# Removes the readme of cnum-vhcm-lab-new.txt, if it is in the directory
list_of_files <- subset(list_of_files, !endsWith(list_of_files, "README.txt"))

# Read in data into a list of data frames (dfl)
dl <- list_of_files %>%
  #set_names(.) %>%
  map(read_tsv, col_names = FALSE) 

# Move foci data in its own object
foci <- dl[[5]]
dl <- dl[-5]

# Name each df in dfl
names(dl) <- c("chip", "mun_2_lab", 
               "dict", "foci_exp", 
               "lang", "speaker", 
               "term") 

# Name all Variables of each df ------------------------------------------------------------------------------------------------ #

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

# Overview on variable description --------------------------------------------------------------------------------------------- #

vars <- tibble("WCS Variable Name" = c("WCS Chip Number", "WCS Grid Row", "WCS Grid Columns", "Concatenation of fields", #1
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
  map_df(ncol)
ov[2,] <- dl %>%
  map_dfr(nrow)

# Function for Stat summary----------------------------------------------------------------------------------------------------- #

my_skim <- skim_with(
  numeric = sfl(complete_rate = NULL, 
                p25 = NULL, 
                p50 = NULL, 
                p75 = NULL, 
                hist = NULL),
  character = sfl(empty = NULL,
                  whitespace = NULL)
)

# Print function for markdown
skim_print <- function(df){
  m <- if_else(all(map_lgl(df, is.character)), 7, 11)
  
  my_skim(df) %>% 
    as_tibble() %>% 
    rename_with(., ~ gsub("character", "char", .x, fixed = TRUE), starts_with("char")) %>% 
    rename_with(., ~ gsub("numeric", "num", .x, fixed = TRUE), starts_with("num")) %>% 
    select(variable = skim_variable, data_type = skim_type, 3, all_of(5:m)) %>%
    mutate(mutate(across(where(is.numeric), round, digits = 1)),
           across(everything(), as.character)) %>% 
    replace(is.na(.), "-")  # Noch die digits begrenzen
}


#### Tidy every single data frame ---------------------------------------------------------------------------------------------- # 


### dl$chip -------------------------------------------------------------------------------------------------------------------- # 

skim_print(dl$chip)

# Change vars in factors
dl$chip <- dl$chip %>%
  mutate(across(c(grid_row, grid_coord), as.factor))

### dl$mun_2_lab --------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$mun_2_lab)
# Looks good; since nrow = 330 there is no need for validate rules

# Make all vars numeric or factor
dl$mun_2_lab <- dl$mun_2_lab %>%
  mutate(across(!c(wcs_mv, mun_hue), as.numeric),
         across(c(wcs_mv, mun_hue), as.factor))

# Test, if factor variables have the right amount of levels
length(levels(dl$mun_2_lab$wcs_mv))

### dl$lang -------------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$lang)
# The summary statistic looks good; it contains all 110 language name. But if we look at the actual data frame, we see, that there 
# are a lot of missing values decoded by "*". We will transform them in NA values and we will further drop the author and file 
# variables, as they are sort of meta data.

dl$lang <- dl$lang %>% 
  select(lang_nr, lang_name, lang_country) %>%
  mutate(lang_country = na_if(lang_country, "*"))

# At this point we drop the variables author, file and file status; if there is an interest in those files, the above selection
# function can be replaced wit the following:
# dl$lang <- dl$lang %>% 
#   unite(Author, Author1:Author3, sep = " ") %>%
#   mutate(lang_country = na_if(lang_country, "*"))

# Since half of the country names are missing and some languages contain special characters which cause problems when displaying
# the data, we use the "ISO 639-3 codes" file from the "Primary WCS Data" section of the archive and substitute lang_name and
# lang_country with values from that file.

# Downloading the additional table, transforming the html table into R data frame 
url3 <- "https://www1.icsi.berkeley.edu/wcs/WCS_SIL_codes.html"
file <- read_html(url3)
tables <- html_nodes(file, "table")
wcs_iso_codes <- as_tibble(html_table(tables[[1]], fill = TRUE)) %>%
  rename(lang_nr = Index, 
         lang_name = Language, 
         iso_693 = `ISO 639-3 Code`, 
         family = Family, 
         lang_country = `Country Where`) %>%
  mutate(lang_country = recode(lang_country, `Mexico|` = "Mexico",
                               `Columbia` = "Colombia",
                               `Indonesia (Irian Jaya)` = "Indonesia",
                               `Peru, Brazil` = "Peru",
                               `USA, Mexico` = "Mexico",
                               `Nigeria, Cameroon` = "Nigeria"))

# Substitute the variables
dl$lang <- wcs_iso_codes %>%
  select(lang_nr, lang_name, lang_country)

### dl$speaker ----------------------------------------------------------------------------------------------------------------- # 

my_skim(dl$speaker)

# Investigating speaker_sex ---------------------------------------------------------------------------------------------------- #

# filter, which other values then M and F are contained in speaker_sex
dl$speaker %>% 
  filter(!speaker_sex %in% c("M","F")) %>%
  distinct(speaker_sex)

# Frequency distribution for all values of speaker_sex
dl$speaker %>% 
  select(speaker_sex) %>% 
  table()

# Frequency distribution of non male/female values in speaker sex 
dl$speaker %>% 
select(speaker_sex) %>% 
  table() %>% 
  as_tibble() %>%
  rename(term = ".") %>%
  rename(Freq = "n") %>%
  mutate(term = as.factor(term)) %>% 
  ggplot(aes(x = term, y = Freq)) +
  geom_col(fill = "#4271AE", width = 0.6, alpha = 0.8, color = "#4271AE") + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_fixed(1/500)

# Recode values either to M,F or missing
dl$speaker <- dl$speaker %>%
  mutate(speaker_sex = case_when(speaker_sex == "m" ~ "M",
                                 speaker_sex == "FA" ~ "F",
                                 speaker_sex == "f" ~ "F",
                                 speaker_sex == "?" ~ NA_character_,
                                 speaker_sex == "*" ~ NA_character_,
                                 TRUE ~ speaker_sex) %>%
           as.factor()) 

# Investigating speaker_age ---------------------------------------------------------------------------------------------------- #

# The frequency table for non-numeric values in speaker_age
dl$speaker%>% 
  filter(!speaker_age %in% 1:99) %>% select(speaker_age) %>% table()

# and the plot of this table
dl$speaker %>% 
  filter(!speaker_age %in% 1:99) %>% 
  select(speaker_age) %>% 
  table() %>% 
  as_tibble() %>%
  rename(term = ".") %>%
  rename(Freq = "n") %>%
  mutate(term = as.factor(term)) %>% 
  ggplot(aes(x = term, y = Freq)) +
  geom_col(fill = "#4271AE", width = 0.6) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_fixed(1/10) +
  theme_bw()

# Change all non-number values in missing
dl$speaker <- dl$speaker %>%
  mutate(speaker_age = case_when(speaker_age == "?" ~ NA_character_,
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
  mutate(across(c(lang_nr, term_nr), as.numeric))

# Issue II & III ---------------------------------------------------------------------------------------------------------------- #

# Second and third issue are related, as we can easyly show by looking at the missing values of term_abb:
dl$dict %>%
  filter(is.na(term))# %>% print(n = Inf)

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
na_lang <- dl$dict %>%
  filter(is.na(term_abb)) %>%
  select(lang_nr)

# We can then reduce the missing term_abb to 4 values and missing term to 9, with an intersection of 4
dl$dict <- dl$dict %>%  
  mutate(term_abb = case_when(term == "'ndaa" ~ "ND", # This recodes the missing term_abb which have a corresponding term
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

# For issue two and three we look at the frequency distribution of term and term_abb variables which appear more then once

# Function to filter duplicates of a variable within a single language; if option l = TRUE, the function returns the language 
# number instead of the values of the inserted variable
non_u_fun <- function(var, l = FALSE) {
  w <- dl$dict %>% 
    group_by(lang_nr, {{var}}) %>%
    mutate(duplicate.flag = n() > 1) %>%
    filter(duplicate.flag == TRUE) 
  
  if(l == TRUE){
    w %>% 
      filter(duplicate.flag == TRUE) %>%
      ungroup() %>%
      distinct(lang_nr)
  }else{
    w %>% 
      select({{var}}, duplicate.flag) %>% 
      unique() %>% 
      pull({{var}})
  }
}

# Plot for the frequency distribution of term
dict_freq_term <- dl$dict %>% 
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

# Plot for the frequency distribution of term_abb
br <- dl$dict %>%
  select(term_abb) %>%
  distinct() %>%
  arrange(term_abb) 
br <- br[as.integer(seq(1, 535, 535/10)),] %>% pull()

dict_freq_term_abb <- dl$dict %>%  # auf den Plot evtl noch einen fill setzen, der binär anzeigt, ob eine doppelung in 
  group_by(term_abb) %>%                   # einer sprache vorliegt
  mutate(duplicate.flag = n() > 1) %>% 
  filter(duplicate.flag) %>%
  arrange(term) %>%
  select(term_abb) %>% 
  table() %>%
  as_tibble() %>%
  rename(term_abb = ".") %>%
  rename(freq = "n") %>%
  mutate(term_abb = as.factor(term_abb), 
         non_u_lang = as_factor(if_else(term_abb %in% non_u_fun(term_abb), "Non-unique", "Unique"))) %>%
  ggplot(aes(x = term_abb, y = freq, colour = non_u_lang)) + 
  geom_point() +
  scale_x_discrete(expand=c(0,0),breaks=br) + 
  scale_colour_manual(values = c("#4271AE","#FF6347"))

# Duplicates of term and term_abb in language 12
dl$dict %>% group_by(lang_nr, term) %>%
  filter(lang_nr == 12) %>%
  mutate(duplicate.flag = n() > 1) %>%
  filter(duplicate.flag == T) %>%
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
    group_by(lang_nr, speaker_nr) %>% 
    distinct({{var}}) %>% 
    count() %>% 
    group_by(lang_nr) %>%
    summarise(mean_lang = mean(n),
              sd = sd(n))
}

# Check the mean for term_abb
grouped_mean(term_abb)

# See highest stanadard deviation
grouped_mean(focus_response) %>% 
  arrange(sd) %>% 
  tail()

# I Grid_coord Problem --------------------------------------------------------------------------------------------------------- #

# Creates a vector with the grid coordinates of the Munsell chart
grid_coordinates <- LETTERS[2:9] %>%
    map(paste0, 0:40) %>% 
    flatten_chr() %>% 
    c("A0", ., "J0")

# Filter all values in foci_exp of grid_coord, which are not in the above vector
dl$foci_exp %>%
  filter(!grid_coord %in% grid_coordinates) %>% 
  select(grid_coord) %>% 
  unique() %>% 
  print(n = Inf)

# Plot for the distribution of the excess grid coordinate values 
dat <- dl$foci_exp %>% # graph noch optimieren
  filter(!grid_coord %in% grid_coordinates) %>%
  select(grid_coord, lang_nr) %>%
  mutate(lang_nr = as.factor(lang_nr))

br <- (unique(dat$lang_nr)[seq(1, 77, 5)])

ggplot(dat, aes(grid_coord, fill = lang_nr)) +
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
rm(dat)

# Row  numbers of original foci file
nrow(foci)

# Filter foci for additional grid coordinates
foci %>%
  filter(!grid_coord %in% grid_coordinates) %>% 
  select(grid_coord) %>% 
  unique() %>%
  print(n = 100)

# Finding excess coordinates "A0..40" and "J0..40"
foci %>%
  filter(!grid_coord %in% grid_coordinates) %>% 
  select(grid_coord) %>% 
  unique() %>%
  slice(51:52)

# Figuring out, how frequent additional A values appear
foci %>% 
  filter(grid_coord == "A0..40")%>% # grid_coord == "J0..40"
  select(lang_nr) %>% 
  unique() 

# 
foci %>% 
  filter(grid_coord == "F10..13G10..13")

# Discard excess coordinates from df
dl$foci_exp <- dl$foci_exp %>% 
  filter(grid_coord %in% grid_coordinates) %>%
  mutate(grid_coord = as.factor(grid_coord))

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
na_foci <- dl$foci_exp %>%
    filter(is.na(term_abb)) %>% 
    select(lang_nr) %>% 
    distinct()

# Compare dict and foci-exp for languages containing missing values for term_abb
setdiff(na_foci, na_lang)
setdiff(na_lang, na_foci)
intersect(na_foci, na_lang)

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
    filter(lang_nr == ln) %>% 
    select(speaker_nr) %>% 
    count() %>% 
    pull()
}

# Helper fun for set difference
splt <- function(ln){
  dl$term %>% 
    filter(lang_nr == ln & chip_nr == 330) %>% 
    distinct(speaker_nr) %>% 
    count() %>% 
    pull()
}

# Taking the set differences for distinct speakers in the "term.txt" and "speaker.txt" file.
full_join(setdiff(dl$term %>% distinct(lang_nr, speaker_nr), 
                  dl$speaker %>% distinct(lang_nr, speaker_nr)) %>% 
            mutate(diff = "T \\ S"),
          setdiff(dl$speaker %>% distinct(lang_nr, speaker_nr),
                  dl$term %>% distinct(lang_nr, speaker_nr)) %>% 
            mutate(diff = "S \\ T")) %>%
  mutate(dig = map_int(.x = lang_nr, ~ splt(.x)),
         dag = map_int(.x = lang_nr, ~ spls(.x)))

# Investigate "T \\ S" cases
dl$term %>% 
  filter(lang_nr == 95, speaker_nr == 26)
splt(95)
dl$speaker %>% 
  filter(lang_nr == 95, speaker_nr == 26)
spls(95)

dl$term %>% 
  filter(lang_nr == 97, speaker_nr == 13)
splt(97)
dl$speaker %>% 
  filter(lang_nr == 97, speaker_nr == 13)
spls(97)

# Locating the double value
dl$speaker %>%
  filter(lang_nr == 97 & speaker_nr %in% c(10:15))

# Correct the misspelled value
dl$speaker <- dl$speaker %>% 
  mutate(speaker_nr = if_else(lang_nr == 97 & speaker_nr == 12 & speaker_age == 19, 13, speaker_nr))

# In the summary stat all values seem in their range, except term_abb has missing values again; we can easily prove, that the
# same problem as for foci_tas occurs:

# Missing term_abb ------------------------------------------------------------------------------------------------------------- #

na_term <- dl$term %>%
  filter(is.na(term_abb)) %>% 
  select(lang_nr) %>% 
  distinct()

intersect(na_term, na_lang)
setdiff(na_term, na_lang)
setdiff(na_lang, na_term)

# Frequency distribution for missing values
freq_NA_term_abb(dl$term)

# Check term_abb for missing values other then NA
dl$term %>% 
  select(term_abb) %>%
  table()

dl$term %>% 
  filter(term_abb == "*") %>%
  count()

# Which languages are affected
dl$term %>% 
  filter(term_abb == "*") %>%
  select(lang_nr) %>%
  distinct()

# Recoding NA values
dl$term <- dl$term %>%
  mutate(term_abb = na_if(term_abb, "*"),
         term_abb = na_if(term_abb, "?"))

# Merge ************************************************************************************************************************ #


# Overview for join ------------------------------------------------------------------------------------------------------------ #

dl %>%
  map_df(~`%in%`(table = colnames(.), x = vars$New)) %>%
  mutate(across(where(is.logical), as.factor)) %>%
  map_df(~ recode(., "TRUE" = "\U2713", "FALSE" = "-")) %>% 
  add_column("Variable" = vars$New) %>%
  column_to_rownames(var = "Variable")

# Merge chip and mun2lab ------------------------------------------------------------------------------------------------------- #

chart <- dl$mun_2_lab %>% 
  mutate(chip_nr = as.numeric(chip_nr)) %>%
  left_join(dl$chip, by = "chip_nr") %>%
  select(c(1, 12, 2:11)) %>%
  arrange(chip_nr)

# Merge term and all dictionary files ------------------------------------------------------------------------------------------ #

term_task <- dl$term %>%
  left_join(dl$dict, by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>%
  left_join(chart, by = "chip_nr") %>%
  left_join(dl$lang, by = "lang_nr")

# Helper function for visualization
sp25 <- function(){term_task %>% 
    group_by(lang_nr) %>%
    distinct(speaker_nr) %>% 
    count() %>% 
    filter(n == 25) %>%
    pull(lang_nr)
}

# Subset relevant data
v <- term_task %>% 
  group_by(lang_nr) %>%
  count() %>%
  filter(lang_nr %in% sp25()) %>%
  mutate(cfill = as.factor(if_else(n > 8250, FALSE, TRUE)))

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
  group_by(lang_nr, term_abb) %>%
  mutate(duplicate.flag = n() > 1,
         term_abb = if_else(duplicate.flag == TRUE, NA_character_, term_abb)) %>% 
  ungroup() %>%
  filter(lang_nr %in% c(1,42,76)) %>% 
  print(n=Inf)

# Merge term with the dictionary data frames
term_task <- dl$term %>%
  left_join(rm_duplicates(dl$dict), by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>%
  left_join(chart, by = "chip_nr") %>%
  left_join(dl$lang, by = "lang_nr")

# Merge foci-exp and all dictionary files -------------------------------------------------------------------------------------- #

# Same merging operation for foci-exp
foci_task <- dl$foci_exp %>%
  left_join(rm_duplicates(dl$dict), by = c("lang_nr", "term_abb"), na_matches = "never") %>%
  left_join(dl$speaker, by = c("lang_nr", "speaker_nr")) %>% 
  left_join(chart, by = "grid_coord") %>%
  left_join(dl$lang, by = "lang_nr")

# Check the length of the new and the old foci dfs
nrow(foci_task) - nrow(dl$foci_exp)

# Write task dfs as csv --------------------------------------------------------------------------------------------------------- #

# Helper fun for printing
make_csv <- function(df_list, df_names, path = getwd()) {
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

# We take the data from the language dictionary and merge them with their corresponding coordinate values
wcs_iso_codes <- left_join(wcs_iso_codes, 
                           lat_long, 
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
a <- dl$mun_2_lab %>% select(7:9) 
names(a) <- c("L", "a", "b")
c <- LAB(a[[1]], a[[2]], a[[3]])

# A 2D plot of the Lab color space for the chips
ggplot(a, aes(x = a, y = b)) +
  geom_point(size = 3, aes(color = hex(c, fixup = TRUE))) +
  scale_color_identity() +
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(size=12)) +
  theme_bw()

# A 3D plot of the Lab color space for the chips
scatterplot3d( a[[3]], a[[2]], a[[1]], color = hex(c, fixup = TRUE), pch = 19,
               angle =100, scale.y=0.4, xlab = "a", ylab = "b", zlab = "L")

# Additional Lab plot with further perspectives
plot(b)

# Frequency distribution of color chips in the foci task -------------------------------------------------------------------------------------------------------- #

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
p <- foci_task %>%
  group_by(L_star, a_star, b_star) %>%
  count(chip_nr) %>% 
  arrange(-n) %>%
  ungroup()

q <- p %>% select(1:3) 

r <- LAB(q[[1]], q[[2]], q[[3]])

p %>%
  ggplot(aes(x = reorder(chip_nr, -n), y = n)) + 
  geom_bar(stat = "identity", fill = hex(r, fixup = TRUE)) +
  scale_x_discrete(breaks = p$chip_nr[seq(1,330, 15)], 
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





