
# Read in data ***************************************************************************************************************** #

library(DBI)
library(RSQLite)
library(here)
library(tidyverse)

# Write each df from dl to data/csv ---------------------------------------------------------------------------------------------- #

# Helper fun
make_csv <- function(df_list, df_names, path = here()) {
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

dir.create(here("data", "csv"))
make_csv(dl, names(dl), here("data", "csv"))

# Write term and foci task as csv
# make_csv(list(term_task, foci_task), 
#          c("term_task", "foci_task"),
#          here("data", "csv"))

# Write each df from dl to data/sqlite ------------------------------------------------------------------------------------------- #

write_sql <- function() {  
  dir.create(here("data", "sql"))
  wcsdb <- DBI::dbConnect(RSQLite::SQLite(), here("data", "sql", "tidy_wcs.sqlite3"))
  
  purrr::walk2(.x = dl, 
               .y = names(dl),
               ~RSQLite::dbWriteTable(wcsdb, .y, .x))
  
  DBI::dbDisconnect(wcsdb)
}

write_sql()

test_sql <- function() {
  path_to_db <- here("data", "sql", "tidy_wcs.sqlite3")
  con <- DBI::dbConnect(RSQLite::SQLite(), 
                        dbname = path_to_db)
  chip_db <- tbl(con, "chip")
  
  cat("Query on db chip table")
  chip_db %>%
    filter(chip_nr > 300) %>%
    print()
  cat("Same query on local copy of chip table")
  chip <- chip_db %>%
    collect() %>%
    filter(chip_nr > 300) %>%
    print()
  DBI::dbDisconnect(con)
}

test_sql()

# Write each df from dl to parquet ---------------------------------------------------------------------------------------------- #
write_parq <- function() {  
  dir.create(here("data", "parquet"))
  purrr::walk2(.x = dl, 
               .y = names(dl), 
               ~write_parquet(.x, 
                              here("data", "parquet", 
                                   paste0(.y, ".parquet")
                              )
               )
  )
}

write_parq()

