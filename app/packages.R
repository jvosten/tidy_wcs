#!/usr/bin/env Rscript

# ***************************************************************************************************************************** #
# ***************************************************  Script installer  ****************************************************** #
# ********************************************  Do not run in interactive mode  *********************************************** #
# ***************************************************************************************************************************** #

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  # stopifnot(args %in% c("--install"))
  
  installer <- function(pkgs) {
    # Install packages not yet installed
    pkgs <- c("colorspace", "shinythemes", "maps", "scatterplot3d", 
              "skimr", "DT", "tidyverse", "wcs", "xml2", "rvest",
              "remotes"
              )
    
    installed_pkgs <- function() {pkgs %in% rownames(installed.packages())}
    
    if (any(installed_pkgs() == FALSE)) {
      cat("\n", "Installing missing packages: ", pkgs[!installed_pkgs()], "\n")
      if("wcs" %in% pkgs[which(!installed_pkgs())]) {
        install.packages("remotes")
        remotes::install_github("jvosten/wcs")
      }
      install.packages(pkgs[!installed_pkgs()], repos = "https://cran.rstudio.com/")
    }
  }
  if (length(args) > 0) {
    installer()
    cat("\n", "All packages installed", "\n")
  }
}

main()