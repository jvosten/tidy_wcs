#!/bin/bash

# Call installer script
Rscript packages.R --install

# Call shiny app file and open in browser
R -e "shiny::runApp('./app.R', launch.browser = TRUE)"

