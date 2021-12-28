#!/usr/bin/env Rscript

# <!---------------------------------------------------------- Notes ----------------------------------------------------------->
#
#
#
# <!---------------------------------------------------------------------------------------------------------------------------->

# Load libraries
# library(colorspace)
# library(shinythemes)
# library(maps)
# library(scatterplot3d)
# library(skimr)
# library(DT)
# library(tidyverse)
# library(wcs)
# library(xml2)
# library(rvest)

load_pkgs <- function() {
  pkgs <- c("colorspace", "shinythemes", "maps",
            "scatterplot3d", "skimr", "DT", "shiny",
            "tidyverse", "wcs", "xml2", "rvest")

  invisible(lapply(pkgs, library, character.only = TRUE))
}
load_pkgs()

# # Load data locally
# list_of_files <- list.files(path = "data/csv", recursive = TRUE,
#                             pattern = "\\.csv$",
#                             full.names = TRUE)
# 
# list_of_files %>%
#   set_names(sub(pattern = "(.*)\\..*$",
#                 replacement = "\\1",
#                 basename(.))) %>%
#   map(read_csv) %>%
#   list2env(., envir = .GlobalEnv)

# Code ------------------------------------------------------------------------------------------------------------------------- #

# ersetzen durch munsell library fkt?
mun_2_lab <- mun_2_lab %>%
  mutate(hex = hex(LAB(mun_2_lab[[7]], 
                       mun_2_lab[[8]], 
                       mun_2_lab[[9]]), 
                   fixup = TRUE)
  )

foci_plot <- left_join(mun_2_lab, 
                       chip, 
                       by = "chip_nr") %>%
  left_join(., 
            foci_exp,
            by = "grid_coord") %>%
  select(-c(2:9, 11:13,16))

get_lat_long <- function() {
  url4 <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
  file <- xml2::read_html(url4)
  tables <- rvest::html_nodes(file, "table")
  lat_long <- as_tibble(rvest::html_table(tables[[1]], 
                                          fill = TRUE)) %>%
    rename(lang_country = name,
           lat = latitude,
           long = longitude) %>%
    select(-country) %>%
    mutate(lang_country = recode(lang_country, `Côte d'Ivoire` = "Ivory Coast",
                                 `Suriname` = "Surinam",
                                 `United States` = "USA"))
}

map_data <- left_join(lang, 
                      get_lat_long(), 
                      by = "lang_country") %>%
  left_join(., 
            speaker,
            by = "lang_nr")

# UI --------------------------------------------------------------------------------------------------------------------------- #

ui <- fluidPage(
  
  # Navbar layer
  navbarPage("World Color Survey", 
             # tags$head(
             #     tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
             #              padding-top:4px !important; 
             #              padding-bottom:0 !important;
             #              height: 50px;
             #              }
             #             .navbar {min-height:25px !important;}'))
             # ),
             theme = shinytheme("united"),
             
             # Panel I --------------------------------------------------------------------------------------------------------- #
             
             # 1 Input: lang_nr; 
             # 3 outputs: top 10 terms, top 10 chips, location of lang_nr on a worldmap
             tabPanel("Languages", fluid = TRUE, icon = icon("globe-americas"),
                      titlePanel("Information on the languages of WCS"),
                      br(),
                      fluidRow(
                        column(4,
                               selectInput("lang_panel1", "Language",
                                           choices = setNames(lang$lang_nr, lang$lang_name),
                                           width = "70%"
                               )
                        )
                      ),
                      fluidRow(
                        column(3, h4(p("Term Task:" , tags$br(), "Top 10 terms for colors in this language"))),
                        column(3, h4(p("Foci Task:" , tags$br(), "10 selection of color chips"))),
                        column(3, h4(p(tags$br(),"Location")))
                      ),
                      fluidRow(
                        column(3, tableOutput("term")),
                        column(3, tableOutput("foci")),
                        column(6, plotOutput("map"))
                      )
             ), 
             
             # Panel II -------------------------------------------------------------------------------------------------------- #
             
             # 3 Inputs: lang, speaker_nr, chip_nr
             # 3 Outputs: info on individual speaker, on speakers selected chips, color of each chip
             tabPanel("Term Task", fluid = TRUE, icon = icon("bar-chart-o"),
                      titlePanel("Results for the Term Task"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          h5(p("Select a language and speaker:")),
                          selectInput("lang_panel2", "Language",
                                      choices = setNames(lang$lang_nr, 
                                                         lang$lang_name),
                                      width = "70%",
                                      selected = 79
                          ),
                          sliderInput(inputId = "speaker_panel2",
                                      label = "Speaker Nr.",
                                      min = 1,
                                      max = 25,
                                      value = 15,
                                      width = "300px"), 
                          h5(p("Color Dictionary:")),
                          numericInput("chipnr_panel2", "Chip Nr.", 
                                       value = 100, 
                                       min = 1, 
                                       max = 330),
                          fluidRow(
                            column(12, plotOutput("chip_dict_output"), height = "50px")
                          ),
                          width = 3
                        ),
                        mainPanel(
                          h4(p("Some information on the speaker:")),
                          tableOutput("speaker_table_output"),
                          h4(p("And the selection of chips made by the speaker:")),
                          br(),
                          dataTableOutput("term_table_output")
                        )
                      )
             ), 
             
             # Panel III ------------------------------------------------------------------------------------------------------- #
             
             # 2 Inputs: lang, speaker_nr
             # 1 Output: colorplot of foci term selection of individual speaker
             tabPanel("Foci Task", fluid = TRUE, icon = icon("chess-board"),
                      titlePanel("Results for the Term Task"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          h5(p("Select a language and speaker:")),
                          selectInput("lang_panel3", "Language",
                                      choices = setNames(lang$lang_nr, lang$lang_name),
                                      width = "70%",
                                      selected = 69
                          ),
                          sliderInput(inputId = "speaker_panel3",
                                      label = "Speaker Nr.",
                                      min = 1,
                                      max = 25,
                                      value = 17,
                                      width = "300px"), 
                          width = 3
                        ),
                        mainPanel(
                          h4(p("The plot shows you the BCT colors of the chosen speaker and the according chip number:")),
                          column(12, plotOutput("fociplot"))
                        )
                      )
             ), 
             
             # Panel IV -------------------------------------------------------------------------------------------------------- #
             
             # 4 Inputs: plot angle, scale_y, axis, grid
             # 1 Output: plot of Munsell color space
             tabPanel("Color Space", fluid = TRUE, icon = icon("adjust"),
                      #tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      #shinythemes::themeSelector(),
                      h4(p("This plot lets you look at the Munsell color space used in the WCS")),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "angle",
                                      label = "Angle between x and y axis ",
                                      min = -360,
                                      max = 360,
                                      value = 100,
                                      width = "300px"),
                          sliderInput(inputId = "scale_y",
                                      label = "Scale of y axis related to x- and z axis",
                                      min = 0,
                                      max = 1,
                                      value = 0.4,
                                      width = "300px"),
                          fluidRow(column(4,
                                          radioButtons(inputId = "axis",
                                                       label = "Plot Axis",
                                                       choices = c("Yes" = 1, "No" = 0),
                                                       selected = 1)
                          ),
                          column(4,
                                 
                                 # Plot Grid yes/no
                                 radioButtons(inputId = "grid",
                                              label = "Plot Grid",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 1))  
                          ), width = 3),
                        mainPanel(
                          column(9, plotOutput("chipnr_plot"))
                        )
                      )
             ), 
             
             # Panel V --------------------------------------------------------------------------------------------------------- #
             
             tabPanel("About", fluid = TRUE, icon = icon("bars"), 
                      fluidRow(
                        column(6,
                               #br(),
                               h4(p("About the Project")),
                               h5(p("This project is intended to facilitate access to the data of the World Color Survey archive. 
                                        The WCS (to be found here", a("https://www1.icsi.berkeley.edu/wcs/", 
                                                                      href = "https://www1.icsi.berkeley.edu/wcs/"), ") was an investigation,
                                        in which ››an average of 24 native speakers of each of 110 unwritten languages were asked (1) to name each of 330 Munsell chips, 
                                        shown in a constant, random order, and (2), exposed to a palette of these chips and asked to to pick out the best example(s) 
                                        ('foci') of the major terms elicited in the naming task.‹‹")),
                               br(),
                               h5(p("The project began as an attempt to combine my interest in Philosophy with a need to practice R, a 
                                        programming language used primarily for analyzing and reporting data.  It has two components.  
                                        The first is this app, which queries a dataset to return information in the form of plots, data tables
                                        etc.  The second is the dataset itself, which I assembled by merging files from the 
                                        WCS Archive.")),
                               br(),
                               h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at 21955156+jvosten@users.noreply.github.com"),
                                  p("The source code for this Shiny app is available soon ", a("on github", href = "https://github.com/gpilgrim2670/SwimMap"), "."))
                               
                               #hr(),
                               
                        ),
                        column(6,
                               #br(),
                               #             HTML('<img src="GregPicCrop.png", height="110px"
                               # style="float:right"/>','<p style="color:black"></p>'),
                               h4(p("About the Author")),
                               uiOutput("img"),
                               helpText(HTML(" &#169"),a("Studio Edelkoort", href = "https://www.edelkoort.com/shopping/sample-product/5899/")),
                        )
                      ) # fluidRow Panel V
             ) # tabPanel V
  ) # Navbar
) # UI

# Server ----------------------------------------------------------------------------------------------------------------------- #

server <- function(input, output, session) {
  
  # Preparation --------------------------------
  ## For Panel I
  term_selection <- reactive(term %>% 
                         filter(lang_nr == input$lang_panel1))
  foci_selection <- reactive(foci_exp %>% 
                         filter(lang_nr == input$lang_panel1))
  map_selection <- reactive(map_data %>%
                        filter(lang_nr == input$lang_panel1) %>%
                        group_by(lang_country, lat, long) %>% count())
  
  ## For Panel II
  term_table_selection <- reactive(purrr::map(list(foci_exp, speaker, term),
                                              ~filter(.x, 
                                             lang_nr == input$lang_panel2 & speaker_nr == input$speaker_panel2)
                                             ) %>%
                                     set_names(c("foci_exp", "speaker", "term"))
                          )
  
  amount_speaker <- function() {
    speaker %>% 
      filter(lang_nr == input$lang_panel2) %>%
      nrow()
  }
  
  observeEvent(input$lang_panel2, {
    updateSliderInput(session, "speaker_panel2", 
                      max = amount_speaker())
  })
  
  rm_duplicates <- function(lang_dict){
    lang_dict %>%
      dplyr::group_by(lang_nr, term_abb) %>%
      dplyr::mutate(duplicate.flag = n() > 1,
                    term_abb = if_else(duplicate.flag == TRUE, NA_character_, term_abb),
                    term_abb = as.factor(term_abb)) %>% 
      dplyr::ungroup() %>%
      dplyr::select(-duplicate.flag)
  }
  
  ## For Panel III
  fociplot_selection <- reactive(foci_plot %>%
                             filter(lang_nr == input$lang_panel3, speaker_nr == input$speaker_panel3))
  
 
  observeEvent(input$lang_panel3, {
    updateSliderInput(session, "speaker_panel3", max = speaker %>% 
                        filter(lang_nr == input$lang_panel3) %>%
                        nrow())
  })
  
  # Output --------------------------------
  
  ## For Panel I
  output$term <- renderTable(term_selection() %>% 
                               count(term_abb) %>% 
                               slice_max(n, n = 10) %>% 
                               rename("Term Abbr."=term_abb, 
                                      "Amount"=n), 
                                width = "80%")
  output$foci <- renderTable(foci_selection() %>% 
                               count(grid_coord) %>% 
                               arrange(-n) %>% 
                               slice(1:10) %>% 
                               rename("Grid Coordinate"=grid_coord, 
                                      "Amount"=n), 
                             width = "80%")
  
  output$map <- renderPlot({
    map_selection() %>%
      ggplot(aes(y = lat, 
                 x = long, 
                 colour = "#FF6347")) +
      geom_point(alpha = 0.9, 
                 size = 10) +
      borders("world") +
      coord_quickmap() +
      geom_text(aes(label = lang_country),  
                size = 5, vjust = 2.2, 
                nudge_y = 0, 
                colour = "grey20", 
                fontface = "bold") +
      scale_x_continuous(expand=c(0,0), 
                         name = element_blank()) + 
      scale_y_continuous(expand=c(0,0), 
                         name = element_blank()) + 
      theme_bw() +
      theme(legend.position = "none") +
      coord_fixed(1.15/1) 
  }, 
  res = 96)
  
  ## For Panel II
  output$term_table_output <- DT::renderDataTable(term_table_selection()$term %>%
                                           dplyr::left_join(rm_duplicates(dict), 
                                                            by = c("lang_nr", "term_abb"), 
                                                            na_matches = "never") %>%
                                           dplyr::left_join(chip, 
                                                            by = "chip_nr") %>%
                                           dplyr::left_join(mun_2_lab, 
                                                            by = "chip_nr") %>%
                                           select(-c(lang_nr, speaker_nr, term_nr, 
                                                     grid_row, grid_col, wcs_mv, 
                                                     wcs_mh)
                                                  ) %>%
                                           mutate(across(.cols = c(chip_nr, 
                                                                   mun_chroma, 
                                                                   mun_value), 
                                                         as.integer)
                                                  ), 
                                         options = list(searching = FALSE), 
                                         rownames = FALSE)
  
  output$speaker_table_output <- renderTable(left_join(term_table_selection()$speaker, 
                                                       lang,
                                                       by = "lang_nr") %>%
                                               mutate(across(.cols = c(lang_nr, speaker_nr), 
                                                             as.integer))
                                             )
 
  output$chip_dict_output <- renderPlot({
    if (input$chipnr_panel2 %in% 1:330) {
      mun_2_lab %>%
        filter(chip_nr == input$chipnr_panel2) %>%
        add_column(x = 0.5, y = 0.5) %>%
        ggplot(aes(x = 0.5, y = 0.5, width = 2)) +
        geom_tile(aes(fill = hex), colour = "grey50") +
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_identity()
    } else {
      rlang::warn(paste0("Value for chip number needs to be in the range of [1, 330], you typed: ", 
                          input$chipnr_panel2))
    }
  }, 
  res = 96, 
  height = 300)
  
  ## For Panel III
  output$fociplot <- renderPlot({
    fociplot_selection() %>%
      ggplot(aes(x=1:nrow(.), y = 0.5, fill=hex)) +
      geom_tile() +
      scale_fill_identity() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(breaks = seq(1, nrow(fociplot_selection()), 1),
                         labels = fociplot_selection()$chip_nr,  
                         expand = c(0,0)) +
      theme(legend.position="none",
            axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank())
  }, res = 96)
  
  ## For Panel IV
  output$chipnr_plot <- renderPlot({
    scatterplot3d(mun_2_lab[[9]], 
                  mun_2_lab[[8]], 
                  mun_2_lab[[7]], 
                  color = mun_2_lab[[10]], 
                  pch = 19,
                  angle = input$angle, 
                  scale.y = input$scale_y,
                  xlab = "a", ylab = "b", zlab = "L"
                  #, axis = as.logical(input$axis), grid = as.logical(input$grid)
    )
  }, res = 96)
  
  ## For Panel V
  output$img <- renderUI({
    tags$img(src = "https://www.edelkoort.com/wp-content/uploads/2014/12/ViewOnColour.001.jpg")
  })
}


# Run -------------------------------------------------------------------------------------------------------------------------- #
shinyApp(ui = ui, server = server)
