## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Data Source: local file
## ================================================================================


library(shiny)
library(leaflet)
library(sp)
library(plotly)
library(htmltools)
library(highcharter)
library(shinyBS)


shinyUI(
    navbarPage(
      "Oxford Roman Economy Project",
      tabPanel("Map of Mines",
               fluidPage(
                 tags$style(type = "text/css", "body { overflow-y: scroll; }"),
                 tags$style(type = "text/css", "#map {height: calc(85vh - 100px) !important;}"),
                 fluidRow(
                   column(
                     selectInput(
                       "selected_map_tile",
                       label = "Map Style",
                       choices = c(
                         "Hydda.Base",
                         "OpenTopoMap",
                         "Thunderforest.Landscape",
                         "Esri.WorldShadedRelief",
                         "Esri.OceanBasemap"
                       ),
                       selected = "Hydda.Base"
                     ),
                     selectInput(
                       "plot_marker",
                       label = "Plot Markers",
                       choices = c("Mine Icon", "Circles")
                     ),
                     width = 3
                   ),
                   column(
                     uiOutput("metal_filter_ui"),
                     bsTooltip(
                       "metal_filter_ui",
                       "Remove metals by selecting and deleting, add new metals by typing their names",
                       "top",
                       options = list(container = "body")
                     ),
                     width = 3
                   ),
                   column(
                     uiOutput("mining_technique_filter_ui", width = "100%"),
                     bsTooltip(
                       "mining_technique_filter_ui",
                       "Remove mining techniques by selecting and deleting, add new metals by typing their names",
                       "top",
                       options = list(container = "body")
                     ),
                     width = 6
                   )
                 ),
                 leafletOutput("map")
               )
               ),
      tabPanel("About",
               includeMarkdown("App_Description.Rmd")
               ),
      collapsible = TRUE
      
    )
      
      # fillPage(leafletOutput("map", height = "100%"))
    )