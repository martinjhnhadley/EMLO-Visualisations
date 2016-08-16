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

shinyUI(navbarPage(
  "Oxford Roman Economy Project",
  tabPanel(
    "Map of Shipwrecks",
    fluidPage(
      tags$style(type = "text/css", "body { overflow-y: scroll; }"),
      tags$style(type = "text/css", "#map {height: calc(85vh - 100px) !important;}"),
      fluidRow(
        column(
          selectInput(
            "plot_marker",
            label = "Plot Markers",
            choices = c("Shipwreck Icon", "Circles")
          ),
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
          width = 4
        ),
        column(
          uiOutput("timeslider_UI"),
          tags$script('$("#timeslider_UI").ionRangeSlider({
    type: "double",
                      grid: true,
                      min: 0,
                      max: 1000,
                      from: 200,
                      to: 800,
                      prefix: "$"
                      });'),
          width = 8
        )
      ),
      leafletOutput("map")
    )
  ),
  tabPanel("About",
           includeMarkdown("App_Description.Rmd")),
  collapsible = TRUE
))