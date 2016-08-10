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


shinyUI(
    fluidPage(
      h3("Oxford Roman Economy Project: Mines"),
      tags$style(type = "text/css", "body { overflow-y: scroll; }"),
      tags$style(type = "text/css", "#map {height: calc(80vh - 100px) !important;}"),
      includeMarkdown("App_Description.Rmd"),
      fluidRow(column(
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
        selectInput(
          "plot_marker",
          label = "Plot Markers",
          choices = c("Mine Icon", "Circles")
        ),
        width = 4
      )),
      leafletOutput("map")
      # fillPage(leafletOutput("map", height = "100%"))
    )
  )