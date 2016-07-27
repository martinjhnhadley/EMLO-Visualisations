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

shinyUI(fluidPage(
  ## Drop minor ticks from sliders
  tags$style(type = "text/css", "
             .irs-grid-pol {display: none;}
             "),
  wellPanel(fluidRow(
    column(h2("Mines in OxRep"), width = 4),
    
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
      width = 4
    ),
    column(
      selectInput(
        "plot_marker",
        label = "Plot Markers",
        choices = c("Mine Icon", "Circles")
      ),
      width = 4
    )
  ),
  includeMarkdown("App_Description.Rmd")),
  
  tabsetPanel(
    tabPanel(
      "Map",
      leafletOutput("mines_map", height = "800px")
    ),
    tabPanel(
      "Some Plots",
      sidebarLayout(
        sidebarPanel(
          selectInput("count_by", label = "Count By",
                      choices = c("sitecountry","siteprovince"))
        ),
        mainPanel(
          highchartOutput("mines_counted_by_chart")
        )
      )
    )
  ), padding = 10))