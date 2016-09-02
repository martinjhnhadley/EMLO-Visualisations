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
library(plyr)
library(dplyr)
library(highcharter)
library(htmlwidgets)
library(webshot)
library(markdown)
library(plotly)
library(httr)
library(lubridate)

## Markers provided by https://mapicons.mapsmarker.com/

source("data-processing.R", local = TRUE)


## =========================== Labellers ====================================
## ==============================================================================

map_point_labeller <-
  function(locname = NA,
           sitename = NA,
           post_0 = NA,
           ante_0 = NA,
           sitecountry = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p>Location Name: ",
      locname,
      "</p>",
      "<p>Site Name: ",
      sitename,
      "</p>",
      "<p>Country: ",
      sitecountry,
      "</p>",
      "<p>Date Range: ",
      "from ",
      post_0,
      " to ",
      ante_0,
      "</p>",
      "<p>",
      "<a href=",
      # Removed until OxRep URLs are reorganised
      # url_generator(sitecountry = sitecountry, sitename = sitename),
      "http://http://oxrep.classics.ox.ac.uk/databases/shipwrecks_database//",
      " target=blank rel='noopener noreferrer'>",
      "OxRep Shipwreck Database",
      "</a>",
      "</p>"
    )
  }


## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  output$timeslider_UI <- renderUI({
    min_date <- min(shipwrecks_with_locations$ante_0)
    max_date <- max(shipwrecks_with_locations$post_0)
    
    sliderInput(
      "selected_time_period",
      label = "Selected Time Period",
      min = round_any(min_date, 100, f = floor),
      # max = round_any(max_date, 50, f = ceiling),
      max = 1800,
      value = c(round_any(min_date, 100, f = floor), 
                # round_any(max_date, 100, f = ceiling)
                1800
      ),
      step = 100,
      width = "100%"
      # timeFormat = "%Y"
    )
  })
  
  output$map <- renderLeaflet({
    shipwreck_points <-
      shipwrecks_locations_spdf[shipwrecks_locations_spdf$ante_0 >= input$selected_time_period[1] &
                                  shipwrecks_locations_spdf$post_0 <= input$selected_time_period[2], ]
    
    map <-
      leaflet(data = shipwreck_points, height = "100%") %>% addProviderTiles(input$selected_map_tile)
    
    switch(input$plot_marker,
           "Shipwreck Icon" = {
             map %>% addMarkers(
               popup = ~ map_point_labeller(
                 sitename = sitename,
                 sitecountry = sitecountry,
                 locname = locname,
                 post_0 = post_0,
                 ante_0 = ante_0
               ),
               icon = makeIcon(
                 "shipwreck.png",
                 iconWidth = 18,
                 iconHeight = 18
               )
             ) %>%
               fitBounds(
                 lng1 = min(shipwrecks_locations_spdf$sitelong),
                 lng2 = max(shipwrecks_locations_spdf$sitelong),
                 lat1 = min(shipwrecks_locations_spdf$sitelat),
                 lat2 = max(shipwrecks_locations_spdf$sitelat)
               )
           },
           "Circles" = {
             map %>% addCircleMarkers(
               popup = ~ map_point_labeller(
                 sitename = sitename,
                 sitecountry = sitecountry,
                 locname = locname,
                 post_0 = post_0,
                 ante_0 = ante_0
               ),
               fillColor = "#FE7569",
               color = "#000",
               stroke = TRUE,
               weight = 2,
               radius = 5,
               fillOpacity = 0.5
             ) %>%
               fitBounds(
                 lng1 = min(shipwrecks_locations_spdf$sitelong),
                 lng2 = max(shipwrecks_locations_spdf$sitelong),
                 lat1 = min(shipwrecks_locations_spdf$sitelat),
                 lat2 = max(shipwrecks_locations_spdf$sitelat)
               )
           })
    
    
  })
  
})
