## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Data Source: local file
## ================================================================================insta

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

## Markers provided by https://mapicons.mapsmarker.com/

## ============== Filter shipwrecks by sites ====================================
## ==============================================================================

source("data-processing.R", local = TRUE)


## =========================== Labellers ====================================
## ==============================================================================

url_generator <- function(sitecountry = NA,
                          sitename = NA) {
  paste0(
    "http://oxrep.classics.ox.ac.uk/databases/sites/",
    gsub(" ", "_", tolower(sitecountry)),
    "/",
    gsub(" ", "_", tolower(sitename)),
    "_mine/"
  )
}

map_point_labeller <-
  function(sitename = NA,
           sitearea = NA,
           miningdistrict = NA,
           siteprovince = NA,
           sitecountry = NA) {
    paste0(
      # "<p>", Name, "</p>",
      "<p>Location Name: ",
      sitename,
      "</p>",
      "<p>Mining District Area: ",
      sitearea,
      "</p>",
      "<p>Province: ",
      siteprovince,
      "</p>",
      "<p>",
      "<a href=",
      url_generator(sitecountry = sitecountry, sitename = sitename),
      " target=blank>",
      "Click for more info",
      "</a>",
      "</p>"
    )
  }


## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {
  output$timeslider_UI <- renderUI({
    min_date <- min(mine_details$evntpost, na.rm = TRUE)
    max_date <- max(mine_details$evntante, na.rm = TRUE)
    
    sliderInput(
      "selected_time_period",
      label = "Selected Time Period",
      min = round_any(min_date, 50, f = floor),
      max = round_any(max_date, 50, f = ceiling),
      value = c(min_date, max_date),
      step = 50,
      width = "100%"
      # timeFormat = "%Y"
    )
  })
  output$metal_filter_ui <- renderUI({
    selectInput(
      "metal_filter",
      label = "Metals to show:",
      choices = metals_vector,
      selected = metals_vector,
      multiple = TRUE,
      width = "100%"
    )
    
  })
  
  output$mining_technique_filter_ui <- renderUI({
    selectInput(
      "mining_technique_filter",
      label = "Mining techniques to show:",
      choices = mining_techniques_vector,
      selected = mining_techniques_vector,
      multiple = TRUE,
      width = "100%"
    )
    
  })
  
  output$map <- renderLeaflet({
    if (is.null(input$metal_filter) |
        is.null(input$mining_technique_filter)) {
      leaflet() %>% addProviderTiles(input$selected_map_tile)
    }
    
    filtered_siteids <-
      intersect(metals_df[metals_df$keywrd %in% input$metal_filter,]$siteid,
                mining_techniques_df[mining_techniques_df$keywrd %in% input$mining_technique_filter,]$siteid)
    
    
    if (length(filtered_siteids) > 0) {
      filtered_mines_with_locations <-
        mines_with_locations[mines_with_locations$siteid %in% filtered_siteids, ]
      
      filtered_mines_with_locations <-
        filtered_mines_with_locations[filtered_mines_with_locations$evntpost >= input$selected_time_period[1] &
                                        filtered_mines_with_locations$evntante <= input$selected_time_period[2],]
      
      map <-
        leaflet(data = filtered_mines_with_locations) %>%
        addProviderTiles(input$selected_map_tile)
      
      
      switch(input$plot_marker,
             "Mine Icon" = {
               map %>% addMarkers(
                 # popup = ~map_point_labeller(sitename = sitename, sitearea = sitearea),
                 popup = ~ map_point_labeller(
                   sitename = sitename,
                   sitearea = sitearea,
                   miningdistrict = miningdistrict,
                   siteprovince = siteprovince,
                   sitecountry = sitecountry
                 ),
                 icon = makeIcon(
                   "mine.png",
                   iconWidth = 18,
                   iconHeight = 18
                 )
               )
             },
             "Circles" = {
               map %>% addCircleMarkers(
                 popup = ~ map_point_labeller(sitename = sitename, sitearea = sitearea),
                 fillColor = "#FE7569",
                 color = "#000",
                 stroke = TRUE,
                 weight = 2,
                 radius = 5,
                 fillOpacity = 0.5
               )
             })
      
    } else {
      leaflet() %>% addProviderTiles(input$selected_map_tile) %>%
        fitBounds(
          lng1 = min(mines_with_locations$sitelong),
          lng2 = max(mines_with_locations$sitelong),
          lat1 = min(mines_with_locations$sitelat),
          lat2 = max(mines_with_locations$sitelat)
        )
    }
    
  })
  
})
