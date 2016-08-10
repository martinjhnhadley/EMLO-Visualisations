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
                          sitename = NA){
  paste0("http://oxrep.classics.ox.ac.uk/databases/sites/",
         gsub(" ","_",tolower(sitecountry)),
         "/",
         gsub(" ","_",tolower(sitename)),
         "_mine/")
}

map_point_labeller <-
  function(sitename = NA,
           sitearea = NA,
           miningdistrict = NA,
           siteprovince = NA,
           sitecountry = NA) {
    paste0(# "<p>", Name, "</p>",
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
      "<a href=",url_generator(sitecountry = sitecountry, sitename = sitename),">",
      "Click for more info",
      "</a>",
      "</p>")
  }


## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({

    map <-
      leaflet(data = mines_with_locations) %>% addProviderTiles(input$selected_map_tile)

    switch(input$plot_marker,
           "Mine Icon" = {
             map %>% addMarkers(
               # popup = ~map_point_labeller(sitename = sitename, sitearea = sitearea),
               popup = ~map_point_labeller(sitename = sitename,
                                           sitearea = sitearea,
                                           miningdistrict = miningdistrict,
                                           siteprovince = siteprovince,
                                           sitecountry = sitecountry),
               icon = makeIcon(
                 "mine.png",
                 iconWidth = 18,
                 iconHeight = 18
               )
             )
           },
           "Circles" = {
             map %>% addCircleMarkers(
               popup = ~map_point_labeller(sitename = sitename, sitearea = sitearea),
               fillColor = "#FE7569",
               color = "#000",
               stroke = TRUE,
               weight = 2,
               radius = 5,
               fillOpacity = 0.5
             )
           })
  
  })
  
})
