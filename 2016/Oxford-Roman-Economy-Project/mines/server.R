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
library(dplyr)
library(highcharter)

## Markers provided by https://mapicons.mapsmarker.com/

## ============== Filter shipwrecks by sites ====================================
## ==============================================================================

imported_shipwrecks <-
  read.csv("data/Current OxREP database/shipwrecks.csv",
           stringsAsFactors = F)
shipwrecks <- imported_shipwrecks

sites_df <-
  read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
## Extract Mines Sites
sites_df <- sites_df[sites_df$sitetype == "Mine", ]
## Drop sites with coords 0,0
sites_df <-
  sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0, ]
## Drop NA in locations
sites_df <-
  sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong), ]
## Make into a spdf

mines_with_locations <-
  SpatialPointsDataFrame(coords = sites_df[, c("sitelong", "sitelat")], data = sites_df)


my_subset <- sites_df %>% select(sitename, sitearea, sitecountry)

foo <- count(my_subset, sitearea)
foo$sitearea

highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(categories = foo$n) %>%
  hc_add_series(name = foo$sitearea, data = foo$sitearea) %>%
  hc_title(text = paste0("Mean number of desktop items aggregated by ","ddd"))

## =========================== Date Filter ====================================
## ==============================================================================


## =========================== Labellers ====================================
## ==============================================================================

mine_labeller <-
  function(sitename = NA,
           sitearea = NA) {
    paste0(# "<p>", Name, "</p>",
      "<p>Location Name: ",
      sitename,
      "</p>",
      "<p>Site Area: ",
      sitearea,
      "</p>")
  }


## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {

  output$mines_map <- renderLeaflet({

    map <-
      leaflet(data = mines_with_locations) %>% addProviderTiles(input$selected_map_tile)

    switch(input$plot_marker,
           "Mine Icon" = {
             map %>% addMarkers(
               popup = ~mine_labeller(sitename = sitename, sitearea = sitearea),
               icon = makeIcon(
                 "mine.png",
                 iconWidth = 18,
                 iconHeight = 18
               )
             )
           },
           "Circles" = {
             map %>% addCircles(
               popup = ~mine_labeller(sitename = sitename, sitearea = sitearea)
             )
           })
  
  })
  
})
