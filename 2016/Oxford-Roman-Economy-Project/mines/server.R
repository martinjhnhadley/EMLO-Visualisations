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
  
  output$mines_counted_by_chart <- renderHighchart({
    
    selected_column <- input$count_by
    # print(selected_column)
    # print(as.data.frame(table(sites_df[,selected_column])))
    # 
    # print(sites_df %>% count(match(selected_column,names(.))))
    
    tally_column <- table(sites_df[, selected_column])
    
    tally_column <- data.frame(
      "measure" = names(tally_column),
      "count" = as.numeric(tally_column)
    )
    
    print(tally_column)
    # 
    # sites_df_counted <- count(sites_df, sitecountry)
    # # print(sites_df_counted)
    # 

    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = tally_column$measure) %>%
      hc_add_series(name = "Mines in Location", data = tally_column$count) %>%
      hc_title(text = paste0("Observations per ",selected_column))
    
  })

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
             map %>% addCircleMarkers(
               popup = ~mine_labeller(sitename = sitename, sitearea = sitearea),
               color = "#FE7569",
               stroke = FALSE,
               radius = 5,
               fillOpacity = 0.5
             )
           })
  
  })
  
})
