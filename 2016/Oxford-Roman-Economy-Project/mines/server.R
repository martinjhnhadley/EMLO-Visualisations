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

## Markers provided by https://mapicons.mapsmarker.com/

## ============== Filter shipwrecks by sites ====================================
## ==============================================================================

source("data-processing.R", local = TRUE)


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
  
  highchart_reactive <- reactive({
    selected_column <- input$count_by
    tally_column <- table(sites_df[, selected_column])
    tally_column <- data.frame(
      "measure" = names(tally_column),
      "count" = as.numeric(tally_column)
    )
    
    highchart() %>%
      hc_chart(type = "bar", zoomType = "xy", panning = TRUE) %>%
      hc_xAxis(categories = tally_column$measure) %>%
      hc_add_series(name = "Mines in Location", data = tally_column$count) %>%
      hc_title(text = paste0("Observations per ",selected_column))
  })
  
  output$mines_counted_by_chart <- renderHighchart({
    highchart_reactive()
  })

  plotly_reactive <- reactive({
    selected_column <- input$count_by
    tally_column <- table(sites_df[, selected_column])
    tally_column <- data.frame(
      "measure" = names(tally_column),
      "count" = as.numeric(tally_column)
    )
    
    
    
    highchart() %>%
      hc_chart(type = "bar", zoomType = "xy", panning = TRUE) %>%
      hc_xAxis(categories = tally_column$measure) %>%
      hc_add_series(name = "Mines in Location", data = tally_column$count) %>%
      hc_title(text = paste0("Observations per ",selected_column))
  })
  
  output$mines_counted_by_chart <- renderPlotly({
    highchart_reactive()
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
  
  ## Dummy download
  # output$download_hchart <- downloadHandler(
  #   filename = function() { paste("highchart-image", '.png', sep='') },
  #   
  #   content = function(file){
  #   foo_hchart <- highchart() %>%
  #     hc_chart(type = "bar", zoomType = "xy", panning = TRUE) %>%
  #     hc_xAxis(categories = c("a","b","c")) %>%
  #     hc_add_series(name = "Mines in Location", data = c(10,12,10)) %>%
  #     hc_title(text = paste0("Observations per"))
  #   
  #   saveWidget(as.widget(foo_hchart),"hc_chart.html")
  #   
  #   webshot("hc_chart.html", file = file,
  #           cliprect = "viewport")
  #   }
    # )

  output$download_hchart <- downloadHandler(
    filename = function() { paste("highchart-image", '.png', sep='') },
    content = function(file) {
      saveWidget(widget = highchart_reactive(), file = "hc_chart.html")
      webshot(url = "hc_chart.html", file = file,
              cliprect = "viewport")
    },
    contentType = "image/png"
  )
  
})
