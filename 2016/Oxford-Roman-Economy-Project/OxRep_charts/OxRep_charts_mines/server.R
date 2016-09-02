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
library(tidyr)

## Markers provided by https://mapicons.mapsmarker.com/

source("data-processing.R", local = TRUE)

## ============== stackedBarChart ===============================================
## ==============================================================================

stacked_hc_chart <- function(data = NA,
                             categories_column = NA,
                             measure_columns = NA,
                             stacking_type = NA,
                             ordering_function = NA) {
  ordered_measure <-
    order(unlist(lapply(measure_columns, function(x) {
      ordering_function(data[, x])
    })),
    decreasing = TRUE) - 1
  
  chart <- highchart() %>%
    hc_xAxis(categories = data[, categories_column],
             title = categories_column)
  
  invisible(lapply(1:length(measure_columns), function(colNumber) {
    chart <<-
      hc_add_series(
        hc = chart,
        name = measure_columns[colNumber],
        data = data[, measure_columns[colNumber]],
        index = ordered_measure[colNumber]
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = TRUE)
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
  
  output$stack_by_UI <- renderUI({
    if (input$count_by == "Number of Mines") {
      return()
    }
    
    selectInput(
      "stack_by",
      label = "Stack by",
      choices = list("Percent" = "percent", "Number of Mines" = "normal")
    )
  })
  
  output$chart <- renderHighchart({
    switch(input$count_by,
           "Number of Mines" = {
             grouped_tally <- mine_details %>%
               group_by_(input$group_by) %>%
               select_(input$group_by, "keywrd") %>%
               count() %>%
               as.data.frame
             
             stacked_hc_chart(
               data = grouped_tally,
               categories_column = input$group_by,
               measure_columns = "n",
               ordering_function = var
             ) %>%
               hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)  %>%
               hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
             
           },
           {
             grouped_tally <- filter(mine_details, keycat == input$count_by) %>%
               group_by_(input$group_by) %>%
               select_(input$group_by, "keywrd") %>%
               count(keywrd) %>%
               spread(keywrd, n) %>%
               as.data.frame()
             
             stacked_hc_chart(
               data = grouped_tally,
               categories_column = input$group_by,
               measure_columns = unique(mine_details[mine_details$keycat == input$count_by, "keywrd"]),
               stacking_type = input$stack_by,
               ordering_function = var
             ) %>%
               hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)
           })
    
  })
  
  output$download_hchart <- downloadHandler(
    filename = function() {
      paste("highchart-image", '.png', sep = '')
    },
    content = function(file) {
      saveWidget(widget = highchart_reactive(), file = "hc_chart.html")
      webshot(url = "hc_chart.html",
              file = file,
              cliprect = "viewport")
    },
    contentType = "image/png"
  )
  
})
