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
    hc_chart(type = "bar", animation = FALSE) %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = TRUE)
}

## =========================== shinyFunction ====================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  output$timeslider_UI <- renderUI({
    min_date <- min(shipwreck_details$ante_0)
    max_date <- max(shipwreck_details$post_0)
    
    sliderInput(
      "selected_time_period",
      label = "Selected Time Period",
      min = round_any(min_date, 100, f = floor),
      max = round_any(max_date, 100, f = ceiling),
      value = c(round_any(min_date, 100, f = floor), round_any(max_date, 100, f = ceiling)),
      step = 100,
      width = "100%"
    )
  })

  grouped_tally <- reactive({
    
    shipwreck_details <-
      shipwreck_details[shipwreck_details$ante_0 >= input$selected_time_period[1] &
                          shipwreck_details$post_0 <= input$selected_time_period[2], ]
    
    group_by <- input$group_by
    
    ## Filter metals %>% count_by %>% select columns %>% tally
    grouped_tally <- as.data.frame(group_by_(shipwreck_details, group_by) %>%
      select_(group_by) %>%
      count()
    )
    
  })
  
  output$chart <- renderHighchart({
    grouped_tally <- grouped_tally()
    print(dput(grouped_tally))
    highchart() %>%
      hc_chart(type = "bar", animation = FALSE, zoomType = "x", panning = TRUE, panKey = 'shift') %>%
      hc_plotOptions(series = list(turboThreshold = 10000)) %>%
      hc_xAxis(categories = unique(grouped_tally[, input$group_by])) %>%
      hc_add_series(name = "Number of Shipwrecks", data = grouped_tally$n)
    
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
