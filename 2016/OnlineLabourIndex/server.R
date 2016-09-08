## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: local file
## ================================================================================


library(shiny)
library(rfigshare)
library(lubridate)
library(plotly)
library(highcharter)
library(dygraphs)
library(xts)
library(dplyr)
library(htmltools)
library(tidyr)

source("data-processing.R", local = T)

stacked_bar_chart <- function(data = NA,
                              categories_column = NA,
                              measure_columns = NA,
                              stacking_type = NA,
                              ordering_function = c,
                              explicit_order = NA) {
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
        index = {
          if (is.na(explicit_order)) {
            ordered_measure[colNumber]
          } else
            explicit_order[colNumber]
        }
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = TRUE)
}

shinyServer(function(input, output, session) {
  output$selected_occupation_UI <- renderUI({
    selectInput(
      "selected_occupation",
      label = "Selected Occupation",
      choices = unique(gig_economy_by_occupation$occupation),
      selected = unique(gig_economy_by_occupation$occupation),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$occupation_xts_highchart <- renderHighchart({
    
    selected_categories <- input$selected_occupation
    print(selected_categories)
    
    hc <- highchart()
    
    invisible(lapply(selected_categories,
                     function(x){
                       filtered <- gig_economy_by_occupation[gig_economy_by_occupation$occupation == x,]
                       hc <<- hc %>% 
                         hc_add_series_xts(xts(filtered$count, filtered$date), name = x)
                     }))
    hc
    
    
  })
  
  output$selected_regions_UI <- renderUI({
    selectInput(
      "selected_region",
      label = "Selected Region",
      choices = unique(gig_economy_by_boundary$country_group),
      selected = unique(gig_economy_by_boundary$country_group),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$region_xts_highchart <- renderHighchart({
    
    selected_categories <- input$selected_region
    print(selected_categories)
    
    hc <- highchart()
    
    invisible(lapply(selected_categories,
                     function(x){
                       filtered <- gig_economy_by_boundary[gig_economy_by_boundary$country_group == x,]
                       aggregated <- aggregate(filtered$count, by=list(date=filtered$timestamp, region = filtered$country_group), FUN=sum)
                       hc <<- hc %>% 
                         hc_add_series_xts(xts(aggregated$x, aggregated$date), name = x)
                     }))
    hc
    
    
  })
  
  output$global_trends_group_by_UI <- renderUI({
    selectInput("global_trends_group_by",
                "Group By",
                choices = list("region" = "country_group","occupation" = "occupation"))
  })
  
  output$global_trends_stack_by_UI <- renderUI({
    selectInput("global_trends_stack_by",
                "Stack By",
                choices = c("percent","number"))
  })
  
  output$global_trends_stacked_bar_chart <- renderHighchart({
    
    x_axis <- input$global_trends_group_by
    y_axis <- setdiff(c("country_group","occupation"), x_axis)
    print(y_axis)
    print(x_axis)
    
    ## Sum by occupation and region
    prepared_data <- aggregate(
      gig_economy_by_boundary$count,
      by = list(x_axis = gig_economy_by_boundary[[x_axis]],
                y_axis = gig_economy_by_boundary[[y_axis]]),
      FUN = sum
    ) %>%
      spread(y_axis, x)
    
    stacked_bar_chart(
      data = prepared_data,
      categories_column = "x_axis",
      measure_columns = setdiff(colnames(prepared_data), c("x_axis")),
      stacking_type = input$global_trends_stack_by
    )
    
    
  })
  
})