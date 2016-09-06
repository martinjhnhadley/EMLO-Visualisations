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

source("data-processing.R", local = T)

shinyServer(function(input, output, session) {
  output$selected_occupation_UI <- renderUI({
    selectInput(
      "selected_occupation",
      label = "Selected Occupation",
      choices = unique(gig_economy_data$occupation),
      selected = unique(gig_economy_data$occupation),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$highchart <- renderHighchart({
    
    selected_categories <- input$selected_occupation
    print(selected_categories)
    
    hc <- highchart()
    
    invisible(lapply(selected_categories,
                     function(x){
                       filtered <- gig_economy_data[gig_economy_data$occupation == x,]
                       hc <<- hc %>% 
                         hc_add_series_xts(xts(filtered$count, filtered$date), name = x)
                     }))
    hc
    
    
  })
  
})