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
      choices = unique(gig_economy_data$occupation)
    )
  })
  
  plot_data <- eventReactive(input$selected_occupation,{
    gig_economy_data[gig_economy_data$occupation == input$selected_occupation,]
  },ignoreNULL = TRUE)
  
  output$dygraph <- renderDygraph({
    plot_data <- plot_data()
    
    new_gigs <- plot_data %>%
      filter(status == "new")
    new_gigs <- xts(new_gigs$count, new_gigs$date)
    
    closed_gigs <- plot_data %>%
      filter(status %in% c("closed", "filled"))
    closed_gigs <- xts(closed_gigs$count, closed_gigs$date)
    
    both_ts <- cbind(new_gigs, closed_gigs)
    names(both_ts) <- c("New", "Closed")
    
    dygraph(both_ts) %>% dyRangeSelector()
    
  })
  
})