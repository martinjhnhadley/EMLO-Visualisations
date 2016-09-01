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
      choices = c(unique(gig_economy_data$occupation), "All Occupations")
    )
  })
  
  plot_data <- eventReactive(input$selected_occupation, {
    if (input$selected_occupation == "All Occupations") {
      new_only <- gig_economy_data %>%
        filter(status == "new")
      all_occupations <- aggregate(new_only$count,
                by = list(Category = new_only$date),
                FUN = sum)
      xts(all_occupations$x, all_occupations$Category)
    } else {
      new_gigs <-
        gig_economy_data[gig_economy_data$occupation == input$selected_occupation,] %>%
        filter(status == "new")
      xts(new_gigs$count, new_gigs$date)
    }
    
  }, ignoreNULL = TRUE)
  
  output$dygraph <- renderHighchart({
    plot_data <- plot_data()
    
    highchart() %>% 
      hc_add_series_xts(plot_data, name = input$selected_occupation)
    
  })
  
})