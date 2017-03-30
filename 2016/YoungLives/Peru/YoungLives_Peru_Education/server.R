## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Paul Dornan
## Data Source: local file
## ================================================================================

library(shiny)
library(highcharter)
library(htmltools)
library(dplyr)
library(tidyr)
library(oidnChaRts)

source("data-processing.R", local = TRUE)

## ============================ shinyServer ==============================
## ======================================================================================

shinyServer(function(input, output){
  
  output$selected_property_type_UI <- renderUI({
    
    selectInput("selected_property_type", label = "Disaggregate data",
                choices = unique(country_schooling$Property.Type))
    
  })
  
  output$selected_category_UI <- renderUI({
    
    if(is.null(input$selected_property_type)){
      return()
    }
    
    selected_property_type <- input$selected_property_type
    
    non_empty_categories <- country_schooling %>%
      filter(Property.Type == selected_property_type &
             !is.na(value)) %>%
      select(category) %>%
      unique() %>%
      unlist(use.names = F)
    non_empty_categories <- categories_list[categories_list %in% non_empty_categories]
    
    if(is.null(input$selected_category)){
      already_selected <- NA
    } else {
      already_selected <- input$selected_category
    }
    
    if(already_selected %in% non_empty_categories){
      selectInput("selected_category", label = "Selected indicator",
                  choices = categories_list[categories_list %in% non_empty_categories],
                  selected = already_selected)
    } else {
      selectInput("selected_category", label = "Selected indicator",
                  choices = categories_list[categories_list %in% non_empty_categories])
    }
  
  })
  
  output$no_data_warning <- renderUI({
    wellPanel("There is no data to show for your currently selected ")
  })
  
  output$comparison_chart <- renderHighchart({
    
    if(is.null(input$selected_category)){
      return()
    }
    
    selected_property_type <- input$selected_property_type
    selected_category <- input$selected_category
    
    ## stacked_bar_chart doesn't handle extrenous columns in early versions
    data_to_viz <- filter(
      country_schooling,
      Property.Type == selected_property_type &
        category == selected_category
    ) %>%
      select(Property, Cohort, value) %>%
      mutate(value = ifelse(is.na(value), 0, value))
    
    bar_chart <- stacked_bar_chart(data = data_to_viz,
                                   library = "highcharter",
                                   categories.column = ~Property,
                                   subcategories.column = ~Cohort, 
                                   value.column = ~value,
                                   subcategories.order = c("Younger Cohort (age 12 in 2013)",
                                                           "Older Cohort (age 12 in 2006)")) %>%
      hc_plotOptions("series" = list("minPointLength" = 3))
    
    if(input$selected_category %in% percentage_categories){
      bar_chart %>%
        hc_yAxis(max = 100)
    } else
      bar_chart
    
  })
  
})