library(shiny)
library(highcharter)
library(htmltools)
library(dplyr)
library(tidyr)

country_schooling <- read.csv(file = "data/india_education_schooling.csv", stringsAsFactors = F)
country_schooling$Sample.Size <- as.numeric(gsub(",","",country_schooling$Sample.Size))

colnames(country_schooling)

measure_list <- c("Percent.children.enrolled.in.school", 
                  "Percent.children.attending.private.schools", "Percent.children.receiving.extra.tuition", 
                  "Average.score.on.3.maths.questions.", "Average.raw.percent.score.in.Maths.test", 
                  "Average.raw.percent.score.in.Telugu.test", "Average.annual.tuition.fee.in.Rs", 
                  "Sample.Size")

measure_list <- setNames(measure_list, trimws(gsub("\\.", " ", measure_list)))

property_measure_groups <- c("2006", "2013")

## ============================ Stacked bar chart function ==============================
## ======================================================================================

stacked_bar_chart <- function(data = NA,
                              categories_column = NA,
                              measure_columns = NA,
                              stacking_type = NA,
                              ordering_function = c) {
  
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

## ============================ shinyServer ==============================
## ======================================================================================

shinyServer(function(input, output){
  
  output$selected_category_UI <- renderUI({
    
    selectInput("selected_category", label = "Selected measure",
                choices = unique(country_schooling$Property.Type))
    
  })
  
  output$selected_measure_UI <- renderUI({
    
    selectInput("selected_measure", label = "Selected measure",
                choices = measure_list)
    
  })
  
  output$comparison_chart <- renderHighchart({
    
    if(is.null(input$selected_measure)){
      return()
    }
    
    print(filter(country_schooling, Property.Type == input$selected_category) %>%
            select_("Property", "Cohort", input$selected_measure) %>%
            spread_("Cohort", input$selected_measure))
    
    data_to_viz <- filter(country_schooling, Property.Type == input$selected_category) %>%
      select_("Property", "Cohort", input$selected_measure) %>%
      spread_("Cohort", input$selected_measure)
    
    
    
    stacked_bar_chart(
      data = data_to_viz,
      categories_column = "Property",
      measure_columns = property_measure_groups
    )
    
    
    
  })
  
})