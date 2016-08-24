library(shiny)
library(highcharter)
library(htmltools)
library(dplyr)
library(tidyr)

country_schooling <- read.csv(file = "data/vietnam_education_schooling.csv", stringsAsFactors = F)
country_schooling$Sample.Size <- as.numeric(gsub(",","",country_schooling$Sample.Size))

measure_list <- c("Percent.children.enrolled.in.school", 
                  "Average.grade.attending", "Percent.children.receiving.extra.tuition", 
                  # "Percentage.of.children.who.can.correctly.solve..which.of.these.is.equal.to.342.", 
                  # "Percentage.of.children.who.can.correctly.solve.a.window.cleaning.problem", 
                  # "Percentage.of.children.who.can.answer.a.rope.cutting.question", 
                  "Average.of.performance.across.3.comparable.maths.questions.as.percentage", 
                  "Sample.Size")

measure_list <- setNames(measure_list, trimws(gsub("\\.", " ", measure_list)))

property_measure_groups <- c("Younger Cohort (age 12 in 2013)", "Older Cohort (age 12 in 2006)")

bar_order <- list(
  "Younger cohort (age 12 in 2013)" = 1,
  "Older cohort (age 12 in 2006)" = 0
)
bar_order_v <- as.numeric(bar_order)

percentage_measures <- as.character(measure_list)[grepl("ercent", measure_list)]

## ============================ Stacked bar chart function ==============================
## ======================================================================================

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

## ============================ shinyServer ==============================
## ======================================================================================

shinyServer(function(input, output){
  
  output$selected_category_UI <- renderUI({
    
    selectInput("selected_category", label = "Disaggregate data",
                choices = unique(country_schooling$Property.Type))
    
  })
  
  output$selected_measure_UI <- renderUI({
    
    selectInput("selected_measure", label = "Selected indicator",
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
    
    bar_chart <- stacked_bar_chart(
      data = data_to_viz,
      categories_column = "Property",
      measure_columns = property_measure_groups,
      explicit_order = bar_order_v
    )
    
    if(input$selected_measure %in% percentage_measures){
      bar_chart %>%
        hc_yAxis(max = 100)
    } else
      bar_chart
    
    
  })
  
})