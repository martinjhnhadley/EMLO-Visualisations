library(ggplot2)
library(shiny)
library(DT)
library(lubridate)
library(plotly)
library(dplyr)
library(shinyBS)

source("data-processing.R", local = T)

shinyServer(function(input, output, session) {
  output$timeline_timline_selected_cols_UI <- renderUI({
    selectInput(
      "timline_selected_cols",
      label = "Columns to show: ",
      choices = displayable_columns,
      selected = initial_columns,
      multiple = TRUE,
      width = "100%"
    )
  })
  
  # output$pulldown_selected_policy_UI <- renderUI({
  #   selectInput("pulldown_selected_policy",
  #               label = "Select a policy",
  #               choices = unique(as.character(timeline_data$Name.Policy)))
  # })
  
  output$pulldown_timeline_selected_cols_UI <- renderUI({
    selectInput(
      "pulldown_selected_cols",
      label = "Columns to show: ",
      choices = displayable_columns,
      multiple = TRUE,
      selected = initial_columns,
      width = "100%"
    )
  })
  
  output$timeline <- renderPlotly({
    ggplotly(ggplot(
      timeline_data,
      aes(
        x = Valid.from..b.,
        xend = Valid.until..c...,
        y = Name.Policy,
        yend = Name.Policy,
        colour = Type.of.policy
      )
    ) +
      geom_segment(size = 3) +
      xlab("Date") + ylab(""))
    
  })
  
  output$timeline_selected_Policy_Table <- renderDataTable({
    event_data <- event_data("plotly_click")
    
    selected_Policy <-
      levels(timeline_data$Name.Policy)[event_data$y]
    
    data_to_show <-
      timeline_data %>% filter(as.character(Name.Policy) == selected_Policy)
    data_to_show[, input$timline_selected_cols]
  }, rownames = FALSE, filter = 'top')
  
  output$pulldown_selected_Policy_Table <- renderDataTable({
    timeline_data[, input$pulldown_selected_cols]
  }, rownames = FALSE, filter = 'top')
  
  output$timeline_selected_Policy_UI <- renderUI({
    event_data <- event_data("plotly_click")
    
    if (is.null(event_data)) {
      wellPanel("Select an event in the timeline to view more details about the policy.")
    } else {
      fluidRow(column(
        uiOutput("timeline_timline_selected_cols_UI"),
        bsTooltip(
          "timeline_timline_selected_cols_UI",
          "Add/remove columns to the table by typing/removing names from here",
          "top",
          options = list(container = "body")
        ),
        dataTableOutput("timeline_selected_Policy_Table"),
        width = 12
      ))
      
    }
    
  })
  
  output$download_spreadsheet <- downloadHandler(
    filename = "policies_are_great.xlsx",
    # desired file name on client
    content = function(con) {
      file.copy("data/policies.xlsx", con)
    }
  )
  
})