## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Mireia Borrell-Porta 
## Data Source: local file
## ================================================================================

library(ggplot2)
library(shiny)
library(DT)
library(lubridate)
library(plotly)
library(dplyr)
library(shinyBS)
library(plyr)


## =========================== Beautification ===================================
## ==============================================================================

gantt_labeler <-
  function(start_date = NA,
           end_date = NA,
           y_axis = NA,
           color = NA) {
    paste0(
      "Policy Name: ",
      y_axis,
      "</br>",
      "Enforcement Period: ",
      start_date,
      " to ",
      end_date,
      "</br>",
      "Policy Type: ",
      color
    )
  }

new_lines_to_p_tags <- function(text) {
  gsub(pattern = "\n", replacement = "<br />", text)
}


## =========================== Shiny Server Fn ===================================
## ==============================================================================

source("data-processing.R", local = T)
source("long-colnames-replacements.R", local = T)

shinyServer(function(input, output, session) {
  output$timeline_timeline_selected_cols_UI <- renderUI({
    selectInput(
      "timeline_selected_cols",
      label = "Columns to show: ",
      choices = long_colnames_replacements,
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
      choices = long_colnames_replacements,
      multiple = TRUE,
      selected = initial_columns,
      width = "100%"
    )
  })
  
  output$timeline <- renderPlotly({
    ggplotly(
      ggplot(
        timeline_data,
        aes(
          x = Valid.from.b.,
          xend = Valid.until.c.,
          y = Name.of.Policy,
          yend = Name.of.Policy,
          colour = Type.of.Policy,
          text = gantt_labeler(
            start_date = Valid.from.b.,
            end_date = Valid.until.c.,
            y_axis = Name.of.Policy,
            color = Type.of.Policy
          )
        )
      ) +
        geom_segment(size = 4) +
        geom_segment(
          aes(
            x = Valid.from.b. - 25,
            xend = Valid.from.b.,
            y = Name.of.Policy,
            yend = Name.of.Policy
          ),
          colour = "black",
          size = 4,
          arrow = arrow(type = "closed")
        ) +
        xlab("Date") + ylab("") + scale_colour_brewer(name = "Type of Policy", type = "qual", palette = "Dark2"),
      tooltip = "text"
    )
    
  })
  
  output$timeline_selected_Policy_Table <- DT::renderDataTable({
    event_data <- event_data("plotly_click")
    
    selected_Policy <-
      levels(timeline_data$Name.of.Policy)[event_data$y]
    
    data_to_show <-
      timeline_data %>% filter(as.character(Name.of.Policy) == selected_Policy)
    data_to_show <- data_to_show[, input$timeline_selected_cols]
    
    colnames(data_to_show) <- mapvalues(
      colnames(data_to_show),
      from = long_colnames_replacements %>% as.character(),
      to = long_colnames_replacements %>% names(),
      warn_missing = F
    )
    
    data_to_show
  
  }, extensions = c("Responsive","FixedHeader"), rownames = FALSE, filter = 'top', escape = FALSE)
  
  output$pulldown_selected_Policy_Table <- DT::renderDataTable({
    
    data_to_show <- timeline_data[, input$pulldown_selected_cols]
    
    colnames(data_to_show) <- mapvalues(
      colnames(data_to_show),
      from = long_colnames_replacements %>% as.character(),
      to = long_colnames_replacements %>% names(),
      warn_missing = F
    )
    
    data_to_show
    
  }, extensions = c("Responsive","FixedHeader"), 
  rownames = FALSE, filter = 'top', escape = FALSE, options = list(fixedHeader = list(header = TRUE)))
  
  output$timeline_selected_Policy_UI <- renderUI({
    event_data <- event_data("plotly_click")
    
    if (is.null(event_data)) {
      wellPanel("Select an event in the timeline to view more details about the policy.")
    } else {
      fluidRow(column(
        uiOutput("timeline_timeline_selected_cols_UI"),
        bsTooltip(
          "timeline_timeline_selected_cols_UI",
          "Add/remove columns to the table by typing/removing names from here",
          "top",
          options = list(container = "body")
        ),
        DT::dataTableOutput("timeline_selected_Policy_Table", width = "100%"),
        width = 12
      ))
      
    }
    
  })
  
  output$download_spreadsheet <- downloadHandler(
    filename = "policies.xlsx",
    # desired file name on client
    content = function(con) {
      file.copy("data/policies.xlsx", con)
    }
  )
  
})