## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Mireia Borrell-Porta (orcid.org/0000-0003-2328-1258)
## Data Source: local file
## ================================================================================

library(ggplot2)
library(shiny)
library(DT)
library(lubridate)
library(plotly)
library(plyr)
library(dplyr)
library(shinyBS)

library(scales)


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
      as.Date(start_date),
      " to ",
      as.Date(end_date),
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
  
  # output$plain_datatable_selected_policy_UI <- renderUI({
  #   selectInput("plain_datatable_selected_policy",
  #               label = "Select a policy",
  #               choices = unique(as.character(timeline_data$Name.Policy)))
  # })
  
  output$timeline <- renderPlotly({
    
    timeline_data$Type.of.Policy <- gsub("allowances policy", "</br>allowances policy", timeline_data$Type.of.Policy )
    cutoff_timeline_data <- timeline_data
    
    cutoff_timeline_data$Valid.from.b.[cutoff_timeline_data$Valid.from.b. < as.Date("1997/01/01")] <- as.Date("1997/01/01")
    
    
    policy_separators <- cutoff_timeline_data %>%
      filter(Valid.from.b. > as.Date("1997/01/01") & Valid.until.c. > as.Date("1997/01/01"))
    
    timeline_ggplot <- ggplot(
      cutoff_timeline_data,
      aes(
        x = Valid.from.b.,
        xend = Valid.until.c.,
        y = Name.of.Policy,
        yend = Name.of.Policy,
        colour = Type.of.Policy
      )
    ) +
      geom_segment(size = 4,
                   aes(
                     # x = Valid.from.b. + 60*60*24*10*3,
                     # xend = Valid.until.c. - 60*60*24*10*3,
                     x = Valid.from.b.,
                     xend = Valid.until.c., # Draw tooltipped geom_segments over everything, make almost invisible
                     y = Name.of.Policy,
                     yend = Name.of.Policy,
                     text = NULL
                   )
      )  + 
      geom_segment(
        data = policy_separators,
        size = 4,
        aes(
          # x = Valid.from.b. - 60*60*24*10*3,
          # xend = Valid.from.b. + 60*60*24*10*3,
          x = Valid.from.b. - 18,
          xend = Valid.from.b. + 18,
          y = Name.of.Policy,
          yend = Name.of.Policy,
          text = NULL
        ),
        color = "black"
      ) +
      geom_segment(size = 4,
                   show.legend = F,
                   aes(
                     x = Valid.from.b.,
                     xend = Valid.until.c.,
                     y = Name.of.Policy,
                     yend = Name.of.Policy,
                     text = gantt_labeler(
                       start_date = Valid.from.b.,
                       end_date = Valid.until.c.,
                       y_axis = Name.of.Policy,
                       color = Type.of.Policy
                     ),
                     alpha = 0.001 # Draw tooltipped geom_segments over everything, make almost invisible
                   )
      )  +
      scale_x_date(
        breaks = seq(as.Date("1997/01/01"), as.Date(paste0(year(
          max(cutoff_timeline_data$Valid.until.c.)
        ) + 1, "-01-01")), "years"),
        labels = date_format("%Y"),
        limits = c(as.Date("1997/01/01"), as.Date(max(cutoff_timeline_data$Valid.until.c.)))
      ) + 
      xlab("") + ylab("") + scale_colour_brewer(name = "Type of Policy",
                                     type = "qual",
                                     palette = "Dark2") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = unit(c(0,0,1,1), "cm"))
    
    
    ggplotly(timeline_ggplot, tooltip = "text")
    
    # Original
    # ggplotly(
    #   ggplot(
    #     timeline_data,
    #     aes(
    #       x = Valid.from.b.,
    #       xend = Valid.until.c.,
    #       y = Name.of.Policy,
    #       yend = Name.of.Policy,
    #       colour = Type.of.Policy,
    #       text = gantt_labeler(
    #         start_date = Valid.from.b.,
    #         end_date = Valid.until.c.,
    #         y_axis = Name.of.Policy,
    #         color = Type.of.Policy
    #       )
    #     )
    #   ) +
    #     geom_segment(size = 4) +
    #     geom_segment(
    #       aes(
    #         x = Valid.from.b. - 25,
    #         xend = Valid.from.b.,
    #         y = Name.of.Policy,
    #         yend = Name.of.Policy
    #       ),
    #       colour = "black",
    #       size = 4,
    #       arrow = arrow(type = "closed")
    #     ) + 
    #     ## add yearly breaks ## + 
    #     xlab("Date") + ylab("") + scale_colour_brewer(name = "Type of Policy", type = "qual", palette = "Dark2"),
    #   tooltip = "text"
    # )
    
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
  
  }, extensions = c("FixedHeader", "Buttons"), 
  rownames = FALSE, 
  escape = FALSE, 
  
  class = 'cell-border stripe', 
  options = list(autoWidth = FALSE,
                 scrollX = TRUE,
                 paging = FALSE,
                 dom = 'Bfrtip',
                 buttons = list(
                   list(extend = "excel",
                        text = "Download current view of data",
                        filename = "Filtered Policies",
                        exportOptions = list(
                          modifier = list(
                            selected = FALSE
                          )
                        )
                   )
                 ),
                 defer = TRUE,
                 fixedHeader = list(header = TRUE),
                 columnDefs = list(list(width = "200px", targets = 0))
                 # fixedColumns = list(leftColumns = 2, rightColumns = 1)
  )
  )
  
  output$plain_datatable_selected_cols_UI <- renderUI({
    selectInput(
      "plain_datatable_selected_cols",
      label = "Columns to show: ",
      choices = long_colnames_replacements,
      multiple = TRUE,
      selected = initial_columns,
      width = "100%"
    )
  })
  
  output$plain_datatable_selected_Policy_Table <- DT::renderDataTable({
    
    data_to_show <- timeline_data[, input$plain_datatable_selected_cols]
    
    colnames(data_to_show) <- mapvalues(
      colnames(data_to_show),
      from = long_colnames_replacements %>% as.character(),
      to = long_colnames_replacements %>% names(),
      warn_missing = F
    )
    
    data_to_show
    
  }, extensions = c("FixedHeader", "Buttons"), 
  rownames = FALSE, 
  filter = 'top', 
  escape = FALSE, 
  
  class = 'cell-border stripe', 
  options = list(autoWidth = FALSE,
                 paging = FALSE,
                 scrollX = TRUE,
                 dom = 'Bfrtip',
                 fixedHeader = list(header = TRUE),
                 buttons = list(
                 list(extend = "excel",
                      text = "Download current view of data",
                      filename = "Filtered Policies",
                      exportOptions = list(
                        modifier = list(
                          selected = FALSE
                        )
                      )
                      )
                 ),
                 columnDefs = list(list(width = "200px", targets = 0))
                 # fixedColumns = list(leftColumns = 2, rightColumns = 1)
                 )
  
  )
  
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
        uiOutput("type_of_details"),
        DT::dataTableOutput("timeline_selected_Policy_Table", width = "100%"),
        width = 12
      ))
      
    }
    
  })
  
  # output$type_of_details <- renderUI(
  #   checkboxGroupInput("type_of_details", "Type of Details", 
  #                      choices = c("Monetary Entitlements", "Non-Monetary Entitlements", "Validity: Date of Childbirth", "Focus of amendments", "Recipient","Territorial.application"),
  #                      selected = NULL,
  #                      inline = TRUE
  #                      )
  # )
  
  # output$download_spreadsheet <- downloadHandler(
  #   filename = "policies.xlsx",
  #   # desired file name on client
  #   content = function(con) {
  #     file.copy("data/policies.xlsx", con)
  #   }
  # )
  # 
})