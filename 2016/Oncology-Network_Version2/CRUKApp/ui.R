## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

library(shiny)
library(igraph)
library(visNetwork)
library(highcharter)
library(shinyBS)

shinyUI(
  navbarPage(
    "",
    tabPanel(
      "Welcome",
      fluidPage(
        "tth"
      )
    ),
    tabPanel(
      "Centre",
      fluidPage(
        "ff"
      )),
    tabPanel(
      "Deparment",
      fluidPage(
        
        
        tags$head(
          tags$script(
            '
            Shiny.addCustomMessageHandler("scrollDown",
            function(color) {
            var y = $(window).scrollTop();  //your current y position on the page
            $(window).scrollTop(y+200);
            }
            );'
    )
          ),
    uiOutput("select_department_UI"),
    uiOutput("department_app_title"),
    uiOutput("department_app_description"),
    bsCollapse(
      id = "collapseExample",
      open = NULL,
      bsCollapsePanel(HTML(
        paste0(
          '<span class="glyphicon glyphicon-plus" aria-hidden="true"></span>',
          " Deparment Overview (click to expand)"
        )
      ),
      fluidPage(
        uiOutput("department_app_collapsile_info")
      ), style = "primary")
    ),
    tabsetPanel(
      tabPanel("People Directory",
               uiOutput("people_directory_UI")),
      tabPanel(
        "Department Collaboration Network",
        fluidPage(
          fluidRow(
            column(
              selectInput(
                "people_or_departments",
                "Show?",
                choices = c("within department", "within whole network")
              ),
              uiOutput("department_network_edge_degree_UI"),
              width = 4
            ),
            column(visNetworkOutput("department_network"),
                   width = 8)
          ),
          highchartOutput("highchart_node_legened", height = "150px")
        )
      )
    )
    # wellPanel(
    #   DT::dataTableOutput("selected_node_table")
    # )
    
    
      )
    ),
    tabPanel(
      "Principal Investigators",
      fluidPage(
        
      )
    )
    
    
  , collapsible = TRUE))