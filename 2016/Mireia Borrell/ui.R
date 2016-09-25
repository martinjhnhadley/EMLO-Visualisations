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
library(plotly)
library(shinyBS)

shinyUI(navbarPage(
  tags$head(tags$style(HTML("#chart .legend .legendtoggle {
   display: none;
                       }
                       /* just for presentation: shows the default cursor insted of the text cursor */
                       #chart .legend .traces .legendtext {
                       cursor: default;
                       }"))),
  "",
  tabPanel("Policy Timelines",
             fluidPage(
               plotlyOutput("timeline"),
               p(),
               uiOutput("timeline_selected_Policy_UI")
             )),
    tabPanel("All Policies",
             fluidPage(fluidRow(
               column(
                 "The table below allows the entire dataset to be explored.",
                 p(),
                 uiOutput("plain_datatable_selected_cols_UI"),
                 bsTooltip(
                   "plain_datatabletimeline_selected_cols_UI",
                   "Add/remove columns to the table by typing/removing names from here",
                   "top",
                   options = list(container = "body")
                 ),
                 DT::dataTableOutput("plain_datatable_selected_Policy_Table", width = "100%"),
                 width = 12
               )
             ))),
    tabPanel(
      HTML('<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'),
      fluidPage(
        includeMarkdown("App_Description.Rmd")
      )
    ), collapsible = TRUE
  ))
  