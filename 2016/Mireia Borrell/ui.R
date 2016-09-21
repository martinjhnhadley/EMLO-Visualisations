## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Mireia Borrell-Porta 
## Data Source: local file
## ================================================================================

ibrary(ggplot2)
library(shiny)
library(DT)
library(plotly)
library(shinyBS)

shinyUI(fluidPage(
  h2("Policy Timelines"),
  wellPanel(
    includeMarkdown("App_Description.Rmd"),
    downloadButton("download_spreadsheet", label = "Download", class = NULL)
  ),
  tabsetPanel(
    tabPanel("Timeline",
             fluidPage(
               plotlyOutput("timeline"),
               uiOutput("timeline_selected_Policy_UI")
             )),
    tabPanel("All Policies",
             fluidPage(fluidRow(
               column(
                 "The table below allows the entire dataset to be explored.",
                 p(),
                 uiOutput("pulldown_timeline_selected_cols_UI"),
                 bsTooltip(
                   "pulldown_timeline_selected_cols_UI",
                   "Add/remove columns to the table by typing/removing names from here",
                   "top",
                   options = list(container = "body")
                 ),
                 DT::dataTableOutput("pulldown_selected_Policy_Table", width = "100%"),
                 width = 12
               )
             )))
  )
  
))