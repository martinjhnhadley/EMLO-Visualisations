library(ggplot2)
library(shiny)
library(DT)
library(plotly)
library(shinyBS)

shinyUI(fluidPage(
  h2("Policy Timelines"),
  wellPanel(includeMarkdown("App_Description.Rmd"),
  downloadButton("download_spreadsheet", label = "Download", class = NULL)),
  tabsetPanel(
    tabPanel("Timeline",
             fluidPage(
               plotlyOutput(
                 "timeline"
               ),
               # DT::dataTableOutput("data_table")
               uiOutput("timeline_selected_Policy_UI")
             )),
    tabPanel("All Policies",
             fluidPage(
               fluidRow(column(
                 "The table below allows the entire dataset to be explored.",p(),
                 # uiOutput("pulldown_selected_policy_UI"),
                 uiOutput("pulldown_timeline_selected_cols_UI"),
                 bsTooltip(
                   "pulldown_timeline_selected_cols_UI",
                   "Add/remove columns to the table by typing/removing names from here",
                   "top",
                   options = list(container = "body")
                 ),
                 DT::dataTableOutput("pulldown_selected_Policy_Table"),
                 width = 12
               ))
               
             ))
  )
  
))