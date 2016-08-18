## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Data Source: local file
## ================================================================================


library(shiny)
library(leaflet)
library(sp)
library(plotly)
library(htmltools)
library(highcharter)
library(shinyBS)


shinyUI(navbarPage(
  "Oxford Roman Economy Project",
  tabPanel(
    "Distribution of Mines",
    fluidPage(
      tags$style(type = "text/css", "body { overflow-y: scroll; }"),
      tags$style(type = "text/css", "#chart {height: calc(85vh - 100px) !important;}"),
      includeMarkdown("Plots_Description.Rmd"),
      fluidRow(
        column(
          selectInput(
            "group_by",
            label = "Group by",
            choices = list("country" = "sitecountry", "province" = "siteprovince", "mine name" = "sitearea")
          ), width = 4),
        column(
          selectInput(
            "count_by",
            label = "Count by",
            choices = c("Metals", "Mining Techniques","Number of Mines")
          ), width = 4),
        column(
          uiOutput("stack_by_UI"),
          width = 4
        )
      ),
      uiOutput("timeslider_UI"),
      highchartOutput("chart")
    )
  ),
  tabPanel("About this tool",
           fillPage(includeMarkdown(
             "App_Description.Rmd"
           )))
))