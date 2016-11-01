## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Paul Dornan
## Data Source: local file
## ================================================================================

library(shiny)
library(highcharter)
library(htmltools)

shinyUI(
  navbarPage(
    theme = "young-lives.css",
    title = "Young Lives: Peru",
    tabPanel(
      "Education Comparison",
      fluidPage(
        fluidRow(column(
          uiOutput("selected_property_type_UI"), width = 6
        ),
        column(uiOutput(
          "selected_category_UI"
        ), width = 6)),
        highchartOutput("comparison_chart")
      )
    ),
    tabPanel("About",
             includeMarkdown(knitr::knit(
               "App_Description.Rmd"
             ))),
    collapsible = TRUE
  )
)