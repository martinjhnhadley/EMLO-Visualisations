## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: local file
## ================================================================================


library(shiny)
library(highcharter)
library(plotly)
library(dygraphs)
library(htmltools)
library(shinyBS)

shinyServer(fluidPage(
  tags$head(
    tags$style(HTML("
                    .navbar .container-fluid, .navbar-collapse {
    padding-left:0;
                    }
                    .navbar-collapse.in {
                    padding-left:30px;
                    }
                    "))
    ),
  # wellPanel(includeMarkdown(knitr::knit(
  #   "App_Description.Rmd"
  # ))),
  # HTML('<img src = "oii_thumbnail.png" style="max-width:270;float:right;margin-right:20px"/>
  # 
  #      <h1>Online Labour Index <a href="https://twitter.com/ilabourproject" class="twitter-follow-button" data-size="large" data-show-count="false">Follow @ilabourproject</a><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script></h1>
  #      '),
  tabsetPanel(
    tabPanel(
      "OLI",
      fluidPage(
        uiOutput("landing_rollmean_k_UI"),
        highchartOutput("landing_xts_highchart", width = "100%", height = "500px")
      )
    ),
    tabPanel(
      "By occupation",
      fluidPage(
          fluidPage(
            # wellPanel("Add/remove occupations in the box below to change the data shown in the chart"),
            fluidRow(
              column(
            uiOutput("selected_occupation_UI"),
            bsTooltip(
              "selected_occupation_UI",
              "Filter occupations by deleting/adding their names",
              "bottom",
              options = list(container = "body")
            ), width = 8),
            column(
            uiOutput("occupation_rollmean_k_UI"),
            width = 4)
            ),
            highchartOutput("occupation_xts_highchart", width = "100%")
        )
      )
    ),
    tabPanel(
      "By employer country",
      fluidPage(
        # wellPanel("Add/remove regions in the box below to change the data shown in the chart"),
        fluidRow(column(
          uiOutput("region_xts_selected_regions_UI"),
          bsTooltip(
            "region_xts_selected_regions_UI",
            "Filter rcountries/regions by deleting/adding their names",
            "bottom",
            options = list(container = "body")
          ),
          width = 7
        ),
        column(
          uiOutput("region_rollmean_k_UI"),
          width = 5
        )),
        highchartOutput("region_xts_highchart", width = "100%"),
        width = "100%"
      )
    ),
    tabPanel(
      "Occupation x country",
      fluidPage(
        # wellPanel("Zoom into the chart by selecting an area of interest, pan around in the chart by holding SHIFT."),
        fluidRow(
          column(
            uiOutput("global_trends_group_by_UI"),
            width = 6
          ),
          column(
            uiOutput("global_trends_stack_by_UI"),
            width = 6
          )
        ),
        highchartOutput("global_trends_stacked_bar_chart", width = "100%", height = "450px")
      )
    ),
    tabPanel(
     HTML('<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'),
      fluidPage(
        wellPanel(includeMarkdown(knitr::knit(
          "App_Description.Rmd"
        )))
      )
    )
  )
  
))