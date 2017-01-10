library(shiny)
library(plotly)
library(highcharter)
library(googleVis)

fluidPage(navbarPage(
  "KMP Poaching",
  tabPanel(
    "Calendar of Activity",
    fluidPage(
      wellPanel("This section of the app allows the dataset to be explored via an interactive calendar. Click on a date for more information"),
      
      fluidRow(
        column(
          wellPanel(selectInput(
            "calendar_shots_or_patrols",
            "Show shots or patrols?",
            choices = c("gunshots", "patrols")
          ),
          uiOutput("calheatmap_select_day_UI")),
          width = 4
        ),
        
        column(
          div(style = 'overflow-x:scroll;overflow-y:hidden;alignment:center', uiOutput("calheatmap_gvis")),
          width = 8,
          style = "min-width:600px"
        )
      )
    )
  ),
  
  tabPanel(
    "Gunshot surface",
    fluidPage(
      plotlyOutput("gunshots_surface")
    )
  ),
  
  collapsible = TRUE
))