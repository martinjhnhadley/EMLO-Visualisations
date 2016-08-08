library(shiny)
library(DT)
library(htmltools)

shinyUI(
  fluidPage(
    wellPanel(
      includeMarkdown("App_Description.Rmd")
    ),
    uiOutput("heliotopes_selected_cols_UI"),
    DT::dataTableOutput("heliotopes_datatable")
  )
)