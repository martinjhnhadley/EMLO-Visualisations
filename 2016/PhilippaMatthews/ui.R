library(shiny)
library(DT)
library(htmltools)

shinyUI(
  fluidPage(
    wellPanel(
      includeMarkdown("App_Description.Rmd")
    ),
    uiOutput("HEPITOPES_selected_cols_UI"),
    fluidPage(DT::dataTableOutput("HEPITOPES_datatable", width = "100%"))
  )
)