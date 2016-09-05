library(shiny)
library(DT)
library(htmltools)

shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),
  wellPanel(includeMarkdown("App_Description.Rmd")),
  uiOutput("HEPITOPES_selected_cols_UI"),
  fluidPage(DT::dataTableOutput("HEPITOPES_datatable", width = "100%"))
))