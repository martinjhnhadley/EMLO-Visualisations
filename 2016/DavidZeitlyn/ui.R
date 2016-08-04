library(shiny)
library(htmltools)
library(visNetwork)

shinyUI(
  fluidPage(
    includeMarkdown("App_Description.Rmd"),
    uiOutput("main_component_or_subcomponents_UI"),
    visNetworkOutput("display_visNetwork")
  )
)