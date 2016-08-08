library(shiny)
library(htmltools)
library(visNetwork)

shinyUI(
  fluidPage(
    h2("Primatology Advisory Lineages"),
    wellPanel(uiOutput("intro_text_at_top"),
    uiOutput("main_component_or_subcomponents_UI")),
    conditionalPanel(
      condition = 'input.main_component_or_subcomponents == 1',
      fillPage(
        uiOutput("main_component_output")
      )
    )
  )
)