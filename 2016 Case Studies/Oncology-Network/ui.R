library(shiny)
library(igraph)
library(visNetwork)

shinyUI(
  fluidPage(
    wellPanel("Extreme Proof of Concept of the CRUK Network at Oxford University"),
    uiOutput("layout_control_UI"),
    visNetworkOutput("crukNetwork", width = "100%")
  )
)