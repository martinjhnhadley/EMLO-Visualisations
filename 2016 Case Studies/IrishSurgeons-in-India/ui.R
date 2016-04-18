library(shiny)
library(leaflet)
library(DT)

shinyUI(
  fluidPage(
    fluidRow(
      column(
             wellPanel(uiOutput("marker_mouseover")),width = 4),
      column(leafletOutput("irelandmap"), width = 8)
    ),
    conditionalPanel(
      "typeof input.irelandmap_marker_click !== 'undefined'",
      h1("Individuals born at the selected address:"),
      DT::dataTableOutput("marker_click")
    )
  ))