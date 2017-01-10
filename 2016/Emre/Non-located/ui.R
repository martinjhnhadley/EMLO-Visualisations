## ====== Emre Located

library(shiny)
library(eventsObserveR)

shinyUI(
  fluidPage(
    wellPanel("Emre's events with locations"),
    uiOutput("species_filter_UI"),
    eventsObserverOutput("emre_events")
  )
)