## ====== Emre Located

library(shiny)
library(eventsObserveR)

shinyUI(
  fluidPage(
    wellPanel("Emre's events with locations"),
    radioButtons("foo", "Should humans (and their domestic animals) be shown in the map?", choices = c("Exclude humans","Include humans"), inline = TRUE),
    uiOutput("species_filter_UI"),
    eventsObserverOutput("emre_events", height = "1000px", width = "680px")
    
  )
)