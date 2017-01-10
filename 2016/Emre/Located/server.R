## ====== Emre Located

library(shiny)
library(eventsObserveR)
library(readr)
library(plyr)
library(dplyr)
library(lubridate)
library(viridis)

source("data-processing.R", local = TRUE)
source("beautification.R", local = TRUE)

## =========================== Apply beautification ==========================
## ============================================================================

emre_events <- emre_events %>%
  mutate(
    title = emre_event_labeller(
      no.individuals,
      species,
      camera.id,
      time,
      no.males,
      no.females,
      no.unknown,
      no.sa,
      no.juv
    )
  ) %>%
  mutate(
    shape = mapvalues(
      species,
      sample_shapes_data$event_type,
      sample_shapes_data$event_shape,
      warn_missing = FALSE
    )
  ) %>%
  mutate(radius = rep(10, nrow(emre_events)))


shinyServer(function(input, output) {
  output$species_filter_UI <- renderUI({
    selectInput(
      "species_to_exclude",
      "Select species to exclude from the visualisation:",
      choices = unique_species,
      selected = c(
        "Humans with or without domestic animals",
        "Humans",
        "Domestic animals"
      ),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  filtered_events <- eventReactive({
    input$species_to_exclude
  },
  emre_events %>% filter(!species %in% input$species_to_exclude))
  
  filtered_legend <- eventReactive({
    input$species_to_exclude
  },
  filtered_events() %>%
    rename(description = species) %>%
    unique() %>%
    mutate(
      shape = mapvalues(
        description,
        sample_shapes_data$event_type,
        sample_shapes_data$event_shape,
        warn_missing = FALSE
      )
    ) %>%
    select(description, shape, event_type_id) %>%
    unique())
  
  
  output$emre_events <- renderEventsObserver({
    eventsObserveR(
      events = filtered_events(),
      place.key = "station",
      places = emre_locations,
      legend = filtered_legend(),
      legend.columns = 2,
      background.image = "test-background.png"
      # size = list(
      #   view.width = 700,
      #   view.height = 800,
      #   interface.width = 900)
    )
    
  })
  
})
