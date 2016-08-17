 library(shiny)
library(htmltools)
library(igraph)
library(visNetwork)
library(plyr)
library(stringr)
library(dplyr)
library(shinyjs)

source("beautification.R", local = TRUE)

source("data-processing.R", local = TRUE)

component_names_for_ui <-
  setNames(as.list(1:length(decomposed_igraph)), c("Main Component", paste(
    "Component Number", 2:length(decomposed_igraph)
  )))

code_names_for_ui <-
  setNames(names_df$id, names_df$name)

shinyServer(function(input, output, session) {
  
  source("onClickInput-State.R", local = TRUE)$value
  
  output$main_component_vertex_proportion <-
    renderText({
      round(100 * {
        vcount(decomposed_igraph[[1]]) / vcount(entire_igraph)
      }, 0)
    })
  
  output$intro_text_at_top <- renderUI({
    includeHTML(knitr::knit("App_Description.Rmd"))
  })
  
  output$main_component_or_subcomponents_UI <- renderUI({
    selectInput("main_component_or_subcomponents",
                label = "show main component or sub-components",
                choices = component_names_for_ui)
  })
  
  
  computed_graph <-
    reactiveValues(igraph_object = NULL,
                   igraph_node_labels = NULL)
  
  
  
  igraph_to_analyse <-
    eventReactive(input$main_component_or_subcomponents,
                  decomposed_igraph[[as.numeric(input$main_component_or_subcomponents)]],
                  ignoreNULL = TRUE)
  
  codes_to_names_list <-
    eventReactive(input$main_component_or_subcomponents, {
      igraph_to_analyse <- igraph_to_analyse()
      replacement_names <-
        mapvalues(
          V(igraph_to_analyse)$name,
          from = names_df$id,
          to = names_df$name,
          warn_missing = F
        )
      setNames(V(igraph_to_analyse)$name, replacement_names)
    }, ignoreNULL = TRUE)
  
  source("main-component.R", local = TRUE)$value
  source("sub-component.R", local = TRUE)$value

  
})