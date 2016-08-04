library(shiny)
library(htmltools)
library(igraph)
library(visNetwork)


source("data-processing.R", local = TRUE)

shinyServer(
  function(input, output, session){
    
    output$main_component_or_subcomponents_UI <- renderUI({
      selectInput("main_component_or_subcomponents",
                  label = "show main component or sub-components",
                  choices = component_names_for_ui)
    })
    
    igraph_to_analyse <- reactive({
      decomposed_igraph[[as.numeric(input$main_component_or_subcomponents)]]
    })
    
    
    
    output$display_visNetwork <- renderVisNetwork({
      
      if(is.null(input$main_component_or_subcomponents)){
        return()
      }
      
      igraph_to_visualise <- igraph_to_analyse()
      visIgraph(igraph_to_visualise)
    })
    
  }
)