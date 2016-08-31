library(igraph)
library(visNetwork)
library(plyr)
library(dplyr)
library(shiny)
library(ForceAtlas2)
library(igraph)
library(DT)

source("data-processing.R", local = T)


## =========================== Server Function ==================================
## ==============================================================================

shinyServer(function(input, output, sessions) {
  source("control_tracker.R", local = TRUE)$value
  source("contract_institution_network.R", local = TRUE)$value
  
  ## =========================== Generate Graph ====================================
  ## ==============================================================================
  
  
  institution_igraph <- reactive({
    institution_igraph <-
      graph.data.frame(d = institution_edges[, 1:4], vertices = institution_nodes[, 1:5])
    V(institution_igraph)$title <- institution_nodes$name
    V(institution_igraph)$color <- institution_nodes$color
    
    institution_igraph
  })
  
  output$this_network <- renderVisNetwork({
    if (is.null(input$layout_control)) {
      return()
    }
    
    switch(
      input$people_or_departments,
      "individuals" = {
        graph_to_plot <- institution_igraph()
      },
      "departments" = {
        graph_to_plot <- contract_instition_network(institution_igraph())
      }
    )
    
    switch(
      input$layout_control,
      "layout_nicely" = {
        visIgraph(graph_to_plot,
                  idToLabel = F,
                  layout = "layout_nicely") %>%
          visOptions(nodesIdSelection = TRUE) %>%
          visEvents(selectNode = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes);
                    ;}")
  },
  "layout_with_fr" = {
    #   visPhysics(
    #     solver = "forceAtlas2Based",
    #     forceAtlas2Based = list(avoidOverlap = 1),
    #     stabilization = list(iterations = 400)
    #   )
    visIgraph(graph_to_plot,
              idToLabel = F,
              layout = "layout_with_fr") %>%
      visOptions(nodesIdSelection = TRUE) %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
  }
      )
    
      })
  
  individual_datatable <- reactive({
    selected_id <-
      institution_nodes[institution_nodes$name == input$current_node_id$nodes, "id"]
    subsetted_edges <-
      filter(institution_edges, from == selected_id |
               to == selected_id)
    subsetted_edges$from <-
      mapvalues(
        subsetted_edges$from,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    subsetted_edges$to <-
      mapvalues(
        subsetted_edges$to,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  
  department_datatable <- reactive({
    department_members <- filter(institution_nodes,
                                 department == input$current_node_id$nodes) %>%
      select(id) %>%
      .[, 1]
    
    subsetted_edges <-
      filter(institution_edges,
             from %in% department_members |
               to %in% department_members)
    
    subsetted_edges$from <-
      mapvalues(
        subsetted_edges$from,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    subsetted_edges$to <-
      mapvalues(
        subsetted_edges$to,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  
  output$selected_node_table <- DT::renderDataTable({
    print(input$this_network_selected)
    
    if (is.null(input$current_node_id$nodes)) {
      return()
    }
    
    onClickInputCheck(show_Details = {
      switch(
        input$people_or_departments,
        "individuals" = {
          individual_datatable()
        },
        "departments" = {
          department_datatable()
        }
      )
    })
    
  })
  
  output$selected_node_table_UI <- renderUI({
    onClickInputCheck(
      never_Clicked = {
        wellPanel("Select a node for more details")
      },
      show_Details = {
        wellPanel(DT::dataTableOutput("selected_node_table"))
      },
      destructive_Change = wellPanel("Select a node for more details")
    )
    
  })
  
  })