library(igraph)
library(visNetwork)
library(plyr)
library(dplyr)
library(shiny)
library(ForceAtlas2)
library(igraph)
library(DT)
library(highcharter)

source("beautification.R", local = T)
source("data-processing.R", local = T)


## =========================== Server Function ==================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  
  
  url_selected_department <- eventReactive(session$clientData$url_search,
                                           {
                                             query <- parseQueryString(session$clientData$url_search)
                                             if (!is.null(query[['department']])) {
                                               query[['department']]
                                             } else
                                               "oncology"
                                           })
  
  output$url_department <- renderUI(
   
    url_selected_department()
    
  )
  
  output$highchart_node_legened <- renderHighchart(
    highchart_legend(legend_names = department_colours$department,legend_colours = department_colours$colours)
  )
  
  source("control_tracker.R", local = TRUE)$value
  source("contract_institution_network.R", local = TRUE)$value
  
  ## =========================== Generate Graph ====================================
  ## ==============================================================================
  

  graph_to_display <- eventReactive(input$people_or_departments,
                                    switch(
                                      input$people_or_departments,
                                      "individuals" = {
                                        institution_igraph
                                      },
                                      "departments" = {
                                        contract_instition_network(institution_igraph)
                                      }
                                    ))
  
  output$displayed_network <- renderVisNetwork({

    graph_to_display <- graph_to_display()
    
    visIgraph(graph_to_display,
              idToLabel = F,
              layout = "layout_nicely") %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE)
                 ) %>%
      visLayout(hierarchical = FALSE) %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
})
  output$displayed_network_properties <- renderUI({
    wellPanel(
      p(paste0("Average path length: ",round(average.path.length(graph_to_display()),2))),
      p(paste0("Number of nodes: ",vcount(graph_to_display())))
    )
  })
  
  observeEvent(
    input$refocus_network,
    visNetworkProxy("displayed_network") %>%
      visFit(nodes = NULL, animation = list(duration = 500))
  )
  
  output$selected_node_sidePanel <- renderUI({
    if(input$displayed_network_selected == ""){
      return()
    }
    onClickInputCheck(
      never_Clicked = return(),
      show_Details = {
        wellPanel(
          paste0("Selected Node: ",input$displayed_network_selected),
          actionButton("scroll_down","Scroll down for details", width = "100%")
          )
      },
      destructive_Change = return()
    )
  })
  
  observeEvent(input$scroll_down, {
    session$sendCustomMessage(type = "scrollDown", 1)
  })
  
  
  individual_datatable <- reactive({
    
    if(input$displayed_network_selected == ""){
      return()
    }
    selected_id <-
      institution_nodes[institution_nodes$name == input$displayed_network_selected, "id"]
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
    print(input$displayed_network_selected)
    
    if(input$displayed_network_selected == ""){
      return()
    }
    
    department_members <- filter(institution_nodes,
                                 department == input$displayed_network_selected) %>%
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

    if (is.null(input$displayed_network_selected)) {
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
    if(input$displayed_network_selected == ""){
      wellPanel("Select a node for more details")
    } else {
    onClickInputCheck(
      never_Clicked = {
        wellPanel("Select a node for more details")
      },
      show_Details = {
        wellPanel(DT::dataTableOutput("selected_node_table"))
      },
      destructive_Change = wellPanel("Select a node for more details")
    )}
    
  })
  
})