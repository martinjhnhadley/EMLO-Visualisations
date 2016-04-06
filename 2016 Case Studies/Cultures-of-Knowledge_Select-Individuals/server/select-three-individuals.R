## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================================================================================
## ===================== Select Three Individuals Network =========================


### ====================================== UI Elements =====================================
### ========================================================================================

output$visNetwork_selected_individual_show_timeslider_UI <-
  renderUI({
    checkboxInput(
      "visNetwork_selected_individual_show_timeslider",
      label = "Remove undated interactions and filter by date?",
      value = TRUE
    )
  })


output$visNetwork_selected_individual_time_period_of_interest_UI <-
  renderUI({
    if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
      return()
    }
    if (!input$visNetwork_selected_individual_show_timeslider) {
      return()
    }
    
    dates <-
      c(multiparty.interactions$DateOne.Year,
        multiparty.interactions$DateTwo.Year)
    dates <- dates[!is.na(dates)]
    
    # Remove an incorrect date
    dates <- dates[dates > 1000]
    
    sliderInput(
      "visNetwork_selected_individual_time_period_of_interest",
      "Time period of interest:",
      min = min(dates) - 1,
      max = max(dates),
      step = 1,
      value = c(min(dates), max(dates))
    )
  })

output$neighbor.degree.UI <- renderUI({
  sliderInput(
    "neighbor.degree",
    label = "Neighbor Degree",
    min = 1,
    max = 3,
    value = 1,
    step = 1
  )
})

output$select.individual.1_UI <- renderUI({
  if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
    return()
  }
  
  ## Only include people who are in the multiparty events!
  people.with.connections <-
    unique(
      c(
        multiparty.interactions$Primary.Participant.Emlo_ID,
        multiparty.interactions$Secondary.Participant.Emlo_ID
      )
    )
  
  people.with.connections <-
    subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if (!is.null(input$visNetwork_selected_individual_show_timeslider)) {
    events.with.dates <-
      multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year), ]
    people.with.dates <-
      unique(
        c(
          events.with.dates$Primary.Participant.Emlo_ID,
          events.with.dates$Secondary.Participant.Emlo_ID
        )
      )
    
    people.with.connections <-
      subset(people.with.connections,
             iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <-
    as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  selectInput(
    "select.individual.1",
    label = "Select individual 1 for prosopography analysis",
    choices = values.list,
    selected = as.character(values.list[1]),
    multiple = FALSE
  )
})

## Select second individual
output$select.individual.2_UI <- renderUI({
  if (is.null(input$select.individual.1)) {
    return()
  }
  
  people.with.connections <-
    unique(
      c(
        multiparty.interactions$Primary.Participant.Emlo_ID,
        multiparty.interactions$Secondary.Participant.Emlo_ID
      )
    )
  people.with.connections <-
    subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if (!is.null(input$visNetwork_selected_individual_show_timeslider)) {
    events.with.dates <-
      multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year), ]
    people.with.dates <-
      unique(
        c(
          events.with.dates$Primary.Participant.Emlo_ID,
          events.with.dates$Secondary.Participant.Emlo_ID
        )
      )
    
    people.with.connections <-
      subset(people.with.connections,
             iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <-
    as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  values.list <-
    values.list[!as.character(values.list) %in% input$select.individual.1]
  
  selectInput(
    "select.individual.2",
    label = "Select individual 2 for prosopography analysis",
    choices = values.list,
    selected = as.character(values.list[2]),
    multiple = FALSE
  )
})

## Select third individual
output$select.individual.3_UI <- renderUI({
  if (is.null(input$select.individual.2)) {
    return()
  }
  
  people.with.connections <-
    unique(
      c(
        multiparty.interactions$Primary.Participant.Emlo_ID,
        multiparty.interactions$Secondary.Participant.Emlo_ID
      )
    )
  people.with.connections <-
    subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if (!is.null(input$visNetwork_selected_individual_show_timeslider)) {
    events.with.dates <-
      multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year), ]
    people.with.dates <-
      unique(
        c(
          events.with.dates$Primary.Participant.Emlo_ID,
          events.with.dates$Secondary.Participant.Emlo_ID
        )
      )
    
    people.with.connections <-
      subset(people.with.connections,
             iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <-
    as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  values.list <-
    values.list[!as.character(values.list) %in% input$select.individual.2]
  
  selectInput(
    "select.individual.3",
    label = "Select individual 3 for prosopography analysis",
    choices = values.list,
    selected = as.character(values.list[3]),
    multiple = FALSE
  )
})

### ====================================== Find Connected Subgrapgh ========================
### ========================================================================================

select.individual.edges <- reactive({
  if (is.null(input$select.individual.2)) {
    return()
  }
  #
  #   ## Test suite
  #   selected.individual.1 <- "908078"
  #   selected.individual.2 <- "300007"
  
  selected.individual.1 <- input$select.individual.1
  selected.individual.2 <- input$select.individual.2
  selected.individual.3 <- input$select.individual.3
  
  selected.interactions <- multiparty.interactions
  
  ## If timeline enabled, filter out individuals
  if (!is.null(input$visNetwork_selected_individual_show_timeslider)) {
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <-
      selected.interactions[{
        selected.interactions$DateOne.Year >= input$visNetwork_selected_individual_time_period_of_interest[1]
      } %in% TRUE &
      {
        selected.interactions$DateOne.Year <= input$visNetwork_selected_individual_time_period_of_interest[2]
      } %in% TRUE , ]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <-
      selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_selected_individual_time_period_of_interest[2] |
                              is.na(selected.interactions$DateTwo.Year), ]
  }
  
  ## Generate edges
  edges <- network.edges.function(selected.interactions)
  ## Get just edges for igraph
  edges <- edges[, c("Primary.Emlo_ID", "Secondary.Emlo_ID")]
  ## Extract nodes
  nodes <- unique(c(edges$Primary.Emlo_ID, edges$Secondary.Emlo_ID))
  ## Generate igraph
  igraph.for.computation <-
    graph.data.frame(edges, nodes, directed = FALSE)
  ## plot igraph
  # plot(igraph.for.computation, vertex.size=2, vertex.label=NA, edge.arrow.size=.2)
  
  ## Set graph.union.fail to FALSE
  graph.union.fail <- FALSE
  
  ## Use tryCatch for errors when no connected edges found!
  tryCatch(
    neighboring_nodes <-
      ## Find neihbouring nodes, graph.union is needed as make_ego_graph outputs a list of graph
      graph.union(
        make_ego_graph(
          igraph.for.computation,
          order = input$neighbor.degree,
          nodes = c(
            selected.individual.1,
            selected.individual.2,
            selected.individual.3
          )
        )
      ),
    error = function(e) {
      graph.union.fail <<- TRUE
    }
  )
  print(graph.union.fail)
  if (graph.union.fail) {
    visN_edges <- "graph.union.fail"
    return()
  }
  
  # plot igraph of neighboring vertices
  # plot(neighboring_nodes,vertex.size=2, vertex.label=V(neighboring_nodes)$name, edge.arrow.size=.2)
  
  ## Subset multiparty.interactions by the node names:
  selected.interactions <-
    subset(
      multiparty.interactions,
      Primary.Participant.Emlo_ID %in% V(neighboring_nodes)$name &
        Secondary.Participant.Emlo_ID %in% V(neighboring_nodes)$name
    )
  
  ## Generates edges for visNetwork
  visN_edges <- network.edges.function(selected.interactions)
  
  ## Return dataframe
  visN_edges
})

### ====================================== Generate visNetwork ========================
### ========================================================================================

## show warning if no edges to display
output$selected.individual.network_no_graph <- renderUI({
  ## If not loaded yet, stop
  if (is.null(input$select.individual.3)) {
    return()
  }
  
  visN_edges <- select.individual.edges()
  ## If graph.union.fail then the visN_edges is null
  if (is.null(visN_edges)) {
    wellPanel(
      "There are no known connections between the individuals selected, subject to the current filter conditions."
    )
  }
})

## Show connections between selected individuals
output$select.individual.network_graph <- renderVisNetwork({
  ## If not loaded yet, stop
  if (is.null(input$select.individual.3)) {
    return()
  }
  
  ## load visN_edges
  visN_edges <- select.individual.edges()
  
  ## If graph.union.fail then the visN_edges is null
  if (is.null(visN_edges)) {
    return()
  }
  
  visN_edges <- data.frame(
    "from" = visN_edges$Primary.Emlo_ID,
    "to" = visN_edges$Secondary.Emlo_ID,
    stringsAsFactors = FALSE
  )
  
  ## Subset people.df by the nodes appearing in the edges:
  visN_nodes <-
    subset(people.df, iperson_id %in% unique(c(visN_edges$from, visN_edges$to)))
  
  ## Pull out data for visNetwork
  visN_nodes <- data.frame(
    "id" = visN_nodes$iperson_id,
    "title" = visN_nodes$Person.Name,
    "label" = visN_nodes$Surname
  )
  
  ## Highlight the selected nodes as red
  node_colors <- rep("lightblue", nrow(visN_nodes))
  node_colors[match(
    c(
      input$select.individual.1,
      input$select.individual.2,
      input$select.individual.3
    ),
    visN_nodes$id
  )] <- "red"
  
  visN_nodes$color <- node_colors
  ## Remove duplicated nodes
  visN_nodes <- visN_nodes[!duplicated(visN_nodes$id), ]
  
  ## Drop edges with nodes not in the node list
  non.conflicting.nodes <-
    intersect(unique(c(visN_edges$from, visN_edges$to)), visN_nodes$id)
  visN_edges <-
    subset(visN_edges,
           from %in% non.conflicting.nodes &
             to %in% non.conflicting.nodes)
  
  ## Visualise
  visNetwork(visN_nodes, visN_edges) %>%
    visNodes(color = list(background = "lightblue", border = "darkblue"),
             size = 10) %>%
    visIgraphLayout() %>%
    visInteraction(
      tooltipDelay = 0.2,
      hideEdgesOnDrag = TRUE,
      dragNodes = FALSE,
      dragView = FALSE,
      zoomView = TRUE
    ) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = FALSE) %>% visInteraction(navigationButtons = TRUE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  })
