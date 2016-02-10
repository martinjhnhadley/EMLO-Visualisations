## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================


### ============= Useful Visualisations Tools ========================= ###

## ggplot Color Function from http://stackoverflow.com/a/8197703/1659890
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### ====================================== Whole Network Page: UI ============================================
### =================================================================================================================

usefulCols_life_events <- c("Primary.Participant.Name","Secondary.Participant.Name","Event.or.Relationship.Type",
                            "Category","DateOne.Year", "DateOne.Month", "DateOne.Day","DateOne.Uncertainty","DateTwo.Year",                 
                            "DateTwo.Month", "DateTwo.Day","DateTwo.Uncertainty","Date.Type","Location.Details","Location.Region",
                            "Location.Country","Textual.Source.Source")

### ========= show/hide advanced options for the whole network

# shinyjs::onclick("toggleDateOptions",
#                  shinyjs::toggle(id = "dateOptions", anim = TRUE)) 

# shinyjs::onclick("toggleAdditionalOptions",
#                  shinyjs::toggle(id = "additionalOptions", anim = TRUE)) 

### ========= Exclude entries without a date

# output$include_interactions_without_dates_UI <- renderUI({
#   checkboxInput(inputId = "include_interactions_without_dates", label = "Include interactions without dates?", value = FALSE)
# })

### ========= Category/Type Selection UI Elements

all_event_types <- reactive(levels(multiparty.interactions$Event.or.Relationship.Type))
all_event_categories <- reactive(levels(multiparty.interactions$Category))

### ====================================== Generate Edges Function ============================================
### =================================================================================================================

network.edges.function <- function(selected.interactions){
  
  edges.of.network <- data.frame("Primary.Emlo_ID" = character(),
                                 "Secondary.Emlo_ID" = character(),
                                 "EcclesiasticalActivities" = character(),
                                 "EducationalActivities" = numeric(),
                                 "FamilyRelationships" = numeric(),
                                 "HierarchicalRelationships" = numeric(),
                                 "LearnedActivities" = numeric(),
                                 "MajorLifeEvents"= numeric(),
                                 "PeerRelationships" = numeric(),
                                 "PoliticalActivities" = numeric(),
                                 "ProfessionalActivities" = numeric(),
                                 "SocialStatusChange" = numeric()
  )
  # Find unique connections (relying on EMLO_ID)
  unique_Connections <- unique(selected.interactions[,c("Primary.Participant.Emlo_ID","Secondary.Participant.Emlo_ID")])
  # Split into a list for easier usage in lapply operations later!
  unique_Connections <- split(unique_Connections, seq(nrow(unique_Connections)))

  
  tally.connections <- function(partners) {
    paired.events <-
      selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == partners[[1]] &
                              selected.interactions$Secondary.Participant.Emlo_ID == partners[[2]],]
    tally.event.type <-
      data.frame(data = factor(
        as.vector(paired.events[,"Category"]),
        levels = levels(selected.interactions$Category)
      ))
    tally.event.type <- as.data.frame(table(tally.event.type))
    totalConnections <- sum(tally.event.type$Freq)
    # Use dcast to make df wide
    tally.event.type <-
      dcast(
        tally.event.type, formula = . ~ tally.event.type , value.var = "Freq", fill = NULL
      )
    # Drop the first column which contains useless data
    tally.event.type[1] <- NULL
    tally.event.type$Primary.Emlo_ID <- partners[[1]]
    tally.event.type$Secondary.Emlo_ID <- partners[[2]]
    tally.event.type$Total.Connections <- totalConnections
    tally.event.type
  }
  
  invisible(lapply(unique_Connections, function(x){
    new.edge <- tally.connections(x)
    edges.of.network <<- rbind(edges.of.network, new.edge)
  }))
  
  # Remove edges where the secondary emlo id is NA
  edges.of.network <- edges.of.network[!is.na(edges.of.network$Secondary.Emlo_ID),]
  
  # Return edges.of.network to the symbol whole.network_edges.of.network
  edges.of.network
}

### ====================================== Whole Network: visNetwork =====================================
### ======================================================================================================


output$visNetwork_wholeNetwork_show_timeslider_UI <- renderUI({
  checkboxInput("visNetwork_wholeNetwork_show_timeslider", label = "Remove undated interactions and filter by date?", value = TRUE)
})

output$visNetwork_wholeNetwork_time_period_of_interest_UI <- renderUI({
  
  if(is.null(input$visNetwork_wholeNetwork_show_timeslider)){
    return()
  }
  
  if (input$visNetwork_wholeNetwork_show_timeslider == TRUE) {
    dates <-
      c(multiparty.interactions$DateOne.Year,multiparty.interactions$DateTwo.Year)
    dates <- dates[!is.na(dates)]
    
    # Remove an incorrect date
    dates <- dates[dates > 1000]
    
    sliderInput(
      "visNetwork_wholeNetwork_time_period_of_interest", "Time period of interest:",
      min = min(dates) - 1,
      max = max(dates),
      step = 1,
      value = c(min(dates), max(dates))
    )
  }
})

output$visNetwork_wholeNetwork_HighlightedCategoryUI <- renderUI({
  selectInput(
    'visNetwork_wholeNetwork_highlightedCategory', 'Event/Relation Type to highlight', choices = all_event_categories(), selected = "FamilyRelationships",  
    multiple = FALSE
  )
})

output$visNetwork_wholeNetwork_ExcludedCategoriesUI <- renderUI({
  selectInput(
    'visNetwork_wholeNetwork_ExcludedCategory', 'Event/Relation Type to exclude', choices = c("None",setdiff(all_event_types(),
                                                                                                             input$visNetwork_wholeNetwork_highlightedCategory)),  
    multiple = FALSE
  )
})

output$visNetwork_wholeNetwork_NumberOfExcluded <- renderUI({
  
  selected.interactions <- multiparty.interactions
  #  Test suite 
  #   visNetwork_wholeNetwork_ExcludedCategory <- "PeerRelationships"
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory,]
  
  if(input$visNetwork_wholeNetwork_show_timeslider == TRUE){
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <- selected.interactions[{selected.interactions$DateOne.Year >= input$visNetwork_wholeNetwork_time_period_of_interest[1]} %in% TRUE & 
    {selected.interactions$DateOne.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2]} %in% TRUE ,]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <- selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2] |
                                                     is.na(selected.interactions$DateTwo.Year),]
    
  }
  
  multiparty.people <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  
  selected.people <- unique(c(selected.interactions$Primary.Participant.Emlo_ID, selected.interactions$Secondary.Participant.Emlo_ID))
  
  HTML(
    paste0(
      "<p>Included interactions: ",nrow(selected.interactions),"</p>",
      "<p>Included individuals: ",length(selected.people),"</p>",
      "<p>Excluded interactions: ",nrow(multiparty.interactions)-nrow(selected.interactions),"</p>",
      "<p>Excluded individuals: ",length(multiparty.people) - length(selected.people),"</p>"
    )
  )
  
  
})


visNetwork_wholeNetwork_nodes <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory,]
  
  if(input$visNetwork_wholeNetwork_show_timeslider == TRUE){
    ## Start experiment area
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <- selected.interactions[{selected.interactions$DateOne.Year >= input$visNetwork_wholeNetwork_time_period_of_interest[1]} %in% TRUE & 
    {selected.interactions$DateOne.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2]} %in% TRUE ,]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <- selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2] |
                                                     is.na(selected.interactions$DateTwo.Year),]
    
    ## End Experiment Area
  }
  
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  
  ### ==== Start Experiment
  
  test_nodes_in_people.df <- subset(people.df, iperson_id %in% nodes.of.network)$iperson_id
  test_nodes_not_in_people.df <- setdiff(nodes.of.network, test_nodes_in_people.df)
  
  ### ==== End Experiment
  
  ## Only include individuals in the people.df data set
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  visNetwork_nodes <- data.frame(
    "Person.Name" = nodes$Person.Name,
    "Surname" = nodes$Surname,
    "emlo_id" = nodes$iperson_id,
    "color" = rep("lightblue", nrow(nodes))
  )
  ## Return for use
  
  visNetwork_nodes
})

visNetwork_wholeNetwork_edges <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory,]
  
  if(input$visNetwork_wholeNetwork_show_timeslider == TRUE){
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <- selected.interactions[{selected.interactions$DateOne.Year >= input$visNetwork_wholeNetwork_time_period_of_interest[1]} %in% TRUE & 
    {selected.interactions$DateOne.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2]} %in% TRUE ,]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <- selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2] |
                                                     is.na(selected.interactions$DateTwo.Year),]
  }
  
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  
  ## Subset people.df by nodes in edges
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  visNetwork_edges <-
    data.frame(
      "source" = as.numeric(
        mapvalues(
          edges$Primary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes) - 1),warn_missing = FALSE
        )
      ),
      "target" = as.numeric(
        mapvalues(
          edges$Secondary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes)-1),warn_missing = FALSE
        )
      ),
      "source.emlo.id" = as.numeric(edges$Primary.Emlo_ID),
      "target.emlo.id" = as.numeric(edges$Secondary.Emlo_ID),
      ## Times the total number of connections by 10 and add 1 if of the highlighted category type
      ## Allows for testing off oddness for colour and size for the edge width
      "Value" = 20 * edges$Total.Connections + edges[,c(input$visNetwork_wholeNetwork_highlightedCategory)],
      "EdgeColor" = mapvalues(edges[,c(input$visNetwork_wholeNetwork_highlightedCategory)] > 0,c(TRUE,FALSE),c("#ff6666","lightblue"))
    )
  
  ## return for use
  
  visNetwork_edges
})

output$visNetwork_wholeNetwork_highlighted_node_UI <- renderUI({
  ## If not loaded yet, stop
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory)){
    return()
  }
  
  visNetwork_nodes <- visNetwork_wholeNetwork_nodes()
  
  labels.list <- as.character(visNetwork_nodes$Person.Name)
  values.list <- as.list(unlist(as.character(visNetwork_nodes$emlo_id)))
  
  names(values.list) <- labels.list
  
  selectInput("highlighted.node", label = "Highlight node",
              choices = values.list, selected = as.character(values.list[1]), multiple = FALSE)
  
  
  }
)

output$visNetwork_wholeNetwork <- renderVisNetwork({
  ## If not loaded yet, stop
  
  if (is.null(input$visNetwork_wholeNetwork_highlightedCategory))
    return()
  
  visNetwork_edges <- visNetwork_wholeNetwork_edges()
  visNetwork_nodes <- visNetwork_wholeNetwork_nodes()
  
  ## Create df for visNetwork
  visN_nodes <- data.frame("id" = visNetwork_nodes$emlo_id,
                           "title" = as.character(visNetwork_nodes$Person.Name),
                           "label" = as.character(visNetwork_nodes$Surname),
                           "color" = as.character(visNetwork_nodes$color)
                           )
  
  visN_nodes$color <- as.character(visN_nodes$color)
  
  visN_edges <- data.frame("from" = visNetwork_edges$source.emlo.id,
                           "to" = visNetwork_edges$target.emlo.id,
                           "color" = visNetwork_edges$EdgeColor,
                           "value" = rescale(visNetwork_edges$Value, to = c(2,10)))

  ## Drop duplicate node:
  visN_nodes <- visN_nodes[!duplicated(visN_nodes$id),]
  
  visN_nodes[visN_nodes$id == input$highlighted.node,]$color <- "red"
  
  print(visN_nodes$color)
  
  ## Make background colour vector
  node.background.color <- rep("lightblue",nrow(visN_nodes))
  ## Set highlighted.node to be red
  node.background.color[visN_nodes$id == input$highlighted.node] <- "red"
  
  ## Drop edges with nodes not in the node list
  non.conflicting.nodes <- intersect(unique(c(visN_edges$from, visN_edges$to)), visN_nodes$id)
  visN_edges <- subset(visN_edges, from %in% non.conflicting.nodes & to %in% non.conflicting.nodes)
  
  ## Make network
  visNetwork(visN_nodes, visN_edges) %>% 
    visNodes(color = list(border = "darkblue"), size = 10) %>%
    visIgraphLayout() %>%
    # visEdges(value = round(rescale(visNetwork_edges$Value, to = c(2,10)))) %>%
    # visEdges(width = 4) %>%
    visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = FALSE, dragNodes = FALSE, dragView = TRUE, zoomView = TRUE) %>%
    visOptions(highlightNearest = TRUE) %>% visLayout(hierarchical = FALSE) %>% 
    visInteraction(navigationButtons = TRUE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")

  })

output$visNetwork_wholeNetwork_selected_node_info <- renderUI({
  
  if(is.null(input$current_node_id)){
    return()
  }
  
  selected.person.name <- people.df[people.df$iperson_id == as.numeric(input$current_node_id$nodes[[1]]),"Person.Name"]
  selected.person.name <- selected.person.name[!is.na(selected.person.name)]
  
  wellPanel(HTML(
    paste0(
      "<h2>",selected.person.name,"'s Connections</h2>",
      "<p>The table below shows all life events involving the selected individual, 
      note the controller allows columns to be added and removed easily.</p>", sep=""
    )))
  
  
})

output$visNetwork_whole_network_connected_life_events_columns_to_show_UI <- renderUI({tagList(selectInput(
  'connected_life_events_Cols', 'Columns to show:',
  usefulCols_life_events, selected = c(
    "Primary.Participant.Name","Secondary.Participant.Name","Event.or.Relationship.Type","DateOne.Year"),
  multiple = TRUE
),tags$style(
  type = "text/css", "select#selCategories + .selectize-control{width: 800px}"
))})

output$visNetwork_whole_network_selected_node <- DT::renderDataTable({
  
  #   if(is.null(input$include_interactions_without_dates)){
  #     return()
  #   }
  
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$visNetwork_wholeNetwork_ExcludedCategory,]
  
  if(!is.null(input$visNetwork_wholeNetwork_show_timeslider)){

    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <- selected.interactions[{selected.interactions$DateOne.Year >= input$visNetwork_wholeNetwork_time_period_of_interest[1]} %in% TRUE & 
    {selected.interactions$DateOne.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2]} %in% TRUE ,]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <- selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_wholeNetwork_time_period_of_interest[2] |
                                                     is.na(selected.interactions$DateTwo.Year),]
  }
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  ## Get selected individual from click
  nodes <- visNetwork_wholeNetwork_nodes()
  selectedIndividual <- as.numeric(input$current_node_id$nodes[[1]])
  
  # Get edges of network
  edges <- visNetwork_wholeNetwork_edges()
  
  
  connectedIndividuals <- c(as.character(edges[edges$source.emlo.id == selectedIndividual, "target.emlo.id"]),
                            as.character(edges[edges$target.emlo.id == selectedIndividual, "source.emlo.id"]))
  
  # Create an empty data.frame with life.event.columns
  connected_life_events <- selected.interactions[0,]
  # Function to extract connected events
  get.connected.life.events <- function(selectedNode, connectedNode){
    connections <- rbind(
      selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == selectedNode & 
                              selected.interactions$Secondary.Participant.Emlo_ID == connectedNode,],
      selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == connectedNode & 
                              selected.interactions$Secondary.Participant.Emlo_ID == selectedNode,]
    )
    connected_life_events <<- rbind(connected_life_events, connections)
  }
  # lapply function
  invisible(lapply(connectedIndividuals, function(x)get.connected.life.events(selectedIndividual, x)))
  
  
  # Drop empty rows:
  connected_life_events <- connected_life_events[!!rowSums(!is.na(connected_life_events)),]
  # Return
  connected_life_events[,input$connected_life_events_Cols, drop = FALSE]
  
})

### ====================================== Selected Two Individuals Prosopography ============================================
### =================================================================================================================

output$visNetwork_selected_individual_show_timeslider_UI <- renderUI({
  checkboxInput("visNetwork_selected_individual_show_timeslider", 
                label = "Remove undated interactions and filter by date?", value = TRUE)
})


output$visNetwork_selected_individual_time_period_of_interest_UI <- renderUI({
  
  if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
    return()
  }
  
  if (!input$visNetwork_selected_individual_show_timeslider) {
    return()
  }
  
  dates <-
    c(multiparty.interactions$DateOne.Year,multiparty.interactions$DateTwo.Year)
  dates <- dates[!is.na(dates)]
  
  # Remove an incorrect date
  dates <- dates[dates > 1000]
  
  sliderInput(
    "visNetwork_selected_individual_time_period_of_interest", "Time period of interest:",
    min = min(dates) - 1,
    max = max(dates),
    step = 1,
    value = c(min(dates), max(dates))
  )
})

output$neighbor.degree.UI <- renderUI({
  sliderInput("neighbor.degree", label = "Neighbor Degree", min = 1, max = 3, value = 1, step = 1)
})

output$select.individual.1_UI <- renderUI({
  
  if (is.null(input$visNetwork_selected_individual_show_timeslider)) {
    return()
  }
  
  ## Only include people who are in the multiparty events!
  people.with.connections <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,
                                      multiparty.interactions$Secondary.Participant.Emlo_ID))
  
  people.with.connections <- subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if(!is.null(input$visNetwork_selected_individual_show_timeslider)){
    events.with.dates <- multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year),]
    people.with.dates <- unique(c(events.with.dates$Primary.Participant.Emlo_ID, events.with.dates$Secondary.Participant.Emlo_ID))
    
    people.with.connections <- subset(people.with.connections, iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <- as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  selectInput("select.individual.1", label = "Select individual 1 for prosopography analysis",
              choices = values.list, selected = as.character(values.list[1]), multiple = FALSE)
  })

## Select second individual
output$select.individual.2_UI <- renderUI({
  
  if (is.null(input$select.individual.1)){
    return()
  }
  
  people.with.connections <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  people.with.connections <- subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if(!is.null(input$visNetwork_selected_individual_show_timeslider)){
    events.with.dates <- multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year),]
    people.with.dates <- unique(c(events.with.dates$Primary.Participant.Emlo_ID, events.with.dates$Secondary.Participant.Emlo_ID))
    
    people.with.connections <- subset(people.with.connections, iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <- as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  values.list <- values.list[!as.character(values.list) %in% input$select.individual.1]
  
  selectInput("select.individual.2", label = "Select individual 2 for prosopography analysis",
              choices = values.list, selected = as.character(values.list[2]), multiple = FALSE)})

## Select third individual
output$select.individual.3_UI <- renderUI({
  
  if (is.null(input$select.individual.2)){
    return()
  }
  
  people.with.connections <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  people.with.connections <- subset(people.df, iperson_id %in% people.with.connections)
  
  ## IF timeline enabled, filter out individuals who are do not appear in life events with DateOne.Year values
  if(!is.null(input$visNetwork_selected_individual_show_timeslider)){
    events.with.dates <- multiparty.interactions[!is.na(multiparty.interactions$DateOne.Year),]
    people.with.dates <- unique(c(events.with.dates$Primary.Participant.Emlo_ID, events.with.dates$Secondary.Participant.Emlo_ID))
    
    people.with.connections <- subset(people.with.connections, iperson_id %in% people.with.dates)
    
  }
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <- as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  values.list <- values.list[!as.character(values.list) %in% input$select.individual.2]
  
  selectInput("select.individual.3", label = "Select individual 3 for prosopography analysis",
              choices = values.list, selected = as.character(values.list[3]), multiple = FALSE)})

select.individual.edges <- reactive({

  if (is.null(input$select.individual.2)){
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
  if(!is.null(input$visNetwork_selected_individual_show_timeslider)){
    
    ## Filter out rows where DateOne.Year is NA or outside of date range
    selected.interactions <- selected.interactions[{selected.interactions$DateOne.Year >= input$visNetwork_selected_individual_time_period_of_interest[1]} %in% TRUE & 
    {selected.interactions$DateOne.Year <= input$visNetwork_selected_individual_time_period_of_interest[2]} %in% TRUE ,]
    ## Filter out rows where DateTwo.Year is greater than the max date allowd
    selected.interactions <- selected.interactions[selected.interactions$DateTwo.Year <= input$visNetwork_selected_individual_time_period_of_interest[2] |
                                                     is.na(selected.interactions$DateTwo.Year),]
  }
  
  ## Generate edges
  edges <- network.edges.function(selected.interactions)
  ## Get just edges for igraph
  edges <- edges[,c("Primary.Emlo_ID","Secondary.Emlo_ID")]
  ## Extract nodes
  nodes <- unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  ## Generate igraph
  igraph.for.computation <- graph.data.frame(edges, nodes, directed = FALSE)
  ## plot igraph
  # plot(igraph.for.computation, vertex.size=2, vertex.label=NA, edge.arrow.size=.2)
  
  ## Set graph.union.fail to FALSE
  graph.union.fail <- FALSE
  
  ## Use tryCatch for errors when no connected edges found!
  tryCatch(neighboring_nodes <- 
             ## Find neihbouring nodes, graph.union is needed as make_ego_graph outputs a list of graph
             graph.union(make_ego_graph(igraph.for.computation, 
                                                           order = input$neighbor.degree, 
                                                           nodes = c(selected.individual.1, 
                                                                     selected.individual.2,
                                                                     selected.individual.3))),
           error = function(e){
             graph.union.fail <<- TRUE
           })
  print(graph.union.fail)
  if(graph.union.fail){
    visN_edges <- "graph.union.fail"
    return()
  }
  
  # plot igraph of neighboring vertices
  # plot(neighboring_nodes,vertex.size=2, vertex.label=V(neighboring_nodes)$name, edge.arrow.size=.2)
  
  ## Subset multiparty.interactions by the node names:
  selected.interactions <- subset(multiparty.interactions, Primary.Participant.Emlo_ID %in% V(neighboring_nodes)$name &
                                    Secondary.Participant.Emlo_ID %in% V(neighboring_nodes)$name)
  
  ## Generates edges for visNetwork
  visN_edges <- network.edges.function(selected.interactions)
  
  ## Return dataframe
  visN_edges
})

## show warning if no edges to display
output$selected.individual.network_no_graph <- renderUI({
  ## If not loaded yet, stop
  if (is.null(input$select.individual.3)){
    return()
  }
  
  visN_edges <- select.individual.edges()
  ## If graph.union.fail then the visN_edges is null
  if(is.null(visN_edges)){
    wellPanel(
      "There are
      no known connections between the individuals selected, subject to the current filter conditions."
    )
  }
}
)

## Show connections between selected individuals
output$select.individual.network_graph <- renderVisNetwork({
  
  ## If not loaded yet, stop
  if (is.null(input$select.individual.3)){
    return()
  }
  
  ## load visN_edges
  visN_edges <- select.individual.edges()

  ## If graph.union.fail then the visN_edges is null
  if(is.null(visN_edges)){
    return()
  }
  
  visN_edges <- data.frame("from" = visN_edges$Primary.Emlo_ID,
                           "to" = visN_edges$Secondary.Emlo_ID,stringsAsFactors = FALSE)

  
  ## Subset people.df by the nodes appearing in the edges:
  visN_nodes <- subset(people.df, iperson_id %in% unique(c(visN_edges$from,visN_edges$to)))
  
  ## Pull out data for visNetwork
  visN_nodes <- data.frame("id" = visN_nodes$iperson_id,
                           "title" = visN_nodes$Person.Name,
                           "label" = visN_nodes$Surname)
  
  ## Highlight the selected nodes as red
  node_colors <- rep("lightblue",nrow(visN_nodes))
  node_colors[match(c(input$select.individual.1,input$select.individual.2,input$select.individual.3), visN_nodes$id)] <- "red"
  
  visN_nodes$color <- node_colors
  
  ## Remove duplicated nodes
  visN_nodes <- visN_nodes[!duplicated(visN_nodes$id),]
  
  ## Drop edges with nodes not in the node list
  non.conflicting.nodes <- intersect(unique(c(visN_edges$from, visN_edges$to)), visN_nodes$id)
  visN_edges <- subset(visN_edges, from %in% non.conflicting.nodes & to %in% non.conflicting.nodes)
  
  ## Visualise
  
  visNetwork(visN_nodes, visN_edges) %>% 
    visEdges(selectable = FALSE) %>%
    visNodes(color = list(background = "lightblue", border = "darkblue"),size = 10) %>% 
    visIgraphLayout() %>%
    visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = TRUE) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = FALSE) %>% visInteraction(navigationButtons = TRUE) %>%
    visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
  
})


