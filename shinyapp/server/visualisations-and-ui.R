### ============= Useful Visualisations Tools ========================= ###

## ggplot Color Function from http://stackoverflow.com/a/8197703/1659890
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### ====================================== Whole Network Page: UI ============================================
### =================================================================================================================

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

network.edges.function.new <- function(selected.interactions){
  
  edges.of.network <- data.frame("Primary.Emlo_ID" = character(),
                                 "Secondary.Emlo_ID" = character()
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
        as.vector(paired.events[,"Event.or.Relationship.Type"]),
        levels = levels(selected.interactions$Event.or.Relationship.Type)
      ))
    tally.event.type <- as.data.frame(table(tally.event.type))
    totalConnections <- sum(tally.event.type$Freq)
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

### ====================================== networkD3 - Whole Network ============================================
### =================================================================================================================


output$networkD3_wholeNetwork_HighlightedCategoryUI <- renderUI({
  selectInput(
    'networkD3_wholeNetwork_highlightedCategory', 'Event/Relation Type to highlight', choices = all_event_categories(), selected = all_event_categories(),  
    multiple = FALSE
  )
})

output$networkD3_wholeNetwork_ExcludedCategoriesUI <- renderUI({
  selectInput(
    'networkD3_wholeNetwork_ExcludedCategory', 'Event/Relation Type to exclude', choices = c("None",setdiff(all_event_types(),
                                                                                                            input$networkD3_wholeNetwork_highlightedCategory)),  
    multiple = FALSE
  )
})

output$networkD3_wholeNetwork_NumberOfExcluded <- renderUI({
  
  selected.interactions <- multiparty.interactions
#  Test suite 
#   networkD3_wholeNetwork_ExcludedCategory <- "PeerRelationships"
  
  selected.interactions <- selected.interactions[selected.interactions$Event.or.Relationship.Type != input$networkD3_wholeNetwork_ExcludedCategory,]
  
  multiparty.people <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  
  selected.people <- unique(c(selected.interactions$Primary.Participant.Emlo_ID, selected.interactions$Secondary.Participant.Emlo_ID))
  
  
  HTML(
    paste0(
      "<p>Excluded interactions: ",nrow(multiparty.interactions)-nrow(selected.interactions),"</p>",
      "<p>Excluded individuals: ",length(multiparty.people) - length(selected.people),"</p>"
    )
  )
  
  
})

### ==== WORKING AREA =====

networkD3_wholeNetwork_nodes <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$networkD3_wholeNetwork_ExcludedCategory,]
  
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  
  ## Subset people.df by nodes in edges
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  networkD3_nodes <- data.frame(
    "name" = 0:(nrow(nodes) - 1),
    "group" = rep(1,nrow(nodes)),
    "size" = rep(1,nrow(nodes)),
    "surname" = nodes$Surname,
    "emlo_id" = nodes$iperson_id
  )
  ## Return for use
  
  networkD3_nodes
})

networkD3_wholeNetwork_edges <- reactive({
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <-
    selected.interactions[selected.interactions$Event.or.Relationship.Type != input$networkD3_wholeNetwork_ExcludedCategory,]
  
  
  ## Apply network.edges.function to selected.interactions
  edges <- network.edges.function(selected.interactions)
  ## Get nodes from edges
  nodes.of.network <-
    unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  
  ## Subset people.df by nodes in edges
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  networkD3_edges <-
    data.frame(
      "source" = as.numeric(
        mapvalues(
          edges$Primary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes) - 1),warn_missing = FALSE
        )
      ),
      "target" = as.numeric(
        mapvalues(
          edges$Secondary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes) -
                                                                      1),warn_missing = FALSE
        )
      ),
      
      
      ## Times the total number of connections by 10 and add 1 if of the highlighted category type
      ## Allows for testing off oddness for colour and size for the edge width
      "value" = 20 * edges$Total.Connections + edges[,c(input$networkD3_wholeNetwork_highlightedCategory)],
      "LinkColor" = c(rep("lightblue",100),rep("red",nrow(edges) -
                                                 100))
    )
  ## return for use
  
  networkD3_edges
})

output$networkD3_wholeNetwork <- renderForceNetwork({
  ## If not loaded yet, stop
  
  if (is.null(input$networkD3_wholeNetwork_highlightedCategory))
    return()
  
    networkD3_wholeNetwork_click_script <- "d3.select(this).select('circle').transition().duration(750).attr('r', 30);
      Shiny.onInputChange('current_node_id', d.index + 1)"
  
  
  ## Script gets row:
#   networkD3_wholeNetwork_click_script <- 'alert("You clicked " + d.name + " which is in row " +
#         (d.index + 1) +  " of your original R data frame");'
  
  networkD3_edges <- networkD3_wholeNetwork_edges()
  
  networkD3_nodes <- networkD3_wholeNetwork_nodes()
  
  forceNetwork(
    Links = networkD3_edges, Nodes = networkD3_nodes, Source = "source",
    Target = "target", Value = "value", NodeID = "surname", zoom = TRUE,
    Group = "group",
    fontSize = 10,
    # linkColour = rep("#bf3eff",nrow(networkD3_edges)),
    # linkColour = linkColors.by.highlight,
    linkColour = JS('function(l) { return l.value % 2 == 0 ? "#ff6666" : "lightblue" }'),
    opacityNoHover = TRUE,
    clickAction = networkD3_wholeNetwork_click_script
  )
})

output$network3D_wholeNetwork_selected_individual_name <- renderText({
  
  nodes <- networkD3_wholeNetwork_nodes()
  
  selected.emlo.id <- nodes[input$current_node_id,"emlo_id"]
  
  people.df[people.df$iperson_id == selected.emlo.id,"Person.Name"]
  
})

### ==== WORKING AREA END ======


# ## ==== OLD: Working
#
# output$networkD3_wholeNetwork <- renderForceNetwork({
#   
#   ## If not loaded yet, stop
#   
#   if (is.null(input$networkD3_wholeNetwork_highlightedCategory))
#     return()
#   
#   ## Set selected.interactions as all multiparty.interactions
#   selected.interactions <- multiparty.interactions
#   
#   ## Drop excluded categoties from multiparty interactions
#   selected.interactions <- selected.interactions[selected.interactions$Event.or.Relationship.Type != input$networkD3_wholeNetwork_ExcludedCategory,]
#   
#   
#   ## Apply network.edges.function to selected.interactions
#   edges <- network.edges.function(selected.interactions)
#   ## Get nodes from edges
#   nodes.of.network <- unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
# 
#   ## Subset people.df by nodes in edges
#   nodes <- subset(people.df, iperson_id %in% nodes.of.network)
# 
#   ## Click script
#   
#   MyClickScript <- 
#     "      d3.select(this).select('circle').transition().duration(750).attr('r', 30);
#     Shiny.onInputChange('current_node_id', nodes)"
#   
#   ## Provide facility to highlight edges
#   
#   networkD3_wholeNetwork_highlightedCategory <- "FamilyRelationship"
#   
#   ## Create df for networkD3
#   
#   ## Map node ids to numbers, recalling nodes MUST go from 0 to nrow(data)-1
#   networkD3_nodes <- data.frame("name" = 0:(nrow(nodes)-1),
#                                 "group" = rep(1,nrow(nodes)),
#                                 "size" = rep(1,nrow(nodes)),
#                                 "surname" = nodes$Surname)
#   
#   ## Create vector encoding colours for selected event type:
#   
#   networkD3_wholeNetwork_highlightedCategory <- "FamilyRelationship"
#   
#   networkD3_edges <- data.frame("source" = as.numeric(mapvalues(edges$Primary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes)-1),warn_missing = FALSE)),
#                                 "target" = as.numeric(mapvalues(edges$Secondary.Emlo_ID, from = nodes$iperson_id, to = 0:(nrow(nodes)-1),warn_missing = FALSE)),
# 
#                                 
#                                 ## Times the total number of connections by 10 and add 1 if of the highlighted category type
#                                 ## Allows for testing off oddness for colour and size for the edge width
#                                 "value" = 10*edges$Total.Connections+edges[,c(input$networkD3_wholeNetwork_highlightedCategory)],
#                                 "LinkColor" = c(rep("lightblue",100),rep("red",nrow(edges)-100)))
#   
#   forceNetwork(Links = networkD3_edges, Nodes = networkD3_nodes, Source = "source",
#                Target = "target", Value = "value", NodeID = "surname", zoom = TRUE,
#                Group = "group",
#                fontSize = 10,
#                # linkColour = rep("#bf3eff",nrow(networkD3_edges)),
#                # linkColour = linkColors.by.highlight,
#                linkColour = JS('function(l) { return l.value % 2 == 0 ? "#ff6666" : "lightblue" }'),
#                opacityNoHover = FALSE)
#   })


### ====================================== Whole Network Page: visNetwork ============================================
### =================================================================================================================

output$event_category_selection_UI <- renderUI({
  selectInput(
    'selected_event_categories', 'Event Categories to include', all_event_categories(), selected = all_event_categories(),  
    multiple = TRUE
  )
})

output$event_type_selection_UI <- renderUI({
  selectInput(
    'selected_event_types', 'Event/Relation Types to include', all_event_types(), selected = all_event_types(),  
    multiple = TRUE
  )
})



whole.network_edges <- reactive({
  
  #   if(is.null(input$include_interactions_without_dates)){
  #     return()
  #   }
  
  ## Set selected.interactions as all multiparty.interactions
  selected.interactions <- multiparty.interactions
  
  #   ## Drop interactions according to date options
  #   selected.interactions <- if(input$include_interactions_without_dates){
  #     selected.interactions
  #   } else {
  #     selected.interactions[!is.na(selected.interactions$DateOne.Year) & !is.na(selected.interactions$DateTwo.Year),]
  #   }
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <- subset(
    selected.interactions,Category %in% input$selected_event_categories &
      Event.or.Relationship.Type %in% input$selected_event_types
  )
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  edges <- network.edges.function(selected.interactions)
  
  edges
})


output$whole.network_visNetwork <- renderVisNetwork({
  
  ## If not loaded yet, stop
  
  if (is.null(input$hierachical_layout_option))
    return()
  
  ## Get edges from whole.network.edges()
  edges <- whole.network_edges()
  ## Get nodes from edges
  nodes.of.network <- unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  ## Subset people.df by nodes in edges
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  
  ## Make igraph
  edges_igraph <- edges[,c("Primary.Emlo_ID","Secondary.Emlo_ID")]
  nodes_igraph <- nodes$iperson_id
  ## generate igraph
  whole.network_igraph <- graph.data.frame(edges_igraph, nodes_igraph, directed = FALSE)
  ## plot igraph
  plot(whole.network_igraph,vertex.size=2, edge.arrow.size=.2)
  ecount(whole.network_igraph)
  
  
  ## Create df for visNetwork
  visN_nodes <- data.frame("id" = nodes$iperson_id,
                           "title" = nodes$Person.Name)
  
  visN_edges <- data.frame("from" = edges$Primary.Emlo_ID,
                           "to" = edges$Secondary.Emlo_ID)
  
  visNetwork(visN_nodes, visN_edges) %>% visNodes(
    color = list(
      background = "lightblue", border = "darkblue"
    ),
    size = 10
  ) %>%
    visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = input$hierachical_layout_option) %>%
    visEvents(selectNode = "function(nodes) {
          Shiny.onInputChange('current_node_id', nodes);
              ;}")
})

### ========= UI for controlling what data is shown for individuals connected to the selected node

usefulCols_life_events <- c("Primary.Participant.Name","Secondary.Participant.Name","Event.or.Relationship.Type",
                            "Category","DateOne.Year", "DateOne.Month", "DateOne.Day","DateOne.Uncertainty","DateTwo.Year",                 
                            "DateTwo.Month", "DateTwo.Day","DateTwo.Uncertainty","Date.Type","Location.Details","Location.Region",
                            "Location.Country","Textual.Source.Source")

output$connected_life_events_columns_to_show_UI <- renderUI(tagList(selectInput(
  'connected_life_events_Cols', 'Columns to show:',
  usefulCols_life_events, selected = c(
    "Primary.Participant.Name","Secondary.Participant.Name","Event.or.Relationship.Type"),
  multiple = TRUE
),tags$style(
  type = "text/css", "select#selCategories + .selectize-control{width: 800px}"
)))

### ======== Display selected.interactions for the selected node in the network

output$selected.individual.name <- renderText({
  selectedIndividual <- input$current_node_id$nodes[[1]]
  selectedIndividual <- people.df[people.df$iperson_id == selectedIndividual,]
  as.character(selectedIndividual$Person.Name)
})

output$selected_node <- DT::renderDataTable({
  
#   if(is.null(input$include_interactions_without_dates)){
#     return()
#   }
  
  ## Set selected.interactions as all multiparty.interactions
  
  selected.interactions <- multiparty.interactions
  
#   ## Drop interactions according to date options
#   
#   selected.interactions <- if(input$include_interactions_without_dates){
#     selected.interactions
#   } else {
#     selected.interactions[!is.na(selected.interactions$DateOne.Year) & !is.na(selected.interactions$DateTwo.Year),]
#   }
  
  ## Drop excluded categoties from multiparty interactions
  selected.interactions <- subset(
    selected.interactions,Category %in% input$selected_event_categories &
      Event.or.Relationship.Type %in% input$selected_event_types
  )
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  selectedIndividual <- input$current_node_id$nodes[[1]]
  
  # selectedIndividual <- 21584
  
  edges <- whole.network_edges()
  
  connectedIndividuals <- c(edges[edges$Primary.Emlo_ID == selectedIndividual, "Secondary.Emlo_ID"],
                            edges[edges$Secondary.Emlo_ID == selectedIndividual, "Primary.Emlo_ID"])

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

  connected_life_events[,input$connected_life_events_Cols, drop = FALSE]
  
  })

### ====================================== Selected Two Individuals Prosopography ============================================
### =================================================================================================================


output$select.individual.1_UI <- renderUI({
  
  ## Only include people who are in the multiparty events!
  
  people.with.connections <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  
  people.with.connections <- subset(people.df, iperson_id %in% people.with.connections)
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <- as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  selectInput("select.individual.1", label = "Select individual for prosopography analysis",
              choices = values.list, selected = "908078", multiple = FALSE)
  })

## Select second individual
output$select.individual.2_UI <- renderUI({
  
  people.with.connections <- unique(c(multiparty.interactions$Primary.Participant.Emlo_ID,multiparty.interactions$Secondary.Participant.Emlo_ID))
  
  people.with.connections <- subset(people.df, iperson_id %in% people.with.connections)
  
  labels.list <- as.character(people.with.connections$Person.Name)
  values.list <- as.list(unlist(as.character(people.with.connections$iperson_id)))
  
  names(values.list) <- labels.list
  
  values.list <- values.list[!labels.list == input$select.individual.1]
  
  selectInput("select.individual.2", label = "Select individual for prosopography analysis",
              choices = values.list, selected = "300007", multiple = FALSE)})

output$neighbor.degree.UI <- renderUI({
  sliderInput("neighbor.degree", label = "Neighbor Degree", min = 1, max = 5, value = 2, step = 1)
})


select.individual.edges <- reactive({

  if (is.null(input$select.individual.2))
    return()
  
#   ## test suite
  selected.individual.1 <- "908078"
# #    # selected.individual.1 <- "900418"
  selected.individual.2 <- "300007"
   
  selected.individual.1 <- input$select.individual.1
  selected.individual.2 <- input$select.individual.2
  
  ## Generate edges
  edges <- network.edges.function(multiparty.interactions)
  ## Get just edges for igraph
  edges <- edges[,c("Primary.Emlo_ID","Secondary.Emlo_ID")]
  ## Extract nodes
  nodes <- unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  ## Generate igraph
  igraph.for.computation <- graph.data.frame(edges, nodes, directed = FALSE)
  ## plot igraph
  # plot(igraph.for.computation, vertex.size=2, vertex.label=NA, edge.arrow.size=.2)
  
  ## test suite
  neighboring_nodes <- graph.union(make_ego_graph(igraph.for.computation, order = 1, nodes = c(selected.individual.1, selected.individual.2)))
  
  ## Find neihbouring nodes, graph.union is needed as make_ego_graph outputs a list of graph
  neighboring_nodes <- graph.union(make_ego_graph(igraph.for.computation, order = input$neighbor.degree, nodes = c(selected.individual.1, selected.individual.2)))
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


## Show connections between selected individuals
output$select.individual.network_graph <- renderVisNetwork({
  
  ## If not loaded yet, stop
  
  if (is.null(input$select.individual.2))
    return()
  
  ## load visN_edges
  
  visN_edges <- select.individual.edges()
  
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
  node_colors[match(c(input$select.individual.1,input$select.individual.2), visN_nodes$id)] <- "red"
  
  visN_nodes$color <- node_colors
  
  ## Visualise
  
  visNetwork(visN_nodes, visN_edges) %>% visNodes(
    color = list(
      border = "darkblue"
    )
  ) %>%
    visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = TRUE) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = FALSE) %>% visInteraction(navigationButtons = TRUE)
    
  
})


### ====================================== Selected Individuals Prosopography ============================================
### =================================================================================================================


output$select.individual_personInfo <- renderTable({
  selected.individual <- input$select.individual
  people.df[people.df$Person.Name == selected.individual,]
  })

output$select.individual_personInfo <- renderTable({
  selected.individual <- input$select.individual
  people.df[people.df$Person.Name == selected.individual,]
})


select.individual_edges <- reactive({
  
  selected.interactions <- subset(
    all.documented.interactions, Primary.Participant.Name %in% input$select.individual |
      Secondary.Participant.Name %in% input$select.individual
  )
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <- droplevels(selected.interactions)
  
  edges <- network.edges.function(selected.interactions)
  
  edges
})

output$select.individual_UI <- renderUI(selectInput("select.individual", label = "Select individual for prosopography analysis",
                                                    choices = unique(c(as.character(all.documented.interactions$Primary.Participant.Name),
                                                                       as.character(all.documented.interactions$Secondary.Participant.Name))
                                                    )))

output$select.individual_visNetwork <- renderVisNetwork({
  
  
  edges <- select.individual_edges()
  
  nodes.of.network <- unique(c(edges$Primary.Emlo_ID,edges$Secondary.Emlo_ID))
  
  nodes <- subset(people.df, iperson_id %in% nodes.of.network)
  
  visN_nodes <- data.frame("id" = nodes$iperson_id,
                           "title" = nodes$Person.Name)
  
  visN_edges <- data.frame("from" = edges$Primary.Emlo_ID,
                           "to" = edges$Secondary.Emlo_ID)
  
  visNetwork(visN_nodes, visN_edges) %>% visNodes(
    color = list(
      background = "lightblue", border = "darkblue"
    )
  ) %>%
    visLayout(hierarchical = input$hierachical_layout_option) %>%
    visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
    visOptions(highlightNearest = TRUE)
  })

