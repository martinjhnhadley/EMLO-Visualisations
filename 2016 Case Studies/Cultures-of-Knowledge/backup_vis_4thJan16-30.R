### ============= Useful Visualisations Tools ========================= ###

## ggplot Color Function from http://stackoverflow.com/a/8197703/1659890
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### ====================================== Connected Individuals Network ============================================
### =================================================================================================================

### ========= Category/Type Selection UI Elements

all_event_types <- reactive(levels(multiparty.interactions$Event.or.Relationship.Type))
all_event_categories <- reactive(levels(multiparty.interactions$Category))

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

### ========= Filter multiparty.interactions into selected.interactions


whole.network_selected.interactions <- reactive({
  # Filter by selected categories etc
  selected.interactions <-
    subset(
      multiparty.interactions,
      Category %in% input$selected_event_categories &
        Event.or.Relationship.Type %in% input$selected_event_types
    )
  
  # Drop levels that are empty (as a result of above subsetting)
  selected.interactions <-
    droplevels(selected.interactions)
  # Return selected.interactions
  selected.interactions
})

### ========= Construct edges

whole.network_edges.of.network <- reactive(
  {
    selected.interactions <- whole.network_selected.interactions()
    edges.of.network <- data.frame("Primary.Emlo_ID" = character(),
                                   "Secondary.Emlo_ID" = character(),
                                   "Connected.Events" = character(),
                                   "Correspondence" = numeric(),
                                   "DeliverySpeech" = numeric(),
                                   "Employment" = numeric(),
                                   "FamilyRelationships" = numeric(),
                                   "FirstContact"= numeric(),
                                   "HoldingAnEcclesiasticalOffice" = numeric(),
                                   "Meeting" = numeric(),
                                   "PromotionToDegree" = numeric(),
                                   "SocialContact" = numeric(),
                                   "Study" = numeric(),
                                   "TeachingActivity" = numeric(),
                                   "Travel" = numeric()
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
    
    # Return edges.of.network to the symbol whole.network_edges.of.network
    edges.of.network
  })


### ========= visNetwork

output$visNetwork_example <- renderVisNetwork({
  
  edges <- whole.network_edges.of.network()
  
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
    visOptions(highlightNearest = TRUE) %>%
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
  selectedIndividual <- input$whole.network_selected.individual$nodes[[1]]
  selectedIndividual <- people.df[people.df$iperson_id == selectedIndividual,]
  as.character(selectedIndividual$Person.Name)
})

output$selected_node <- DT::renderDataTable({
  selected.interactions <- whole.network_selected.interactions()
  
  selectedIndividual <- input$current_node_id$nodes[[1]]
  
  print(input$current_node_id)
  # selectedIndividual <- 21584
  
  edges <- whole.network_edges.of.network()
  
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

