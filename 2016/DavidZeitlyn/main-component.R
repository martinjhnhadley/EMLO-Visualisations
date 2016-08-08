## main component


output$main_component_select_individuals_ui <- renderUI({
  print("main component person")
  selectInput(
    "main_component_select_individuals",
    label = "",
    choices = codes_to_names_list(),
    width = "100%",
    multiple = TRUE,
    selected = "Merge_02898"
    # options = list(
    #   maxItems = 5,
    #   placeholder = 'Please select at least one individual by typing their name here...'
    # )
  )
})

output$main_component_degree_slider_UI <- renderUI({
  sliderInput(
    "main_component_degree",
    label = "Neighbour Degree",
    min = 1,
    max = 6,
    value = 2
  )
})

output$main_component_subgraph <- renderVisNetwork({
  if (is.null(input$main_component_select_individuals)) {
    return()
  }
  
  subgraph <- graph.union(
    make_ego_graph(
      igraph_to_analyse(),
      order = input$main_component_degree,
      nodes = input$main_component_select_individuals
    )
  )
  
  ## Get color_* attributes, paste together as a vector and use gsub to remove NAs
  V(subgraph)$color <-
    trimws(gsub("NA", "", do.call("paste", vertex.attributes(subgraph)[names(vertex.attributes(subgraph))[grepl("color", names(vertex.attributes(subgraph)))]])))
  
  V(subgraph)$group <- mapvalues(
    V(subgraph)$color,
    from = as.character(advisor_supervisor_color_scheme),
    to = names(advisor_supervisor_color_scheme)
  )
  
  V(subgraph)$title <-
    mapvalues(V(subgraph)$name, from = names_df$id, to = names_df$name)
  V(subgraph)$label <-
    mapvalues(V(subgraph)$name, from = names_df$id, to = names_df$name)
  
  visIgraph(subgraph, idToLabel = FALSE) %>%
    visInteraction(
      tooltipDelay = 0.2,
      hideEdgesOnDrag = FALSE,
      dragNodes = FALSE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = FALSE) %>%
    visLegend(addNodes = node_legend, useGroups = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  
  })

output$main_component_selectedNode_SuperExam_DT <- renderDataTable({
  selected_node_id <- input$current_node_id$node[[1]]
  
  super_examiner_table <- merge(supervisors_df, examiners_df)
  names(super_examiner_table) <-
    c(
      "Author",
      "Supervisor",
      "Supervisor Affiliation",
      "Examiner",
      "Examiner Affiliation"
    )
  super_examiner_table <- super_examiner_table[, c(1, 2, 4)]
  
  super_examiner_table <-
    filter(super_examiner_table,
           Supervisor == selected_node_id |
             Examiner == selected_node_id)
  
  super_examiner_table$Author <-
    mapvalues(super_examiner_table$Author,
              from = names_df$id,
              to = names_df$name)
  super_examiner_table$Supervisor <-
    mapvalues(super_examiner_table$Supervisor,
              from = names_df$id,
              to = names_df$name)
  super_examiner_table$Examiner <-
    mapvalues(super_examiner_table$Examiner,
              from = names_df$id,
              to = names_df$name)
  
  super_examiner_table
  
}, options = list("language" = list("emptyTable" = "No known supervisor/examiner information")))

observeEvent(input$main_component_scrolldown_button, {
  session$sendCustomMessage(type = "scrollCallback", 1)
})

output$main_component_scrolldown_UI <- renderUI({
  if (is.null(input$current_node_id)) {
    return()
  } else {
    actionButton("main_component_scrolldown_button",
                 label = HTML(paste0(
                   "Click to see<br>",
                   names_df[names_df$id == input$current_node_id$node[[1]], "name"],
                   "'s <br>connections"
                 )))
    
  }
  
})

output$main_component_selectedNode_Authoring_DT <- renderDataTable({
  selected_node_id <- input$current_node_id$node[[1]]
  
  head(authors_df)
  
  author_table <- filter(authors_df, Author_id == selected_node_id)
  colnames(author_table) <- c("Author", "Author Affiliation", "Date")
  
  
  author_table$Author <-
    mapvalues(author_table$Author,
              from = names_df$id,
              to = names_df$name)
  
  author_table
  
}, options = list("language" = list("emptyTable" = "No known author information")))


output$main_component_selectNode_UI <- renderUI({
  if (is.null(input$current_node_id)) {
    return()
  } else
    fluidRow(column(wellPanel(
      h3("Supervisor/Examiner Relations"),
      dataTableOutput("main_component_selectedNode_SuperExam_DT")),
      width = 6
    ),
    column(wellPanel(
      h3("Authoring History"),
      dataTableOutput("main_component_selectedNode_Authoring_DT")),
      width = 6
    ))
  
})

# output$main_component_output <- renderUI({
#   fluidPage(
#     wellPanel(
#       "Please select up to 3 individuals in the box below to view their advisory lineage:",
#       uiOutput("main_component_select_individuals_ui"),
#       uiOutput("main_component_degree_slider_UI")
#     ),
#     visNetworkOutput("main_component_subgraph"),
#     wellPanel("lots of stuff underneathddddddd")
#
#   )
# })