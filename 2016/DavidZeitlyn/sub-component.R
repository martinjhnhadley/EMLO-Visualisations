## subcomponent

output$sub_component_visN <- renderVisNetwork({
  
  subgraph <- igraph_to_analyse()
  
  ## Get color_* attributes, paste together as a vector and use gsub to remove NAs
  V(subgraph)$color <-
    trimws(gsub("NA", "", do.call("paste", vertex.attributes(subgraph)[names(vertex.attributes(subgraph))[grepl("color", names(vertex.attributes(subgraph)))]])))
  
  V(subgraph)$group <- mapvalues(
    V(subgraph)$color,
    from = as.character(advisor_supervisor_color_scheme),
    to = names(advisor_supervisor_color_scheme),
    warn_missing = FALSE
  )
  
  V(subgraph)$title <-
    mapvalues(V(subgraph)$name, from = names_df$id, to = names_df$name, warn_missing = FALSE)
  V(subgraph)$label <-
    mapvalues(V(subgraph)$name, from = names_df$id, to = names_df$name, warn_missing = FALSE)
  
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



output$sub_component_selectedNode_SuperExam_DT <- renderDataTable({
  
  onClickInputCheck(show_Details = {
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
                to = names_df$name,
                warn_missing = FALSE)
    super_examiner_table$Supervisor <-
      mapvalues(super_examiner_table$Supervisor,
                from = names_df$id,
                to = names_df$name,
                warn_missing = FALSE)
    super_examiner_table$Examiner <-
      mapvalues(super_examiner_table$Examiner,
                from = names_df$id,
                to = names_df$name,
                warn_missing = FALSE)
    
    super_examiner_table
  }, 
  destructive_Change = return()
  )
  
}, options = list(
  "language" = list("emptyTable" = "No known supervisor/examiner information")
))

observeEvent(input$sub_component_scrolldown_button, {
  session$sendCustomMessage(type = "scrollCallback", 1)
})

output$sub_component_scrolldown_UI <- renderUI({
  onClickInputCheck(show_Details = {
    if (is.null(input$current_node_id)) {
      return()
    } else {
      actionButton("sub_component_scrolldown_button",
                   label = HTML(
                     paste0("Click to see<br>",
                            names_df[names_df$id == input$current_node_id$node[[1]], "name"],
                            "'s <br>connections")
                   ))
      
    }
  },
  destructive_Change = return())
  
})

output$sub_component_selectedNode_Authoring_DT <- renderDataTable({
  
  onClickInputCheck(show_Details = {
    selected_node_id <- input$current_node_id$node[[1]]
    
    author_table <- filter(authors_df, Author_id == selected_node_id)
    colnames(author_table) <-
      c("Author", "Author Affiliation", "Date")
    
    author_table$Author <-
      mapvalues(author_table$Author,
                from = names_df$id,
                to = names_df$name,
                warn_missing = FALSE)
    
    author_table
  },
  never_Clicked = return()
  )
  
}, options = list("language" = list("emptyTable" = "No known author information")))


output$sub_component_selectNode_UI <- renderUI({
  onClickInputCheck(
    show_Details = {
      fluidRow(column(wellPanel(
        h3("Supervisor/Examiner Relations"),
        dataTableOutput("sub_component_selectedNode_SuperExam_DT")
      ),
      width = 6),
      column(wellPanel(
        h3("Authoring History"),
        dataTableOutput("sub_component_selectedNode_Authoring_DT")
      ),
      width = 6))
    }, 
    destructive_Change = return()
  )
  
})

