## Shared Graph Functionality 


## ========================== Generate visIgraph from igraph =================================
## ===========================================================================================

generate_visIgraph <- function(igraph_object = NA){
  igraph_object <- igraph_object
  ## Get color_* attributes, paste together as a vector and use gsub to remove NAs
  V(igraph_object)$color <-
    trimws(gsub("NA", "", do.call("paste", vertex.attributes(igraph_object)[names(vertex.attributes(igraph_object))[grepl("color", names(vertex.attributes(igraph_object)))]])))
  
  V(igraph_object)$group <- mapvalues(
    V(igraph_object)$color,
    from = as.character(advisor_supervisor_color_scheme),
    to = names(advisor_supervisor_color_scheme),
    warn_missing = FALSE
  )
  
  V(igraph_object)$title <-
    mapvalues(
      V(igraph_object)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  V(igraph_object)$label <-
    mapvalues(
      V(igraph_object)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  
  visIgraph(igraph_object, idToLabel = FALSE) %>%
    visInteraction(
      tooltipDelay = 0.2,
      hideEdgesOnDrag = FALSE,
      dragNodes = FALSE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE)) %>%
    visLayout(hierarchical = FALSE) %>%
    visLegend(addNodes = node_legend, useGroups = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
}

## ========================== selected node details ==========================================
## ===========================================================================================

super_examiner_DT <- function(selected_node_id = NA){
  
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
    filter(
      super_examiner_table,
      Supervisor == selected_node_id |
        Examiner == selected_node_id
    )
  
  super_examiner_table$Author <-
    mapvalues(
      super_examiner_table$Author,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  super_examiner_table$Supervisor <-
    mapvalues(
      super_examiner_table$Supervisor,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  super_examiner_table$Examiner <-
    mapvalues(
      super_examiner_table$Examiner,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  
  super_examiner_table
  
}

author_DT <- function(selected_node_id = NA){
  
  author_table <-
    filter(authors_df, Author_id == selected_node_id)
  colnames(author_table) <-
    c("Author", "Author Affiliation", "Date")
  
  author_table$Author <-
    mapvalues(
      author_table$Author,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  
  author_table
  
}

## ========================== scroll up/down buttons =========================================
## ===========================================================================================

scrolldown_button <- function(selected_node_id = NA, actionButton_id = NA){
  onClickInputCheck(show_Details = {
    if (is.null(selected_node_id)) {
      return()
    }
    
    if (selected_node_id == "") {
      return()
    }
    actionButton(actionButton_id,
                 label = HTML(paste0(
                   "Click to see<br>",
                   names_df[names_df$id == selected_node_id, "name"],
                   "'s <br>connections"
                 )))
    
  },
  destructive_Change = return())
}