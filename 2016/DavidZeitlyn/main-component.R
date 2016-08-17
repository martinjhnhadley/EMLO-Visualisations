## main component


output$main_component_select_individuals_ui <- renderUI({
  igraph_to_analyse <- decomposed_igraph[[1]]
  
  replacement_names <-
    mapvalues(
      V(igraph_to_analyse)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = F
    )
  codes_to_names_list <-
    setNames(V(igraph_to_analyse)$name, replacement_names)
  
  selectInput(
    "main_component_select_individuals",
    label = "",
    choices = codes_to_names_list,
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
  
  igraph_to_analyse <- decomposed_igraph[[1]]
  
  replacement_names <-
    mapvalues(
      V(igraph_to_analyse)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  codes_to_names_list <-
    setNames(V(igraph_to_analyse)$name, replacement_names)
  
  subgraph <- graph.union(
    make_ego_graph(
      igraph_to_analyse,
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
    to = names(advisor_supervisor_color_scheme),
    warn_missing = FALSE
  )
  
  V(subgraph)$title <-
    mapvalues(
      V(subgraph)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  V(subgraph)$label <-
    mapvalues(
      V(subgraph)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = FALSE
    )
  
  visIgraph(subgraph, idToLabel = FALSE) %>%
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
  
  })

observeEvent(
  input$refocus_main_component,
  visNetworkProxy("main_component_subgraph") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

output$main_component_selectedNode_SuperExam_DT <- renderDataTable({
  onClickInputCheck(show_Details = {
    # selected_node_id <- input$current_node_id$node[[1]]
    
    selected_node_id <- input$main_component_subgraph_selected
    
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
  },
  destructive_Change = return())
  
}, options = list(
  "language" = list("emptyTable" = "No known supervisor/examiner information")
))

observeEvent(input$main_component_scrolldown_button, {
  session$sendCustomMessage(type = "scrollDown", 1)
})

observeEvent(input$main_component_scrollup_button, {
  session$sendCustomMessage(type = "scrollUp", 1)
})

output$main_component_scrolldown_UI <- renderUI({
  onClickInputCheck(show_Details = {
    if (is.null(input$main_component_subgraph_selected)) {
      return()
    }
    
    if (input$main_component_subgraph_selected == "") {
      return()
    }
    actionButton("main_component_scrolldown_button",
                 label = HTML(paste0(
                   "Click to see<br>",
                   names_df[names_df$id == input$main_component_subgraph_selected, "name"],
                   "'s <br>connections"
                 )))
    
  },
  destructive_Change = return())
})

output$main_component_scrollup_UI <- renderUI({
  actionButton("main_component_scrollup_button", "Scroll Up", width = "100%")
})

output$main_component_selectedNode_Authoring_DT <- renderDataTable({
  onClickInputCheck(show_Details = {
    # selected_node_id <- input$current_node_id$node[[1]]
    
    selected_node_id <- input$main_component_subgraph_selected
    
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
  },
  never_Clicked = return())
  
}, options = list("language" = list("emptyTable" = "No known author information")))


output$main_component_selectNode_UI <- renderUI({
  if (input$main_component_subgraph_selected == "") {
    return()
  }
  
  onClickInputCheck(show_Details = {
    # print(input$main_component_subgraph_selected)
    fluidPage(fluidRow(column(
      uiOutput("main_component_scrollup_UI"),
      width = 12
    )),
    fluidRow(column(
      wellPanel(
        h3("Supervisor/Examiner Relations"),
        dataTableOutput("main_component_selectedNode_SuperExam_DT")
      ),
      width = 6
    ),
    column(
      wellPanel(
        h3("Authoring History"),
        dataTableOutput("main_component_selectedNode_Authoring_DT")
      ),
      width = 6
    )))
  },
  destructive_Change = return())
  
})