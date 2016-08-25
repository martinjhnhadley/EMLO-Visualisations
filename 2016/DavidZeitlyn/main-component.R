

## ========================== Subgraph options (nodes, vertex degree) ========================
## ===========================================================================================

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

## ========================== Generate Subgraph and Selected Node Table ======================
## ===========================================================================================

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
  
  generate_visIgraph(subgraph)
  
})

observeEvent(
  input$refocus_main_component,
  visNetworkProxy("main_component_subgraph") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

## ============================== Subgraph Selected Nodes Tables and UI ======================
## ===========================================================================================

output$main_component_selectedNode_SuperExam_DT <- DT::renderDataTable({
  onClickInputCheck(show_Details = {
    super_examiner_DT(input$main_component_subgraph_selected)
    
  },
  destructive_Change = return())
  
}, options = list(
  "language" = list("emptyTable" = "No known supervisor/examiner information")
))

output$main_component_selectedNode_Authoring_DT <- DT::renderDataTable({
  onClickInputCheck(show_Details = {
    author_DT(input$main_component_subgraph_selected)
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
        DT::dataTableOutput("main_component_selectedNode_SuperExam_DT")
      ),
      width = 6
    ),
    column(
      wellPanel(
        h3("Authoring History"),
        DT::dataTableOutput("main_component_selectedNode_Authoring_DT")
      ),
      width = 6
    )))
  },
  destructive_Change = return())
  
})

## ===
## ===


output$main_component_filter_node_type_UI <- renderUI(
  selectInput(
    "main_component_filter_node_type",
    label = "Show only individuals who:",
    choices = list(
      "Supervised",
      "Examined",
      "Both supervised and examined",
      "Did not examine their own student"
    ),
    selected = "Did not examine their own student"
  )
)

## ============================================  Filtered Network ===========================
## ===========================================================================================

filtered_network_igraph <- eventReactive(input$main_component_filter_node_type,{
  
  if (is.null(input$main_component_filter_node_type)) {
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
  
  switch(
    input$main_component_filter_node_type,
    "Supervised" = {
      filtered_graph <-
        induced_subgraph(igraph_to_analyse,
                         V(igraph_to_analyse)[V(igraph_to_analyse)$supervised > 0])
    },
    "Examined" = {
      filtered_graph <-
        induced_subgraph(igraph_to_analyse,
                         V(igraph_to_analyse)[V(igraph_to_analyse)$examined > 0])
    },
    "Both supervised and examined" = {
      filtered_graph <-
        induced_subgraph(igraph_to_analyse,
                         V(igraph_to_analyse)[V(igraph_to_analyse)$examined > 0 &
                                                V(igraph_to_analyse)$supervised > 0])
    },
    "Did not examine their own student" = {
      filtered_graph <-
        induced_subgraph(igraph_to_analyse,
                         V(igraph_to_analyse)[V(igraph_to_analyse)$number_own_examined == 0])
    }
  )
  
  filtered_graph <-
    simplify(delete.vertices(filtered_graph,
                             V(filtered_graph)[degree(filtered_graph) == 0]))
  filtered_graph
},ignoreNULL = TRUE)

output$main_component_filtered_network <- renderVisNetwork({
  if (is.null(input$main_component_filter_node_type)) {
    return()
  }
  
  filtered_network_igraph <- filtered_network_igraph()
  
  generate_visIgraph(filtered_network_igraph)
  
})

## ====================== Filtered Network: Selected Node Tables & UI ========================
## ===========================================================================================


output$main_component_filtered_selectedNode_SuperExam_DT <-
  DT::renderDataTable({
    onClickInputCheck(show_Details = {
      super_examiner_DT(input$main_component_filtered_network_selected)
      
    },
    destructive_Change = return())
    
  }, options = list(
    "language" = list("emptyTable" = "No known supervisor/examiner information")
  ))

output$main_component_filtered_selectedNode_Authoring_DT <-
  DT::renderDataTable({
    onClickInputCheck(show_Details = {
      author_DT(input$main_component_filtered_network_selected)
    },
    never_Clicked = return())
    
  },     
  rownames = FALSE,
  filter = 'top',
  escape = FALSE,
  extensions = "Responsive",
  options = list("language" = list("emptyTable" = "No known author information")))


output$main_component_filtered_selectNode_UI <- renderUI({
  if (input$main_component_filtered_network_selected == "") {
    return()
  }
  
  if (is.null(input$main_component_filtered_network_selected)) {
    return()
  }
  
  print(input$main_component_filtered_network_selected)
  
  onClickInputCheck(show_Details = {
    # print(input$main_component_subgraph_selected)
    fluidPage(fluidRow(column(
      uiOutput("main_component_filtered_scrollup_UI"),
      width = 12
    )),
    fluidRow(column(
      wellPanel(
        h3("Supervisor/Examiner Relations"),
        DT::dataTableOutput("main_component_filtered_selectedNode_SuperExam_DT")
      ),
      width = 6
    ),
    column(
      wellPanel(
        h3("Authoring History"),
        DT::dataTableOutput("main_component_filtered_selectedNode_Authoring_DT")
      ),
      width = 6
    )))
  },
  destructive_Change = return())
  
})


## ============================================  Scrolling buttons ===========================
## ===========================================================================================

observeEvent(input$main_component_scrolldown_button, {
  session$sendCustomMessage(type = "scrollDown", 1)
})

observeEvent(input$main_component_filtered_scrolldown_button, {
  session$sendCustomMessage(type = "scrollDown", 1)
})

observeEvent(input$main_component_scrollup_button, {
  session$sendCustomMessage(type = "scrollUp", 1)
})

observeEvent(input$main_component_filtered_scrollup_button, {
  session$sendCustomMessage(type = "scrollUp", 1)
})

output$main_component_scrolldown_UI <- renderUI({
  scrolldown_button(
    selected_node_id = input$main_component_subgraph_selected,
    actionButton_id = "main_component_scrolldown_button"
  )
  
})

output$main_component_filtered_scrolldown_UI <- renderUI({
  scrolldown_button(
    selected_node_id = input$main_component_filtered_network_selected,
    actionButton_id = "main_component_scrolldown_button"
  )
  
})

output$main_component_scrollup_UI <- renderUI({
  actionButton("main_component_scrollup_button", "Scroll Up", width = "100%")
})

output$main_component_filtered_scrollup_UI <- renderUI({
  actionButton("main_component_filtered_scrollup_button",
               "Scroll Up",
               width = "100%")
})
