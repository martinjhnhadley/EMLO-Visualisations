## main component


output$main_component_select_individuals_ui <- renderUI({
  print("main component person")
  selectInput("main_component_select_individuals",
              label = "select individuals",
              choices = codes_to_names_list(),
              width = "100%",
              multiple = TRUE
              # options = list(
              #   maxItems = 5,
              #   placeholder = 'Please select at least one individual by typing their name here...'
              # )
              )
})

output$main_component_degree_slider_UI <- renderUI({
  sliderInput("main_component_degree",
              label = "Neighbour Degree",
              min = 1,
              max = 6,
              value = 2)
})

output$main_component_subgraph <- renderVisNetwork({
  # 
  # if(is.null(input$main_component_select_individuals)){
  #   return()
  # }
  # 
  subgraph <- graph.union(
    make_ego_graph(
      igraph_to_analyse(),
      order = input$main_component_degree,
      nodes = input$main_component_select_individuals
      # nodes = c("AU_16161")
    )
  )
  
  visIgraph(subgraph) %>%
    visInteraction(
      tooltipDelay = 0.2,
      hideEdgesOnDrag = FALSE,
      dragNodes = FALSE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = FALSE)
  
})

output$test_now_ui <- renderUI({
  if(is.null(input$main_component_or_subcomponents)){
    print("return")
    return()
  }
  selectInput("variable", "Variable:",
              test_2(),
              multiple = TRUE
              # choices = c("1","3")
              )
})

output$main_component_output <- renderUI({
  fillPage(
    h1("main"),
    uiOutput("test_now_ui"),
    # uiOutput("main_component_select_individuals_ui"),
    "here",
    sidebarLayout(
      sidebarPanel(
        uiOutput("main_component_degree_slider_UI")
      ),
      mainPanel(
        "dd"
        # visNetworkOutput("main_component_subgraph")
        
        )
    )
    
  )
})

# observeEvent(
#   input$main_component_or_subcomponents,
#   {
#     computed_graph$igraph_object <- decomposed_igraph[[as.numeric(input$main_component_or_subcomponents)]]
# 
#   }, ignoreNULL = TRUE
# )

test_1 <- eventReactive(input$main_component_or_subcomponents,
                   {
  decomposed_igraph[[as.numeric(input$main_component_or_subcomponents)]]
}, ignoreNULL = FALSE)

test_2 <- eventReactive(input$main_component_or_subcomponents,
                        {
                          selected_graph <- decomposed_igraph[[as.numeric(input$main_component_or_subcomponents)]]
  replacement_names <-
    mapvalues(
      V(selected_graph)$name,
      from = names_df$id,
      to = names_df$name,
      warn_missing = F
    )
  setNames(V(selected_graph)$name, replacement_names)
  
}, ignoreNULL = FALSE)
# 
# observeEvent(
#   input$main_component_or_subcomponents,
#   {
# 
#     computed_graph$igraph_node_labels <- {
#       print(class(computed_graph$igraph_object))
#       replacement_names <-
#         mapvalues(
#           V(computed_graph$igraph_object)$name,
#           from = names_df$id,
#           to = names_df$name,
#           warn_missing = F
#         )
#       setNames(V(computed_graph$igraph_object)$name, replacement_names)
#     }
#   }, ignoreNULL = TRUE
# )