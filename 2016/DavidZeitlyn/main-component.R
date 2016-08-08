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

  if(is.null(input$main_component_select_individuals)){
    return()
  }

  subgraph <- graph.union(
    make_ego_graph(
      igraph_to_analyse(),
      order = input$main_component_degree,
      nodes = input$main_component_select_individuals
    )
  )
  
  print(vertex.attributes(subgraph)[names(vertex.attributes(subgraph))[grepl("color",names(vertex.attributes(subgraph)))]])
  
  print(trimws(gsub("NA", "", do.call("paste", vertex.attributes(subgraph)[names(vertex.attributes(subgraph))[grepl("color",names(vertex.attributes(subgraph)))]]))))
  
  V(subgraph)$color <- trimws(gsub("NA", "", do.call("paste", vertex.attributes(subgraph)[names(vertex.attributes(subgraph))[grepl("color",names(vertex.attributes(subgraph)))]])))
  
  # 
  # print(vertex.attributes(subgraph)$color_1 %>% dput())
  # print(vertex.attributes(subgraph)$color_2 %>% dput())
  
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



output$main_component_output <- renderUI({
  fillPage(
    wellPanel("Please select up to 3 individuals in the box below to view their advisory lineage:",
    uiOutput("main_component_select_individuals_ui")),
    sidebarLayout(
      sidebarPanel(
        uiOutput("main_component_degree_slider_UI")
      ),
      mainPanel(
          visNetworkOutput("main_component_subgraph")
        )
    )
    
  )
})