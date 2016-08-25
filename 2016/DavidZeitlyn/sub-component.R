## subcomponent

output$sub_component_visN <- renderVisNetwork({
  subgraph <- igraph_to_analyse()
  
  generate_visIgraph(subgraph)
  
})

observeEvent(
  input$refocus_sub_component,
  visNetworkProxy("sub_component_visN") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)


output$sub_component_selectedNode_SuperExam_DT <- DT::renderDataTable({
  onClickInputCheck(show_Details = {
    selected_node_id <- input$current_node_id$node[[1]]
    
    super_examiner_DT(input$sub_component_visN_selected)
  },
  destructive_Change = return())
  
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

output$sub_component_selectedNode_Authoring_DT <- DT::renderDataTable({
  onClickInputCheck(show_Details = {
    author_DT(input$sub_component_visN_selected)
  },
  never_Clicked = return())
  
}, options = list("language" = list("emptyTable" = "No known author information")))


output$sub_component_selectNode_UI <- renderUI({
  onClickInputCheck(show_Details = {
    fluidRow(column(wellPanel(
      h3("Supervisor/Examiner Relations"),
      DT::dataTableOutput("sub_component_selectedNode_SuperExam_DT")
    ),
    width = 6),
    column(wellPanel(
      h3("Authoring History"),
      DT::dataTableOutput("sub_component_selectedNode_Authoring_DT")
    ),
    width = 6))
  },
  destructive_Change = return())
  
})
