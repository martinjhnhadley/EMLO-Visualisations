## ============ onClickEvent ==========================================
## ==============================================================================

control_tracker <-
  reactiveValues(
    selected_node = 0,
    destructive_inputs = 0,
    both = 0,
    check = 1
  )

observeEvent(c(input$people_or_departments), {
  control_tracker$destructive_inputs <-
    control_tracker$destructive_inputs + 1
})

observeEvent(c(input$people_or_departments, input$current_node_id), {
  control_tracker$both <- control_tracker$both + 1
})

observeEvent(c(input$current_node_id), {
  control_tracker$check <-
    list(control_tracker$destructive_inputs,
         control_tracker$both)
})


onClickInputCheck <- function(never_Clicked = return(),
                              show_Details = NA,
                              destructive_Change = "Node selection reset - pleae select a new node") {
  if (control_tracker$destructive_inputs == 1) {
    if (is.null(input$current_node_id)) {
      never_Clicked
    } else {
      show_Details
    }
    
  } else {
    if (control_tracker$destructive_inputs > control_tracker$check[1]) {
      destructive_Change
    } else {
      show_Details
    }
  }
}