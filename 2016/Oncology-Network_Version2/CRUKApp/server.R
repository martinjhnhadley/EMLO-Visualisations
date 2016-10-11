## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

library(igraph)
library(visNetwork)
library(plyr)
library(dplyr)
library(shiny)
library(ForceAtlas2)
library(igraph)
library(DT)
library(highcharter)

source("shared_graph_functions.R", local = T)
source("beautification.R", local = T)
source("data-processing.R", local = T)



capwords <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## =========================== Server Function ==================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  source("control_tracker.R", local = TRUE)$value
  
  ## ================= URL_SELECTED_DEPARTMENT and furniture ======================
  ## ==============================================================================
  
  url_selected_department <-
    eventReactive(session$clientData$url_search,
                  {
                    query <- parseQueryString(session$clientData$url_search)
                    if (!is.null(query[['department']])) {
                      capwords(query[['department']])
                      print(capwords(query[['department']]))
                    } else
                      "Oncology"
                  }, ignoreNULL = FALSE)
  
  
  output$select_department_UI <- renderUI(
    selectInput("selected_department",
                label = "Select Department",
                choices = unique(institution_nodes$department),
                width = "100%")
  )
  
  
  output$department_app_title <-
    renderUI(h1(paste(input$selected_department, "Profile")))
  
  output$department_app_description <- renderUI(wellPanel(
    p(
      paste(
        "This lorem ipsum content is about the ",
        input$selected_department,
        "department and will describe what the department does and how amazing they are - it might also include links."
      )
    ),
    p(
      "The content may also include hyperlinks, and could be pulled into the app via an external source (such that it may be updateable) - however, such functionality would be beyond a Live Data Case Study."
    )
  ))
  
  output$department_app_collapsile_info <- renderUI(fluidRow(
    column(paste0(
      "Members of Department: ",
      sum(institution_nodes$department == input$selected_department)
    ),
    width = 6),
    column(paste0("Head of Department: ", "Benjamina"),
           width = 6),
    column(paste0(
      "Collaborations in Department:", ecount(induced_subgraph(
        institution_igraph,
        institution_nodes[institution_nodes$department == input$selected_department,
                          "name"]
      ))
    ),
    width = 6),
    column(paste0(
      "Research Support Contact:", "Henrietta"
    ),
    width = 6)
  ))
  
  ## ================= People Directory DT/UI =====================================
  ## ==============================================================================
  
  output$people_directory_DT <- DT::renderDataTable({
    institution_nodes[institution_nodes$department == input$selected_department, c("name", "institution", "department")]
  }, rownames = FALSE,
  # filter = FALSE,
  escape = FALSE,
  extensions = "Responsive",
  options = list("language" = list("sSearch" = "Filter:")))
  
  output$people_directory_UI <- renderUI(fluidPage(
    paste("These are the people in the ", input$selected_department),
    DT::dataTableOutput("people_directory_DT")
  ))
  
  ## ================= Department Network DT/UI =====================================
  ## ==============================================================================
  
  graph_to_display <-
    eventReactive(c(input$people_or_departments, input$vertex_degree),
                  switch(
                    input$people_or_departments,
                    "within department" = {
                      departmental_nodes <-
                        institution_nodes[institution_nodes$department == input$selected_department, "name"]
                      
                      induced_subgraph(institution_igraph, departmental_nodes)
                      
                    },
                    
                    "within whole network" = {
                      graph.union(make_ego_graph(institution_igraph,
                                                 order = 2,
                                                 nodes = institution_nodes[institution_nodes$department == input$selected_department, "name"])) %>% igraph_deduplicate_vertex_attr()
                    },
                    
                    ## Note that this does not work, hence unavailable in options
                    "unavailable_department_level_interactions" = {
                      if (is.null(input$vertex_degree)) {
                        return()
                      }
                      department_ego_networks <-
                        graph.union(
                          make_ego_graph(
                            institution_igraph,
                            order = input$vertex_degree,
                            nodes = institution_nodes[institution_nodes$department ==
                                                        input$selected_department, "name"]
                          )
                        )
                      
                      
                      igraph_deduplicate_vertex_attr(department_ego_networks)
                      
                    }
                  ))
  
  output$department_network <- renderVisNetwork({
    if (input$people_or_departments == "within whole network" &
        is.null(input$vertex_degree)) {
      return()
    }
    graph_to_display <- as.undirected(graph_to_display())
    
    
    switch(
      input$people_or_departments,
      "within department" = {
        visIgraph(graph_to_display,
                  idToLabel = F)
      },
      "within whole network" = {
        visIgraph(graph_to_display,
                  idToLabel = F,
                  layout = "layout_with_lgl")
      }
    ) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE)) %>%
      visLayout(hierarchical = FALSE) %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
})
  
  output$department_network_edge_degree_UI <- renderUI({
    if (input$people_or_departments == "within whole network") {
      wellPanel(
        sliderInput(
          "vertex_degree",
          label = "Vertex Degree",
          min = 1,
          max = 5,
          step = 1,
          value = 1
        )
      )
    }
  })
  
  
  ## ================= Legend =====================================
  ## ==============================================================================
  
  output$highchart_node_legened <- renderHighchart(
    highchart_legend(
      legend_names = department_colours$department,
      legend_colours = department_colours$colours
    )
  )
  
  
  
  ## =========================== Generate Graph ====================================
  ## ==============================================================================
  
  
  
  
  output$displayed_network <- renderVisNetwork({
    graph_to_display <- graph_to_display()
    
    visIgraph(graph_to_display,
              idToLabel = F,
              layout = "layout_nicely") %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE)) %>%
      visLayout(hierarchical = FALSE) %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
})
  output$displayed_network_properties <- renderUI({
    wellPanel(p(paste0(
      "Average path length: ", round(average.path.length(graph_to_display()), 2)
    )),
    p(paste0(
      "Number of nodes: ", vcount(graph_to_display())
    )))
  })
  
  observeEvent(
    input$refocus_network,
    visNetworkProxy("displayed_network") %>%
      visFit(nodes = NULL, animation = list(duration = 500))
  )
  
  output$selected_node_sidePanel <- renderUI({
    if (input$displayed_network_selected == "") {
      return()
    }
    onClickInputCheck(
      never_Clicked = return(),
      show_Details = {
        wellPanel(
          paste0("Selected Node: ", input$displayed_network_selected),
          actionButton("scroll_down", "Scroll down for details", width = "100%")
        )
      },
      destructive_Change = return()
    )
  })
  
  observeEvent(input$scroll_down, {
    session$sendCustomMessage(type = "scrollDown", 1)
  })
  
  
  individual_datatable <- reactive({
    if (input$displayed_network_selected == "") {
      return()
    }
    selected_id <-
      institution_nodes[institution_nodes$name == input$displayed_network_selected, "id"]
    subsetted_edges <-
      filter(institution_edges, from == selected_id |
               to == selected_id)
    subsetted_edges$from <-
      mapvalues(
        subsetted_edges$from,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    subsetted_edges$to <-
      mapvalues(
        subsetted_edges$to,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  
  department_datatable <- reactive({
    print(input$displayed_network_selected)
    
    if (input$displayed_network_selected == "") {
      return()
    }
    
    department_members <- filter(institution_nodes,
                                 department == input$displayed_network_selected) %>%
      select(id) %>%
      .[, 1]
    
    subsetted_edges <-
      filter(institution_edges,
             from %in% department_members |
               to %in% department_members)
    
    subsetted_edges$from <-
      mapvalues(
        subsetted_edges$from,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    subsetted_edges$to <-
      mapvalues(
        subsetted_edges$to,
        from = institution_nodes$id,
        to = institution_nodes$name,
        warn_missing = FALSE
      )
    select(subsetted_edges,
           from,
           to,
           collaborations,
           publication.name,
           publication.date)
  })
  
  output$selected_node_table <- DT::renderDataTable({
    if (is.null(input$displayed_network_selected)) {
      return()
    }
    
    onClickInputCheck(show_Details = {
      switch(
        input$people_or_departments,
        "individuals" = {
          individual_datatable()
        },
        "departments" = {
          department_datatable()
        }
      )
    })
    
  })
  
  output$selected_node_table_UI <- renderUI({
    if (input$displayed_network_selected == "") {
      wellPanel("Select a node for more details")
    } else {
      onClickInputCheck(
        never_Clicked = {
          wellPanel("Select a node for more details")
        },
        show_Details = {
          wellPanel(DT::dataTableOutput("selected_node_table"))
        },
        destructive_Change = wellPanel("Select a node for more details")
      )
    }
    
  })
  
  })