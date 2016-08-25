library(shiny)
library(htmltools)
library(visNetwork)
library(DT)

shinyUI(fluidPage(
  tags$head(
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollDown",
      function(color) {
      var y = $(window).scrollTop();  //your current y position on the page
      $(window).scrollTop(y+450);
      }
      );'
    ),
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollUp",
      function(color) {
      var y = $(window).scrollTop();  //your current y position on the page
      $(window).scrollTop(y-650);
      }
      );'
    )
    ),
  
  h2("Primatology Advisory Lineages"),
  wellPanel(
    uiOutput("intro_text_at_top"),
    uiOutput("main_component_or_subcomponents_UI")
  ),
  conditionalPanel(condition = 'input.main_component_or_subcomponents == 1',
                   # fluidPage(
                   # fluidPage(
                   tabsetPanel(
                     tabPanel(
                       "Filter by examiner/supevisor",
                       wellPanel(
                         "Filter the main lineage by 'supervisor' or 'examiner' etc., note that individuals who are completely disconnected from the network are not shown.",
                         fluidRow(
                           column(
                             uiOutput("main_component_filter_node_type_UI"),
                             width = 6
                           ),
                           column(
                             div(style = "margin-top:10%;white-space: normal;", uiOutput("main_component_filtered_scrolldown_UI")),
                             width = 4,
                             center = "align"
                           )
                         )
                       ),
                       visNetworkOutput("main_component_filtered_network"),
                       uiOutput("main_component_filtered_selectNode_UI")
                     ),
                     tabPanel(
                       "Search for individuals",
                       wellPanel(
                         "Please select individuals in the box below to view their advisory lineage.
                         
                         Note that by default the individual with the highest edge degree (Dunbar, Robin I. M.) is displayed.
                         ",
                         fluidRow(column(
                           uiOutput("main_component_select_individuals_ui"), width = 9
                         ),
                         column(
                           div(
                             style = "margin-top:10%;white-space: normal;",
                             actionButton("refocus_main_component", "Refocus network")
                           ),
                           width = 3
                         )),
                         
                         fluidRow(
                           column(
                             uiOutput("main_component_degree_slider_UI"),
                             width = 8,
                             center = "align"
                           ),
                           column(
                             div(style = "margin-top:10%;white-space: normal;", uiOutput("main_component_scrolldown_UI")),
                             width = 4,
                             center = "align"
                           ),
                           center = "align"
                         ),
                         tags$style(type = 'text/css', "#main_component_degree_slider_UI { width:100%;}"),
                         tags$style(type = 'text/css', "#main_component_scrolldown_button { width:100%;}")
                       ),
                       visNetworkOutput("main_component_subgraph", height = "500px"),
                       uiOutput("main_component_selectNode_UI")
  )
    )),
  conditionalPanel(
    condition = 'input.main_component_or_subcomponents > 1',
    # fluidPage(
    actionButton("refocus_sub_component", "Refocus network"),
    visNetworkOutput("sub_component_visN"),
    uiOutput("sub_component_selectNode_UI")
    # ))
    # style = "overflow-y:scroll"
  )
))