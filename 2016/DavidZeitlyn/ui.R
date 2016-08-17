library(shiny)
library(htmltools)
library(visNetwork)

shinyUI(fluidPage(
  tags$head(
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollCallback",
      function(color) {
      var y = $(window).scrollTop();  //your current y position on the page
      $(window).scrollTop(y+450);
      }
      );'
    )
    ),
  
  h2("Primatology Advisory Lineages"),
  wellPanel(
    uiOutput("intro_text_at_top"),
    uiOutput("main_component_or_subcomponents_UI")
  ),
  conditionalPanel(
    condition = 'input.main_component_or_subcomponents == 1',
    # fluidPage(
    # fluidPage(
    wellPanel(
      "Please select up to 3 individuals in the box below to view their advisory lineage.
      
      Note that by default the individual with the highest edge degree (Dunbar, Robin I. M.) is displayed.
      ",
      uiOutput("main_component_select_individuals_ui"),
      
      fluidRow(
        column(uiOutput("main_component_degree_slider_UI"), width = 8,center = "align"),
        column(
          div(style = "margin-top:10%;background-color: yellow;white-space: normal;", uiOutput("main_component_scrolldown_UI")),
          width = 4,
          center = "align"
        ),
        center = "align"
      ),
      tags$style(type='text/css', "#main_component_degree_slider_UI { width:100%;}"),
      tags$style(type='text/css', "#main_component_scrolldown_button { width:100%;}")
    ),
    visNetworkOutput("main_component_subgraph", height = "500px"),
    uiOutput("main_component_selectNode_UI")
    
    # )
  ),
  conditionalPanel(condition = 'input.main_component_or_subcomponents > 1',
                   # fluidPage(
                   visNetworkOutput("sub_component_visN"),
                   uiOutput("sub_component_selectNode_UI")
                   # ))
                   # style = "overflow-y:scroll"
  )))