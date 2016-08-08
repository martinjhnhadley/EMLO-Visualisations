library(shiny)
library(htmltools)
library(visNetwork)

shinyUI(
  fluidPage(
    includeMarkdown("App_Description.Rmd"),
    uiOutput("main_component_or_subcomponents_UI"),
    # visNetworkOutput("display_visNetwork"),
    # uiOutput("display_selected_graph"),
    conditionalPanel(
      condition = 'input.main_component_or_subcomponents == 1',
      
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
      
      
    )
  )
)