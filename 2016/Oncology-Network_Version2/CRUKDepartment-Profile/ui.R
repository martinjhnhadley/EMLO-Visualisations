library(shiny)
library(igraph)
library(visNetwork)
library(highcharter)

shinyUI(fluidPage(
  tags$head(
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollDown",
      function(color) {
      var y = $(window).scrollTop();  //your current y position on the page
      $(window).scrollTop(y+200);
      }
      );'
    )),
  wellPanel("Collaboration overview for a specific department, note that the specified department comes from the query URL i.e. livedata.shinyapps.io/CRUKDepartment-Profile?department=oncology. Sensibly, if no department is given the default is oncology."),
  wellPanel(uiOutput("url_department")),
  fluidRow(
    column(
      wellPanel(
      selectInput("people_or_departments",
                  label = "Display:",
                  choices = c("individuals","departments")),
      actionButton("refocus_network", "Refocus Network", width = "100%")),
        uiOutput("displayed_network_properties"),
      uiOutput("selected_node_sidePanel")
      
    , width = 4),
    column(
      visNetworkOutput("displayed_network", width = "100%")
      ,width = 8
    )
  ),
  highchartOutput("highchart_node_legened", height = "150px"),
  uiOutput("selected_node_table_UI")
  # wellPanel(
  #   DT::dataTableOutput("selected_node_table")
  # )
))