library(shiny)
library(DT)
library(htmltools)

shinyUI(navbarPage(
  "",
  tabPanel(
    "Early Christian Author Search",
    fluidPage(
      uiOutput("author_selection_UI"),
      DT::dataTableOutput("author_search_datatable", width = "100%")
    )
  ),
  tabPanel(
    "Search by References",
    fluidPage(
      uiOutput("references_selected_cols_UI"),
      DT::dataTableOutput("references_datatable", width = "100%")
    )
  ),
  tabPanel(
    "Texts and Authors",
    fluidPage(
      uiOutput("authors_and_texts_selected_cols_UI"),
      DT::dataTableOutput("authors_and_texts_datatable", width = "100%")
    )
  ),
  tabPanel("Large Dataset test",
           DT::dataTableOutput("large_datatable_test", width = "100%")),
  tabPanel("About",
           fluidPage(wellPanel(
             includeMarkdown("App_Description.Rmd")
           ))),
  collapsible = TRUE
))