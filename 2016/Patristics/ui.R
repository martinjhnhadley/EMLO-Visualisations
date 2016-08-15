library(shiny)
library(DT)
library(htmltools)

shinyUI(fluidPage(
  wellPanel(includeMarkdown("App_Description.Rmd")),
  
  tabsetPanel(
    tabPanel(
      "Early Christian Author Search",
      fluidPage(uiOutput("author_selection_UI"),
                h3("Epistles"),
                DT::dataTableOutput("author_epistles_datatable"),
                h3("Texts"),
                dataTableOutput("author_texts_datatable"),
                h3("References"),
                dataTableOutput("author_references_datatable"),
                width = "100%"
      )
    ),
    tabPanel("References",
             fluidPage(
               uiOutput("references_selected_cols_UI"),
               DT::dataTableOutput("references_datatable", width = "100%")
             )),
    tabPanel(
      "Texts and Authors",
      fluidPage(
        uiOutput("authors_and_texts_selected_cols_UI"),
        DT::dataTableOutput("authors_and_texts_datatable", width = "100%")
      )
    )
  )
))