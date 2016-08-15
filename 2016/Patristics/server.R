library(DT)
library(shiny)

references_url <-
  read.csv(file = "references_url.csv", stringsAsFactors = F)[1, ]
references_df <- read.csv(references_url, stringsAsFactors = F)

authors_and_texts_url <-
  read.csv(file = "authors_and_texts.csv", stringsAsFactors = F)[1, ]
authors_and_texts_df <-
  read.csv(authors_and_texts_url, stringsAsFactors = F)

epitstles_url <-
  read.csv(file = "epistles_url.csv", stringsAsFactors = F)[1, ]
epitstles_df <- read.csv(epitstles_url, stringsAsFactors = F)

colnames(references_df) <- gsub("[.]", " ", colnames(references_df))
colnames(authors_and_texts_df) <-
  gsub("[.]", " ", colnames(authors_and_texts_df))

authors_and_texts_initial_columns <- c(
  "Early Christian Writer",
  "Text in English",
  "Text in Latin",
  "Start Date Range",
  "End Date Range"
)

references_initial_columns <-
  c(
    "Epistle",
    "Start Verse",
    "End Verse",
    "Early Christian Write",
    "Reference Text in English"
  )

shinyServer(function(input, output) {
  output$references_selected_cols_UI <- renderUI({
    selectInput(
      "references_selected_cols",
      label = "Columns to show: ",
      choices = colnames(references_df),
      selected = references_initial_columns,
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$authors_and_texts_selected_cols_UI <- renderUI({
    selectInput(
      "authors_and_texts_selected_cols",
      label = "Columns to show: ",
      choices = colnames(authors_and_texts_df),
      selected = authors_and_texts_initial_columns,
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$author_selection_UI <- renderUI({
    names <- unique(authors_and_texts_df$`Early Christian Writer`)
    
    selectizeInput(
      "author_selection",
      label = "Search for an Early Christian author: ",
      choices = names,
      selected = NULL,
      width = "100%",
      options = list(
        placeholder = 'Please start typing the name of an author'
        # onInitialize = I('function() { this.setValue("Please start typing the name of an author"); }')
      )
    )
  })
  
  output$author_epistles_datatable <- DT::renderDataTable({

    author_selection <- input$author_selection
    authors_epistles <-
      references_df[references_df$`Early Christian Writer` == author_selection,]$Epistle
    
    epitstles_df[epitstles_df$Epistle %in% authors_epistles,]
    
  }, rownames = FALSE,
  filter = 'top',
  escape = FALSE,
  extensions = "Responsive",
  options = list(
    pageLength = 5,
    responsive = TRUE,
    autoWidth = FALSE,
    "language" = list("search" = "Filter:",
                      "zeroRecords" = "There are no epistles matching your query")
  ))
  
  output$author_references_datatable <- DT::renderDataTable({
    
    author_selection <- input$author_selection
    references_df[references_df$`Early Christian Writer` == author_selection,]
    
  }, rownames = FALSE,
  filter = 'top',
  escape = FALSE,
  extensions = "Responsive",
  options = list(
    pageLength = 5,
    responsive = TRUE,
    autoWidth = FALSE,
    "language" = list("search" = "Filter:",
                      "zeroRecords" = "There are no references matching your query")
  ))
  
  output$author_texts_datatable <- DT::renderDataTable({
    
    author_selection <- input$author_selection
    authors_and_texts_df[authors_and_texts_df$`Early Christian Writer` == author_selection,]
    
  },
  rownames = FALSE,
  filter = 'top',
  escape = FALSE,
  extensions = "Responsive",
  options = list(
    pageLength = 5,
    responsive = TRUE,
    autoWidth = FALSE,
    "language" = list("search" = "Filter:",
                      "zeroRecords" = "There are no texts matching your query")
  ))
  
  output$author_all_info_UI <- renderUI({
    
    fluidRow(
      column(
      h3("Epistles"),
      DT::dataTableOutput("author_epistles_datatable"),
      h3("Texts"),
      dataTableOutput("author_texts_datatable"),
      h3("References"),
      dataTableOutput("author_references_datatable"),
      width = 12)
    )
    
  })
  
  output$authors_and_texts_datatable <- DT::renderDataTable(
    authors_and_texts_df[, input$authors_and_texts_selected_cols],
    rownames = FALSE,
    filter = 'top',
    escape = FALSE,
    extensions = "Responsive",
    options = list(
      pageLength = 5,
      responsive = TRUE,
      autoWidth = FALSE,
      "language" = list("search" = "Filter:",
                        "zeroRecords" = "There are no references matching your query")
    )
  )
  
  output$references_datatable <- DT::renderDataTable(
    references_df[, input$references_selected_cols],
    rownames = FALSE,
    filter = 'top',
    escape = FALSE,
    extensions = "Responsive",
    options = list(
      pageLength = 5,
      responsive = TRUE,
      autoWidth = FALSE,
      "language" = list("search" = "Filter:",
                        "zeroRecords" = "There are no references matching your query")
    )
  )
  
})