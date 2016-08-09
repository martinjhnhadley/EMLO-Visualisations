library(DT)
library(shiny)

obscured_url <-
  read.csv(file = "obscured_url.csv", stringsAsFactors = F)[1, ]

heliotope_df <- read.csv(obscured_url, stringsAsFactors = F)

colnames(heliotope_df) <- gsub("[.]", " ", colnames(heliotope_df))

heliotope_df$`Publication year` <-
  as.numeric(heliotope_df$`Publication year`)

initial_columns <-
  c(
    "Record number",
    "HBV protein",
    "Sequence numbers within protein  based on reference sequence ",
    "Database",
    "Authors",
    "Title",
    "Journal",
    "Publication year",
    "DOI"
  )

shinyServer(function(input, output) {
  output$HEPITOPES_selected_cols_UI <- renderUI({
    selectInput(
      "HEPITOPES_selected_cols",
      label = "Columns to show: ",
      choices = colnames(heliotope_df),
      selected = initial_columns,
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$HEPITOPES_datatable <- DT::renderDataTable(
    heliotope_df[, input$HEPITOPES_selected_cols],
    rownames = FALSE,
    filter = 'top',
    escape = FALSE,
    extensions = "Responsive",
    options = list("language" = list("sSearch" = "Filter:"))
  )
  
})