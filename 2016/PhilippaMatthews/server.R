library(DT)
library(shiny)

obscured_url <-
  read.csv(file = "obscured_url.csv", stringsAsFactors = F)[1, ]

heliotope_df <- read.csv(obscured_url, stringsAsFactors = F,check.names = F)

# ## Drop unnecessary columns
heliotope_df <-
  heliotope_df[,!colnames(heliotope_df) %in% c(
    "Starting sequence number (based on Hepitopes reference sequence)",
    "New record number",
    "Optimal epitope vs. OLP"
  )]

heliotope_df$`Publication year` <-
  as.numeric(heliotope_df$`Publication year`)

initial_columns <-
  c(
    "Optimal amino acid sequence(s)",
    "HLA restriction(s); NS=not specified",
    "HBV protein",
    "Database",
    "Authors",
    "Title",
    "Journal",
    "Publication year",
    "DOI (NA=not available)"
  )

## Reorder columns in dataframe by the initial_columns to ensure correct display in selectInput
colnames(heliotope_df) <- colnames(heliotope_df)[order(match(colnames(heliotope_df), initial_columns))]

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
  
  output$HEPITOPES_datatable <- DT::renderDataTable({
    selected_columns <- input$HEPITOPES_selected_cols

    heliotope_df[, selected_columns]},
    rownames = FALSE,
    filter = 'top',
    escape = FALSE,
    extensions = "Responsive",
    options = list("language" = list("sSearch" = "Filter:"))
  )
  
})