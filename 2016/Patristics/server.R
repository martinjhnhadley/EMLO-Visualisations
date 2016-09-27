library(DT)
library(shiny)
library(dplyr)
library(readr)

new_url <- "https://docs.google.com/spreadsheets/d/1vurgaT5eNeVa6zw6YVDfKmDKXojEghmsAjCWWea8CEU/pub?gid=213555275&single=true&output=csv"

new_df <- read_csv(new_url)

references_url <-
  read.csv(file = "references_url.csv", stringsAsFactors = F)[1,]
references_df <- read.csv(references_url, stringsAsFactors = F)

authors_and_texts_url <-
  read.csv(file = "authors_and_texts.csv", stringsAsFactors = F)[1,]
authors_and_texts_df <-
  read.csv(authors_and_texts_url, stringsAsFactors = F)

epitstles_url <-
  read.csv(file = "epistles_url.csv", stringsAsFactors = F)[1,]
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
    "Early Christian Writer",
    "Reference Text English",
    "Epistle",
    "Start Verse",
    "End Verse"
  )

url_generator <- function(book_name = NA,
                          begin_chapter = NA,
                          begin_v = NA,
                          end_chapter = NA,
                          end_v = NA,
                          link_text = NA) {
  paste0(
    "<a href=",
    "https://www.biblegateway.com/passage/?search=",
    gsub(" ", "+", book_name),
    "+",
    begin_chapter,
    "%3A",
    begin_v,
    "+-+",
    end_chapter,
    "%3A",
    end_v,
    "&version=NRSV",
    ">",
    '<span class="glyphicon glyphicon-new-window" aria-hidden="true"></span>',
    "</a>"
  )
}



shinyServer(function(input, output) {
  
  output$large_datatable_test <- DT::renderDataTable({
    new_df
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
      options = list(placeholder = 'Please start typing the name of an author'
                     # onInitialize = I('function() { this.setValue("Please start typing the name of an author"); }'))
      )
    )})
  
  output$author_search_datatable <- DT::renderDataTable({
    selected_author <- input$author_selection
    
    selected_references <- references_df %>%
      filter(`Early Christian Writer` == selected_author) %>%
      rename(`Text in English` = `Reference Text  English `)
    
    selected_texts <- authors_and_texts_df %>% 
      filter(`Early Christian Writer` == selected_author) %>%
      filter(`Text in English` %in% selected_references$`Text in English`)
    
    author_search <- left_join(selected_texts, selected_references)
    print(str(author_search))
    author_search$`Link to Passage` <- url_generator(
      book_name = author_search$Epistle,
      begin_chapter = author_search$`Start Chapter`,
      begin_v = author_search$`Start Verse`,
      end_chapter = author_search$`End Chapter`,
      end_v = author_search$`End Verse`,
      link_text = "none"
    )
    
    author_search[, c("Link to Passage",setdiff(colnames(author_search), "Link to Passage"))]
    
  }, rownames = FALSE,
  filter = 'top',
  escape = FALSE,
  extensions = "Responsive",
  options = list(
    pageLength = 10,
    responsive = TRUE,
    autoWidth = FALSE,
    "language" = list("search" = "Filter:",
                      "zeroRecords" = "There are no epistles matching your query")
  ))
    
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
    
    output$references_datatable <- DT::renderDataTable({
      
      
      references_df$`Link to Passage` <- url_generator(
        book_name = references_df$Epistle,
        begin_chapter = references_df$`Start Chapter`,
        begin_v = references_df$`Start Verse`,
        end_chapter = references_df$`End Chapter`,
        end_v = references_df$`End Verse`,
        link_text = "none"
      )
      
      
      references_df[, c("Link to Passage",input$references_selected_cols)]
      },
      
      rownames = FALSE,
      filter = 'top',
      escape = FALSE,
      extensions = "Responsive",
      options = list(
        pageLength = 10,
        responsive = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = c(0))),
        "language" = list("search" = "Filter:",
                          "zeroRecords" = "There are no references matching your query")
      )
    )
    
    output$authors_and_texts_datatable <- DT::renderDataTable(
      authors_and_texts_df[, input$authors_and_texts_selected_cols],
      rownames = FALSE,
      filter = 'top',
      escape = FALSE,
      extensions = "Responsive",
      options = list(
        pageLength = 10,
        responsive = TRUE,
        autoWidth = FALSE,
        "language" = list("search" = "Filter:",
                          "zeroRecords" = "There are no references matching your query")
      )
    )
    
})