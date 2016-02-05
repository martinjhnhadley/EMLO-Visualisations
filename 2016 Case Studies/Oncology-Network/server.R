library(igraph)
library(visNetwork)
library(dplyr)
library(shiny)

cruk.nodes.df <- read.csv("data/CRUK-oxford-nodes.csv")
cruk.edges.df <- read.csv("data/CRUK-oxford-edges.csv")
## colnames need to be lower case
colnames(cruk.edges.df) <- tolower(colnames(cruk.edges.df))
colnames(cruk.nodes.df) <- tolower(colnames(cruk.nodes.df))
## visNetwork wants from and to not source and target
colnames(cruk.edges.df)[colnames(cruk.edges.df) == c("source", "target")] <- c("from","to")
## the vertex tooltip is added by way of the title column:
cruk.nodes.df$title <- cruk.nodes.df$label
## edge width is specified by value
cruk.edges.df$value <- (cruk.edges.df$weight == 10) + 1

# ## Circle is very dense
# visNetwork(node = cruk.nodes.df, edges = cruk.edges.df) %>% 
#   visNodes(label = FALSE) %>%
#   visIgraphLayout(layout = "layout_in_circle", type = "full") %>%
#   visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = FALSE, dragView = FALSE, zoomView = TRUE) %>%
#   visOptions(highlightNearest = TRUE)
#   

shinyServer(
  function(input, output, sessions){
    
    output$layout_control_UI <- renderUI({
      selectInput("layout_control", 
                  choices = c("layout_nicely","layout_as_tree", "layout_in_circle","layout.grid"),
                  selected = "layout_nicely", multiple = FALSE,
                  label = "Layout")
    })
    
    
    
    output$crukNetwork <- renderVisNetwork({
      
      if(is.null(input$layout_control)){
        return()
      }
      
      visNetwork(node = cruk.nodes.df, edges = cruk.edges.df, width = "100%") %>% 
        visNodes(label = FALSE) %>%
        visIgraphLayout(layout = input$layout_control) %>%
        visInteraction(tooltipDelay = 0.2, hideEdgesOnDrag = TRUE, dragNodes = TRUE, dragView = FALSE, zoomView = TRUE) %>%
        visOptions(highlightNearest = TRUE) %>%
        visInteraction(navigationButtons = TRUE)
      
    })
    
  }
)