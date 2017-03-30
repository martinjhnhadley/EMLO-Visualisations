library(plyr)
library(tidyverse)
library(highcharter)
library(RColorBrewer)
library(shinyjs)

source("data-processing.R", local = TRUE)

shinyServer(
  function(input, output){
  
    source("figure2A.R", local = TRUE)$value
    
    ## ==== About Page
    output$about_page_UI <- renderUI({
      includeHTML(knitr::knit("About_Page.Rmd"))
    })
    
  }
)