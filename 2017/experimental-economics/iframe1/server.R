library(tidyverse)
library(highcharter)
library(RColorBrewer)
library(shinyjs)
library(RCurl)
library(shinyBS)

source("data-processing.R", local = TRUE)
source("beta-highchart-feature.R", local = TRUE)

shinyServer(
  function(input, output, session){
    
    source("figure1.R", local = TRUE)$value
    source("figureA2.R", local = TRUE)$value
    
  }
)