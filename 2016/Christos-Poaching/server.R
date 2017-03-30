library(shiny)
library(plotly)
library(highcharter)
library(shinyjs)
library(readr)
library(plyr)
library(dplyr)
library(lubridate)
library(googleVis)
library(DT)
library(tidyr)
library(RColorBrewer)
library(shinyjs)


source("data-processing.R", local = T)

source("calendar_heatmap.R", local = T)

pretty_int_hours <- function(int_hour){
  if(int_hour < 10){
    hr <- paste0("0", int_hour)
  } else
    hr <- int_hour
  paste0(hr,":00")
}



shinyServer(function(input, output, session) {

  source("tab_rainfall_patrol_gunshots.R", local = TRUE)$value
  
  source("tab_date_tod_gunshots.R", local = TRUE)$value  
  
  source("tab_calendar_heatmap.R", local = TRUE)$value

  
  })