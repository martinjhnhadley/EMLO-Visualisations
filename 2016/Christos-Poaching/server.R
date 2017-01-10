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

source("data-processing.R", local = T)

source("calendar_heatmap.R", local = T)

shinyServer(function(input, output, session) {
  calheatmap_data <- eventReactive({
    input$calendar_shots_or_patrols
  },
  
  switch(
    input$calendar_shots_or_patrols,
    "gunshots" = {
      kmp_gunshots %>%
        select(date, total.gunshots.on.day) %>%
        unique() %>%
        filter(total.gunshots.on.day > 0)
    },
    "patrols" = {
      kmp_patrols %>%
        select(date, total.patrols.on.day) %>%
        unique() %>%
        filter(total.patrols.on.day > 0)
    }
  ))
  
  output$calheatmap_shots_DT <- renderDataTable({
    kmp_gunshots %>%
      filter(date == input$calheatmap_select_day) %>%
      group_by(timing) %>%
      mutate(shots.detected = sum(gunscore)) %>%
      select(timing, shots.detected) %>%
      unique()
  },
  rownames = FALSE,
  options = list(dom = 't'))
  
  output$calheatmap_patrols_DT <- renderDataTable({
    kmp_patrols %>%
      filter(date == input$calheatmap_select_day) %>%
      select(timing, total.patrols.on.day) %>%
      unique()
  },
  rownames = FALSE,
  options = list(dom = 't'))
  
  output$calheatmap_shot_summary <- renderUI({
    if (any(kmp_gunshots$date == input$calheatmap_select_day)) {
      fluidRow(column(
        "Summary of shots fired:",
        dataTableOutput("calheatmap_shots_DT"),
        width = 12
      ))
    } else {
      fluidRow(column("no shots detected on that day", width = 12))
    }
  })
  
  output$calheatmap_patrol_summary <- renderUI({
    if (any(kmp_patrols$date == input$calheatmap_select_day)) {
      fluidRow(column(
        "Summary of patrols:",
        dataTableOutput("calheatmap_patrols_DT"),
        width = 12
      ))
    } else {
      fluidRow(column("no patrols detected on that day", width = 12))
    }
  })
  
  
  output$calheatmap_select_day_UI <- renderUI({
    if (is.null(input$calheatmap_select_day)) {
      return()
    }
    
    fluidRow(column(
      strong(paste(
        "Date selected:", input$calheatmap_select_day
      )),
      uiOutput("calheatmap_shot_summary"),
      uiOutput("calheatmap_patrol_summary"),
      width = 12
    ))
    
  })
  
  output$calheatmap_gvis <- renderGvis({
    calheatmap_data() %>%
      calendar_heatmap(
        color.axis =
          "{minValue: 0,
        colors: ['#ffffd9',
        '#edf8b1',
        '#c7e9b4',
        '#7fcdbb',
        '#41b6c4',
        '#1d91c0'
        ],
        maxValue: 300}",
        cell.size = 10,
        cal.width = "600",
        cal.height = "450px"
      )
})
  
  output$gunshots_surface <- renderPlotly({
    
    shots_by_month_then_hour <- kmp_gunshots %>%
      filter(!is.na(tod)) %>%
      select(tod, month, total.gunshots.on.day) %>%
      group_by(month, tod) %>%
      mutate(shots.by.hour.and.month = n()) %>%
      select(-total.gunshots.on.day) %>%
      ungroup() %>%
      unique() %>%
      spread(month, shots.by.hour.and.month)
    
    shots_by_month_then_hour <- shots_by_month_then_hour %>%
      as.data.frame()
    
    shots_by_month_then_hour[is.na(shots_by_month_then_hour)] <- 0
    
    rownames(shots_by_month_then_hour) <- shots_by_month_then_hour$month
    
    shots_matrix <- shots_by_month_then_hour %>%
      select(-tod) %>%
      as.matrix()
    
    plot_ly(
      y = rownames(shots_matrix),
      x = colnames(shots_matrix),
      z = shots_matrix
    ) %>%
      add_surface() %>%
      layout(scene = list(xaxis = list(title = "Month"),
                          yaxis = list(title = "Hour"),
                          zaxis = list(title = "Total Gunshots")))
    
  })
  
  })