## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562.v56
## ================================================================================


library(shiny)
library(rfigshare)
library(lubridate)
library(plotly)
library(highcharter)
library(dygraphs)
library(xts)
library(dplyr)
library(htmltools)
library(tidyr)

source("data-processing.R", local = T)

stackoverflow_ma <-
  function(x, n = 5, sides = 1) {
    stats::filter(x, rep(1 / n, n), sides = sides)
  } # http://stackoverflow.com/a/4862334/1659890

stacked_bar_chart <- function(data = NA,
                              categories_column = NA,
                              measure_columns = NA,
                              stacking_type = NA,
                              ordering_function = c,
                              explicit_order = NULL) {
  ordered_measure <-
    order(unlist(lapply(measure_columns, function(x) {
      ordering_function(data[, x])
    })),
    decreasing = TRUE) - 1
  
  chart <- highchart() %>%
    hc_xAxis(categories = data[, categories_column],
             title = categories_column)
  
  invisible(lapply(1:length(measure_columns), function(colNumber) {
    chart <<-
      hc_add_series(
        hc = chart,
        name = measure_columns[colNumber],
        data = data[, measure_columns[colNumber]],
        index = {
          if (is.null(explicit_order)) {
            ordered_measure[colNumber]
          } else
            explicit_order[colNumber]
        }
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = TRUE)
} %>%
  hc_credits(text = 'Source: Online Labour Index',
             enabled = TRUE,
             href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
             position = list(align = "right"))

shinyServer(function(input, output, session) {
  output$selected_occupation_UI <- renderUI({
    selectInput(
      "selected_occupation",
      label = HTML("Selected Occupations <span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>"),
      choices = c(unique(
        gig_economy_by_occupation$occupation
      ), "Total"),
      selected = setdiff(unique(
        gig_economy_by_occupation$occupation
      ), "Total"),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$landing_rollmean_k_UI <- renderUI({
    radioButtons(
      "landing_rollmean_k",
      label = "",
      choices = list(
        "Show daily value" = 1,
        "Show 28-day moving average" = 28
      ),
      selected = 28,
      inline = TRUE
    )
  })
  
  output$landing_xts_highchart <- renderHighchart({
    selected_categories <- "Total"
    
    
    
    
    # invisible(lapply(selected_categories,
    #                  function(x) {
    #                    filtered <-
    #                      gig_economy_by_occupation[gig_economy_by_occupation$occupation == x,]
    #                    xts_data <- xts(filtered$count, filtered$date)
    #                    xts_data[index(xts_data)] <- as.vector(stackoverflow_ma(as.vector(xts_data), n = as.numeric(input$landing_rollmean_k_UI)))
    #                    print(xts_data)
    #                    hc <<- hc %>%
    #                      hc_add_series_xts(na.omit(xts_data), name = x, index = which(legend_order == x) - 1)
    #                  }))
    #
    #
    
    filtered <-
      gig_economy_by_occupation[gig_economy_by_occupation$occupation == "Total", ]
    xts_data <- xts(filtered$count, filtered$date)
    xts_data[index(xts_data)] <-
      as.vector(stackoverflow_ma(as.vector(xts_data), n = as.numeric(input$landing_rollmean_k)))
    
    highchart() %>%
      hc_add_series_xts(na.omit(xts_data), name = "Total") %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      hc_rangeSelector(buttons = list(
        list(
          type = 'month',
          count = 1,
          text = '1mth'
        ),
        list(
          type = 'month',
          count = 3,
          text = '3mth'
        ),
        list(
          type = 'month',
          count = 6,
          text = '6mth'
        ),
        list(type = 'ytd',
             text = 'YTD'),
        list(
          type = 'year',
          count = 1,
          text = '1y'
        ),
        list(type = 'all',
             text = 'All')
      )) %>%
      hc_credits(text = 'Source: Online Labour Index',
                 enabled = TRUE,
                 href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
                 position = list(align = "right"))
    
    
    
  })
  
  output$occupation_rollmean_k_UI <- renderUI({
    radioButtons(
      "occupation_rollmean_k",
      label = "",
      choices = list(
        "Show daily value" = 1,
        "Show 28-day moving average" = 28
      ),
      selected = 28,
      inline = TRUE
    )
  })
  
  
  output$occupation_xts_highchart <- renderHighchart({
    selected_categories <- input$selected_occupation
    
    legend_order <- gig_economy_by_occupation %>%
      filter(occupation %in% selected_categories) %>%
      group_by(occupation) %>%
      mutate(total = sum(count)) %>%
      select(occupation, total) %>%
      arrange(desc(total)) %>%
      select(occupation) %>%
      unique() %>%
      unlist(use.names = F)
    
    hc <- highchart()
    
    invisible(lapply(selected_categories,
                     function(x) {
                       filtered <-
                         gig_economy_by_occupation[gig_economy_by_occupation$occupation == x, ]
                       xts_data <-
                         xts(filtered$count, filtered$date)
                       xts_data[index(xts_data)] <-
                         as.vector(stackoverflow_ma(as.vector(xts_data), n = as.numeric(input$occupation_rollmean_k)))
                       hc <<- hc %>%
                         hc_add_series_xts(na.omit(xts_data),
                                           name = x,
                                           index = which(legend_order == x) - 1)
                     }))
    hc %>% hc_tooltip(valueDecimals = 0) %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      hc_rangeSelector(buttons = list(
        list(
          type = 'month',
          count = 1,
          text = '1mth'
        ),
        list(
          type = 'month',
          count = 3,
          text = '3mth'
        ),
        list(
          type = 'month',
          count = 6,
          text = '6mth'
        ),
        list(type = 'ytd',
             text = 'YTD'),
        list(
          type = 'year',
          count = 1,
          text = '1y'
        ),
        list(type = 'all',
             text = 'All')
      )) %>%
      hc_credits(text = 'Source: Online Labour Index',
                 enabled = TRUE,
                 href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
                 position = list(align = "right"))
    
    
  })
  
  
  
  # output$region_xts_group_by_UI <- renderUI({
  #   selectInput("region_xts_group_by",
  #               "Group By",
  #               choices = list("Country Group (Otto: what should I be called)" = "country_group","Individual Countries (Otto: what should I be called)" = "country"),
  #               width = "100%")
  # })
  
  output$region_xts_selected_regions_UI <- renderUI({
    selectInput(
      "region_xts_selected_region",
      label = HTML("Selected Countries/Regions <span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>"),
      # choices = unique(gig_economy_by_boundary[["country_group"]]),
      choices = c(
        "United States",
        "Canada",
        "other Americas",
        "United Kingdom",
        "other Europe",
        "Australia",
        "India",
        "other Asia and Oceania",
        "all Africa"
      ),
      selected = c(
        "United States",
        "Canada",
        "other Americas",
        "United Kingdom",
        "other Europe",
        "Australia",
        "India",
        "other Asia and Oceania",
        "all Africa"
      ),
      multiple = TRUE,
      width = "100%"
    )
  })
  
  output$region_rollmean_k_UI <- renderUI({
    radioButtons(
      "region_rollmean_k",
      label = "",
      choices = list(
        "Show daily value" = 1,
        "Show 28-day moving average" = 28
      ),
      selected = 28,
      inline = TRUE
    )
  })
  
  output$region_xts_highchart <- renderHighchart({
    selected_categories <- input$region_xts_selected_region
    
    tallied_data <- gig_economy_by_boundary %>%
      group_by(country_group, timestamp) %>%
      mutate(total = sum(count)) %>%
      select(-count) %>%
      distinct(total) %>% # keep ONLY total and groups
      group_by(timestamp) %>%
      mutate(total = total / sum(total)) %>%
      as.data.frame()
    
    
    total_count <- gig_economy_by_occupation %>%
      filter(occupation == "Total") %>%
      select(date, count) %>%
      as.data.frame()
    total_count <- total_count[total_count$date %in% tallied_data$timestamp,]
    
    invisible(lapply(total_count$date, function(x){
      tallied_data[tallied_data$timestamp == x,"total"] <<- tallied_data[tallied_data$timestamp == x,"total"] * total_count[total_count$date == x, "count"]
    }))
    
    tallied_data <- as_data_frame(tallied_data)
    
    legend_order <- tallied_data %>%
      ungroup() %>%
      arrange(desc(total)) %>%
      select(country_group) %>%
      unique() %>%
      unlist(use.names = F)
    
    hc <- highchart()
    
    invisible(lapply(selected_categories,
                     function(x) {
                       filtered <-
                         tallied_data[tallied_data$country_group == x, ]
                       xts_data <-
                         xts(filtered$total, filtered$timestamp)
                       xts_data[index(xts_data)] <-
                         as.vector(stackoverflow_ma(as.vector(xts_data), n = as.numeric(input$region_rollmean_k)))
                       hc <<- hc %>%
                         hc_add_series_xts(na.omit(xts_data),
                                           name = x,
                                           index = which(legend_order == x) - 1)
                       
                     }))
    hc %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_yAxis(title = list("text" = "Online Labour Index"),  "opposite" = FALSE) %>%
      hc_rangeSelector(buttons = list(
        list(
          type = 'month',
          count = 1,
          text = '1mth'
        ),
        list(
          type = 'month',
          count = 3,
          text = '3mth'
        ),
        list(
          type = 'month',
          count = 6,
          text = '6mth'
        ),
        list(type = 'ytd',
             text = 'YTD'),
        list(
          type = 'year',
          count = 1,
          text = '1y'
        ),
        list(type = 'all',
             text = 'All')
      )) %>%
      hc_credits(text = 'Source: Online Labour Index',
                 enabled = TRUE,
                 href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
                 position = list(align = "right"))
    
    
  })
  
  output$global_trends_group_by_UI <- renderUI({
    selectInput(
      "global_trends_group_by",
      "Group By",
      choices = list(
        "Top 5 countries (others aggregated by region)" = "country_group",
        "Occupation" = "occupation",
        "Top 20 countries" = "country"
      ),
      width = "100%"
    )
  })
  
  # output$global_trends_stack_by_UI <- renderUI({
  #   selectInput("global_trends_stack_by",
  #               "Stack By",
  #               choices = c("percent","number"))
  # })
  
  
  output$global_trends_stack_by_UI <- renderUI({
    radioButtons(
      "global_trends_stack_by",
      "",
      choices = c("Within group" = "percent", "Market share" = "normal"),
      selected = "Market share",
      width = "100%",
      inline = TRUE
    )
  })
  
  output$global_trends_stacked_bar_chart <- renderHighchart({
    if (is.null(input$global_trends_group_by)) {
      return()
    }
    
    x_axis <- input$global_trends_group_by
    
    if (x_axis == "occupation") {
      y_axis <- "country_group"
    } else
      y_axis <- "occupation"
    
    ## Sum by occupation and region
    
    switch (
      x_axis,
      "country" = {
        y_axis_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(x_axis) %>%
          summarise(total = sum(count)) %>%
          arrange(desc(total)) %>%
          select_(x_axis) %>%
          unlist(use.names = FALSE) %>%
          .[1:20]
        
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          
          group_by_(x_axis, y_axis) %>%
          summarise(total = sum(count)) %>% {
            
            
            foo <- .
            foo$total <- 100 * {foo$total / sum(foo$total)}
            as_data_frame(foo)
            
          } %>%
          filter(country %in% y_axis_order) %>%
          spread_(y_axis, "total") %>%
          rename(x_axis = country) %>%
          as.data.frame()
        
        bar_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          filter(country %in% y_axis_order) %>%
          group_by_(y_axis) %>%
          mutate(mean = mean(count)) %>%
          select(occupation, mean) %>%
          group_by(occupation) %>%
          arrange(mean) %>%
          select(occupation) %>%
          ungroup() %>%
          unique() %>%
          unlist(use.names = F)
        
        
        hc <- stacked_bar_chart(
          data = prepared_data[match(y_axis_order, prepared_data$x_axis),],
          categories_column = "x_axis",
          measure_columns = setdiff(colnames(prepared_data), c("x_axis")),
          stacking_type = input$global_trends_stack_by,
          explicit_order = {match(setdiff(colnames(prepared_data),"x_axis"), bar_order)-1}
        ) %>% hc_chart(zoomType = "x",
                       panning = TRUE,
                       panKey = 'shift') %>%
          hc_tooltip(
            formatter = JS(
              "function() {return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            )
          )
        
        if(input$global_trends_stack_by == "percent"){
          hc %>% hc_yAxis(max = 100)
        } else {
          hc
        }
      },
      "occupation" = {
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(x_axis, y_axis) %>%
          summarise(total = sum(count)) %>% {
            
            
            foo <- .
            foo$total <- 100 * {foo$total / sum(foo$total)}
            as_data_frame(foo)
            
          } %>%
          spread_(y_axis, "total") %>%
          rename_("x_axis" = x_axis) %>%
          as.data.frame(., stringAsFactors = FALSE)
        
        y_axis_order <- prepared_data %>%
          colnames() %>%
          setdiff("x_axis")
        
        
        
        
        occupation_axis_order <- prepared_data %>% gather(x_axis, x) %>% .[,c(1,3)] %>%
          group_by(x_axis) %>%
          mutate(total = sum(x)) %>%
          arrange(desc(total)) %>%
          ungroup() %>%
          select(x_axis) %>%
          unique() %>%
          unlist(use.names = F)
        
        hc <- stacked_bar_chart(
          data = prepared_data[match(occupation_axis_order, prepared_data$x_axis),],
          categories_column = "x_axis",
          measure_columns = setdiff(colnames(prepared_data), c("x_axis")),
          stacking_type = input$global_trends_stack_by,
          ordering_function = mean,
          explicit_order = match(y_axis_order, rev(
            c(
              "United States",
              "Canada",
              "other Americas",
              "United Kingdom",
              "other Europe",
              "Australia",
              "India",
              "other Asia and Oceania",
              "all Africa"
            )
          )) - 1
          # explicit_order = 0:8
        ) %>% hc_chart(zoomType = "x",
                       panning = TRUE,
                       panKey = 'shift') %>%
          # hc_yAxis(max = 100) %>%
          hc_tooltip(
            formatter = JS(
              "function() {return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            )
          )
        
          if(input$global_trends_stack_by == "percent"){
            hc %>% hc_yAxis(max = 100)
          } else {
            hc
          }
        
        
        
      },
      "country_group" = {
        # y_axis_order <- gig_economy_by_boundary %>%
        #   group_by_(x_axis) %>%
        #   summarise(total = sum(count)) %>%
        #   arrange(desc(total)) %>%
        #   select_(x_axis) %>%
        #   unlist(use.names = FALSE)
        
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(x_axis, y_axis) %>%
          summarise(total = sum(count)) %>% {
            
            
            foo <- .
            foo$total <- 100 * {foo$total / sum(foo$total)}
            as_data_frame(foo)
            
          } %>%
          spread_(y_axis, "total") %>%
          rename_("x_axis" = x_axis) %>%
          as.data.frame()
        
        bar_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(y_axis) %>%
          mutate(mean = mean(count)) %>%
          select(occupation, mean) %>%
          group_by(occupation) %>%
          arrange(mean) %>%
          select(occupation) %>%
          ungroup() %>%
          unique() %>%
          unlist(use.names = F)
        
        hc <- stacked_bar_chart(
          data = prepared_data[match(
            c(
              "United States",
              "Canada",
              "other Americas",
              "United Kingdom",
              "other Europe",
              "Australia",
              "India",
              "other Asia and Oceania",
              "all Africa"
            ),
            
            prepared_data$x_axis
          ),],
          categories_column = "x_axis",
          measure_columns = setdiff(colnames(prepared_data), c("x_axis")),
          stacking_type = input$global_trends_stack_by,
          explicit_order = {match(setdiff(colnames(prepared_data),"x_axis"), bar_order)-1}
        ) %>% hc_chart(zoomType = "x",
                       panning = TRUE,
                       panKey = 'shift') %>%
          hc_tooltip(
            formatter = JS(
              "function() {return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            )
          )
        
        if(input$global_trends_stack_by == "percent"){
          hc %>% hc_yAxis(max = 100)
        } else {
          hc
        }
      }
    )
    
  })
  
})