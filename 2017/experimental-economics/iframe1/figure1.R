observeEvent(input$fig1_reset, {
  shinyjs::reset("fig1_controls")
})

output$fig1_selected_countries_UI <- renderUI({

  available_countries <- data_figure1 %>%
    select(country) %>%
    .[[1]] %>%
    unique()
  
  
  selectInput(
    "fig1_selected_countries",
    label = "Countries to show",
    choices = sort(available_countries),
    selected = NULL,
    multiple = TRUE,
    width = "100%"
  )
})

output$fig1_citation_selectize_UI <- renderUI({
  
  if (is.null(input$fig1_selected_countries)) {
    available_citations <- data_figure1 %>%
      select(citation) %>%
      .[[1]] %>%
      unique()
  } else {
    available_citations <- data_figure1 %>%
      filter(country %in% input$fig1_selected_countries) %>%
      select(citation) %>%
      .[[1]] %>%
      unique()
  }
  
  selectInput(
    "fig1_citation_selectize",
    label = "Studies to show",
    choices = sort(available_citations),
    selected = NULL,
    multiple = TRUE,
    width = "100%"
  )
})

output$fig1_selected_true_distribution_UI <- renderUI({
  selectInput(
    "fig1_selected_true_distribution",
    label = "Selected true distributions",
    choices = sort(unique(data_figure1$true_distribution_text)),
    selected = NULL,
    multiple = TRUE,
    width = "100%"
  )
})

data_fig1_hc_bubble <-
  eventReactive(
    c(
      input$fig1_population_options,
      input$fig1_citation_selectize,
      input$fig1_repeated_or_oneshot,
      input$fig1_location,
      input$fig1_controls_suggested,
      input$fig1_draw_or_mind,
      input$fig1_selected_true_distribution,
      input$fig1_selected_countries,
      citation
    ),
    {
      
      if(is.null(input$fig1_selected_countries)){
        data_figure1 <- data_figure1
      } else {
        data_figure1 <- data_figure1 %>%
          filter(country %in% input$fig1_selected_countries)
      }
      
      print("which countries to include")
      print(data_figure1 %>%
              select(country) %>%
              unique() %>%
              .[[1]])
      
      switch(
        input$fig1_population_options,
        "0" = {
          data_figure1 <- data_figure1 %>%
            filter(student == 0)
        },
        "1" = {
          data_figure1 <- data_figure1 %>%
            filter(student == 1)
        },
        "both" = {
          data_figure1 <- data_figure1
        }
      )
      print(paste("1", nrow(data_figure1)))
      switch(
        input$fig1_repeated_or_oneshot,
        "0" = {
          data_figure1 <- data_figure1 %>%
            filter(repeated == 0)
        },
        "1" = {
          data_figure1 <- data_figure1 %>%
            filter(repeated == 1)
        },
        "both" = {
          data_figure1 <- data_figure1
        }
      )
      print(paste("2", nrow(data_figure1)))
      switch(
        input$fig1_location,
        "0" = {
          data_figure1 <- data_figure1 %>%
            filter(remote == 0)
        },
        "1" = {
          data_figure1 <- data_figure1 %>%
            filter(remote == 1)
        },
        "both" = {
          data_figure1 <- data_figure1
        }
      )
      print(paste("3", nrow(data_figure1)))
      switch(
        input$fig1_controls_suggested,
        "0" = {
          data_figure1 <- data_figure1 %>%
            filter(control_rolls == 0)
        },
        "1" = {
          data_figure1 <- data_figure1 %>%
            filter(control_rolls == 1)
        },
        "both" = {
          data_figure1 <- data_figure1
        }
      )
      print(paste("4", nrow(data_figure1)))
      switch(
        input$fig1_draw_or_mind,
        "0" = {
          data_figure1 <- data_figure1 %>%
            filter(internal_lying == 0)
        },
        "1" = {
          data_figure1 <- data_figure1 %>%
            filter(internal_lying == 1)
        },
        "both" = {
          data_figure1 <- data_figure1
        }
      )
      print(paste("5", nrow(data_figure1)))
      
      if (nrow(data_figure1) == 0) {
        # no data
        return(data_frame())
      } else {
        if (!is.null(input$fig1_citation_selectize)) {
          data_figure1 <- data_figure1 %>%
            filter(citation %in% input$fig1_citation_selectize) %>%
            filter(country %in% input$fig1_selected_countries)
        }
      }
      
      if (!is.null(input$fig1_selected_true_distribution)) {
        data_figure1 <- data_figure1 %>%
          filter(true_distribution_text %in% input$fig1_selected_true_distribution) %>%
          filter(country %in% input$fig1_selected_countries)
      }
      
      data_figure1
      
      
    },
    ignoreNULL = TRUE
  )

output$fig1_hc_bubble_UI <- renderUI({
  print(class(highchartOutput("fig1_hc_bubble")))
  show("loading-fig1")
  if (is.null(highchartOutput("fig1_hc_bubble"))) {
    "empty"
  } else {
    highchartOutput("fig1_hc_bubble")
  }
  
})

output$fig1_hc_bubble <- renderHighchart({

  print("print data_fig1_hc_bubble() below")
  print(nrow(data_fig1_hc_bubble()) == 0)
  
  
  if (nrow(data_fig1_hc_bubble()) == 0) {
    hide("loading-fig1")
    return(highchart())
  }
  
  data_fig1_hc_bubble <- data_fig1_hc_bubble()
  
  ## ==== FIX ME
  
  # if (is.null(input$fig1_selected_countries)) {
  #   selected_citations <- data_fig1_hc_bubble %>%
  #     select(citation) %>%
  #     .[[1]] %>%
  #     unique()
  # } else {
  #   selected_citations <- data_fig1_hc_bubble %>%
  #     filter(country %in% input$fig1_selected_countries) %>%
  #     select(citation) %>%
  #     .[[1]] %>%
  #     unique()
  # }
  # 
  
  
  hc <- highchart() %>%
    hc_add_series(
      data = data_fig1_hc_bubble,
      type = "bubble",
      hcaes(
        x = payoff_mm,
        y = standardized_report_per_round,
        size = subjects,
        color = color
      ),
      showInLegend = FALSE
    )
  
  hide("loading-fig1")
  
  hc %>% hc_yAxis(
    min = -1,
    max = 1,
    lineWidth = 1,
    lineColor = "black",
    gridLines = FALSE,
    gridLineWidth = 0,
    title = list(text = "Standardised Report"),
    plotLines = list(list(
      value = 0,
      width = 2,
      color = "black",
      dashStyle = "Dash"
    ))
  ) %>%
    hc_xAxis(
      max = 70,
      min = 0.03,
      lineColor = "black",
      type = "logarithmic",
      title = list(text = "Maximal payoff from misreporting (in 2015 USD)"),
      tickInterval = 0.4,
      minTickInterval = 0.4
    ) %>%
    hc_tooltip(
      formatter = JS(
        "function(){
        console.log(this);
        return '<b><a href=http://google.co.uk>Study:</a> </b>' + this.point.citation + '<br/>' +
        '<b>Treatment: </b>' + this.point.treatment + '<br/>' +
        '<b>Country: </b>' + this.point.country + '<br/>' +
        '<b>Standardised Report: </b>' + Highcharts.numberFormat(this.point.y,2) + '<br/>';}"
        
      )
    ) %>%
    hc_chart(zoomType = "xy", pinchType = "xy") %>%
    hc_add_event_point(event = "click")
  
  })


observeEvent(input$fig1_hc_bubble_click,
             {
               toggleModal(session, "bubbleModal", toggle = "toggle")
             })

output$bubble_model_UI <- renderUI({
  data_fig1_hc_bubble <- data_fig1_hc_bubble()
  x_coord <- input$fig1_hc_bubble_click$x
  y_coord <- input$fig1_hc_bubble_click$y
  
  study <- data_fig1_hc_bubble %>%
    filter(
      standardized_report_per_round == input$fig1_hc_bubble_click$y &
        payoff_mm == input$fig1_hc_bubble_click$x
    )
  
  url <- study[["weblink"]]
  
  if (is.na(url)) {
    weblink <- p(strong("Weblink: "), "No link provided!")
  } else {
    weblink <-
      p(strong("Weblink: "),
        tags$a(href = url, url, target = "_blank"))
  }
  
  fluidPage(p(strong("Citation: "), study[["citation"]]),
            p(strong("Country: "), study[["country"]]),
            p(strong("Number of subjects: "), study[["subjects"]]),
            weblink)
})
