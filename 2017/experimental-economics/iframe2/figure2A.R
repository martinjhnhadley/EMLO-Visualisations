observeEvent(input$fig2A_reset, {
  shinyjs::reset("fig2A_controls")
})

output$fig2A_selected_continents_UI <- renderUI({
  selectInput(
    "fig2A_selected_continents",
    label = "Selected Continents",
    choices = unique(data_figure2A$continent),
    selected = unique(data_figure2A$continent),
    multiple = TRUE
  )
})

output$fig2A_selected_countries_UI <- renderUI({
  if (is.null(input$fig2A_selected_continents)) {
    return()
  }
  
  available_countries <- data_figure2A %>%
    filter(continent %in% input$fig2A_selected_continents) %>%
    select(country) %>%
    .[[1]] %>%
    unique()
  
  
  selectInput(
    "fig2A_selected_countries",
    label = "Countries to show",
    choices = available_countries,
    selected = NULL,
    multiple = TRUE
  )
})



output$fig2A_citation_selectize_UI <- renderUI({
  if (is.null(input$fig2A_selected_continents)) {
    return()
  }
  
  if (is.null(input$fig2A_selected_countries)) {
    available_citations <- data_figure2A %>%
      filter(continent %in% input$fig2A_selected_continents) %>%
      select(citation) %>%
      .[[1]] %>%
      unique()
  } else {
    available_citations <- data_figure2A %>%
      filter(country %in% input$fig2A_selected_countries) %>%
      select(citation) %>%
      .[[1]] %>%
      unique()
  }
  
  selectInput(
    "fig2A_citation_selectize",
    label = "Studies to show",
    choices = available_citations,
    selected = NULL,
    multiple = TRUE
  )
  ## If nothing selected, don't use!
})

data_fig2A_line_hc <- eventReactive(
  c(
    input$fig2A_incentive_range,
    input$fig2A_population_options,
    input$fig2A_citation_selectize,
    input$fig2A_selected_countries,
    input$fig2A_selected_continents
  ),
  {
    data_figure2A <- data_figure2A %>%
      filter(
        payoff_mm >= as.numeric(input$fig2A_incentive_range[1]) &
          payoff_mm <= as.numeric(input$fig2A_incentive_range[2])
      )
    
    
    switch(
      input$fig2A_population_options,
      "0" = {
        data_figure2A <- data_figure2A %>%
          filter(student == 0)
      },
      "1" = {
        data_figure2A <- data_figure2A %>%
          filter(student == 1)
      },
      "2" = {
        data_figure2A <- data_figure2A
      }
    )
    
    if (!is.null(input$fig2A_citation_selectize)) {
      data_figure2A <- data_figure2A %>%
        filter(citation %in% input$fig2A_citation_selectize)
    }
    
    
    print("here")
    data_figure2A
    
  },
  ignoreInit = FALSE
)

output$fig2A_line_hc <- renderHighchart({
  if (is.null(input$fig2A_selected_continents)) {
    return()
  }
  
  
  data_fig2A_line_hc <- data_fig2A_line_hc()
  
  if (is.null(input$fig2A_selected_countries)) {
    print("hello")
    selected_countries <- data_fig2A_line_hc %>%
      filter(continent %in% input$fig2A_selected_continents) %>%
      select(country) %>%
      .[[1]] %>%
      unique()
  } else {
    selected_countries <- data_fig2A_line_hc %>%
      filter(country %in% input$fig2A_selected_countries) %>%
      select(country) %>%
      .[[1]] %>%
      unique()
  }
  
  data_fig2A_line_hc <- data_fig2A_line_hc %>%
    filter(country %in% selected_countries)
  
  hchart(
    data_fig2A_line_hc,
    "scatter",
    hcaes(
      x = standardized_report_per_round,
      y = percent,
      group = treatment,
      color = colour,
      lineColor = colour,
      name = country
    ),
    # marker = list(
    lineWidth = 1
    # libneColor = "red"
    # )
  ) %>%
    hc_tooltip(
      formatter = JS(
        "function (){
        console.log(this);
        return '<b>Citation: </b>' + this.point.citation +
        '<br/>' +
        '<b>Treatment: </b>' + this.point.treatment +
        '<br/>' +
        '<b>Percent: </b>' + Highcharts.numberFormat(this.point.y, 2);
}"
        )
      )
  
  })
