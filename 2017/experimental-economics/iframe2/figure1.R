observeEvent(input$fig1_reset, {
  shinyjs::reset("fig1_controls")
})

output$fig1_selected_continents_UI <- renderUI({
  selectInput(
    "fig1_selected_continents",
    label = "Selected Continents",
    choices = unique(data_figure1$continent),
    selected = unique(data_figure1$continent),
    multiple = TRUE
  )
})


add_country_bubbles <- function(hc, data, country_name) {
  hc %>%
    hc_add_series(
      data = data %>%
        filter(country == country_name),
      type = "bubble",
      hcaes(
        x = payoff_mm,
        y = standardized_report_per_round,
        size = subjects,
        color = colour
      ),
      name = country_name,
      color = country_colours[country_colours$country == country_name, "colour"][[1]]
    )
}

output$fig1_hc_bubble <- renderHighchart({

  if(is.null(input$fig1_selected_continents)){
    return()
  }
  
  data_figure1 <- data_figure1 %>%
    filter(payoff_mm >= as.numeric(input$fig1_incentive_range[1]) &
             payoff_mm <= as.numeric(input$fig1_incentive_range[2]))
  
  switch(input$fig1_population_options,
         "0" = {
           data_figure1 <- data_figure1 %>%
             filter(student == 0)
         },
         "1" = {
           data_figure1 <- data_figure1 %>%
             filter(student == 1)
         },
         "2" = {
           data_figure1 <- data_figure1
         })
  
  selected_countries <- data_figure1 %>%
    filter(continent %in% input$fig1_selected_continents) %>%
    select(country) %>%
    .[[1]] %>%
    unique()
  
  hc <- highchart()
  invisible(lapply(selected_countries, function(x) {
    hc <<- hc %>% add_country_bubbles(data_figure1, x)
  }))
  
  hc %>% hc_yAxis(
    min = -1,
    max = 1,
    title = list(text = "Standardised Report")
  ) %>%
    hc_xAxis(
      max = 60,
      min = 0.08,
      type = "logarithmic",
      title = list(text = "Maximal payoff from misreporting (in 2015 USD)"),
      tickInterval = 0.4,
      minTickInterval = 0.4
    ) %>%
    hc_tooltip(
      formatter = JS(
        "function(){
        console.log(this);
        return '<b>Citation: </b>' + this.point.citation + '<br/>' +
        '<b>Treatment: </b>' + this.point.treatment + '<br/>' +
        '<b>Standardised Report: </b>' + Highcharts.numberFormat(this.point.y,2);}"
        
      )
    )
  
  })
