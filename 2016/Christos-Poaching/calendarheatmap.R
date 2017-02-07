output$calendar_heatmap_timeperiod_UI <- renderUI({
  
  sliderInput("calendar_heatmap_timeperiod",
              label = "Time range of interest",
              min = min(knp_gunshots_intgraph_shiny$date),
              max = max(knp_gunshots_intgraph_shiny$date),
              value =c(min(knp_gunshots_intgraph_shiny$date), max(knp_gunshots_intgraph_shiny$date)))
  
})

output$calendar_heatmap_animals_killed_UI <- renderUI({
  
  if(is.null(input$calendar_heatmap_timeperiod)){
    return()
  }
  
  gunshots <- knp_gunshots_intgraph_shiny %>%
    filter(date >= input$calendar_heatmap_timeperiod[1] & 
             date <= input$calendar_heatmap_timeperiod[2]) %>%
    select(gunscore) %>%
    .[[1]]
  
  wellPanel(
    paste("Total Animals killed in selected time period (72% accuracy assumed):", round(0.72 * sum(gunshots)))
  )
  
})

output$calendar_heatmap_hc <- renderHighchart({
  
  if(is.null(input$calendar_heatmap_timeperiod)){
    return()
  }
  
  time_of_day_heatmap <- knp_gunshots_intgraph_shiny %>%
    select(gunscore, date, tod) %>%
    filter(date >= input$calendar_heatmap_timeperiod[1] & 
             date <= input$calendar_heatmap_timeperiod[2]) %>%
    group_by(tod, date) %>%
    mutate(gunscore = sum(gunscore)) %>%
    unique() %>%
    ungroup()
  
  hchart(time_of_day_heatmap, "heatmap", hcaes(x = date, y = tod, value = gunscore)) %>%
    hc_tooltip(
      formatter = JS(
        "function () {
        return '<b>' + 'Date: </b> ' + this.series.xAxis.categories[this.point.x] + '<br>' +
        '<b>Time of day: </b>' + this.point.y + ':00' + '<br>' +
        '<b>Gunscore: </b>' + this.point.value;
}"
      )
      ) %>%
    hc_legend(align = "right", layout = "vertical") %>%
    hc_yAxis(
      min = 0,
      max = 23,
      title = list(text = "Time of Dat")
    ) %>%
    hc_xAxis(
      title = list(text = "Date")
    ) %>%
    hc_chart(zoomType = "xy", pinchType = "xy")

})

output$calendar_weekdays_hc <- renderHighchart({
  
  weekdays_barchart <- knp_gunshots_intgraph_shiny %>%
    select(gunscore, weekday, date) %>%
    group_by(weekday) %>%
    filter(date >= input$calendar_heatmap_timeperiod[1] & 
             date <= input$calendar_heatmap_timeperiod[2]) %>%
    mutate(shots.per.weekday = n()) %>%
    ungroup() %>%
    select(-gunscore, -date) %>%
    unique() %>%
    mutate(shots.per.weekday = 100 * {shots.per.weekday / sum(shots.per.weekday)})
  
  weekdays_barchart <- weekdays_barchart[order(as.numeric(mapvalues(
    weekdays_barchart$weekday,
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    1:7
  ))), ]
  
  
  hchart(weekdays_barchart, "bar", hcaes(x = weekday, y = shots.per.weekday)) %>%
    hc_tooltip(
      formatter = JS(
        "function () {
        return '<b>' + 'Day of Week:</b> ' + this.point.name + '<br>' +
        '<b>% of shots fired on this day: </b>' + Highcharts.numberFormat(this.point.y,2) + '%';
}"
      )
      ) %>%
    hc_yAxis(title = list(text = "% of gunshots")) %>%
    hc_xAxis(title = list(text = ""))
  
})


