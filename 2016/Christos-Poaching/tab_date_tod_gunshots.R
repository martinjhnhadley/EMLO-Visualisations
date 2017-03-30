## Date / Time of Day / Gunshots

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
    select(gunscore, date, tod) %>%
    group_by(tod, date) %>%
    mutate(number.of.shots = n()) %>%
    ungroup() %>%
    select(number.of.shots) %>%
    sum()
  
  
  
  wellPanel(
    paste("Total Animals killed in selected time period (73.9% accuracy assumed):", prettyNum(round(0.739 * sum(gunshots)),big.mark=",",scientific=FALSE))
  )
  
})


output$calendar_surface_plotly <- renderPlotly({
  
  if(is.null(input$calendar_heatmap_timeperiod)){
    return()
  }
  
  time_of_day_heatmap <- knp_gunshots_intgraph_shiny %>%
    filter(date >= input$calendar_heatmap_timeperiod[1] &
             date <= input$calendar_heatmap_timeperiod[2]) %>%
    select(gunscore, date, tod) %>%
    group_by(tod, date) %>%
    mutate(number.of.shots = n()) %>%
    select(-gunscore) %>%
    # mutate(gunscore = sum(gunscore)) %>%
    ungroup() %>%
    unique()
    
  
  time_of_day_heatmap$date %>% duplicated() %>% sum()
  
  
  time_of_day_surface <- time_of_day_heatmap %>%
    spread(date, number.of.shots)
  
  
  time_of_day_surface <- time_of_day_surface %>%
    as.data.frame() %>%
    mutate(tod = unlist(lapply(time_of_day_surface$tod, pretty_int_hours)))
  
  
  time_of_day_surface[is.na(time_of_day_surface)] <- 0
  
  ## Reorder hours for plot
  time_of_day_surface <- time_of_day_surface[match(c("12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", 
                                                               "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", 
                                                               "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00"), time_of_day_surface$tod), ]
  
  ## Matrix needs rownames
  rownames(time_of_day_surface) <- time_of_day_surface$tod
  ## tod must be removed to make data square
  time_of_day_surface <- time_of_day_surface %>%
    select(-tod)
  
  ## plotly needs a matrix for surface plots
  time_of_day_surface <- time_of_day_surface %>%
    as.matrix()
  
  colorRampPalette(brewer.pal(9,"Reds"))(15)
  col2rgb("#FCAF93")[,1]
  
  plot_ly(
    y = rownames(time_of_day_surface),
    x = colnames(time_of_day_surface),
    z = time_of_day_surface
  ) %>%
    add_surface(
      # colorscale = colorRampPalette(brewer.pal(9,"Reds"))(15)
      colors = "Reds"
                ) %>%
    colorbar(title = "Number of Shots") %>%
    layout(scene = list(xaxis = list(title = "Month"),
                        yaxis = list(title = "Hour"),
                        zaxis = list(title = "Total Gunshots")))
  
 
})

output$calendar_heatmap_hc <- renderHighchart({
  
  if(is.null(input$calendar_heatmap_timeperiod)){
    return()
  }
  
  show("loading-calendar_heatmap_hc")
  
  time_of_day_heatmap <- knp_gunshots_intgraph_shiny %>%
    filter(date >= input$calendar_heatmap_timeperiod[1] &
             date <= input$calendar_heatmap_timeperiod[2]) %>%
    select(gunscore, date, tod) %>%
    group_by(tod, date) %>%
    mutate(number.of.shots = n()) %>%
    select(-gunscore) %>%
    # mutate(gunscore = sum(gunscore)) %>%
    unique() %>%
    ungroup()
  
  ## prettify hours, supply as factor with correct ordering for chart
  time_of_day_heatmap <- time_of_day_heatmap %>%
    mutate(time = factor(
      unlist(lapply(
        time_of_day_heatmap$tod, pretty_int_hours
      )),
      levels = c(
        "12:00",
        "13:00",
        "14:00",
        "15:00",
        "16:00",
        "17:00",
        "18:00",
        "19:00",
        "20:00",
        "21:00",
        "22:00",
        "23:00",
        "00:00",
        "01:00",
        "02:00",
        "03:00",
        "04:00",
        "05:00",
        "06:00",
        "07:00",
        "08:00",
        "09:00",
        "10:00",
        "11:00"
      )
    ))

  
  hc <- hchart(time_of_day_heatmap, "heatmap", 
         hcaes(x = date, y = time, value = number.of.shots)) %>%
    hc_yAxis(
      title = list(text = "Time of Day")
    ) %>%
    hc_xAxis(
      type = "datetime",
      title = list(text = "Date")
    ) %>%
    hc_chart(zoomType = "xy", pinchType = "xy") %>%
    hc_colorAxis(
      min = 0,
      # type = "logarithmic",
      minColor = "#fff5f0",
      maxColor = "#67000d") %>%
    hc_tooltip(
      formatter = JS(
        "function () {
        return '<b>' + 'Date: </b> ' + this.series.xAxis.categories[this.point.x] + '<br>' +
        '<b>Time of day: </b>' + this.point.y + ':00' + '<br>' +
        '<b>Number of gunshots: </b>' + this.point.value;
}"
      )
      ) %>%
    hc_legend(align = "right", layout = "vertical") %>%
    hc_chart(zoomType = "xy", pinchType = "xy")
  
  hide("loading-calendar_heatmap_hc")
  
  hc
  
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

output$month_by_hour_gunshots_surface <- renderPlotly({
  
  shots_by_month_then_hour <- knp_gunshots %>%
    filter(!is.na(tod)) %>%
    select(tod, month, total.gunshots.on.day) %>%
    group_by(month, tod) %>%
    mutate(shots.by.hour.and.month = n()) %>%
    select(-total.gunshots.on.day) %>%
    ungroup() %>%
    unique() %>%
    spread(month, shots.by.hour.and.month)
  
  shots_by_month_then_hour <- shots_by_month_then_hour %>%
    as.data.frame() %>%
    mutate(tod = unlist(lapply(shots_by_month_then_hour$tod, pretty_int_hours)))
  
  shots_by_month_then_hour[is.na(shots_by_month_then_hour)] <- 0
  
  ## Reorder hours for plot
  shots_by_month_then_hour <- shots_by_month_then_hour[match(c("12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", 
                                                               "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", 
                                                               "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00"), shots_by_month_then_hour$tod), ]
  
  ## Matrix needs rownames
  rownames(shots_by_month_then_hour) <- shots_by_month_then_hour$tod
  ## tod must be removed to make data square
  shots_by_month_then_hour <- shots_by_month_then_hour %>%
    select(-tod)
  
  ## Re-order columns by calendar order of months
  shots_by_month_then_hour <- shots_by_month_then_hour[ , c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")]
  
  ## plotly needs a matrix for surface plots
  shots_matrix <- shots_by_month_then_hour %>%
    as.matrix()
  
  plot_ly(
    y = rownames(shots_matrix),
    x = colnames(shots_matrix),
    z = shots_matrix
  ) %>%
    add_surface(colors = "Reds") %>%
    colorbar(title = "Number of Shots") %>%
    layout(scene = list(xaxis = list(title = "Month"),
                        yaxis = list(title = "Hour"),
                        zaxis = list(title = "Total Gunshots")))
  
})
