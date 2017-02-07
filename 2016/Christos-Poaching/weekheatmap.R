output$weekheatmap_selected_sensors_UI <- renderUI(
  checkboxGroupInput("weekheatmap_selected_sensors", label = "Selected Sensors",
                     choices = unique(weekly_patrols_and_gunshots$sensor),
                     selected = unique(weekly_patrols_and_gunshots$sensor),
                     inline = TRUE
                     )
)

weekheatmap_data <- eventReactive(c(
  input$weekheatmap_years_selected,
  input$weekheatmap_time_of_shot,
  input$weekheatmap_selected_sensors
),
{
  hc_data <- weekly_patrols_and_gunshots %>%
    filter(year %in% input$weekheatmap_years_selected &
             sensor %in% input$weekheatmap_selected_sensors)
  
  if(nrow(hc_data) == 0){
    print("kill")
    return(data.frame())
  }
  print("continue")
  switch(
    input$weekheatmap_time_of_shot,
    "All Shots" = {
      hc_data <- hc_data %>%
        select(rainfall, shotstotalpday, patrol.grid.total) %>%
        rename(shots = shotstotalpday)
    },
    "Diurnal Shots" = {
      hc_data <- hc_data %>%
        select(rainfall, shotsdiurnpday, patrol.grid.total) %>%
        rename(shots = shotsdiurnpday)
    },
    "Noctural Shots" = {
      hc_data <- hc_data %>%
        select(rainfall, shotsnoctpday, patrol.grid.total) %>%
        rename(shots = shotsnoctpday)
    }
  )
  print(hc_data)
  hc_data[is.na(hc_data)] <- 0
  
  ## Bin rainfall and patrol
  hc_data <- hc_data %>%
    unique() %>%
    mutate(rainfall.range = cut(
      rainfall,
      breaks = seq(0, 130, 10),
      labels = paste0(seq(0, 120, 10), "-", seq(10, 130, 10)),
      include.lowest = TRUE
    )) %>%
    mutate(
      patrol.range = cut(
        patrol.grid.total,
        breaks = seq(0, 120000, 10000),
        labels = paste0(format(seq(0, 110000, 10000)), "-", format(seq(10000, 120000, 10000))),
        include.lowest = TRUE
      )
    ) %>%
    select(shots, rainfall.range, patrol.range)
  
  hc_data <- hc_data %>%
    group_by(rainfall.range, patrol.range) %>%
    mutate(mean.shots = mean(shots)) %>%
    select(-shots) %>%
    ungroup() %>%
    unique()
  
  hc_data
  
})


output$weekheatmap_hc <- renderHighchart({
  
  weekheatmap_data <- weekheatmap_data()
  
  if(nrow(weekheatmap_data) == 0 ){
    return()
  }
  
  hchart(weekheatmap_data, "heatmap", hcaes(x = rainfall.range, y = patrol.range, value = mean.shots)) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, valueDecimals = 2, 
                          format = "{point.value:.2f}"
        )
      )) %>%
    hc_tooltip(
      formatter = JS(
        "function () {
        return '<b>' + 'Rainfall (mm):</b> ' + this.series.xAxis.categories[this.point.x] + '<br>' +
        '<b>Patrol (km): </b>' + this.series.yAxis.categories[this.point.y] + '<br>' +
        '<b>Mean gunshots: </b>' + Highcharts.numberFormat(this.point.value,4);
}"
      )
      ) %>%
    hc_legend(align = "right", layout = "vertical") %>%
    hc_xAxis(title = list(text = "Rainfall (mm)")) %>%
    hc_yAxis(title = list(text = "Patrols (km)")) %>%
    hc_credits(
      text = 'Data from WildCRU',
      enabled = TRUE,
      href = 'https://wildcru.org'
    ) %>%
    hc_title(text = "Mean gunshots") %>%
    hc_xAxis(gridLineWidth = 1)
})

output$weekheatmap_hc_display <- renderUI({
  weekheatmap_data <- weekheatmap_data()
  
  if(nrow(weekheatmap_data) > 0){
    print("show")
    highchartOutput("weekheatmap_hc", height = "500px")
    
  } else {
    wellPanel("No gunshots were recorded matching your current settings")
  }
  
})


