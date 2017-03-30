output$figA2_hc <- renderHighchart({

highchart() %>%
  hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>%
  hc_add_series(
    data = data_figureA2,
    type = "line",
    hcaes(x = standardized_report_per_round,
          y = percent_1),
    name = "Quartile 1"
  ) %>%
  hc_add_series(
    data = data_figureA2,
    type = "line",
    hcaes(x = standardized_report_per_round,
          y = percent_2),
    name = "Quartile 2"
  ) %>%
  hc_add_series(
    data = data_figureA2,
    type = "line",
    hcaes(x = standardized_report_per_round,
          y = percent_3),
    name = "Quartile 3"
  ) %>%
  hc_add_series(
    data = data_figureA2,
    type = "line",
    hcaes(x = standardized_report_per_round,
          y = percent_4),
    name = "Quartile 4"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    valueDecimals = 2,
    formatter = JS(
      "function () {
      var s = '<b>' + 'Incentive Level: ' + Highcharts.numberFormat(this.x,2) + '</b>';
      $.each(this.points, function () {
      s += '<br/>' + '<b>' + this.series.name + ': </b>' + Highcharts.numberFormat(this.point.y,2) + '%';
      });
      return s;}"
    )
    ) %>%
  hc_xAxis(title = list(text = "Standardised Report"),
           gridLineWidth = 0,
           min = -1,
           # endOnTick = TRUE,
           # startOnTick = TRUE,
           max = 1,
           # minTickInterval = 0.4,
           # minorTickInterval = 0.4,
           tickInterval = 0.4,
           floor = -1,
           softMax = 1
           # tickAmount = 6
           # ticks = c(-1, -0.6, -.2, .2, .6, 1)
           ) %>%
  hc_yAxis(
    title = list(text = "Percent"),
    plotLines = list(list(
      value = 16,
      gridLineWidth = 0,
      width = 2,
      color = "black",
      dashStyle = "Dash"
    ))
  )
})
