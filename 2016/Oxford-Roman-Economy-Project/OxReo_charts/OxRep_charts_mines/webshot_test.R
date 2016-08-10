foo_hchart <- highchart() %>%
  hc_chart(type = "bar", zoomType = "xy", panning = TRUE) %>%
  hc_xAxis(categories = c("a","b","c")) %>%
  hc_add_series(name = "Mines in Location", data = c(10,12,10)) %>%
  hc_title(text = paste0("Observations per"))

saveWidget(as.widget(foo_hchart),"hc_chart.html")

webshot("hc_chart.html", file = "test.png",
        cliprect = "viewport")


plotly::