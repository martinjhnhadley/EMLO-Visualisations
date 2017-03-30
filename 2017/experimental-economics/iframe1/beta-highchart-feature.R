## Shiny click-events were not implemented in highcharter 0.5.0
## This checks for hc_add_event_point and if it doesn't exist initialises
## Check here for further details https://github.com/jbkunst/highcharter/issues/48

if (exists("hc_add_event_point", where = "package:highcharter", mode = "function")) {
  return() # function already exists
} else {
  hc_add_event_point <-
    function(hc,
             series = "series",
             event = "click") {
      fun <- paste0(
        "function(){
        var pointinfo = {series: this.series.name, seriesid: this.series.id,
        name: this.name, x: this.x, y: this.y, category: this.category.name}
        window.x = this;
        console.log(pointinfo);
        
        if (typeof Shiny != 'undefined') { Shiny.onInputChange(this.series.chart.renderTo.id + '_' + '",
        event,
        "', pointinfo); }
        }"
)
      
      fun <- JS(fun)
      
      eventobj <- structure(list(structure(list(
        structure(list(structure(
          list(fun),
          .Names = event
        )),
        .Names = "events")
      ),
      .Names = "point")),
      .Names = series)
      
      hc$x$hc_opts$plotOptions <- rlist::list.merge(hc$x$hc_opts$plotOptions,
                                                    eventobj)
      
      hc
      
      
        }
}
