## calendar_heatmap

calendar_heatmap <- function(data,
                             cal.width = "700px",
                             cal.height = "500px",
                             cell.size = 12,
                             color.axis = "{minValue: -1,  colors: ['#FFFFFF', '#00FF00']}") {
  gvisCalendar(
    data,
    options = list(
      width = cal.width,
      height = cal.height,
      colorAxis = color.axis,
      # noDataPattern = "{
      #   backgroundColor: '#76a7fa',
      #   color: '#a0c3ff'
      # }",
      calendar=paste0("{ cellSize: ",cell.size," }"),
      legend = "{
        position: 'bottom',
        textStyle: {
          color: 'black',
          fontSize: 16
        }
      }",
      gvis.listener.jscode = "
      var selected_date = data.getValue(chart.getSelection()[0].row,0);
      var parsed_date = selected_date.getFullYear()+'-'+(selected_date.getMonth()+1)+'-'+selected_date.getDate();
      Shiny.onInputChange('calheatmap_select_day',parsed_date)"
    )
    )
  
}

calendar_year_by_year <- function(x){}