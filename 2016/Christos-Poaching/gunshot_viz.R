library(shiny)
library(googleVis)
server <- function(input, output) {
  output$dates_plot <- renderGvis({
    gvisCalendar(Cairo,
                 options = list(
                   colorAxis = "{
                   minValue: 0,  
                   colors: ['E9967A', 'A52A2A']
  }",
                                     gvis.listener.jscode = "
                   var selected_date = data.getValue(chart.getSelection()[0].row,0);
                   var parsed_date = selected_date.getFullYear()+'-'+(selected_date.getMonth()+1)+'-'+selected_date.getDate();
                   Shiny.onInputChange('selected_date',parsed_date)")
                 ,
                 options = list(
                   colorAxis = "{
                   minValue: 0,  
                   colors: ['E9967A', 'A52A2A']
  }",
                                     gvis.listener.jscode = "
                   var selected_date = data.getValue(chart.getSelection()[0].row,0);
                   var parsed_date = selected_date.getFullYear()+'-'+(selected_date.getMonth()+1)+'-'+selected_date.getDate();
                   Shiny.onInputChange('selected_date',parsed_date)")
                 )
})
  output$date <- renderText({
    input$selected_date
  })
  }

ui <- shinyUI(fluidPage( 
  htmlOutput("dates_plot"),
  textOutput("date")
))

shinyApp(ui = ui, server = server)