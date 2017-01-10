## From http://stackoverflow.com/questions/31944667/using-javascript-to-configure-a-googlevis-event-listener-in-shiny

library(shiny)
library(googleVis)
server <- function(input, output) {
  output$dates_plot <- renderGvis({
    gvisCalendar(Cairo,
                 options = list(
                   width="100%",
                   colorAxis = "{
                   colors: ['E9967A', 'A52A2A']
  }",
                                     gvis.listener.jscode = "
                   var selected_date = data.getValue(chart.getSelection()[0].row,0);
                   var parsed_date = selected_date.getFullYear()+'-'+(selected_date.getMonth()+1)+'-'+selected_date.getDate();
                   Shiny.onInputChange('selected_date',parsed_date)"
                   )
                 )
})
  output$date <- renderText({
    input$selected_date
  })
  }

ui <- shinyUI(fillPage( 
  uiOutput("dates_plot", width = "100%s"),
  textOutput("date")
))

shinyApp(ui = ui, server = server)