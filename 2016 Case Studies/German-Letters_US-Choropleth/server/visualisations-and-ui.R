## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================ Timeline UI Elements ==========================================
## ================================================================================

output$time_period_of_interest_UI <- renderUI({
  if (is.null(input$show_timeslider)) {
    return()
  }
  
  if (is.null(input$show_letters_where_receive_unknown)) {
    return()
  }
  
  dates <- entries_with_locations$Date[!is.na(entries_with_locations$Date)]
  
  if (input$show_timeslider == TRUE) {
    sliderInput(
      "time_period_of_interest",
      "Time period of interest:",
      min = min(dates),
      max = max(dates),
      step = 1,
      value = c(min(dates), max(dates)),
      width = "800px",
      timeFormat = "%F"
    )
  }
  
})

output$show_timeslider_UI <- renderUI({
  checkboxInput("show_timeslider", label = "Filter letters by date sent?", value = TRUE)
})

output$show_letters_where_receive_unknown_UI <- renderUI({
  checkboxInput("show_letters_where_receive_unknown",
                label = "Include letters where receive location unknown?",
                value = TRUE)
})

## ================ Letter Filters (Dates etc) ====================================
## ================================================================================


output_of_input <- "1834-04-12"

entries_with_locations$Date >= "1834-04-12"

subset(entries_with_locations,
       Date >= "1834-04-12",
       Date <= "1900-04-12")

letters_sent_between_dates <-
  function(start.year = NA,
           end.year = NA,
           data = NA) {
    if (input$show_letters_where_receive_unknown) {
      letters.for.analysis <- data
    } else {
      letters.for.analysis <-
        data[data$Sender.LatLong.String != "NA NA" &
               data$Receiver.LatLong.String != "NA NA",]
    }
    
    letters.for.analysis <- data
    if (input$show_timeslider == TRUE) {
    
    letters.for.analysis <- letters.for.analysis[!is.na(entries_with_locations$Date),]
    
    # letters.for.analysis$Date <- as.POSIXct(letters.for.analysis$Date)
    letters.for.analysis <- subset(letters.for.analysis,
                                   Date >= start.year &
                                     Date <= end.year)
    
    letters.for.analysis
    
    } else
      letters.for.analysis
    
  }


