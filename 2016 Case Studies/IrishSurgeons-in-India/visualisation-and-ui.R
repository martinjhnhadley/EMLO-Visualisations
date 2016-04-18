## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

## =========================== Section Title ====================================
## ==============================================================================

output$irelandmap <- renderLeaflet({
  m <- leaflet(place_of_birth) %>%
    addTiles() %>%
    setView(lat = 53.347778,
            lng = -6.259722,
            zoom = 6) %>%
    addCircleMarkers(
      lng = ~ Lon,
      lat = ~ Lat,
      radius = rescale(place_of_birth$latlong.location.tally, to = c(5, 20)),
      popup = paste0(
        "Location Name: ",
        as.character(place_of_birth$PoB..Town.or.Parish.),
        "<br/>",
        "Births at location: ",
        as.character(place_of_birth$latlong.location.tally)
      )
    )
  m
})

output$marker_click <- DT::renderDataTable({
  
  selected_marker <- paste(input$irelandmap_marker_click$lat,input$irelandmap_marker_click$lng)
  print(selected_marker)
  
  
  place_of_birth[place_of_birth$Lat == input$irelandmap_marker_click$lat &
                   place_of_birth$Lon == input$irelandmap_marker_click$lng,]
  
  # HTML(paste0(
  #   "<p>",
  #   "marker click lat:",
  #   input$irelandmap_marker_click$lat,
  #   "</p>"
  # ))
})

output$marker_mouseover <- renderUI({
  HTML(paste0(
    "<p>",
    "marker mouseover lat:",
    input$irelandmap_marker_mouseover$lat,
    "</p>"
  ))
})


output$selected_circle <- renderUI({
  # if (is.null(input$ireland_map_popup_click)) {
  #   return()
  # }
  # 
  print(input$irelandmap_hover)
  
  HTML(input$irelandpopup_click)
})