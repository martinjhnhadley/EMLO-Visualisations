## Calendar heatmap


calheatmap_data <- eventReactive({
  input$calendar_shots_or_patrols
},

switch(
  input$calendar_shots_or_patrols,
  "gunshots" = {
    knp_gunshots %>%
      select(date, total.gunshots.on.day) %>%
      unique() %>%
      filter(total.gunshots.on.day > 0)
  },
  "patrols" = {
    knp_patrols %>%
      select(date, total.patrols.on.day) %>%
      unique() %>%
      filter(total.patrols.on.day > 0)
  }
))

output$calheatmap_shots_DT <- renderDataTable({
  knp_gunshots %>%
    filter(date == input$calheatmap_select_day) %>%
    group_by(timing) %>%
    mutate(shots.detected = sum(gunscore)) %>%
    select(timing, shots.detected) %>%
    unique()
},
rownames = FALSE,
options = list(dom = 't'))

output$calheatmap_patrols_DT <- renderDataTable({
  knp_patrols %>%
    filter(date == input$calheatmap_select_day) %>%
    select(timing, total.patrols.on.day) %>%
    unique()
},
rownames = FALSE,
options = list(dom = 't'))

output$calheatmap_shot_summary <- renderUI({
  if (any(knp_gunshots$date == input$calheatmap_select_day)) {
    fluidRow(column(
      "Summary of shots fired:",
      dataTableOutput("calheatmap_shots_DT"),
      width = 12
    ))
  } else {
    fluidRow(column("no shots detected on that day", width = 12))
  }
})

output$calheatmap_patrol_summary <- renderUI({
  if (any(knp_patrols$date == input$calheatmap_select_day)) {
    fluidRow(column(
      "Summary of patrols:",
      dataTableOutput("calheatmap_patrols_DT"),
      width = 12
    ))
  } else {
    fluidRow(column("no patrols detected on that day", width = 12))
  }
})


output$calheatmap_select_day_UI <- renderUI({
  if (is.null(input$calheatmap_select_day)) {
    return()
  }
  
  fluidRow(column(
    strong(paste(
      "Date selected:", input$calheatmap_select_day
    )),
    uiOutput("calheatmap_shot_summary"),
    uiOutput("calheatmap_patrol_summary"),
    width = 12
  ))
  
})

output$calheatmap_gvis <- renderGvis({
  calheatmap_data() %>%
    calendar_heatmap(
      color.axis =
        "{minValue: 0,
      colors: ['#ffffd9',
      '#edf8b1',
      '#c7e9b4',
      '#7fcdbb',
      '#41b6c4',
      '#1d91c0'
      ],
      maxValue: 300}",
      cell.size = 10,
      cal.width = "600",
      cal.height = "450px"
    )
  })