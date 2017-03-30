library(shiny)
library(highcharter)
library(shinyjs)
library(shinyBS)

shinyUI(
  navbarPage(
    useShinyjs(),
    "Experimental Economics",
    tabPanel(
      "Average report by incentive level",
      fluidPage(
        fluidRow(
          column(actionButton("fig2A_reset", "Back to full sample"),
                 width = 3),
          div(id = "fig2A_controls", # div provides the target for the reset button,
              fluidPage(
                column(radioButtons("fig2A_population_options",
                                    label = "Students vs General Population",
                                    choices = list("Both" = 2, "Students" = 1, "General Population" = 0),
                                    selected = 2, inline = TRUE), width = 4),
                column(uiOutput("fig2A_selected_continents_UI"),
                       width = 5)),
              bsCollapsePanel(HTML(
                paste0(
                  '<span class="glyphicon glyphicon-plus" aria-hidden="true"></span>',
                  "Additional Filters"
                )
              ),
              fluidRow(
                column(
                  # uiOutput("fig2A_selected_continents_UI"),
                  uiOutput("fig2A_selected_countries_UI"),
                  uiOutput("fig2A_citation_selectize_UI"),
                  width = 6),
                column(sliderInput("fig2A_incentive_range",
                                   label = "Incentive Range",
                                   min = 0,
                                   max = 60,
                                   value = c(0, 60),
                                   step = 10), width = 6)),
              wellPanel("Drag a rectangle to zoom"), style = "primary"))),
        highchartOutput("fig2A_line_hc")
      )
      
    ),
    # tabPanel(
    #   "Distribution of reports by incentive level",
    #   fluidPage(
    #     highchartOutput("figA2_hc", height = "500px")
    #   )
    # ),
    tabPanel(
      "About",
      fluidPage(
        includeMarkdown(knitr::knit("tab_about.Rmd"))
      )
    )
  )
)