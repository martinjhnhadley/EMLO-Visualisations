library(shiny)
library(highcharter)
library(shinyjs)
library(shinyBS)

shinyUI(navbarPage(
  useShinyjs(),
  "Experimental Economics",
  tabPanel(
    "Average report by incentive level",
    fluidPage(
      includeCSS("www/animate.min.css"),
      # provides pulsating effect
      includeCSS("www/loading-content.css"),
      fluidRow(
        column(
          actionButton("fig1_reset", "Back to full sample"),
          bsTooltip(
            "fig1_reset",
            "Click here to return the visualisation to our defaults",
            "bottom",
            options = list(container = "body")
          ),
          width = 3
        ),
        div(
          id = "fig1_controls",
          # div provides the target for the reset button,
          fluidPage(
            column(
              radioButtons(
                "fig1_population_options",
                label = "Students vs General Population",
                choices = list(
                  "Both" = "both",
                  "Students" = 1,
                  "General Population" = 0
                ),
                selected = "both",
                inline = TRUE
              ),
              width = 5
            ),
            column(
              uiOutput("fig1_selected_countries_UI"),
              bsTooltip(
                "fig1_selected_countries_UI",
                "Filter the experiments by continents by deleting/adding their names",
                "bottom",
                options = list(container = "body")
              ),
              width = 4
            )
          ),
          bsCollapsePanel(
            HTML(
              paste0(
                '<span class="glyphicon glyphicon-plus" aria-hidden="true"></span>',
                "&nbsp;Click here for more filters &nbsp;&nbsp;&nbsp; (and click on a circle to open a study or draw a square to zoom)"
              )
            ),
            fluidRow(
              column(
                uiOutput("fig1_citation_selectize_UI"),
                bsTooltip(
                  "fig1_citation_selectize_UI",
                  "Filter the experiments by citations by deleting/adding their names",
                  "bottom",
                  options = list(container = "body")
                ),
                uiOutput("fig1_selected_true_distribution_UI"),
                bsTooltip(
                  "fig1_selected_true_distribution_UI",
                  "Filter the experiments by true distribution.",
                  "bottom",
                  options = list(container = "body")
                ),
                radioButtons(
                  "fig1_location",
                  label = "Experiment location?",
                  choices = list(
                    "Both" = "both",
                    "Only lab" = 0,
                    "Only Online/Telephone" = 1
                  ),
                  selected = "both",
                  inline = TRUE
                ),
                width = 6
              ),
              column(
                radioButtons(
                  "fig1_controls_suggested",
                  label = "Control Rolls Suggested?",
                  choices = list(
                    "Both" = "both",
                    "Experimenter suggested control rolls" = 1,
                    "Experimenter did not suggest control rolls" = 0
                  ),
                  selected = "both",
                  inline = TRUE
                ),
                radioButtons(
                  "fig1_draw_or_mind",
                  label = "Was random draw or state of mind reported?",
                  choices = list(
                    "Both" = "both",
                    "Reporting random draw" = 1,
                    "Reporting State of Mind" = 0
                  ),
                  selected = "both",
                  inline = TRUE
                ),
                radioButtons(
                  "fig1_repeated_or_oneshot",
                  label = "Repeated vs. One-shot Reporting",
                  choices = list(
                    "Both" = "both",
                    "Only Repeated" = 1,
                    "Only One-shot Reporting" = 0
                  ),
                  selected = "both",
                  inline = TRUE
                ),
                width = 6
              )
              
            ),
            style = "primary"
          )
        )
      ),
      div(
        id = "loading-fig1",
        class = "loading-content",
        h2(class = "animated infinite pulse", "Loading data...")
      ),
      # highchartOutput("fig1_hc_bubble")
      uiOutput("fig1_hc_bubble_UI"),
      bsModal(
        "bubbleModal",
        "Study Info",
        trigger = "fig1_hc_bubble_click",
        size = "large",
        uiOutput("bubble_model_UI")
      )
    )
  ),
  tabPanel(
    "Distribution of reports by incentive level",
    fluidPage(highchartOutput("figA2_hc", height = "500px"))
  ),
  tabPanel("About",
           fluidPage(includeMarkdown(
             knitr::knit("tab_about.Rmd")
           )))
))