# Large Shiny Apps with NavbarPage Layout

![Screenshot of app](https://raw.githubusercontent.com/martinjhnhadley/Rmd-and-Shiny-Templates/master/Shiny-Templates/Large-Apps/navbarMenu/screenshot.png "Optional Title")

This template is useful for the development of large Shiny apps utilising the `navbarPage` layout, the features are as follows:

- `navbarMenu` items are contained within distinct `.R` files
- Visualisations/UI and data processing are separated into distinct folders
- Anchor links to individual pages (requires javascript and takes ~3 second to redirect) thanks to https://github.com/rstudio/shiny/issues/772#issuecomment-112919149


# Structure

```R
== ShinyApp
| - ui.R # calls shinyUI and pulls in contents of ./ui
| - server.R # calls shinyServer and pulls in contents of ./server
| -- ui
    | -- landing-tab.R # tabPanel containing information about the Shiny app
    | -- navbar-menu-tab.R # navbarMenu with children, including anchor link
| -- server
    | -- data-processing.R # load external data and process data.frame columns
    | -- visualisations-and-ui.R # include all output* expressions and renderUI here 
```

# Ownership

This layout was inspired by the [Alaska/Western Canada and Historical and Projected Climate Shiny App](http://shiny.snap.uaf.edu/akcan_climate/) built by [Matthew Leonawicz](http://leonawicz.github.io/).


