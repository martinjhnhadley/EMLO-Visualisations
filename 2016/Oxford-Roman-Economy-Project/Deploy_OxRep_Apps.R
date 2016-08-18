### Deploy apps
library(shiny)
library(rsconnect)


deployApp(appDir = "OxRep_charts/OxRep_charts_mines",
          appName = "OxRep_charts_mines",
          launch.browser = FALSE)

deployApp(appDir = "OxRep_charts/OxRep_charts_shipwrecks",
          appName = "OxRep_charts_shipwrecks",
          launch.browser = FALSE)

deployApp(appDir = "OxRep_maps/OxRep_maps_mines/",
          appName = "OxRep_maps_mines",
          launch.browser = FALSE)

deployApp(appDir = "OxRep_maps/OxRep_maps_shipwrecks",
          appName = "OxRep_maps_shipwrecks",
          launch.browser = FALSE)
