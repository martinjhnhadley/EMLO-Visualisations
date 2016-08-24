### Deploy apps
library(shiny)
library(rsconnect)


deployApp(appDir = "Ethiopia/YoungLives_Ethiopia_Education/",
          appName = "YoungLives_Ethiopia_Education",
          launch.browser = FALSE)

deployApp(appDir = "India/YoungLives_India_Education/",
          appName = "YoungLives_India_Education",
          launch.browser = FALSE)

deployApp(appDir = "Vietnam/YoungLives_Vietnam_Education/",
          appName = "YoungLives_Vietnam_Education",
          launch.browser = FALSE)