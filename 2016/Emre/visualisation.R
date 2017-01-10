## eventsObserver
# if (!require('devtools')) install.packages('devtools')
# devtools::install_github('ox-it/eventsObserveR')
library(eventsObserveR)

source("data-processing.R", local = TRUE)

legend.df <- emre_data %>%
  select(species, color) %>%
  rename(description = species) %>%
  unique()


## ===== Tooltip

emre_event_labeller <- function(
  no.individuals,
  species,
  camera.id,
  time,
  no.males,
  no.females,
  no.unknown,
  no.sa,
  no.juv){
  paste0(
    no.individuals," ",species," by ", camera.id, " at ", time,
    " ",
    no.males, " males, ", no.females, " females, ", no.unknown, " unknown.",
    " ",
    no.sa, " small adults, ", no.juv, " small animals."
  )
}

emre_data <- emre_data %>%
  mutate(title = emre_event_labeller(
    no.individuals,
    species,
    camera.id,
    time,
    no.males,
    no.females,
    no.unknown,
    no.sa,
    no.juv)
  )

## ==== Filter out missing dates

emre_data <- emre_data %>%
  filter(!is.na(time))

## ==== Visualisation

eventsObserveR(events = emre_data,
               place.key = "station",
               places = all_locations,
               legend = legend.df,
               legend.columns = 2,
               size = list(
                 view.width = 1040,
                 view.height = 720,
                 interface.width = 1200,
                 interface.height = 900))
