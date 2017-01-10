## ====================================== emre_events ========================== 
## ============================================================================ 

emre_events <- read_csv(
  read_csv("data/secret_events_url.csv") %>%
    select(events_url) %>%
    unlist(use.names = F)
)

colnames(emre_events) <- tolower(gsub("[.]", "", colnames(emre_events)))
colnames(emre_events) <- gsub(" ", ".", colnames(emre_events))

unique_species <- emre_events %>%
  select(species) %>%
  unique() %>%
  unlist(use.names = F)

## Generate station_id and species id
emre_events <- emre_events %>%
  mutate(station_id = plyr::mapvalues(
    camera.id,
    from = unique_species,
    to = 1:length(unique_species),
    warn_missing = FALSE
  )) %>%
  mutate(event_type_id = plyr::mapvalues(
    species,
    from = unique_species,
    to = 1:length(unique_species),
    warn_missing = FALSE
  )) %>%
  mutate(station = gsub("_Camera_[A-Z]", "", camera.id)) 

## Tidy and format data
emre_events <- emre_events %>%
  mutate(time = seconds_to_period(time)) %>% # time in seconds rather than milliseconds
  mutate(time = dmy_hms(paste(date, time))) %>%
  mutate(radius = rep(5, nrow(emre_events))) %>%
  mutate(color = plyr::mapvalues(species, 
                                 from = unique_species, 
                                 to = viridis::viridis(length(unique_species)),
                                 warn_missing = FALSE)) %>% 
  mutate(color = substr(color, 1, nchar(color) - 2)) # chrome doesn't suppport 8 character hex colors (yet) but Firefox does


## ==== Filter out missing dates
emre_events <- emre_events %>%
  filter(!is.na(time))

emre_events <- emre_events %>%
  mutate(place_id = as.numeric(gsub("Station_", "", station)) - 1)

## ==== emre_locations
## ==============
emre_locations <- read_csv(
  read_csv("data/secret_locations.csv") %>%
    select(events_url) %>%
    unlist(use.names = F)
)
emre_locations <-
  read_csv("http://users.ox.ac.uk/~oucs0030/temp/Locations-FOQMRuVAifoTmfWMDgsz6a.csv")
colnames(emre_locations) <-
  tolower(gsub("[.]", "", colnames(emre_locations)))
colnames(emre_locations) <- gsub(" ", ".", colnames(emre_locations))

emre_locations <- emre_locations %>%
  mutate(id = station.no - 1) %>%
  rename(x = x.coordinate,
         y = y.coordinate)

no_empties <- setdiff(1:max(emre_locations$id), emre_locations$id)
empties <- data.frame(
  id = no_empties,
  x = rep(0, length(no_empties)),
  y = rep(0, length(no_empties)),
  station.no = no_empties + 1
)

emre_locations <- full_join(emre_locations, empties) %>%
  arrange(station.no)
emre_locations <- emre_locations %>%
  mutate(
    color = "pink",
    radius = 10,
    title = paste("Station", station.no)
  )
