

## ==== emre_data
## ==============
emre_data <- read_csv(read_csv("data/secret_events_url.csv") %>%
                        select(events_url) %>%
                        unlist(use.names = F))
colnames(emre_data) <- tolower(gsub("[.]","",colnames(emre_data))) 
colnames(emre_data) <- gsub(" ",".",colnames(emre_data))

unique_species <- emre_data %>%
  select(species) %>%
  unique() %>%
  unlist(use.names = F)

emre_data <- emre_data %>%
  mutate(station_id = plyr::mapvalues(camera.id, from = unique_species, to = 1:length(unique_species))) %>%
  mutate(species_id = plyr::mapvalues(species, from = unique_species, to = 1:length(unique_species))) %>%
  mutate(station = gsub("_Camera_[A-Z]", "", camera.id)) %>%
  mutate(time = seconds_to_period(time)) %>% # time in seconds rather than milliseconds
  mutate(time = dmy_hms(paste(date,time))) %>%
  mutate(radius = rep(5, nrow(emre_data))) %>%
  mutate(color = plyr::mapvalues(species, from = unique_species, to = viridis::viridis(length(unique_species)))) %>% # chrome doesn't suppport 8 character hex colors (yet) but Firefox does
  mutate(color = substr(color,1,nchar(color)-2)) %>%
  filter(!species %in% c("Humans with or without domestic animals", "Humans", "Domestic animals", ""))

## ==== emre_locations
## ==============
emre_locations <- read_csv(read_csv("data/secret_locations.csv") %>%
                        select(events_url) %>%
                        unlist(use.names = F))
emre_locations <- read_csv("http://users.ox.ac.uk/~oucs0030/temp/Locations-FOQMRuVAifoTmfWMDgsz6a.csv")
colnames(emre_locations) <- tolower(gsub("[.]","",colnames(emre_locations))) 
colnames(emre_locations) <- gsub(" ",".",colnames(emre_locations))

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

all_locations <- full_join(emre_locations, empties) %>%
  arrange(station.no)
all_locations <- all_locations %>%
  mutate(color = rep("pink", nrow(all_locations)),
         radius = 10,
         title = paste("Station", station.no))

emre_data <- emre_data %>%
  mutate(place_id = as.numeric(gsub("Station_", "", station)) - 1)

## ===== Legend

legend.df <- emre_data %>%
  select(species, color) %>%
  rename(description = species) %>%
  unique()








