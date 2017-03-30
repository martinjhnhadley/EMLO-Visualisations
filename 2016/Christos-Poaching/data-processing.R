knp_gunshots <- read_csv("data/knp_gunshots.csv") %>%
  arrange(date)

knp_patrols <- read_csv("data/knp_patrols.csv")


sensor_locations <- read_csv("data/sensor-locations.csv")


weekly_patrols_and_gunshots <- read_csv("data/x(rainful)-by-y(patrol)-by-z(gunshots).csv")
colnames(weekly_patrols_and_gunshots) <- make.names(colnames(weekly_patrols_and_gunshots))
colnames(weekly_patrols_and_gunshots) <- tolower(colnames(weekly_patrols_and_gunshots))
colnames(weekly_patrols_and_gunshots)


knp_gunshots_intgraph_shiny <- read_csv("data/KNP_gunshots_IntGraph_Shiny.csv")
colnames(knp_gunshots_intgraph_shiny) <- make.names(colnames(knp_gunshots_intgraph_shiny))
colnames(knp_gunshots_intgraph_shiny) <- tolower(colnames(knp_gunshots_intgraph_shiny))
knp_gunshots_intgraph_shiny <- knp_gunshots_intgraph_shiny %>%
  mutate(date = dmy(date_formatted))


knp_operationdates_of_sensors <- read_csv("data/KNP_OperationDates_of_Sensors.csv")
colnames(knp_operationdates_of_sensors) <- make.names(colnames(knp_operationdates_of_sensors))
colnames(knp_operationdates_of_sensors) <- tolower(colnames(knp_operationdates_of_sensors))
colnames(knp_operationdates_of_sensors)
