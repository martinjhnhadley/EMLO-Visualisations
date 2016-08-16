
imported_shipwrecks <-
  read.csv("data/Current OxREP database/shipwrecks.csv",
           stringsAsFactors = F)
shipwrecks <- imported_shipwrecks

sites_df <-
  read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
## Extract Wreck Sites
sites_df <- sites_df[sites_df$sitetype == "Wreck Site",]
## Drop sites with coords 0,0
sites_df <-
  sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0,]
## Drop NA in locations
sites_df <-
  sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong),]


## Drop those sitenames that are not also in the shipwrecks.csv file
sites_df <- sites_df[sites_df$sitename %in% shipwrecks$sitename, ]
## Merge dataframes:
shipwrecks_with_locations <- merge(sites_df, shipwrecks)
## Remove undated:
shipwrecks_with_locations <-
  shipwrecks_with_locations[!is.na(shipwrecks_with_locations$ante_0) &
                              !is.na(shipwrecks_with_locations$ante_0),]

## ================== Remove OCK Sites ====================================
## ==============================================================================

ock_sites <- read.csv(file = "data/Current OxREP database/OCK Wrecks to remove from visualisation.csv", stringsAsFactors = F)

shipwrecks_with_locations <- shipwrecks_with_locations[!shipwrecks_with_locations$id %in% ock_sites$id,]

## ================== Replace area codes etc ====================================
## ==============================================================================

imported_areas <-
  read.csv("data/Current OxREP database/areas.csv",
           stringsAsFactors = F)
areas_df <- imported_areas

country_replacements <- filter(areas_df, areatype == "Country") %>%
  select(areaid, areaname)

province_replacements <- filter(areas_df, areatype == "Roman province") %>%
  select(areaid, areaname)


replacements_fn <- function(data = NA, replacements = NA){
  mapvalues(data, from = replacements$areaid, to = replacements$areaname)
}

shipwrecks_with_locations$sitecountry <- replacements_fn(data = shipwrecks_with_locations$sitecountry, replacements = country_replacements)

# areas_df$areacountry <- replacements_fn(data = areas_df$sitecountry, replacements = country_replacements)
#
#
# sites_df$sitearea <- replacements_fn(data = sites_df$sitearea, replacements = mining_district_replacements)



## ======================= SpatialPointsDataFrame ====================================
## ==============================================================================

## Delete duplicates as unneeded
shipwrecks_locations <- shipwrecks_with_locations[!duplicated(shipwrecks_with_locations),]

shipwrecks_locations_spdf <-
  SpatialPointsDataFrame(coords = shipwrecks_locations[, c("sitelong", "sitelat")], data = shipwrecks_locations)

## ======================= Experiments ====================================
## ==============================================================================


shipwrecks_locations$sitename

