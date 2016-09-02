imported_shipwrecks <-
  read.csv("data/Current OxREP database/shipwrecks.csv",
           stringsAsFactors = F)
shipwrecks <- imported_shipwrecks

sites_df <-
  read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
## Extract Wreck Sites
sites_df <- sites_df[sites_df$sitetype == "Wreck Site",]

# # In charts all shipwrecks should be included, even those without precise location
# ## Drop sites with coords 0,0
# sites_df <-
#   sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0,]
# ## Drop NA in locations
# sites_df <-
#   sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong),]

## Drop those sitenames that are not also in the shipwrecks.csv file
sites_df <- sites_df[sites_df$sitename %in% shipwrecks$sitename, ]
## Merge dataframes:
shipwreck_details <- merge(sites_df, shipwrecks)
## Remove undated:
shipwreck_details <-
  shipwreck_details[!is.na(shipwreck_details$ante_0) &
                              !is.na(shipwreck_details$post_0),]

## ================== Remove OCK Sites ====================================
## ==============================================================================

ock_sites <- read.csv(file = "data/Current OxREP database/OCK Wrecks to remove from visualisation.csv", stringsAsFactors = F)

shipwreck_details <- shipwreck_details[!shipwreck_details$id %in% ock_sites$id,]

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

water_replacements <- filter(areas_df, areatype == "Water") %>%
  select(areaid, areaname)

replacements_fn <- function(data = NA, replacements = NA){
  mapvalues(data, from = replacements$areaid, to = replacements$areaname)
}

shipwreck_details$sitecountry <- replacements_fn(data = shipwreck_details$sitecountry, replacements = country_replacements)

shipwreck_details$sitecountry[is.na(shipwreck_details$sitecountry)] <- "Not Specified"


shipwreck_details$sitearea <- replacements_fn(data = shipwreck_details$sitearea, replacements = water_replacements)

shipwreck_details$sitearea[is.na(shipwreck_details$sitearea)] <- "Not Specified"

## ================== Experiments =========================================
## ==============================================================================

