## ============== Extract shipwrecks from sites =================================
## ==============================================================================

imported_shipwrecks <-
  read.csv("data/Current OxREP database/shipwrecks.csv",
           stringsAsFactors = F)
shipwrecks <- imported_shipwrecks

sites_df <-
  read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
## Extract Mines Sites
sites_df <- sites_df[sites_df$sitetype == "Mine", ]
## Drop sites with coords 0,0
sites_df <-
  sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0, ]
## Drop NA in locations
sites_df <-
  sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong), ]
## Make into a spdf

mines_with_locations <-
  SpatialPointsDataFrame(coords = sites_df[, c("sitelong", "sitelat")], data = sites_df)

## ======================= Country codes etc ====================================
## ==============================================================================

imported_areas <-
  read.csv("data/Current OxREP database/areas.csv",
           stringsAsFactors = F)
area_codes <- imported_areas

country_replacements <- filter(area_codes, areatype == "Country") %>%
  select(areaid, areaname)

province_replacements <- filter(area_codes, areatype == "Roman province") %>%
  select(areaid, areaname)

replacements_fn <- function(data = NA, replacements = NA){
  mapvalues(data, from = replacements$areaid, to = replacements$areaname)
}

sites_df$sitecountry <- replacements_fn(data = sites_df$sitecountry, replacements = country_replacements)
sites_df$siteprovince <- replacements_fn(data = sites_df$siteprovince, replacements = province_replacements)






