

## !Warning! Commented out as a query output now provides everything cheaply

# ## ============== Extract mines from sites =================================
# ## ==============================================================================
#
#
# sites_df <-
#   read.csv("data/Current OxREP database/sites.csv", stringsAsFactors = F)
# ## Extract Mines Sites
# sites_df <- sites_df[sites_df$sitetype == "Mine", ]
# ## Drop sites with coords 0,0
# sites_df <-
#   sites_df[sites_df$sitelat != 0 & sites_df$sitelong != 0, ]
# ## Drop NA in locations
# sites_df <-
#   sites_df[!is.na(sites_df$sitelat) | !is.na(sites_df$sitelong), ]
# 
# str(sites_df)
#
# ## ================== Replace area codes etc ====================================
# ## ==============================================================================
#
# imported_areas <-
#   read.csv("data/Current OxREP database/areas.csv",
#            stringsAsFactors = F)
# areas_df <- imported_areas
#
# country_replacements <- filter(areas_df, areatype == "Country") %>%
#   select(areaid, areaname)
#
# province_replacements <- filter(areas_df, areatype == "Roman province") %>%
#   select(areaid, areaname)
#
# mining_district_replacements <-
#   filter(areas_df, areatype == "Mining district") %>%
#   select(areaid, areaname)
#
# replacements_fn <- function(data = NA, replacements = NA){
#   mapvalues(data, from = replacements$areaid, to = replacements$areaname)
# }
#
# sites_df$sitecountry <- replacements_fn(data = sites_df$sitecountry, replacements = country_replacements)
#
# areas_df$areacountry <- replacements_fn(data = areas_df$sitecountry, replacements = country_replacements)
#
# sites_df$siteprovince <- replacements_fn(data = sites_df$siteprovince, replacements = province_replacements)
#
# sites_df$sitearea <- replacements_fn(data = sites_df$sitearea, replacements = mining_district_replacements)

## ==== Mine site metals and techniques.xlsx ====================================
## ==============================================================================

xlsx_convert_import <-
  function(inputFile = NA,
           outputFile = NA,
           stringsAsFactors = FALSE) {
    if (file.exists(outputFile)) {
      imported_data <<-
        read.csv(outputFile, stringsAsFactors = stringsAsFactors)
    } else {
      library(xlsx)
      xlsx_import <- read.xlsx(inputFile, sheetIndex = 1)
      write.csv(xlsx_import, file = outputFile, row.names = FALSE)
      remove(xlsx_import)
      imported_data <<-
        read.csv(outputFile, stringsAsFactors = stringsAsFactors)
    }
  }

mine_details <-
  xlsx_convert_import(
    inputFile = "data/Current OxREP database/Mine site metals and techniques.xlsx",
    outputFile = "mine_techniques_and_metals.csv",
    stringsAsFactors = F
  )

## Extract Mines Sites
mine_details <- mine_details[mine_details$Site.type == "Mine",]
## Drop sites with coords 0,0
mine_details <-
  mine_details[mine_details$Latitude != 0 &
                 mine_details$Longitude != 0,]
## Drop NA in locations
mine_details <-
  mine_details[!is.na(mine_details$Latitude) |
                 !is.na(mine_details$Longitude),]

str(mine_details)

colnames(mine_details) <-
  c(
    "siteid",
    "sitename",
    "sitetype",
    "Description",
    "sitelat",
    "sitelong",
    "sitearea",
    "sitecountry",
    "siteprovince",
    "keycat",
    "keywrd"
  )

## =============== Mine Dates  ==================================================
## ==============================================================================

mine_dates <- read.csv(file = "data/Current OxREP database/Mine site dates.csv",stringsAsFactors = F)

colnames(mine_dates) <- gsub("[.]","",tolower(colnames(mine_dates)))

str(mine_dates)

mine_details <- mine_dates %>%
  select(siteid, evntpost, evntante) %>%
  right_join(mine_details, by = "siteid")

str(mine_details)

## Drop NAs
mine_details <- mine_details[!is.na(mine_details$evntpost),]
mine_details <- mine_details[!is.na(mine_details$evntante),]


## ======================= Categorisations ====================================
## ==============================================================================


metals_df <- select(mine_details, siteid, keycat, keywrd) %>%
  filter(keycat == "Metals")

metals_vector <- filter(mine_details, keycat == "Metals") %>%
  select(keywrd) %>%
  unlist() %>%
  unique()

mining_techniques_df <-
  select(mine_details, siteid, keycat, keywrd) %>%
  filter(keycat == "Mining Techniques")


missing_mining_technique_site_id <-
  setdiff(mine_details$siteid, mining_techniques_df$siteid)

invisible(lapply(missing_mining_technique_site_id, function(id) {
  mining_techniques_df <<- bind_rows(
    mining_techniques_df,
    data.frame(
      "siteid" = id,
      "keycat" = "Mining Techniques",
      "keywrd" = "Unknown Technique"
    )
  )
}))

mining_techniques_vector <-
  filter(mining_techniques_df, keycat == "Mining Techniques") %>%
  select(keywrd) %>%
  unlist() %>%
  unique()

## ======================= SpatialPointsDataFrame ====================================
## ==============================================================================

mine_locations <-
  select(
    mine_details,
    siteid,
    evntpost,
    evntante,
    sitename,
    sitelat,
    sitelong,
    sitearea,
    sitecountry,
    siteprovince
  )

## Delete duplicates as unneeded
mine_locations <- mine_locations[!duplicated(mine_locations),]

mines_with_locations <-
  SpatialPointsDataFrame(coords = mine_locations[, c("sitelong", "sitelat")], data = mine_locations)

## ======================= Experiments ====================================
## ==============================================================================

# max(mines_with_locations$sitelat)
# 
# 
# fitBounds(
#   map = leaflet(),
#   lng1 = min(mines_with_locations$sitelong),
#   lng2 = max(mines_with_locations$sitelong),
#   lat1 = min(mines_with_locations$sitelat),
#   lat2 = min(mines_with_locations$sitelat)
# )




