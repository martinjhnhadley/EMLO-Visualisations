
## !Warning! Commented out as a query output now provides everything cheaply

# ## ============== Extract shipwrecks from sites =================================
# ## ==============================================================================
# 
# imported_shipwrecks <-
#   read.csv("data/Current OxREP database/shipwrecks.csv",
#            stringsAsFactors = F)
# shipwrecks <- imported_shipwrecks
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

xlsx_convert_import <- function(inputFile = NA, outputFile = NA, stringsAsFactors = FALSE){
  if(file.exists(outputFile)){
    imported_data <<- read.csv(outputFile, stringsAsFactors = stringsAsFactors)
  } else {
    library(xlsx)
    xlsx_import <- read.xlsx(inputFile, sheetIndex = 1)
    write.csv(xlsx_import, file = outputFile, row.names = FALSE)
    remove(xlsx_import)
    imported_data <<- read.csv(outputFile, stringsAsFactors = stringsAsFactors)
  }
}

mine_details <-
  xlsx_convert_import(inputFile = "data/Current OxREP database/Mine site metals and techniques.xlsx",
                      outputFile = "mine_techniques_and_metals.csv",
                      stringsAsFactors = F)

## Extract Mines Sites
mine_details <- mine_details[mine_details$Site.type == "Mine", ]
## Drop sites with coords 0,0
mine_details <-
  mine_details[mine_details$Latitude != 0 & mine_details$Longitude != 0, ]
## Drop NA in locations
mine_details <-
  mine_details[!is.na(mine_details$Latitude) | !is.na(mine_details$Longitude), ]

str(mine_details)

colnames(mine_details) <- c("siteid", "sitename", "sitetype", "Description", "sitelat", 
                            "sitelong", "sitearea", "sitecountry", "siteprovince", "keycat", 
                            "keywrd")

## ======================= SpatialPointsDataFrame ====================================
## ==============================================================================

mines_with_locations <-
  SpatialPointsDataFrame(coords = mine_details[, c("sitelong", "sitelat")], data = mine_details)




