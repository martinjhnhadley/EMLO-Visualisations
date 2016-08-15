

## !Warning! Commented out as a query output now provides everything cheaply

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


mine_details$sitearea[mine_details$sitearea == ""] <- "Unnamed Mine"

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

## ======================= Experiments ====================================
## ========================================================================


highchart() %>%
  hc_chart(type = "bar") %>%
  hc_xAxis(categories = unique(mine_details[,input$group_by])) %>%
  hc_add_series(name = "Number of mines", data = grouped_tally$n)


fake_data <- data.frame(
  "start" = c(-1000, -500,10,100,-1000, -500,10,100),
  "end" = c(-950, -450, 60, 150,-1000, -500,10,100),
  "location" = c(rep("A",4), rep("B",4))
)


pretty_date_range <- pretty(fake_data$start)

pretty_date_range

highchart() %>%
  hc_chart(type = "area") %>%
  hc_xAxis(categories = pretty_date_range) %>%
  hc_add_series(data = 11:18) %>%
  hc_add_series(data = 21:15)







