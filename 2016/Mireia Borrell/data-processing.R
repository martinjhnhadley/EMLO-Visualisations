## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Mireia Borrell-Porta (orcid.org/0000-0003-2328-1258)
## Data Source: local file
## ================================================================================

## ========================== Get from Figshare ================================

# start <- Sys.time()
# import_fs_xlsx <- read.xlsx("data/policies.xlsx", sheetIndex = 1,startRow = 2, header = F)
# ncol(import_fs_xlsx)
# 
# import_fs_xlsx_header <- read.xlsx("data/policies.xlsx", sheetIndex = 1, rowIndex = 1, header = F)
# 
# import_fs_xlsx %>% head()
# 
# new_headings <- import_fs_xlsx_header[1,] %>% 
#   unlist(use.names = F) %>% 
#   as.character() %>%
#   gsub("^[\n]","",.) %>%
#   gsub("[\n]"," ",.) %>%
#   trimws()

## =========================== Import ====================================
## ==============================================================================

xlsx_convert_import <- function(inputFile = NA,
                                outputFile = NA,
                                stringsAsFactors = NA) {
  if (file.exists(outputFile)) {
    imported_data <<-
      read.csv(outputFile, stringsAsFactors = stringsAsFactors, check.names = F)
  } else {
    library(xlsx)
    xlsx_import <- read.xlsx(inputFile, sheetIndex = 1)
    write.csv(xlsx_import, file = outputFile, row.names = FALSE)
    remove(xlsx_import)
    imported_data <<-
      read.csv(outputFile, stringsAsFactors = stringsAsFactors, check.names = F)
  }
}
imported_timeline_data <-
  xlsx_convert_import(
    inputFile = "data/policies.xlsx",
    outputFile = "data/policies.csv",
    stringsAsFactors = FALSE
  )

## =========================== Column Names ====================================
## ==============================================================================

colnames(imported_timeline_data) <-
  imported_timeline_data %>% colnames() %>%
  trimws() %>%
  gsub("^X.", "", .) %>%
  gsub("[.]{2,}", ".", .) %>%
  trimws()

timeline_data <- imported_timeline_data

## =========================== Import ====================================
## ==============================================================================


# 
# read.xlsx("data/policies.xlsx", sheetIndex = 1) %>% colnames()
# 
# 
# timeline_data <- imported_timeline_data
# 
# timeline_data %>% colnames()
# 
# colnames(timeline_data)
# as.data.frame(timeline_data)

## =========================== Handle Dates ====================================
## ==============================================================================

timeline_data$Valid.from.b. <- ymd(timeline_data$Valid.from.b.)

## Convert ongoing to today's date, non-ongoing convert from Excel date format to as.Date
ongoings_entries <-
  which(timeline_data$Valid.until.c. == "ongoing")
excel_dates_entries <-
  which(timeline_data$Valid.until.c. != "ongoing")

timeline_data$Valid.until.c.[excel_dates_entries] <-
  as.character(as.POSIXct(
    as.numeric(timeline_data$Valid.until.c.[excel_dates_entries]) * (60 *
                                                                          60 * 24),
    origin = "1899-12-30"
  ) %>% as.Date)

timeline_data$Valid.until.c.[ongoings_entries] <-
  as.character(Sys.Date())
## Final fix
timeline_data$Valid.until.c. <-
  as.Date(timeline_data$Valid.until.c.)

fix_childbirth_related <- function(data) {
  returned_data <- data
  ongoings_entries <- which(data == "ongoing")
  dash_entries <- which(data == "-")
  excel_dates_entries <- grep("[0-9]", data)
  
  returned_data[excel_dates_entries] <-
    as.character(as.POSIXct(as.numeric(data[excel_dates_entries]) * (60 * 60 * 24),
                            origin = "1899-12-30") %>% as.Date)
  
  returned_data[ongoings_entries] <- as.character(Sys.Date())
  
  returned_data[dash_entries] <- NA
  
  as.Date(returned_data)
}

timeline_data$Valid.from.childbirth.related.date.d. <-
  fix_childbirth_related(timeline_data$Valid.from.childbirth.related.date.d.)

timeline_data$Valid.until.childbirth.related.date.e. <-
  fix_childbirth_related(timeline_data$Valid.until.childbirth.related.date.e.)

## =========================== Sort by earliest date ====================================
## ==============================================================================

## Find earliest date for each policy
earliest_date_by_Name_of_Policy <-
  timeline_data[timeline_data$Valid.from.b. == ave(timeline_data$Valid.from.b.,
                                                    timeline_data$Name.of.Policy,
                                                    FUN =
                                                      min), ]

earliest_date_by_Name_of_Policy$Name.of.Policy <-
  as.factor(earliest_date_by_Name_of_Policy$Name.of.Policy)

## Force order by date then Name.Policy
earliest_date_by_Name_of_Policy <-
  earliest_date_by_Name_of_Policy[order(
    earliest_date_by_Name_of_Policy$Valid.from.b.,
    earliest_date_by_Name_of_Policy$Name.of.Policy
  ),]



## Reorder the levels in Name.Policy for the gantt chart
timeline_data$Name.of.Policy <-
  factor(timeline_data$Name.of.Policy, levels = rev(levels(
    earliest_date_by_Name_of_Policy$Name.of.Policy
  )))




## =========================== Format Text ======================================
## ==============================================================================


lapply(
  colnames(timeline_data)[which(unlist(lapply(colnames(timeline_data), function(x)class(timeline_data[,x]))) == "character")],
  function(x){
    timeline_data[,x] <<- new_lines_to_p_tags(timeline_data[,x])
  }
)

