## =========================== Import ====================================
## ==============================================================================

xlsx_convert_import <- function(inputFile = NA,
                                outputFile = NA,
                                stringsAsFactors = NA) {
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
imported_timeline_data <-
  xlsx_convert_import(
    inputFile = "data/policies.xlsx",
    outputFile = "data/policies.csv",
    stringsAsFactors = FALSE
  )

timeline_data <- imported_timeline_data

## =========================== Handle Dates ====================================
## ==============================================================================

timeline_data$Valid.from..b. <- ymd(timeline_data$Valid.from..b.)

## Convert ongoing to today's date, non-ongoing convert from Excel date format to as.Date
ongoings_entries <-
  which(timeline_data$Valid.until..c... == "ongoing")
excel_dates_entries <-
  which(timeline_data$Valid.until..c... != "ongoing")

timeline_data$Valid.until..c...[excel_dates_entries] <-
  as.character(as.POSIXct(
    as.numeric(timeline_data$Valid.until..c...[excel_dates_entries]) * (60 *
                                                                          60 * 24),
    origin = "1899-12-30"
  ) %>% as.Date)

timeline_data$Valid.until..c...[ongoings_entries] <-
  as.character(Sys.Date())
## Final fix
timeline_data$Valid.until..c... <-
  as.Date(timeline_data$Valid.until..c...)

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

timeline_data$Valid.from...childbirth.related.date..d. <-
  fix_childbirth_related(timeline_data$Valid.from...childbirth.related.date..d.)

timeline_data$Valid.until...childbirth.related.date..e.. <-
  fix_childbirth_related(timeline_data$Valid.until...childbirth.related.date..e..)

## =========================== Sort by earliest date ====================================
## ==============================================================================

aggregate(data = timeline_data, Name.Policy ~ Valid.from..b., FUN = sort)
## Find earliest date for each policy
earliest_date_by_Name_of_Policy <-
  timeline_data[timeline_data$Valid.from..b. == ave(timeline_data$Valid.from..b.,
                                                    timeline_data$Name.Policy,
                                                    FUN =
                                                      min), ]

earliest_date_by_Name_of_Policy$Name.Policy <-
  as.factor(earliest_date_by_Name_of_Policy$Name.Policy)

## Force order by date then Name.Policy
earliest_date_by_Name_of_Policy <-
  earliest_date_by_Name_of_Policy[order(
    earliest_date_by_Name_of_Policy$Valid.from..b.,
    earliest_date_by_Name_of_Policy$Name.Policy
  ),]



## Reorder the levels in Name.Policy for the gantt chart
timeline_data$Name.Policy <-
  factor(timeline_data$Name.Policy, levels = rev(levels(
    earliest_date_by_Name_of_Policy$Name.Policy
  )))

timeline_data %>% filter(as.character(Name.Policy) == "Parental leave")

initial_columns <- c(
  "Type.of.policy",
  "Name.Policy",
  "Year..a.",
  "Focus.of.Amendments.to.Policy",
  "Valid.from..b.",
  "Valid.until..c..."
)

displayable_columns <- c(
  "Type.of.policy",
  "Name.Policy",
  "Year..a.",
  "Focus.of.Amendments.to.Policy",
  "Legislative.basis",
  "Valid.from..b.",
  "Valid.until..c...",
  "Valid.from...childbirth.related.date..d.",
  "Valid.until...childbirth.related.date..e..",
  "Recipient",
  "type...Monetary.",
  "type..time.",
  "type..services.",
  "General.functioning.structure",
  "Territorial.application"
)

## =========================== Format Text ======================================
## ==============================================================================


lapply(
  colnames(timeline_data)[which(unlist(lapply(colnames(timeline_data), function(x)class(timeline_data[,x]))) == "character")],
  function(x){
    timeline_data[,x] <<- new_lines_to_p_tags(timeline_data[,x])
  }
)

