## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: local file
## ================================================================================

## =========================== Load file ========================================
## ==============================================================================

fs_deposit_id <- 3761562
deposit_details <- fs_details(fs_deposit_id)
deposit_details <- unlist(deposit_details$files)
deposit_details <- data.frame(split(deposit_details, names(deposit_details)),stringsAsFactors = F)

timeseries_file_names <- deposit_details$name[grepl("OLIdata_",deposit_details$name)]

timeseries_most_recent_file <-
  timeseries_file_names[grepl(max(as.Date(
    gsub(".*_|[.txt]*", "", timeseries_file_names), "%Y-%m-%d"
  )), timeseries_file_names)]

deposit_details[deposit_details$name == timeseries_most_recent_file,]

timeseries_download <- deposit_details[deposit_details$name == timeseries_most_recent_file,]$download_url

txt_import <- read.table(timeseries_download,sep = ",",stringsAsFactors = F)

colnames(txt_import) <- as.character(txt_import[1,])

txt_import <- txt_import[2:nrow(txt_import),]

txt_import$date <- as.Date(txt_import$date)
txt_import$count <- as.numeric(txt_import$count)
## Make symbol for visualising:
gig_economy_data <- txt_import
## Extract only new jobs
gig_economy_data <- gig_economy_data %>%
  filter(status == "new")
