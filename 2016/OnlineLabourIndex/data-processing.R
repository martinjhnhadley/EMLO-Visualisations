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
## Convert filesizes to bytes with http://stackoverflow.com/a/10911686/1659890
convb <- function(x){
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)             
  unit[unit==""] <- "1" 
  
  mult <- c("1"=1, " KB"=1024, " MB"=1024^2, "G"=1024^3)
  num * unname(mult[unit])
}
deposit_details$size <- convb(deposit_details$size)

timeseries_file_names <- deposit_details$name[grepl("OLIdata_",deposit_details$name)]

timeseries_most_recent_file <-
  timeseries_file_names[grepl(max(as.Date(
    gsub(".*_|[.txt]*", "", timeseries_file_names), "%Y-%m-%d"
  )), timeseries_file_names)]

deposit_details[deposit_details$name == timeseries_most_recent_file,]

timeseries_download <- deposit_details[deposit_details$name == timeseries_most_recent_file,]$download_url

txt_import <- read.table(timeseries_download,sep = ",",stringsAsFactors = F, header = T)

txt_import$date <- as.Date(txt_import$date)
txt_import$count <- as.numeric(txt_import$count)
## Make symbol for visualising:
gig_economy_by_occupation <- txt_import
## Extract only new jobs
gig_economy_by_occupation <- gig_economy_data %>%
  filter(status == "new")


## =========================== Boundary Data ========================================
## ==============================================================================

boundary_file_name <- deposit_details %>%
  filter(name %in% deposit_details$name[grepl("bcountrydata_",deposit_details$name)]) %>%
  filter(size == max(size)) %>%
  select(download_url) %>%
  as.character()

boundary_import <- read.table(boundary_file_name,sep = ",",stringsAsFactors = F, header = T)

boundary_import$timestamp <- as.Date(boundary_import$timestamp)
gig_economy_by_boundary <- boundary_import


## =========================== Experiment ========================================
## ==============================================================================

# selected_categories <- c("other Americas","other Asia and Oceania")
# 
# gig_economy_by_boundary[["country_group"]]
# 
# 
# x_axis <- "country_group"
# y_axis <- setdiff(c("country_group","occupation"), x_axis)
# print(y_axis)
# 
# ## Sum by occupation and region
# prepared_data <- aggregate(gig_economy_by_boundary$count, 
#           by=list(x_axis=gig_economy_by_boundary[[x_axis]],
#                   y_axis = gig_economy_by_boundary[[y_axis]]), 
#           FUN=sum) %>%
#   spread(y_axis, x)
# 
# stacked_bar_chart(data = prepared_data,categories_column = "x_axis",
#                   measure_columns = setdiff(colnames(prepared_data),c("x_axis")),
#                   stacking_type = "normal")





