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
library(readr)

fs_deposit_id <- 3761562
deposit_details <- fs_details(fs_deposit_id)

deposit_details <- unlist(deposit_details$files)
deposit_details <- data.frame(split(deposit_details, names(deposit_details)),stringsAsFactors = F)

occupation_import <- read_csv(deposit_details[grepl("OLIdata_",deposit_details$name),"download_url"])
occupation_import$date <- as.Date(occupation_import$date)
occupation_import$count <- as.numeric(occupation_import$count)
## Make symbol for visualising:
gig_economy_by_occupation <- occupation_import
## Extract only new jobs
gig_economy_by_occupation <- gig_economy_by_occupation %>%
  filter(status == "new")

region_import <- read_csv(deposit_details[grepl("bcountrydata_",deposit_details$name),"download_url"])
region_import$timestamp <- as.Date(region_import$timestamp)
## Make symbol for visualising:
gig_economy_by_boundary <- region_import


str(gig_economy_by_boundary)

## ===== Experiment

x_axis <- "country"
y_axis <- "occupation"


x_axis <- "occupation"
y_axis <- "country"



# ## ==== The data for this visualisation is deposited in Figshare and updated every 24h
# ## ==== However, loading the data from Figshare takes 10+ seconds.
# ## ==== Therefore the code for obtaining the data from Figshare is commented out and
# ## ==== the data is obtained directly from the academic's department server.
# # fs_deposit_id <- 3761562
# # deposit_details <- fs_details(fs_deposit_id)
# # 
# # deposit_details <- unlist(deposit_details$files)
# # deposit_details <- data.frame(split(deposit_details, names(deposit_details)),stringsAsFactors = F)
# # 
# # ## Convert filesizes to bytes with http://stackoverflow.com/a/10911686/1659890
# # convb <- function(x){
# #   ptn <- "(\\d*(.\\d+)*)(.*)"
# #   num  <- as.numeric(sub(ptn, "\\1", x))
# #   unit <- sub(ptn, "\\3", x)             
# #   unit[unit==""] <- "1" 
# #   
# #   mult <- c("1"=1, " KB"=1024, " MB"=1024^2, "G"=1024^3)
# #   num * unname(mult[unit])
# # }
# # deposit_details$size <- convb(deposit_details$size)
# # 
# # timeseries_file_names <- deposit_details$name[grepl("OLIdata_",deposit_details$name)]
# # 
# # timeseries_most_recent_file <-
# #   timeseries_file_names[grepl(max(as.Date(
# #     gsub(".*_|[.txt]*", "", timeseries_file_names), "%Y-%m-%d"
# #   )), timeseries_file_names)]
# # 
# # deposit_details[deposit_details$name == timeseries_most_recent_file,]
# # 
# # timeseries_download <- deposit_details[deposit_details$name == timeseries_most_recent_file,]$download_url
# # 
# # txt_import <- read.table(timeseries_download,sep = ",",stringsAsFactors = F, header = T)
# 
# txt_import <- read_csv("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/OLIdata.txt")
# 
# txt_import$date <- as.Date(txt_import$date)
# txt_import$count <- as.numeric(txt_import$count)
# ## Make symbol for visualising:
# gig_economy_by_occupation <- txt_import
# ## Extract only new jobs
# gig_economy_by_occupation <- gig_economy_by_occupation %>%
#   filter(status == "new")
# 
# ## =========================== Boundary Data ========================================
# ## ==============================================================================
# 
# ## ==== The data for this visualisation is deposited in Figshare and updated every 24h
# ## ==== However, loading the data from Figshare takes 10+ seconds.
# ## ==== Therefore the code for obtaining the data from Figshare is commented out and
# ## ==== the data is obtained directly from the academic's department server.
# # boundary_file_name <- deposit_details %>%
# #   filter(name %in% deposit_details$name[grepl("bcountrydata_",deposit_details$name)]) %>%
# #   filter(size == max(size)) %>%
# #   select(download_url) %>%
# #   as.character()
# # 
# # # boundary_import <- read.table(boundary_file_name,sep = ",",stringsAsFactors = F, header = T)
# # 
# # boundary_import <- read.table(boundary_file_name,sep = ",",stringsAsFactors = F)
# 
# boundary_import <- read_csv("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/bcountrydata.txt")
# 
# boundary_import$timestamp <- as.Date(boundary_import$timestamp)
# gig_economy_by_boundary <- boundary_import
# 
# ## =========================== Experiment ========================================
# ## ==============================================================================
# 
# gig_economy_by_boundary$country_group %>% unique()

