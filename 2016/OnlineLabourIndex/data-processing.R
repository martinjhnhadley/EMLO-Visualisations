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

# fs_deposit_id <- 3761562
# deposit_details <- fs_details(fs_deposit_id)
# 
# deposit_details <- unlist(deposit_details$files)
# deposit_details <- data.frame(split(deposit_details, names(deposit_details)),stringsAsFactors = F)
# 
# ## Convert filesizes to bytes with http://stackoverflow.com/a/10911686/1659890
# convb <- function(x){
#   ptn <- "(\\d*(.\\d+)*)(.*)"
#   num  <- as.numeric(sub(ptn, "\\1", x))
#   unit <- sub(ptn, "\\3", x)             
#   unit[unit==""] <- "1" 
#   
#   mult <- c("1"=1, " KB"=1024, " MB"=1024^2, "G"=1024^3)
#   num * unname(mult[unit])
# }
# deposit_details$size <- convb(deposit_details$size)
# 
# timeseries_file_names <- deposit_details$name[grepl("OLIdata_",deposit_details$name)]
# 
# timeseries_most_recent_file <-
#   timeseries_file_names[grepl(max(as.Date(
#     gsub(".*_|[.txt]*", "", timeseries_file_names), "%Y-%m-%d"
#   )), timeseries_file_names)]
# 
# deposit_details[deposit_details$name == timeseries_most_recent_file,]
# 
# timeseries_download <- deposit_details[deposit_details$name == timeseries_most_recent_file,]$download_url
# 
# 
# txt_import <- read.table(timeseries_download,sep = ",",stringsAsFactors = F, header = T)

txt_import <- read_csv("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/OLIdata.txt")

txt_import$date <- as.Date(txt_import$date)
txt_import$count <- as.numeric(txt_import$count)
## Make symbol for visualising:
gig_economy_by_occupation <- txt_import
## Extract only new jobs
gig_economy_by_occupation <- gig_economy_by_occupation %>%
  filter(status == "new")

## =========================== Boundary Data ========================================
## ==============================================================================

# boundary_file_name <- deposit_details %>%
#   filter(name %in% deposit_details$name[grepl("bcountrydata_",deposit_details$name)]) %>%
#   filter(size == max(size)) %>%
#   select(download_url) %>%
#   as.character()
# 
# # boundary_import <- read.table(boundary_file_name,sep = ",",stringsAsFactors = F, header = T)
# 
# boundary_import <- read.table(boundary_file_name,sep = ",",stringsAsFactors = F)
boundary_import <- read_csv("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/bcountrydata.txt")

# colnames(boundary_import) <- c("timestamp","count","country_group","country","occupation")

boundary_import$timestamp <- as.Date(boundary_import$timestamp)
gig_economy_by_boundary <- boundary_import

str(gig_economy_by_boundary)



## =========================== Experiment ========================================
## ==============================================================================
# 
# str(gig_economy_by_boundary)
# 
# unique(gig_economy_by_boundary$country_group)
# 
# selected_categories = unique(gig_economy_by_boundary$country_group)
# 
# normalit<-function(m){
#   (m - min(m))/(max(m)-min(m))
# }
# 
# rep(letters[1:4], 2, 4*4)
# round(runif(12, min = 10, max = 40))
# 
# selected_categories <- c(letters[1:4])
# 
# df <- data.frame(
#   "date" = rep(1:4, 2, 4*4),
#   "country_group" = rep(letters[1:4], 2, 4*4),
#   "count" = round(runif(16, min = 10, max = 40))
# )
# df
# filter(df,
#        country_group %in% selected_categories) %>%
#   group_by(country_group) %>%
#   mutate(count = normalit(count))
# 
# 
# 
# 
# unique(gig_economy_by_boundary$country_group)
# 
# selected_categories = unique(gig_economy_by_boundary$country_group)
# 
# filter(gig_economy_by_boundary,
#        country_group %in% selected_categories) %>%{
#          filtered <- .
#          aggregate(
#            gig_economy_by_boundary$count,
#            by = list(
#              date = gig_economy_by_boundary$timestamp,
#              region = gig_economy_by_boundary$country_group
#            ),
#            FUN = sum
#          )
#        } %>%
#   group_by(region) %>%
#   mutate(x = 100*normalit(x))
# 
# 
# group_normalised <- filter(gig_economy_by_boundary,
#        country_group %in% selected_categories) %>%
#   group_by(country_group) %>%
#   mutate(count = normalit(count)) %>% {
#     group_normalised <- .
#     aggregate(
#       group_normalised$count,
#       by = list(
#         date = group_normalised$timestamp,
#         region = group_normalised$country_group
#       ),
#       FUN = sum
#     )
#   }
# 
# group_normalised[group_normalised$date == "2016-09-06",]$x %>% sum()
# 
# 
# 
# str(foo_group_normalised)
# 
# foo_group_normalised$count %>% max()
# 
# invisible(lapply(selected_categories,
#                  function(x){
#                    filtered <- gig_economy_by_boundary[gig_economy_by_boundary$country_group == x,]
#                    aggregated <- aggregate(filtered$count, by=list(date=filtered$timestamp, region = filtered$country_group), FUN=sum)
#                    aggregated
#                  }))
# 
# 
# 
# 
# 
# 
# 
