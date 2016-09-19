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


## ===== Experiment

# vect <- 1:10
# 
# foo <- gig_economy_by_occupation %>%
#   filter(occupation == "Total") %>%
#   select(date, count)
# 
# foo <- xts(foo$count, foo$date)
# 
# 
# rollapply(as.zoo(foo), width = 28, FUN = mean, align = "center", partial = TRUE)
# 
# 
# stackoverflow_ma <- function(x, n=5, sides = 1){stats::filter(x,rep(1/n,n), sides = sides)} # http://stackoverflow.com/a/4862334/1659890
# 
# 
# foo[index(foo)] <- as.vector(my_ma(as.vector(foo), n = 28))
# 
# na.omit(foo) %>% head()

# 
# foo %>% class()
# 
# as.xts(stackoverflow_ma(foo, n = 28), index(foo)) %>% class()





