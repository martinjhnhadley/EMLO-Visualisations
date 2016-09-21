## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562.v56
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

tail(gig_economy_by_occupation)

## Extract only new jobs
gig_economy_by_occupation <- gig_economy_by_occupation %>%
  filter(status == "new")

region_import <- read_csv(deposit_details[grepl("bcountrydata_",deposit_details$name),"download_url"])
region_import$timestamp <- as.Date(region_import$timestamp)
## Make symbol for visualising:
gig_economy_by_boundary <- region_import



## ===== Experiment
# 
# x_axis <- "country"
# y_axis <- "occupation"
# y_axis_order <- gig_economy_by_boundary %>%
#   filter(timestamp == max(timestamp)) %>%
#   group_by_(x_axis) %>%
#   summarise(total = sum(count)) %>%
#   arrange(desc(total)) %>%
#   select_(x_axis) %>%
#   unlist(use.names = FALSE) %>%
#   .[1:20]
# 
# prepared_data <- gig_economy_by_boundary %>%
#   filter(timestamp == max(timestamp)) %>%
#   filter(country %in% y_axis_order) %>%
#   group_by_(x_axis, y_axis) %>%
#   summarise(total = sum(count)) %>%
#   spread_(y_axis, "total") %>%
#   rename(x_axis = country) %>%
#   as.data.frame()
# 
# print(gig_economy_by_boundary %>%
#         group_by_(x_axis) %>%
#         summarise(total = sum(count)) %>%
#         arrange(desc(total)) %>%
#         select(total) %>%
#         unlist(use.names = FALSE))
# 
# 
# prepared_data[match(y_axis_order, prepared_data$x_axis), ]
# 
# 
# gig_economy_by_boundary %>%
#   group_by_(x_axis) %>%
#   summarise(total = sum(count)) %>%
#   arrange(desc(total)) %>%
#   head(20)
# 
# stacked_bar_chart(
#   data = prepared_data[match(y_axis_order, prepared_data$x_axis), ],
#   categories_column = "x_axis",
#   measure_columns = setdiff(colnames(prepared_data), c("x_axis")),
#   stacking_type = "normal",
#   ordering_function = mean
# ) %>%
#   hc_credits(text = 'Source: Online Labour Index',
#              enabled = TRUE,
#              href = 'ilabour.oii.ox.ac.uk/online-labour-index',
#              position = list(align = "right")) %>%
#   hc_annotations(list(
#     # anchorX = "right",
#     # anchorY = "center",
#     xAxis = -20,
#     yAxis = 10,
#     title = list(
#       text = "Annotated chart!"
#     )))
