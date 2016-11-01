## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Paul Dornan
## Data Source: local file
## ================================================================================

country_schooling <- read.csv(file = "data/vietnam_education_schooling.csv", stringsAsFactors = F)
country_schooling$Sample.Size <- as.numeric(gsub(",","",country_schooling$Sample.Size))

categories_list <- setdiff(colnames(country_schooling), c("Property","Property.Type","Cohort"))
categories_list <- setNames(categories_list, trimws(gsub("\\.", " ", categories_list)))

country_schooling <- country_schooling %>%
  gather(category, value, which(!colnames(country_schooling) %in% setdiff(colnames(country_schooling), categories_list)))

subcategories_list <- c("Younger Cohort (age 12 in 2013)", "Older Cohort (age 12 in 2006)")

subcategories_order <- list(
  "Younger Cohort (age 12 in 2013)" = 1,
  "Older Cohort (age 12 in 2006)" = 0
)
subcategories_order <- as.numeric(subcategories_order)

## Use to modify axes for percentages
percentage_categories <- as.character(categories_list)[grepl("ercent", categories_list)]