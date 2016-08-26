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

## File isn't perfect, import and perfect
txt_import <- read.table("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/OLIdata.txt",sep = ",",stringsAsFactors = F)

colnames(txt_import) <- as.character(txt_import[1,])

txt_import <- txt_import[2:nrow(txt_import),]

txt_import$date <- as.Date(txt_import$date)
txt_import$count <- as.numeric(txt_import$count)
## Make symbol for visualising:
gig_economy_data <- txt_import


## =========================== Playground =======================================
## ==============================================================================
