## Function for converting xlsx to csv in a nice way
xlsx_convert_import <- function(inputFile = NA, outputFile = NA){
  if(file.exists(outputFile)){
    imported_data <<- read.csv(outputFile)
  } else {
    library(xlsx)
    xlsx_import <- read.xlsx(inputFile, sheetIndex = 1)
    write.csv(xlsx_import, file = outputFile, row.names = FALSE)
    remove(xlsx_import)
    imported_data <<- read.csv(outputFile)
  }
}

xlsx_convert_import(inputFile = "data/bigModLables.xlsx", outputFile = "data/bigModLables.csv")
xlsx_convert_import(inputFile = "data/modesedges.xlsx", outputFile = "data/modesedges.csv")
xlsx_convert_import(inputFile = "data/modlabelsall.xlsx", outputFile = "data/modlabelsall.csv")

library(readr)

with_japense <- read_csv("data/modlabelsall.csv")
modesedges <- read_csv("data/modesedges.csv")
bigModLables <- read_csv("data/bigModLables.csv")

library(dplyr)

rescaled_sizes <- log(with_japense$Size + 5)

with_japense %>%
  rename(id = OSLOM.ID,
         label = Label) %>%
  select(-Size) %>%
  mutate(size = rescaled_sizes,
         title = label) -> nodes

edges <- modesedges

## www.datastorm-open.github.io/visNetwork/
library(visNetwork)
visNetwork(nodes, edges) %>%
  visLayout(improvedLayout = TRUE) %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE)

## Shiny https://github.com/ox-it/OxfordIDN_Training_Interactive-Viz-with-R

