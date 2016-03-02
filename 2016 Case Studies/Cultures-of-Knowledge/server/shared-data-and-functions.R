## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Arno Bosse (http://orcid.org/0000-0003-3681-1289)
## Data Source: emlo.bodleian.ox.ac.uk
## ================================================================================

## ================================================================================
## ===================== Share Data and Function ==================================




usefulCols_life_events <- c("Primary.Participant.Name","Secondary.Participant.Name","Event.or.Relationship.Type",
                            "Category","DateOne.Year", "DateOne.Month", "DateOne.Day","DateOne.Uncertainty","DateTwo.Year",                 
                            "DateTwo.Month", "DateTwo.Day","DateTwo.Uncertainty","Date.Type","Location.Details","Location.Region",
                            "Location.Country","Textual.Source.Source","Primary.Participant.Emlo_ID","Secondary.Participant.Emlo_ID")

### ========= show/hide advanced options for the whole network

# shinyjs::onclick("toggleDateOptions",
#                  shinyjs::toggle(id = "dateOptions", anim = TRUE)) 

# shinyjs::onclick("toggleAdditionalOptions",
#                  shinyjs::toggle(id = "additionalOptions", anim = TRUE)) 

### ========= Exclude entries without a date

# output$include_interactions_without_dates_UI <- renderUI({
#   checkboxInput(inputId = "include_interactions_without_dates", label = "Include interactions without dates?", value = FALSE)
# })

### ========= Category/Type Selection UI Elements

all_event_types <- reactive(levels(multiparty.interactions$Event.or.Relationship.Type))
all_event_categories <- reactive(levels(multiparty.interactions$Category))

### ====================================== Generate Edges Function ============================================
### =================================================================================================================

network.edges.function <- function(selected.interactions){
  
  edges.of.network <- data.frame("Primary.Emlo_ID" = character(),
                                 "Secondary.Emlo_ID" = character(),
                                 "EcclesiasticalActivities" = character(),
                                 "EducationalActivities" = numeric(),
                                 "FamilyRelationships" = numeric(),
                                 "HierarchicalRelationships" = numeric(),
                                 "LearnedActivities" = numeric(),
                                 "MajorLifeEvents"= numeric(),
                                 "PeerRelationships" = numeric(),
                                 "PoliticalActivities" = numeric(),
                                 "ProfessionalActivities" = numeric(),
                                 "SocialStatusChange" = numeric()
  )
  # Find unique connections (relying on EMLO_ID)
  unique_Connections <- unique(selected.interactions[,c("Primary.Participant.Emlo_ID","Secondary.Participant.Emlo_ID")])
  # Split into a list for easier usage in lapply operations later!
  unique_Connections <- split(unique_Connections, seq(nrow(unique_Connections)))
  
  
  tally.connections <- function(partners) {
    paired.events <-
      selected.interactions[selected.interactions$Primary.Participant.Emlo_ID == partners[[1]] &
                              selected.interactions$Secondary.Participant.Emlo_ID == partners[[2]],]
    tally.event.type <-
      data.frame(data = factor(
        as.vector(paired.events[,"Category"]),
        levels = levels(selected.interactions$Category)
      ))
    tally.event.type <- as.data.frame(table(tally.event.type))
    totalConnections <- sum(tally.event.type$Freq)
    # Use dcast to make df wide
    tally.event.type <-
      dcast(
        tally.event.type, formula = . ~ tally.event.type , value.var = "Freq", fill = NULL
      )
    # Drop the first column which contains useless data
    tally.event.type[1] <- NULL
    tally.event.type$Primary.Emlo_ID <- partners[[1]]
    tally.event.type$Secondary.Emlo_ID <- partners[[2]]
    tally.event.type$Total.Connections <- totalConnections
    tally.event.type
  }
  
  invisible(lapply(unique_Connections, function(x){
    new.edge <- tally.connections(x)
    edges.of.network <<- rbind(edges.of.network, new.edge)
  }))
  
  # Remove edges where the secondary emlo id is NA
  edges.of.network <- edges.of.network[!is.na(edges.of.network$Secondary.Emlo_ID),]
  
  # Return edges.of.network to the symbol whole.network_edges.of.network
  edges.of.network
}