### ============= Import People Sheet ========================= ###

people.df <- read.csv("data/master_people_sheet.csv")
## Convert name to character vector for simplicity later
people.df$Person.Name <- as.character(people.df$Person.Name)
## Sort by name
people.df <- people.df[order(people.df$Person.Name),]

## ===== Add surnames
## Empty character vector
surnames.list <- as.character()
## If there's a comma assume first word is surname, otherwise keep whole name
add.surname.fn <- function(name){
  if(grepl(",",name)){
    surnames.list <<- append(surnames.list, sapply(strsplit(name, ","), "[[", 1))
  } else {
    surnames.list <<- append(surnames.list,name)
  }
}
## lapply invisibly
invisible(lapply(people.df$Person.Name,function(x)add.surname.fn(x)))
## Append surname column to people.df
people.df$Surname <- surnames.list

### ============= Import Places Sheet =========================
places.df <- read.csv("data/master_places_sheet.csv")

### ============= Import All Life Events ========================= 

life.events.df <- read.csv("data/master_life_events_sheet.csv")
life.events.df$Primary.Participant.Emlo_ID <- as.character(life.events.df$Primary.Participant.Emlo_ID)
life.events.df$Secondary.Participant.Emlo_ID <- as.character(life.events.df$Secondary.Participant.Emlo_ID)

### ============= Non-self Referential Events (i.e. not births) =============

multiparty.interactions <- life.events.df[!is.na(life.events.df$Secondary.Participant.Emlo_ID),]
multiparty.interactions <- droplevels(multiparty.interactions)


# ### ============== Old =========================
# 
# # find the individuals that are documented in the people sheet
# documented.people <- intersect(c(multiparty.interactions$Primary.Participant.Emlo_ID,
#                                  multiparty.interactions$Secondary.Participant.Emlo_ID),people.df$iperson_id)
# # All 148 documented people exist in the multiparty.interactions, 245 undocumented people are included!
# # Remove undocumented individuals from the multiparty.interactions
# # Only 272 interactions survive out of a total of 555 multiparty interactions in the original dataset.
# # For now re-assign multiparty.interactions to these documented interactions:
# 
# multiparty.interactions <- subset(multiparty.interactions,
#                                   Primary.Participant.Emlo_ID %in% documented.people &
#                                     Secondary.Participant.Emlo_ID %in% documented.people)

# ### ============= Documented Interactions (including individuals with only self-referential events) ============= ###
# 
# all.documented.interactions <- subset(life.events.df,
#        Primary.Participant.Emlo_ID %in% documented.people &
#          Secondary.Participant.Emlo_ID %in% documented.people)
# 
# all.documented.interactions <- droplevels(all.documented.interactions)


