## Code for cleansing the raw-data and generating the datafiles in data/

library(readr)
library(lubridate)

## ===== kmp_gunshots =========================
## ========================================

kmp_gunshots <- read.csv("data-raw/kmp_gunshots.csv") # don't use read_csv as there are hms objects I don't like dealing with

colnames(kmp_gunshots) <- gsub("_", ".", tolower(colnames(kmp_gunshots)))

kmp_gunshots <- kmp_gunshots %>%
  as_data_frame() %>%
  rename(date = date.formatted) %>%
  mutate(date = as.character(date)) %>%
  mutate(time.formatted = as.character(time.formatted)) %>% 
  mutate(date.time = paste(date, time.formatted)) %>%
  mutate(date.time = dmy_hms(date.time)) %>%
  mutate(date = dmy(date)) %>%
  select(-day, -hours, -time.formatted) %>% # drop unnecessary
  mutate_if(is.factor, as.character)

## ===== Rainy season tags

rainy_seasons <- kmp_gunshots %>%
  select(month, season) %>%
  unique() 

## ===== Add in days that are NOT covered in the data

## Setdiff coerces as.POSIXct so easier to stick with characters
all_days <- as.character(seq(from = min(kmp_gunshots$date), to = max(kmp_gunshots$date), by = "days"))
measured_days <- as.character(kmp_gunshots$date)
missing_days <- ymd(setdiff(all_days, measured_days))

kmp_missing_gunshots <- kmp_gunshots[FALSE,]

add_missing_gunshot_day <- function(date) {
  data.frame(
    number = NA,
    siteid = NA,
    gunscore = 0,
    date = date,
    year = year(date),
    weekday = weekdays(date),
    month = as.character(month(
      date, label = TRUE, abbr = FALSE
    )),
    season = mapvalues(
      as.character(month(
        date, label = TRUE, abbr = FALSE
      )),
      from = rainy_seasons$month,
      to = rainy_seasons$season,
      warn_missing = FALSE
    ),
    timing = NA,
    year.di = NA,
    date.time = NA
  )
}

invisible(lapply(missing_days, function(x){
  kmp_missing_gunshots <<- kmp_missing_gunshots %>%
    bind_rows(add_missing_gunshot_day(x))
}))

## Add in missing days
kmp_gunshots <- kmp_gunshots %>%
  bind_rows(kmp_missing_gunshots)

## Calculate gunshots.on.day
kmp_gunshots <- kmp_gunshots %>%
  group_by(date) %>%
  mutate(total.gunshots.on.day = sum(gunscore)) %>%
  ungroup()

write_csv(x = kmp_gunshots, path = "data/kmp_gunshots.csv")

## ===== kmp_patrols =========================
## ========================================

kmp_patrols <- read.csv("data-raw/patrols_cleaned.csv", stringsAsFactors = F)

colnames(kmp_patrols) <- gsub("_", ".", tolower(colnames(kmp_patrols)))

kmp_patrols <- kmp_patrols %>%
  mutate(date.time = dmy_hms(paste(date, time))) %>%
  mutate(date = dmy(date))

## ===== Add in days that are NOT covered in the data

kmp_patrols <- kmp_patrols %>%
  mutate(patrols = 1) # each record is for ONE unique patrol

## Setdiff coerces as.POSIXct so easier to stick with characters
all_days <- as.character(seq(from = min(kmp_patrols$date), to = max(kmp_patrols$date), by = "days"))
measured_days <- as.character(kmp_patrols$date)
missing_days <- ymd(setdiff(all_days, measured_days))

kmp_missing_patrols <- kmp_patrols[FALSE,]

add_missing_patrol_day <- function(date) {
  data.frame(
    id.new = NA,
    patrols = 0, # there are no recorded patrols on this day
    id.gis = NA,
    date = date,
    time = NA,
    trail = NA,
    length = NA,
    period = NA,
    month = as.character(month(
      date, label = TRUE
    )),
    night = NA,
    date.time = NA
  )
}

invisible(lapply(missing_days, function(x){
  kmp_missing_patrols <<- kmp_missing_patrols %>%
    bind_rows(add_missing_patrol_day(x))
}))

## Add in missing days
kmp_patrols <- kmp_patrols %>%
  bind_rows(kmp_missing_patrols)

## Calculate gunshots.on.day
kmp_patrols <- kmp_patrols %>%
  group_by(date) %>%
  mutate(total.patrols.on.day = sum(patrols)) %>%
  select(-patrols) # now redundant

## Make similar to kmp_shots


kmp_patrols <- kmp_patrols %>%
  mutate(night = mapvalues(night,c("YES","NO", "DUD"), c("Nocturnal", "Diurnal", "Unknown"))) %>%
  rename(timing = night)


write_csv(x = kmp_patrols, path = "data/kmp_patrols.csv")

## ===== sensor_locations =========================
## ========================================

sensor_locations <- read_csv("data-raw/sensor-locations.csv")

write_csv(sensor_locations, path = "data/sensor-locations.csv")

