## beautification

## ====================================== labeller ==========================
## ============================================================================

emre_event_labeller <- function(no.individuals,
                                species,
                                camera.id,
                                time,
                                no.males,
                                no.females,
                                no.unknown,
                                no.sa,
                                no.juv) {
  paste0(
    no.individuals,
    " ",
    species,
    " by ",
    camera.id,
    " at ",
    time,
    " ",
    no.males,
    " males, ",
    no.females,
    " females, ",
    no.unknown,
    " unknown.",
    " ",
    no.sa,
    " small adults, ",
    no.juv,
    " small animals."
  )
}

## ====================================== shapes ==========================
## ============================================================================

sample_shapes_data <- data.frame(
  event_type = {
    unique_species
  },
  event_shape = {
    sample(
      c(
        "http://image.flaticon.com/icons/svg/23/23957.svg",
        "http://image.flaticon.com/icons/svg/61/61264.svg",
        "http://image.flaticon.com/icons/svg/109/109239.svg",
        "http://image.flaticon.com/icons/svg/109/109254.svg",
        "http://image.flaticon.com/icons/svg/58/58777.svg",
        "http://image.flaticon.com/icons/svg/34/34872.svg",
        "http://image.flaticon.com/icons/svg/32/32904.svg"
      ),
      length(unique_species),
      replace = TRUE
    )
  },
  stringsAsFactors = F
)

## ====================================== legend ==========================
## ============================================================================

emre_legend <- emre_events %>%
  rename(description = species) %>%
  unique() %>%
  mutate(
    shape = mapvalues(
      description,
      sample_shapes_data$event_type,
      sample_shapes_data$event_shape,
      warn_missing = FALSE
    )
  ) %>%
  select(description, shape, event_type_id) %>%
  unique()