
data_figure2A <- read_csv("data/export_figure_2_6_states.csv")
colnames(data_figure2A) <- make.names(trimws(tolower(colnames(data_figure2A))))

data_6states <- read_csv("data/export_figure_2_6_states.csv")


data_figure2A <- data_figure2A %>%
  rename(standardized_report_per_round = X.standardized_report_per_round)

data_figure2A$country %>% unique()




qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual', ]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
country_colours <- data_frame(
  country = unique(data_figure2A$country),
  colour = col_vector[1:length(unique(data_figure2A$country))]
)

data_figure2A <- data_figure2A %>%
  mutate(colour = mapvalues(country, from = country_colours$country, to = country_colours$colour))
