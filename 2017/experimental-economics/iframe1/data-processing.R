data_figure1 <- read_csv("data/export_figure_1.csv")
colnames(data_figure1) <- make.names(tolower(colnames(data_figure1)))
## Kill na country
data_figure1 <- data_figure1 %>%
  filter(!is.na(country))


data_figureA2 <- read_csv("data/export_figure_A2.csv")
colnames(data_figureA2) <- make.names(trimws(tolower(colnames(data_figureA2))))

# data_figureA2 <- data_figureA2 %>%
#   rename(standardized_report_per_round = X.standardized_report_per_round)


qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual', ]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


citation_colors <- data_frame(
  citation = unique(data_figure1$citation),
  color = sample(unique(col_vector),
                 length(unique(data_figure1$citation)), replace = TRUE)
)


data_figure1 <- data_figure1 %>%
  mutate(color = plyr::mapvalues(data_figure1$citation,
                            from = unique(data_figure1$citation),
                            to = citation_colors$color))

