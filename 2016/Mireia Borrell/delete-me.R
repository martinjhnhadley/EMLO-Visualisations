cutoff_timeline_data <- timeline_data

cutoff_timeline_data %>% colnames()

  
dput(sample_data)

valid_columns <- c("Type.of.Policy",                                                                                             "Name.of.Policy",  
                   "Valid.from.b.",                                                                                               "Valid.until.c.")

sample_data <- cutoff_timeline_data[, valid_columns]


sample_data$Type.of.Policy

sample_data$Type.of.Policy <- mapvalues(sample_data$Type.of.Policy, from = unique(sample_data$Type.of.Policy), to = c("A","B","C"))

sample_data$Valid.from.b.[sample_data$Valid.from.b. < as.Date("1997/01/01")] <- as.Date("1997/01/01")
gantt_separators <- sample_data %>%
  filter(Valid.from.b. > as.Date("1997/01/01") & Valid.until.c. > as.Date("1997/01/01"))

timeline_ggplot <- ggplot(
  sample_data,
    aes(
      x = Valid.from.b.,
      xend = Valid.until.c.,
      y = Name.of.Policy,
      yend = Name.of.Policy,
      colour = Type.of.Policy
    )
  ) +
  geom_segment(size = 4,
               aes(
                 x = Valid.from.b.,
                 xend = Valid.until.c.,
                 y = Name.of.Policy,
                 yend = Name.of.Policy,
                 text = NULL
               )
  )  + 
  geom_segment(
    data = gantt_separators,
    size = 4,
               aes(
                 x = Valid.from.b. - 18,
                 xend = Valid.from.b. + 18,
                 y = Name.of.Policy,
                 yend = Name.of.Policy,
                 text = NULL
               ),
               color = "black"
  ) +
  geom_segment(size = 4,
               show.legend = F,
               aes(
                 x = Valid.from.b.,
                 xend = Valid.until.c.,
                 y = Name.of.Policy,
                 yend = Name.of.Policy,
                 text = paste(Valid.from.b.,
                   Valid.until.c.,
                  Name.of.Policy,
                  Type.of.Policy
                 ),
                 alpha = 0.001 # Draw tooltipped geom_segments over everything, make almost invisible
               )
  )  +
  scale_x_date(
    breaks = date_breaks("12 month"),
    labels = date_format("%b %Y"),
    limits = c(as.Date("1997/01/01"), as.Date(max(cutoff_timeline_data$Valid.until.c.)))
  ) + 
    xlab("") + scale_colour_brewer(name = "Type of Policy",
                                  type = "qual",
                                  palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = unit(c(0,0,1,0), "cm"))


ggplotly(timeline_ggplot, tooltip = "text")