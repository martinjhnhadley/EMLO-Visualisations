cutoff_timeline_data <- timeline_data

cutoff_timeline_data %>% colnames()


valid_columns <- c("Type.of.Policy",                                                                                             "Name.of.Policy",  
                   "Valid.from.b.",                                                                                               "Valid.until.c.")

sample_data <- cutoff_timeline_data[, valid_columns]

sample_data$Type.of.Policy

sample_data$Type.of.Policy <- mapvalues(sample_data$Type.of.Policy, from = unique(sample_data$Type.of.Policy), to = c("A","B","C"))

sample_data$Name.of.Policy <- mapvalues(sample_data$Name.of.Policy, from = unique(sample_data$Name.of.Policy), to = letters[1:15])


sample_data <- structure(list(Type.of.Policy = c("A", "A", "A", "A", "A", "A", 
                                                 "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                                                 "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                                                 "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "C", "C", 
                                                 "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", 
                                                 "C", "C", "C", "C", "C", "C", "C", "C", "C", "C"), Name.of.Policy = structure(c(8L, 
                                                                                                                                 8L, 8L, 8L, 8L, 8L, 7L, 7L, 7L, 7L, 6L, 6L, 6L, 6L, 15L, 15L, 
                                                                                                                                 15L, 15L, 9L, 9L, 9L, 9L, 9L, 9L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 
                                                                                                                                 3L, 5L, 5L, 5L, 5L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 11L, 11L, 
                                                                                                                                 11L, 12L, 1L, 1L, 1L, 1L, 1L, 1L, 13L, 13L, 13L, 13L, 13L, 13L, 
                                                                                                                                 2L, 2L, 14L, 14L, 14L, 14L, 14L, 14L, 14L), .Label = c("l", "n", 
                                                                                                                                                                                        "g", "f", "h", "c", "b", "a", "e", "i", "j", "k", "m", "o", "d"
                                                                                                                                 ), class = "factor"), Valid.from.b. = structure(c(8926, 9057, 
                                                                                                                                                                                   10940, 12015, 13422, 16405, 10940, 11697, 15772, 16530, 12029, 
                                                                                                                                                                                   14705, 16530, 16405, 12029, 13422, 16530, 16405, 8927, 11049, 
                                                                                                                                                                                   12148, 13422, 16161, 16251, 8927, 12148, 13422, 16405, 12029, 
                                                                                                                                                                                   14705, 16530, 16405, 12029, 13422, 16530, 16405, 10470, 12509, 
                                                                                                                                                                                   13239, 14123, 14853, 15949, 16321, 9959, 10379, 10869, 11053, 
                                                                                                                                                                                   12148, 13244, 13975, 15070, 15436, 17262, 12148, 13975, 15070, 
                                                                                                                                                                                   15436, 16252, 17262, 15824, 16902, 9958, 10413, 12149, 12539, 
                                                                                                                                                                                   13248, 15712, 16252), class = "Date"), Valid.until.c. = structure(c(9056, 
                                                                                                                                                                                                                                                       10939, 12014, 13421, 17067, 17067, 11696, 15771, 16529, 17067, 
                                                                                                                                                                                                                                                       14704, 16529, 17067, 17067, 13421, 17067, 17067, 17067, 11048, 
                                                                                                                                                                                                                                                       12147, 13421, 16160, 17067, 17067, 12147, 13421, 17067, 17067, 
                                                                                                                                                                                                                                                       14704, 16529, 17067, 17067, 13421, 16529, 17067, 17067, 12508, 
                                                                                                                                                                                                                                                       13238, 14122, 14852, 15948, 16320, 17067, 10378, 10868, 12147, 
                                                                                                                                                                                                                                                       12147, 13243, 13974, 15069, 15435, 17261, 17067, 13974, 15069, 
                                                                                                                                                                                                                                                       15435, 16251, 17261, 17067, 16901, 17067, 10412, 12148, 12538, 
                                                                                                                                                                                                                                                       13247, 15711, 16251, 17067), class = "Date")), .Names = c("Type.of.Policy", 
                                                                                                                                                                                                                                                                                                                 "Name.of.Policy", "Valid.from.b.", "Valid.until.c."), row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                     -68L), class = "data.frame")




sample_data$Valid.from.b.[sample_data$Valid.from.b. < as.Date("1997/01/01")] <- as.Date("1997/01/01")

gantt_separators <- sample_data %>%
  filter(Valid.from.b. > as.Date("1997/01/01") & Valid.until.c. > as.Date("1997/01/01"))

basic_plot <- ggplot(
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
  )


{basic_plot + geom_segment(
    data = gantt_separators,
    size = 4,
               aes(
                 x = Valid.from.b. - 18,
                 xend = Valid.from.b. + 18,
                 y = Name.of.Policy,
                 yend = Name.of.Policy,
                 text = NULL
               ),
               color = "black") +
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
  )} %>% ggplotly(tooltip = "text")



gantt_separators <- sample_data %>%
  filter(Valid.from.b. > as.Date("1997/01/01") & Valid.until.c. > as.Date("1997/01/01"))

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