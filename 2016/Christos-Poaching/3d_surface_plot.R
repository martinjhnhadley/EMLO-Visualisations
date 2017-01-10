## 3D surface plots

x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500
plot_ly() %>% add_surface(x = ~ x, y = ~ y, z = ~ volcano)


corr <- cor(diamonds[vapply(diamonds, is.numeric, logical(1))])

corr

plot_ly(x = rownames(corr),
        y = colnames(corr),
        z = corr) %>%
  add_surface() %>%
  colorbar(limits = c(-1, 1))

class(corr)

## gunshots:
## x: time
## y: month
## z: number of gunshots

shots_by_month_then_hour <- kmp_gunshots %>%
  filter(!is.na(tod)) %>%
  select(tod, month, total.gunshots.on.day) %>%
  group_by(month, tod) %>%
  mutate(shots.by.hour.and.month = n()) %>%
  select(-total.gunshots.on.day) %>%
  ungroup() %>%
  unique() %>%
  spread(month, shots.by.hour.and.month)

shots_by_month_then_hour <- shots_by_month_then_hour %>%
  as.data.frame()

shots_by_month_then_hour[is.na(shots_by_month_then_hour)] <- 0

rownames(shots_by_month_then_hour) <- shots_by_month_then_hour$month

shots_matrix <- shots_by_month_then_hour %>%
  select(-tod) %>%
  as.matrix()

plot_ly(
  y = rownames(shots_matrix),
  x = colnames(shots_matrix),
  z = shots_matrix
) %>%
  add_surface() %>%
  layout(scene = list(xaxis = list(title = "Month"),
         yaxis = list(title = "Hour"),
         zaxis = list(title = "Total Gunshots")))
