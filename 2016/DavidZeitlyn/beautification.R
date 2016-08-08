advisor_supervisor_color_scheme <- list(
  "Examined_own_student" = "#80b1d3",
  "Examined_and_Supervised" = "#fb8072",
  "Supervised_Only" = "#8dd3c7",
  "Examined_Only" = "#bebada",
  "Authored_Only" = "#ffffb3"
)

node_legend <-
  data.frame(
    label = c(
      "Examined Own",
      "Examiner & Supervisor",
      "Supervisor",
      "Examinor",
      "Author"
    ),
    # shape = c("icon","icon","icon"),
    # icon.code = c("f007","f0c0","f007"),
    icon.color = as.character(advisor_supervisor_color_scheme),
    icon.size = c(48,48,48,48,48),
    id = 1:5
  )


set_node_colour <- function(node) {
  if (node$N_own_students_examined > 0) {
    advisor_supervisor_color_scheme$Examined_own_student
  } else {
    if (node$supervised > 0 & node$examined) {
      advisor_supervisor_color_scheme$Examined_and_Supervised
    } else {
      if (node$supervised > 0) {
        advisor_supervisor_color_scheme$Supervised_Only
      } else {
        if (node$examined > 0) {
          advisor_supervisor_color_scheme$Examined_Only
        } else {
          advisor_supervisor_color_scheme$Authored_Only
        }
      }
    }
  }
}
