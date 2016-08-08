advisor_supervisor_color_scheme <- list(
  "Examined_own_student" = "#80b1d3",
  "Examined_and_Supervised" = "#fb8072",
  "Supervised_Only" = "#8dd3c7",
  "Examined_Only" = "#bebada",
  "Authored_Only" = "#ffffb3"
)


set_node_colour <- function(node){
  if(node$N_own_students_examined > 0) {
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

head(entire_graph_node_df)

entire_graph_node_df[1,]

set_node_colour(entire_graph_node_df[1,])


foo <- as.vector(lapply(1:nrow(entire_graph_node_df), function(x) set_node_colour(entire_graph_node_df[x,])))


sum(is.na(foo))



