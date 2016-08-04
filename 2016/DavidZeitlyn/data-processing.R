## ================ Import Files =============================
## ===========================================================

authors_df <-
  read.csv(
    "data/authors.tab",
    sep = "\t",
    quote = "",
    stringsAsFactors = F
  )

examiners_df <-
  read.csv(
    "data/examiners.tab",
    sep = "\t",
    quote = "",
    stringsAsFactors = F
  )

ids_vs_numbers <-
  read.csv(
    "data/ids_vs_numbers.tab",
    sep = "\t",
    quote = "",
    stringsAsFactors = F
  )

supervisors_df <-
  read.csv(
    "data/supervisors.tab",
    sep = "\t",
    quote = "",
    stringsAsFactors = F
  )
## TODO: trimws on the columns....


## ================ Fix mutant Names  =============================
## ===========================================================

## There are some names with vertical tabs \v that need to be fixed
examiners_df <-
  as.data.frame(lapply(examiners_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)
supervisors_df <-
  as.data.frame(lapply(supervisors_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)
authors_df <-
  as.data.frame(lapply(authors_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)

## Remove those entries where supervisor_id and examiner_id are ""

examiners_df <- examiners_df[examiners_df$Examiner_id != "", ]
examiners_df <- examiners_df[examiners_df$Author_id != "", ]
supervisors_df <-
  supervisors_df[supervisors_df$Supervisor_id != "", ]
supervisors_df <- supervisors_df[supervisors_df$Author_id != "", ]

## ================ Build Complete igraph  =============================
## ===========================================================

library(igraph)
## nodes
all_nodes <- ids_vs_numbers
colnames(all_nodes) <-
  c("id", "supervised", "examined", "N_own_students_examined")
## edges
all_edges <-
  data.frame(Map(c, examiners_df, supervisors_df), stringsAsFactors = F)
colnames(all_edges) <-
  c("from", "to", "Examiner_Affilition_if_different")

entire_igraph <-
  simplify(graph.data.frame(all_edges, vertices = all_nodes))

non_zero_igraph <-
  simplify(delete.vertices(entire_igraph, V(entire_igraph)[degree(entire_igraph) ==
                                                             0]))

decomposed_igraph <- decompose(non_zero_igraph)
## order vcounts
component_vcounts <- unlist(lapply(decomposed_igraph, function(x)vcount(x)))
decomposed_igraph <- decomposed_igraph[rev(order(component_vcounts))]

component_names_for_ui <-
  setNames(as.list(1:length(decomposed_igraph)), c("Main Component", paste(
    "Component Number", 2:length(decomposed_igraph)
  )))

## ================ Missing Nodes =============================
## ===========================================================

main_component <- decomposed_igraph[[1]]

ecount(main_component)

decompose(main_component)





