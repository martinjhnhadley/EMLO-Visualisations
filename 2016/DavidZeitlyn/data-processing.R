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


names_df <-
  read.csv(
    "data/names2.txt",
    stringsAsFactors = F,
    quote = "",
    sep = "\t",
    header = F
  )

## ================ Fix mutant Names  =============================
## ===========================================================

## The names2.txt file is garbled and the names need to be reconstructed.


garbled_names <- names_df[names_df$V3 != "",]
garbled_names$V2 <- paste(garbled_names$V2, garbled_names$V3)
names_df[names_df$V3 != "",] <- garbled_names
names_df <- names_df[,1:2]
colnames(names_df) <- c("id","name")
## Attempt to convert bad characters
names_df$name <- stringi::stri_trans_general(names_df$name, "latin-ascii")
## Remove bad characters
names_df$name <- stringi::stri_trans_general(names_df$name, "latin-ascii")
names_df$name <- iconv(names_df$name, "latin1", "ASCII", sub="")
names_df$name <- gsub("\032", "", names_df$name)
names_df$name <- gsub("\v","",names_df$name)

## There are some names with vertical tabs \v that need to be fixed and trimws
examiners_df <-
  as.data.frame(lapply(examiners_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)
examiners_df <-
  as.data.frame(lapply(examiners_df, function(y)
    trimws(y)), stringsAsFactors = F)
supervisors_df <-
  as.data.frame(lapply(supervisors_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)
supervisors_df <-
  as.data.frame(lapply(supervisors_df, function(y)
    trimws(y)), stringsAsFactors = F)
authors_df <-
  as.data.frame(lapply(authors_df, function(y)
    gsub("[\v]", "", y)), stringsAsFactors = F)
authors_df <-
  as.data.frame(lapply(authors_df, function(y)
    trimws(y)), stringsAsFactors = F)

## Remove those entries where supervisor_id and examiner_id are ""

examiners_df <- examiners_df[examiners_df$Examiner_id != "", ]
examiners_df <- examiners_df[examiners_df$Author_id != "", ]
supervisors_df <-
  supervisors_df[supervisors_df$Supervisor_id != "", ]
supervisors_df <- supervisors_df[supervisors_df$Author_id != "", ]

## ================ Find Missing Names  =============================
## ===========================================================

all_ids <- unique(c(examiners_df$Examiner_id,examiners_df$Author_id,supervisors_df$Supervisor_id,supervisors_df$Author_id,authors_df$Author_id))

missing_names <- setdiff(names_df$id, all_ids)

## ================ Build Complete igraph  =============================
## ===========================================================

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
## Name vertices
V(entire_igraph)$name <- mapvalues(V(entire_igraph)$name, from = names_df$id, to = names_df$name)

## Add supervisor/advisor property
V(entire_igraph)$supervisor <- ids_vs_numbers$supervised > 0
V(entire_igraph)$examined <- ids_vs_numbers$examined > 0
V(entire_igraph)$number_supervised <- ids_vs_numbers$supervised
V(entire_igraph)$number_examined <- ids_vs_numbers$examined
V(entire_igraph)$number_own_examined <- ids_vs_numbers$N_own_students_examined
entire_graph_node_df <- as.data.frame(vertex.attributes(entire_igraph))

V(entire_igraph)$color <- unlist(lapply(1:nrow(entire_graph_node_df), function(x)set_node_colour(entire_graph_node_df[x,])))

## Simplify graph
non_zero_igraph <-
  simplify(delete.vertices(entire_igraph, V(entire_igraph)[degree(entire_igraph) ==
                                                             0]))

decomposed_igraph <- decompose(non_zero_igraph)
## order vcounts
component_vcounts <- unlist(lapply(decomposed_igraph, function(x)vcount(x)))
decomposed_igraph <- decomposed_igraph[rev(order(component_vcounts))]

## ================ Experiment Area ==========================
## ===========================================================




