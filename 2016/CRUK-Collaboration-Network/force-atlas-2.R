

departments_g <- contract_instition_network(institution_igraph)

invisible(network <- visIgraph(
  # as.undirected(departments_g),
  as.undirected(institution_igraph),
  layout = "layout.forceatlas2",
  directed = FALSE,
  k = 1000, # repulsion
  delta = 80, # attraction
  ks = 10, # speed constant
  ksmax = 20, # limits speed
  # autostab parameters can't be found
  gravity = 100,
  iterations = 200,
  nohubs = FALSE,
  randomSeed = 1
  # linlog = TRUE
))

network %>%
  visEdges(smooth = list("enabled" = TRUE,
                         "type" = "curvedCCW"))

help("layout.forceatlas2")
