## ============ Contract institute network ==========================================
## ==============================================================================

contract_instition_network <- function(uncontracted_graph = NA){
  contracted_graph <-
    contract(uncontracted_graph, mapping = as.numeric(
      mapvalues(
        institution_nodes$department,
        from = department_colours$department,
        to = 1:nrow(department_colours)
      )
    ))
  
  V(contracted_graph)$name <- unique(institution_nodes$department)
  V(contracted_graph)$title <- V(contracted_graph)$name
  V(contracted_graph)$color <-
    mapvalues(unlist(V(contracted_graph)$name),
              from = department_colours$department,
              to = department_colours$colours)
  
  contracted_graph <-
    simplify(as.undirected(contracted_graph))
  contracted_graph
  
}
