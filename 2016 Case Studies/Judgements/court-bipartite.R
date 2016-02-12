
## References have type 0
## Courts have type 1
## there are 68 types of reference
## there are 5 courts

nodes.names <- c(reference.type.names,as.character(courtids.df$court.name))
nodes.ids <- 1:(length(nodes.names))
nodes.types <- c(rep(0,length(reference.type.names)),rep(1,length(courtids.df$court.name)))
nodes.df <- data.frame("id" = nodes.ids,
                       "names" = nodes.names,
                       "types" = nodes.types, stringsAsFactors = F) 




## Write a function that connects a case to each reference type that has a non-zero count
references.per.court.fn <- function(court){
  ## Delete argument assignment
  # court <- 5
  
  ## Find types that are referenced
  types.referenced.names <- reference.type.names[unlist(
    judgements.df[judgements.df$Court.ID == court,reference.type.names], use.names = F) > 0]
  types.referenced.names
  
  ## Where no references are made exit the function
  if(length(types.referenced.names) == 0){
    return()
  }
  
  ## Get node ids
  types.referenced.ids <- nodes.df[nodes.df$names %in% types.referenced.names,"id"]
  
  ## The actual case number is offset by the total number of reference types!
  ## See nodes.df definition for details
  court.node.id <- length(reference.type.names) + court
  
  ## Generate edge.list for the case
  edge.list <- vector()
  for (i in 1:length(types.referenced.ids)) {
    edge.list <- append(edge.list,types.referenced.ids[i])
    edge.list <- append(edge.list, court.node.id)
  }
  edge.list
}


## Create an empty vector
all.edges <- vector()
## Iterate through all cases:
for(i in 1:nrow(courtids.df)){
  all.edges <<- append(all.edges,references.per.court.fn(i))
}

all.edges


## Make an igraph
judgement.basic.graph <- graph(all.edges, directed = F)
judgement.basic.graph <- simplify(judgement.basic.graph)
plot(judgement.basic.graph)
# Remove nodes without connections!
judgement.basic.graph <- delete.vertices(judgement.basic.graph,which(degree(judgement.basic.graph)<1))
plot(judgement.basic.graph)


### Bipartite


library(plyr)

g <- make_bipartite_graph(
  ## This is the list of node types as enumerated through the list of all.edges:
  mapvalues(unique(all.edges), from = nodes.df$id, to = nodes.df$types),
  ## Map all.edges to be consecutive numbers
  mapvalues(all.edges, from = unique(all.edges), to = 1:length(unique(all.edges)))
)
lay <- layout.bipartite(g, maxiter = 1000, hgap = 100, vgap = 100)
plot(g, layout=lay[,2:1],vertex.size = 5,vertex.label = NA)

get.edgelist(g)

edges.df <- as.data.frame(get.edgelist(g))
colnames(edges.df) <- c("from","to")


nodes.df

library(visNetwork)

length(V(g))

mapvalues(all.edges, from = unique(all.edges), to = 1:length(unique(all.edges)))

unique(all.edges)




# V(g)$label <- nodes.df[nodes.df$id %in% unique(all.edges),]$names
V(g)$title <- nodes.df[nodes.df$id %in% unique(all.edges),]$names


visIgraph(g, idToLabel = FALSE) %>% visNodes(size = 10) %>%visIgraphLayout(layout = "layout.bipartite")


