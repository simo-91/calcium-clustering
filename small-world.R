# Small-worldness
smallworldness <- function(graph) {
  # generate an equivalent Erdos Renyi network (same no. of edges and nodes, random allocation)
  random_smallworld <- erdos.renyi.game(n = vcount(graph), p = ecount(graph), type = "gnm")
  # Small-worldness as sigma = ( C(g)/C(random-g) ) / ( (L(g)/L(random-g) )
  sigma.graph <- (transitivity(graph)/transitivity(random_smallworld)) / (average.path.length(graph)/average.path.length(random_smallworld))
  return(sigma.graph)
}
