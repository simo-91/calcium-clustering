library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
# Load cells/ROIs coordinates
calciumXY <- read.csv(file.choose())

calciumXY <- calciumXY %>%
  rename(
    "Cell" = X.1,
  )

#Ignore this if already done in crosscorrlag1.R
cmat <- as.data.frame(cmat)
colnames(cmat) <- rownames(cmat)
############################################

graph <- graph.adjacency(as.matrix(cmat), weighted = TRUE)

# Threshold correlation
graph <- delete.edges(graph, which(E(graph)$weight <0.6))


ggraph(graph, layout = as.matrix(calciumXY)[, c("X", "Y")]) +
  geom_edge_fan(aes(colour = weight)) +
  geom_node_point(size = 4) +
  scale_edge_color_gradient()
