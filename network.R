library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
library(tidyverse)

#Ignore this if already done in crosscorrlag1.R
cmat <- as.data.frame(cmat)
colnames(cmat) <- rownames(cmat)
############################################


# Load cells/ROIs coordinates
calciumXY <- read.csv(file.choose())

calciumXY <- calciumXY %>%
  rename(
    "Cell" = X.1,
  )

calciumXY <- calciumXY %>%
  select()

graph <- graph.adjacency(as.matrix(cmat), weighted = TRUE)

# Threshold correlation
graph <- delete.edges(graph, which(E(graph)$weight <0.8))

# DISPLAY NETWORK
ggraph(graph, layout = as.matrix(calciumXY)[, c("X", "Y")]) +
  geom_edge_link(aes(colour = weight))+
  geom_node_point(size = 2)+
    scale_edge_color_gradient(
      low = "blue",
      high = "red",
      space = "Lab",
      na.value = "grey50",
      guide = "edge_colourbar"
    )+
  theme_minimal()
# +theme(legend.position="none",   
          # rect = element_rect(fill = "transparent"))



# OVERLAY
# #ggraph(graph, layout = as.matrix(calciumXY)[, c("X", "Y")]) +
#   geom_edge_link(aes(colour = weight), width = 0.5)+
#   scale_edge_width(range = c(0.2, 2))+
#   geom_node_point(size = 0)+
#   scale_edge_color_gradient(
#     low = "blue",
#     high = "red",
#     space = "Lab",
#     na.value = "grey50",
#     guide = "edge_colourbar"
#   )+
#   theme_void()+
#   theme(legend.position="none",   
#   rect = element_rect(fill = "transparent"))
