library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
library(tidyverse)

#Ignore this if already done in crosscorrlag1.R
#cmat <- as.data.frame(cmat)
#colnames(cmat) <- rownames(cmat)
############################################



graph <- graph.adjacency(as.matrix(cmat), weighted = TRUE)

# Threshold correlation
graph <- delete.edges(graph, which(E(graph)$weight <0.75))

# DISPLAY NETWORK
ggraph(graph, layout = as.matrix(posXY)[, c("X", "Y")]) +
  geom_edge_link(aes(colour = weight))+
  geom_node_point(size = 1)+
  # geom_node_text(aes(label = posXY$Cell))+
    scale_edge_color_gradient(
      low = "blue",
      high = "red",
      space = "Lab",
      na.value = "grey50",
      guide = "edge_colourbar"
    )+
  theme(rect = element_rect(fill = "transparent"),
        legend.position = "none")+
  scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed



# OVERLAY
transparentgraph <- ggraph(graph, layout = as.matrix(posXY)[, c("X", "Y")]) +
  geom_edge_link(aes(colour = weight), width = 0.5)+
  scale_edge_width(range = c(0.2, 2))+
  geom_node_point(size = 0)+
  scale_edge_color_gradient(
    low = "blue",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = "edge_colourbar"
  )+
  theme_void()+
  theme(legend.position="none",
  rect = element_rect(fill = "transparent"))+
  scale_y_reverse()


#Save transparent graph
ggsave(plot = transparentgraph, file = "network.png", 
       type = "cairo-png",  bg = "transparent",
       width = 20, height = 15, units = "cm", dpi = 800)
