library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
library(tidyverse)
library(tidygraph)
library(ggiraph)

graph <- graph.adjacency(as.matrix(cmat), weighted = TRUE)

# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
graph <- delete.edges(graph, which(E(graph)$weight <0.70))


# Label RFP cells as such
posXY$RFP <- posXY$Cell %in% RFPcells
# DISPLAY NETWORK
AKT1hindbrain1.graph <- ggraph(graph, layout = as.matrix(posXY)[, c("X", "Y")]) +
                            geom_edge_link(aes(colour = weight, edge_width = weight))+
                            scale_edge_width_continuous(range = c(0.1, 1))+
                            geom_node_point(aes(shape = as.factor(posXY$Cluster), colour = as.factor(posXY$Cluster), size = as.factor(posXY$RFP)))+
                          # geom_point_interactive(aes(x = posXY$X, y = posXY$Y, shape = as.factor(posXY$Cluster), colour = as.factor(posXY$Cluster), tooltip = as.factor(posXY$Cluster), data_id = as.factor(posXY$Cluster)), size  = 2)+
                            scale_edge_color_viridis(
                              alpha = 0.6,
                              begin = 0,
                              end = 1,
                              discrete = FALSE,
                              option = "inferno",
                              direction = 1
                            )+
                            theme_graph()+
                            ggtitle("AKT1 hindbrain 1")+
                            scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed

girafe(ggobj = AKT1hindbrain1.graph)

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
ggsave(plot = CTRL5dpfhi1.graph, file = "CTRL5dpfhi1.graph.png", 
       device = "png",  bg = "transparent",
       width = 20, height = 15, units = "cm", dpi = 800)
