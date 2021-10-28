library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
library(tidyverse)
library(tidygraph)
library(ggiraph)
library(ggnewscale)

HRAS5dpf_hind2_15min.graph <- graph.adjacency(as.matrix(cmat), mode = "undirected", weighted = TRUE, diag = FALSE)

# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
HRAS5dpf_hind2_15min.graph <- delete.edges(HRAS5dpf_hind2_15min.graph, which(E(HRAS5dpf_hind2_15min.graph)$weight <0.60))

##### Plot network
g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
g.sizes.RFP <- c("TRUE" = 2.5,"FALSE" = 1)
g.shapes.RFP <- c("TRUE" = 21, "FALSE" = 21)

g.palette.Ca <- brewer.pal(10, )

HRAS5dpf_hind2_15min.graph.plt <- ggraph(HRAS5dpf_hind2_15min.graph, 
                                         layout = as.matrix(HRAS5dpf_hind2_15min.posXY)[, c("X", "Y")]) +
                            geom_edge_link(aes(colour = weight, alpha = weight))+
                            scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
                         # calcium levels
                            geom_node_point(aes(colour = HRAS5dpf_hind2_15min.posXY$Mean.dF), size = 3)+
                            scale_colour_gradient(low = "black", high = "green")+
                         # RFP positives
                            new_scale("fill")+
                            new_scale("shape")+
                            geom_node_point(aes(fill = as.factor(HRAS5dpf_hind2_15min.posXY$RFP),
                                                size = as.factor(HRAS5dpf_hind2_15min.posXY$RFP),
                                                shape = as.factor(HRAS5dpf_hind2_15min.posXY$RFP)))+
                            scale_fill_manual(values = g.palette.RFP, name = "RFP")+
                            scale_shape_manual(values = g.shapes.RFP, name = "RFP")+
                            scale_size_manual(values = g.sizes.RFP, name = "RFP")+
                            # geom_node_point(aes(fill = as.factor(HRAS5dpf_hind2_15min.posXY$synchron),
                            #                     size = as.factor(HRAS5dpf_hind2_15min.posXY$synchron),
                            #                     shape = as.factor(HRAS5dpf_hind2_15min.posXY$synchron)))+
                            # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                            # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                            # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                            # # geom_node_label(aes(label = HRAS5dpf_hind2_15min.posXY$Cell), repel = FALSE)+
                            scale_edge_color_viridis(name = "Correlation",
                              alpha = 1,
                              begin = 0,
                              end = 1,
                              discrete = FALSE,
                              option = "inferno",
                              direction = 1
                            )+
                            theme_graph(plot_margin = margin(5, 5, 5, 5))+
                            theme(legend.position = "right",
                                  legend.margin	= margin(1,1,1,1),
                                  legend.key.size = unit(0.5, 'cm'), #change legend key size
                                  # legend.key.height = unit(1, 'pt'), #change legend key height
                                  # legend.key.width = unit(1, 'pt'), #change legend key width
                                  # legend.title = element_text(size=5), #change legend title font size
                                  # legend.text = element_text(size=4),
                                  # legend.text.align = 0
                          )+
                            ggtitle("HRAS5dpf_hind2_15min")+
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
ggsave(plot = HRAS5dpf_hind2_15min.graph, file = "HRAS5dpf_hind2_15min.graph.png", 
       device = "png",  bg = "transparent",
       width = 20, height = 15, units = "cm", dpi = 800)
