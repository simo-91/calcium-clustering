
# Libraries ---------------------------------------------------------------
library(ggplot2)
library(igraph)
library(ggraph)
library(visNetwork) 
library(tidyverse)
library(tidygraph)
library(ggiraph)
library(ggnewscale)
library(grid)
library(gridExtra)
library(RColorBrewer)


# # Graph setup -------------------------------------------------------------
# ID0025.graph <- graph.adjacency(as.matrix(cmat), mode = "undirected", weighted = TRUE, diag = FALSE)
# ID0025.graph.raw <- graph.adjacency(as.matrix(cmat.RFP), mode = "undirected", weighted = TRUE, diag = FALSE)
# # Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# ID0025.graph <- delete.edges(ID0025.graph, which(E(ID0025.graph)$weight <0.75))
# 
# ##### Plot network
# g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
# g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
# g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)
# 
# # g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
# g.sizes.RFP <- c("TRUE" = 6)
# g.shapes.RFP <- c("TRUE" = 25)
# 
# # Hubs
# ID0025.hub_score <- (round(hub_score(ID0025.graph.raw)$value))/nrow(cmat) #normalized
# # maybe divide per no. of cells???
# 
# 
# ID0025.graph.plt <- ggraph(ID0025.graph, 
#                                          layout = as.matrix(ID0025.posXY)[, c("X", "Y")]) +
#                             geom_edge_density(aes(fill = weight))+
#                             geom_edge_link(aes(colour = weight, alpha = weight))+
#                             scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
#                             scale_edge_color_viridis(name = "F. Corr",
#                                                      alpha = 1,
#                                                      begin = 0.3,
#                                                      end = 1,
#                                                      discrete = FALSE,
#                                                      option = "inferno",
#                                                      direction = 1
#                             )+
#                           # Calcium levels and RFP positives
#                             geom_node_point(aes(fill = ID0025.posXY$Mean.dF,
#                                                 size = as.factor(ID0025.posXY$RFP),
#                                                 shape = as.factor(ID0025.posXY$RFP),
#                                                 colour = as.factor(ID0025.posXY$RFP)))+
#                             scale_fill_viridis_b()+
#                             scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2.5))+
#                             scale_shape_manual(values = c("TRUE" = 25, "FALSE" = 21))+
#                             scale_colour_manual(values = c("TRUE" = "#fc9272", "FALSE" = "black"))+
#                             labs(shape = "RFP",
#                                  colour = "RFP",
#                                  size = "RFP",
#                                  fill = "Ca")+
#                           # Hubs
#                             annotate("text", x=10, y=10, 
#                                      label = ID0025.hub_score)+
#                             # geom_node_point(aes(fill = as.factor(ID0025.posXY$synchron),
#                             #                     size = as.factor(ID0025.posXY$synchron),
#                             #                     shape = as.factor(ID0025.posXY$synchron)))+
#                             # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
#                             # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
#                             # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
#                             # geom_node_label(aes(label = ID0025.posXY$Cell), repel = TRUE)+
#                             
#                             theme_graph(plot_margin = margin(5, 5, 5, 5))+
#                             theme(legend.position = "right",
#                                   legend.margin	= margin(1,1,1,1),
#                                   legend.key.size = unit(0.5, 'cm'), #change legend key size
#                                   # legend.key.height = unit(1, 'pt'), #change legend key height
#                                   # legend.key.width = unit(1, 'pt'), #change legend key width
#                                   # legend.title = element_text(size=5), #change legend title font size
#                                   # legend.text = element_text(size=4),
#                                   # legend.text.align = 0
#                           )+
#                             ggtitle("ID0025n")+
#                             scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
# 



########################################################
# ################### RFP graph ########################
# ########################################################
ID0025.graph.RFP <- graph.adjacency(as.matrix(cmat.RFPt), mode = "undirected", weighted = TRUE, diag = FALSE)

# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# Set weight threshold
ID0025.graph.RFP <- delete.edges(ID0025.graph.RFP, which(E(ID0025.graph.RFP)$weight <0.10))

##### Plot network
# g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
# g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
# g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

# g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
# g.sizes.RFP <- c("TRUE" = 6)
# g.shapes.RFP <- c("TRUE" = 25)

# Hubs
# ID0025.hub_score.RFP <- (round(hub_score(ID0025.graph.RFP)$value))/nrow(cmat.RFP) #normalized on absolute adj matrix!


# Hub score of each node --------------------------------------------------
# ID0025.hub.score.RFP <- hub_score(ID0025.graph.RFP, scale = TRUE)
# ID0025.posXY.RFP$`Hub score` <- round(ID0025.hub.score.RFP$vector, digits = 2)
# ID0025.posXY.RFP$HScore <- (ID0025.posXY.RFP$`Hub score`)*100

# Robustness
ID0025.cohesion <- cohesion(ID0025.graph.RFP)

# Communities detection ---------------------------------------------------
# greedy method (hierarchical, fast method)
ID0025.graph.clusters.RFP = cluster_leading_eigen(ID0025.graph.RFP)
ID0025.posXY.RFP$Community <- ID0025.graph.clusters.RFP$membership
# ID0025.posXY.RFP$Member <- duplicated(ID0025.posXY.RFP$Community) to be fixed
# ID0025.posXY.RFP$degree <- ordered(degree(ID0025.graph.RFP))

# Network plot ------------------------------------------------------------
ID0025.graph.RFP.plt <- ggraph(ID0025.graph.RFP, 
                                          layout = as.matrix(ID0025.posXY.RFP)[, c("X", "Y")]) +
                                          geom_edge_link(aes(colour = weight, alpha = weight))+
                                          scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
                                          scale_edge_color_viridis(name = "F. Corr",
                                                                   alpha = 1,
                                                                   begin = 0.3,
                                                                   end = 1,
                                                                   discrete = FALSE,
                                                                   option = "inferno",
                                                                   direction = 1
                                          )+
                                          # Calcium levels and degrees
                                          geom_node_point(aes(fill = ordered(cluster_leading_eigen(ID0025.graph.RFP)$membership),
                                                              size = ordered(degree(ID0025.graph.RFP))),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0025.posXY.RFP$Cell), 
                                                          repel = TRUE,
                                                         size = 2.5)+
                                          geom_node_text(aes(label = ordered(cluster_leading_eigen(ID0025.graph.RFP)$membership)),
                                                         colour = "white",
                                                         fontface = 1,
                                                         size = 3)+
                                          scale_fill_brewer(palette = "Set1",
                                                            guide = "none")+
                                          scale_size_manual(values = c("0" = 5, "1" = 8, "2" = 9, "3" = 10,
                                                                       "4" = 11, "5" = 12, "6" = 13, "7" = 14,
                                                                       "8" = 15, "9" = 16, "10" = 17),
                                                            guide = "none")+
                                          # scale_shape_manual(values = c("TRUE" = 25, "FALSE" = 21))+
                                          # scale_colour_manual(values = c("TRUE" = "#fc9272", "FALSE" = "black"))+
                                          # labs(fill = "Ca")+
                                          # Hubs
                                          # annotate("text", x=45, y=20, 
                                          #           label = "W. thresh. = 0.50")+
                                          # annotate("text", x=40, y=35,
                                          #          label = "Cohesion = ")+
                                          # annotate("text", x=70, y=35,
                                          #          label = ID0025.cohesion)+
                                          # geom_node_point(aes(fill = as.factor(ID0025.posXY$synchron),
                                          #                     size = as.factor(ID0025.posXY$synchron),
                                          #                     shape = as.factor(ID0025.posXY$synchron)))+
                                          # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                                          # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                                          # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                                          # geom_node_label(aes(label = ID0025.posXY$Cell), repel = TRUE)+
                                          
                                          theme_graph(background = "white",
                                                      plot_margin = margin(5, 5, 5, 5))+
                                          theme(legend.position = "right",
                                                legend.margin	= margin(1,1,1,1),
                                                legend.key.size = unit(0.5, 'cm'), #change legend key size
                                                # legend.key.height = unit(1, 'pt'), #change legend key height
                                                # legend.key.width = unit(1, 'pt'), #change legend key width
                                                # legend.title = element_text(size=5), #change legend title font size
                                                # legend.text = element_text(size=4),
                                                # legend.text.align = 0
                                          )+
                                          ggtitle("ID0025")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed


# Histogram count of degrees ----------------------------------------------

ID0025.degree.RFP.hist <- ggplot(ID0025.posXY.RFP, aes(x = degree))+ 
                            stat_count()

# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(T.RFPt, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0025.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0025.scree.RFP <- ggpar(ID0025.scree.RFP, title = element_blank())
# Viz ---------------------------------------------------------------------
gs = c(ID0025.graph.RFP.plt, ID0025.degree.RFP.hist,
      ID0025.RFP.grid, ID0025.RFPsum2.plt, ID0025.scree.RFP)

lay <- rbind(c(1,1,3,3),
             c(1,1,3,3),
             c(2,5,4,4))
ID0025.arranged <- grid.arrange(ID0025.graph.RFP.plt, ID0025.degree.RFP.hist,
                     ID0025.RFPt.grid, ID0025.RFPsum2.plt, ID0025.scree.RFP,
                     layout_matrix = lay)


#Save whole graph + raster + activity plot + coactive cells/time
ggsave(plot = ID0025.arranged, file = "ID0025.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
ggsave(plot = ID0025.arranged, file = "/Users/Simo/calcium-clustering/plots/ID0025.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)




# plot communities with shaded regions
# plot(c2, ID0025.graph.RFP, layout=as.matrix(ID0025.posXY.RFP)[, c("X", "Y")])
