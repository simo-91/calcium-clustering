
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

# Graph setup -------------------------------------------------------------
ID0026.graph <- graph.adjacency(as.matrix(cmat), mode = "undirected", weighted = TRUE, diag = FALSE)
ID0026.graph.raw <- graph.adjacency(as.matrix(cmat.RFP), mode = "undirected", weighted = TRUE, diag = FALSE)
# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
ID0026.graph <- delete.edges(ID0026.graph, which(E(ID0026.graph)$weight <0.75))

##### Plot network
g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

# g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
g.sizes.RFP <- c("TRUE" = 6)
g.shapes.RFP <- c("TRUE" = 25)

# Hubs
ID0026.hub_score <- (round(hub_score(ID0026.graph.raw)$value))/nrow(cmat) #normalized
# maybe divide per no. of cells???


ID0026.graph.plt <- ggraph(ID0026.graph, 
                                         layout = as.matrix(ID0026.posXY)[, c("X", "Y")]) +
                            geom_edge_density(aes(fill = weight))+
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
                          # Calcium levels and RFP positives
                            geom_node_point(aes(fill = ID0026.posXY$Mean.dF,
                                                size = as.factor(ID0026.posXY$RFP),
                                                shape = as.factor(ID0026.posXY$RFP),
                                                colour = as.factor(ID0026.posXY$RFP)))+
                            scale_fill_viridis_b()+
                            scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2.5))+
                            scale_shape_manual(values = c("TRUE" = 25, "FALSE" = 21))+
                            scale_colour_manual(values = c("TRUE" = "#fc9272", "FALSE" = "black"))+
                            labs(shape = "RFP",
                                 colour = "RFP",
                                 size = "RFP",
                                 fill = "Ca")+
                          # Hubs
                            annotate("text", x=10, y=10, 
                                     label = ID0026.hub_score)+
                            # geom_node_point(aes(fill = as.factor(ID0026.posXY$synchron),
                            #                     size = as.factor(ID0026.posXY$synchron),
                            #                     shape = as.factor(ID0026.posXY$synchron)))+
                            # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                            # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                            # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                            # geom_node_label(aes(label = ID0026.posXY$Cell), repel = TRUE)+
                            
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
                            ggtitle("ID0026n")+
                            scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed




########################################################
# ################### RFP graph ########################
# ########################################################
ID0026.graph.RFP <- graph.adjacency(as.matrix(cmat.RFP), mode = "undirected", weighted = TRUE, diag = FALSE)

# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# Set weight threshold
ID0026.graph.RFP <- delete.edges(ID0026.graph.RFP, which(E(ID0026.graph.RFP)$weight <0.50))

##### Plot network
# g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
# g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
# g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

# g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
# g.sizes.RFP <- c("TRUE" = 6)
# g.shapes.RFP <- c("TRUE" = 25)

# Hubs
# ID0026.hub_score.RFP <- (round(hub_score(ID0026.graph.RFP)$value))/nrow(cmat.RFP) #normalized on absolute adj matrix!


# Hub score of each node --------------------------------------------------
# ID0026.hub.score.RFP <- hub_score(ID0026.graph.RFP, scale = TRUE)
# ID0026.posXY.RFP$`Hub score` <- round(ID0026.hub.score.RFP$vector, digits = 2)
# ID0026.posXY.RFP$HScore <- (ID0026.posXY.RFP$`Hub score`)*100

# Robustness
ID0026.cohesion <- cohesion(ID0026.graph.RFP)

# Communities detection ---------------------------------------------------
# greedy method (hiearchical, fast method)
ID0026.graph.clusters.RFP = cluster_leading_eigen(ID0026.graph.RFP)
ID0026.posXY.RFP$Community <- ID0026.graph.clusters.RFP$membership
ID0026.posXY.RFP$Member <- duplicated(ID0026.posXY.RFP$Community)


# Network plot ------------------------------------------------------------
ID0026.graph.RFP.plt <- ggraph(ID0026.graph.RFP, 
                                          layout = as.matrix(ID0026.posXY.RFP)[, c("X", "Y")]) +
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
                                          geom_node_point(aes(fill = ID0026.posXY.RFP$Mean.dF,
                                                              size = ordered(degree(ID0026.graph.RFP))),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0026.posXY.RFP$Cell), 
                                                          repel = TRUE,
                                                         size = 2.5)+
                                          geom_node_text(aes(label = degree(ID0026.graph.RFP)),
                                                         colour = "white",
                                                         fontface = 2,
                                                         size = 3)+
                                          scale_fill_continuous(type = "viridis")+
                                          scale_size_manual(values = c("0" = 5, "1" = 8, "2" = 9, "3" = 10))+
                                          # scale_shape_manual(values = c("TRUE" = 25, "FALSE" = 21))+
                                          # scale_colour_manual(values = c("TRUE" = "#fc9272", "FALSE" = "black"))+
                                          labs(fill = "Ca",
                                               size = "Degree (k)")+
                                          # Hubs
                                          annotate("text", x=45, y=20, 
                                                    label = "W. thresh. = 0.50")+
                                          annotate("text", x=40, y=35,
                                                   label = "Cohesion = ")+
                                          annotate("text", x=70, y=35,
                                                   label = ID0026.cohesion)+
                                          # geom_node_point(aes(fill = as.factor(ID0026.posXY$synchron),
                                          #                     size = as.factor(ID0026.posXY$synchron),
                                          #                     shape = as.factor(ID0026.posXY$synchron)))+
                                          # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                                          # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                                          # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                                          # geom_node_label(aes(label = ID0026.posXY$Cell), repel = TRUE)+
                                          
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
                                          ggtitle("ID0026")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed


# Histogram count of degrees ----------------------------------------------
ID0026.posXY.RFP$degree <- degree(ID0026.graph.RFP)
ID0026.degree.RFP.hist <- ggplot(ID0026.posXY.RFP, aes(x = degree))+ 
                            geom_histogram(binwidth = 1)

# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(tRFP, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0026.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0026.scree.RFP <- ggpar(ID0026.scree.RFP, title = element_blank())
# Viz ---------------------------------------------------------------------
gs = c(ID0026.graph.RFP.plt, ID0026.degree.RFP.hist,
      ID0026.RFP.grid, ID0026.RFPsum2.plt, ID0026.scree.RFP)

lay <- rbind(c(1,1,3,3),
             c(1,1,3,3),
             c(2,5,4,4))
ID0026.arranged <- grid.arrange(ID0026.graph.RFP.plt, ID0026.degree.RFP.hist,
                     ID0026.RFP.grid, ID0026.RFPsum2.plt, ID0026.scree.RFP,
                     layout_matrix = lay)


#Save whole graph + raster + activity plot + coactive cells/time
ggsave(plot = ID0026.arranged, file = "ID0026.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
ggsave(plot = ID0026.arranged, file = "/Users/Simo/calcium-clustering/plots/ID0026.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)




# plot communities with shaded regions
# plot(c2, ID0026.graph.RFP, layout=as.matrix(ID0026.posXY.RFP)[, c("X", "Y")])
