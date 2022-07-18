```{r libraries, message=FALSE, warning=FALSE, include=FALSE}
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
library(pals)
```



```{r AKT1/HRAS cells graph}

ID0031.graph.RFP <- graph.adjacency(as.matrix(cmat.RFPt), mode = "undirected", weighted = TRUE, diag = FALSE) # using thresholded values



# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# Set weight threshold (set to 0.30 as per literature: Avitan et al., 2017 http://dx.doi.org/10.1016/j.cub.2017.06.056)
ID0031.graph.RFP <- delete.edges(ID0031.graph.RFP, which(E(ID0031.graph.RFP)$weight <0.30))





# Robustness
ID0031.cohesion <- cohesion(ID0031.graph.RFP)



# Isolate RFP+ XY from posXY dataframe ------------------------------------
ID0031.posXY$RFP <- ID0031.posXY$Cell %in% ID0031.RFPcells

ID0031.posXY.RFP <- ID0031.posXY %>%
                      subset(redcell==1)

saveRDS(ID0031.posXY.RFP, file = "~/calcium-clustering/dataID0031.posXY.RFP.rds")
write.csv(ID0031.posXY.RFP, file = "~/calcium-clustering/dataID0031.posXY.RFP.csv")




# Communities detection ---------------------------------------------------
# greedy method (hierarchical, fast method)
ID0031.graph.clusters.RFP = cluster_leading_eigen(ID0031.graph.RFP)
ID0031.posXY.RFP$Community <- ID0031.graph.clusters.RFP$membership

# ID0031.posXY.RFP$Member <- duplicated(ID0031.posXY.RFP$Community) to be fixed
# ID0031.posXY.RFP$degree <- ordered(degree(ID0031.graph.RFP))



# Network plot ------------------------------------------------------------
# Make composite palette
colorz = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
communities.palette.big <- colorz
# communities.palette <- c(brewer.pal(9,'Set1'),
#                          brewer.pal(8,'Set2'),
#                          brewer.pal(12,'Set3'),
#                          brewer.pal(8, 'Dark2'),
#                          brewer.pal(8, 'Accent'),
#                          brewer.pal(9,'Pastel1'),
#                          brewer)
communities.guide <- guide_legend(title = element_text("Communities"),
                                  label = FALSE,
                                  keywidth = 0.1)

ID0031.graph.RFP.plt <- ggraph(ID0031.graph.RFP, 
                                          layout = as.matrix(ID0031.posXY.RFP)[, c("X", "Y")]) +
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
                                          geom_node_point(aes(fill = ordered(cluster_leading_eigen(ID0031.graph.RFP)$membership),
                                                              size = degree(ID0031.graph.RFP)),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0031.posXY.RFP$Cell), 
                                                         colour = "red",
                                                          repel = TRUE,
                                                         size = 2.5)+
                                          geom_node_text(aes(label = ordered(cluster_leading_eigen(ID0031.graph.RFP)$membership)),
                                                         colour = "black",
                                                         fontface = 1,
                                                         size = 3)+
                                          scale_fill_manual(values = colorz,
                                                            guide = "none")+
                                          scale_size_continuous(range = c(5, 12),
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
                                          #          label = ID0031.cohesion)+
                                          # geom_node_point(aes(fill = as.factor(ID0031.posXY$synchron),
                                          #                     size = as.factor(ID0031.posXY$synchron),
                                          #                     shape = as.factor(ID0031.posXY$synchron)))+
                                          # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                                          # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                                          # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                                          # geom_node_label(aes(label = ID0031.posXY$Cell), repel = TRUE)+
                                          
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
                                          ggtitle("ID0031 RFP+")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed



# Histogram count of degrees ----------------------------------------------
ID0031.posXY.RFP$Degree <- degree(ID0031.graph.RFP)
max.y.degree <- length(ID0031.RFPcells)

ID0031.degree.RFP.hist <- gghistogram(ID0042.posXY.RFP, x = "Degree", y = "..count..",
                                      binwidth = 1)


# Mean degree
ID0031.degree.mean.RFP <- mean(degree(ID0031.graph.RFP))


# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(T.RFPt, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0031.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0031.scree.RFP <- ggpar(ID0031.scree.RFP, title = element_blank())


# Viz ---------------------------------------------------------------------
gs = c(ID0031.graph.RFP.plt, ID0031.degree.RFP.hist,
      ID0031.RFP.grid, ID0031.RFPsum2.plt, ID0031.scree.RFP)

lay <- rbind(c(1,1,3,3),
             c(1,1,3,3),
             c(2,5,4,4))
ID0031.arranged <- grid.arrange(ID0031.graph.RFP.plt, ID0031.degree.RFP.hist,
                     ID0031.RFP.grid, ID0031.RFPsum2.plt, ID0031.scree.RFP,
                     layout_matrix = lay)


#Save whole graph + raster + activity plot + coactive cells/time
ggsave(plot = ID0031.arranged, file = "ID0031.whole.RFP.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
ggsave(plot = ID0031.arranged, file = "/Users/Simo/calcium-clustering/plots/ID0031.whole.RFP.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
```


```{r general graph2}

ID0031.graph <- graph.adjacency(as.matrix(cmat.allcellst), mode = "undirected", weighted = TRUE, diag = FALSE) # using thresholded values



# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# Set weight threshold (set to 0.30 as per literature: Avitan et al., 2017 http://dx.doi.org/10.1016/j.cub.2017.06.056)
ID0031.graph <- delete.edges(ID0031.graph, which(E(ID0031.graph)$weight <0.30))

# Robustness
ID0031.cohesion <- cohesion(ID0031.graph)

# Communities detection ---------------------------------------------------
# greedy method (hierarchical, fast method)
ID0031.graph.clusters = leading.eigenvector.community(ID0031.graph, options = list(maxiter = 1000000))
ID0031.posXY$Community <- ID0031.graph.clusters$membership

# ID0031.posXY.RFP$Member <- duplicated(ID0031.posXY.RFP$Community) to be fixed
# ID0031.posXY.RFP$degree <- ordered(degree(ID0031.graph.RFP))



# Network plot ------------------------------------------------------------
# Make composite palette
colorz = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colorz.invert = rev(colorz)
communities.palette.big <- c(colorz, brewer.pal(9,'Set1'),
                         brewer.pal(8,'Set2'),
                         brewer.pal(12,'Set3'),
                         brewer.pal(8, 'Dark2'),
                         brewer.pal(8, 'Accent'),
                         brewer.pal(9,'Pastel1'),
                         colorz.invert)

communities.guide <- guide_legend(title = element_text("Communities"),
                                  label = FALSE,
                                  keywidth = 0.1)

ID0031.graph.plt <- ggraph(ID0031.graph, 
                                          layout = as.matrix(ID0031.posXY)[, c("X", "Y")]) +
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
                                          geom_node_point(aes(fill = ordered(leading.eigenvector.community(ID0031.graph, options = list(maxiter = 1000000))$membership),
                                                              size = degree(ID0031.graph)),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0031.posXY$Cell), 
                                                         colour = "red",
                                                          repel = TRUE,
                                                         size = 2.5)+
                                          geom_node_text(aes(label = ordered(leading.eigenvector.community(ID0031.graph, options = list(maxiter = 1000000))$membership)),
                                                         colour = "black",
                                                         fontface = 1,
                                                         size = 3)+
                                          scale_fill_manual(values = communities.palette.big,
                                                            guide = "none")+
                                          scale_size_continuous(range = c(5, 12),
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
                                          #          label = ID0031.cohesion)+
                                          # geom_node_point(aes(fill = as.factor(ID0031.posXY$synchron),
                                          #                     size = as.factor(ID0031.posXY$synchron),
                                          #                     shape = as.factor(ID0031.posXY$synchron)))+
                                          # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                                          # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                                          # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                                          # geom_node_label(aes(label = ID0031.posXY$Cell), repel = TRUE)+
                                          
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
                                          ggtitle("ID0031")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed

# Save graph
ggsave(plot = ID0031.graph.plt, file = "/Users/Simo/calcium-clustering/plots/ID0031.graph.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)

# Connections between normal and RFP+ cells
ID0031.graph.btw.plt <- ggraph(ID0031.graph, 
                                          layout = as.matrix(ID0031.posXY)[, c("X", "Y")]) +
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
                                          geom_node_point(aes(fill = as.factor(ID0031.posXY$redcell),
                                                              size = as.factor(ID0031.posXY$redcell)),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0031.posXY$Cell), 
                                                         colour = "blue",
                                                          repel = TRUE,
                                                         size = 2.5)+
                                         scale_fill_manual(values = c("0" = "grey",
                                                                      "1" = "red"),
                                                            guide = "none")+
                                         scale_size_manual(values = c("0" = 5,
                                                                      "1" = 8),
                                                           guide = "none")+
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
                                          ggtitle("ID0031 RFP+ highlighted")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed

# Save graph
ggsave(plot = ID0031.graph.btw.plt, file = "/Users/Simo/calcium-clustering/plots/ID0031.graph.btw.plt.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
```











# ```{r palettes to ignore now}
g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
g.sizes.RFP <- c("TRUE" = 6)
g.shapes.RFP <- c("TRUE" = 25)
```


```{r hubs score}
# Hubs
# ID0031.hub_score.RFP <- (round(hub_score(ID0031.graph.RFP)$value))/nrow(cmat.RFP) #normalized on absolute adj matrix!


# Hub score of each node --------------------------------------------------
# ID0031.hub.score.RFP <- hub_score(ID0031.graph.RFP, scale = TRUE)
# ID0031.posXY.RFP$`Hub score` <- round(ID0031.hub.score.RFP$vector, digits = 2)
# ID0031.posXY.RFP$HScore <- (ID0031.posXY.RFP$`Hub score`)*100
```


# ```{r general graph}
# # Graph setup -------------------------------------------------------------
ID0031.graph <- graph.adjacency(as.matrix(cmat), mode = "undirected", weighted = TRUE, diag = FALSE)

# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
ID0031.graph <- delete.edges(ID0031.graph, which(E(ID0031.graph)$weight <0.30))

##### Plot network
g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
g.sizes.RFP <- c("TRUE" = 6)
g.shapes.RFP <- c("TRUE" = 25)

# Hubs
ID0031.hub_score <- (round(hub_score(ID0031.graph.raw)$value))/nrow(cmat) #normalized
# maybe divide per no. of cells???


ID0031.graph.plt <- ggraph(ID0031.graph,
                                          layout = as.matrix(ID0031.posXY)[, c("X", "Y")]) +
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
                           # RFP/redcell positives
                             geom_node_point(aes(fill = ID0031.posXY$Mean.dF,
                                                 size = as.factor(ID0031.posXY$RFP),
                                                 shape = as.factor(ID0031.posXY$RFP),
                                                 colour = as.factor(ID0031.posXY$RFP)))+
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
                                      label = ID0031.hub_score)+
                            # geom_node_point(aes(fill = as.factor(ID0031.posXY$synchron),
                                                  # size = as.factor(ID0031.posXY$synchron),
                                                  # shape = as.factor(ID0031.posXY$synchron)))+
                             # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                             # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                             # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                             geom_node_label(aes(label = ID0031.posXY$Cell), repel = TRUE)+
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
                             ggtitle("ID0031n")+ 
                             scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
                             
```