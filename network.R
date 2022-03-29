```{r libraries, message=FALSE, warning=FALSE}
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
```{r general graph}
# # Graph setup -------------------------------------------------------------
# ID0034.graph <- graph.adjacency(as.matrix(cmat), mode = "undirected", weighted = TRUE, diag = FALSE)
# ID0034.graph.raw <- graph.adjacency(as.matrix(cmat.RFP), mode = "undirected", weighted = TRUE, diag = FALSE)
# # Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# ID0034.graph <- delete.edges(ID0034.graph, which(E(ID0034.graph)$weight <0.75))
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
# ID0034.hub_score <- (round(hub_score(ID0034.graph.raw)$value))/nrow(cmat) #normalized
# # maybe divide per no. of cells???
# 
# 
# ID0034.graph.plt <- ggraph(ID0034.graph, 
#                                          layout = as.matrix(ID0034.posXY)[, c("X", "Y")]) +
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
#                             geom_node_point(aes(fill = ID0034.posXY$Mean.dF,
#                                                 size = as.factor(ID0034.posXY$RFP),
#                                                 shape = as.factor(ID0034.posXY$RFP),
#                                                 colour = as.factor(ID0034.posXY$RFP)))+
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
#                                      label = ID0034.hub_score)+
#                             # geom_node_point(aes(fill = as.factor(ID0034.posXY$synchron),
#                             #                     size = as.factor(ID0034.posXY$synchron),
#                             #                     shape = as.factor(ID0034.posXY$synchron)))+
#                             # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
#                             # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
#                             # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
#                             # geom_node_label(aes(label = ID0034.posXY$Cell), repel = TRUE)+
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
#                             ggtitle("ID0034n")+
#                             scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
# 
```

```{r AKT1/HRAS cells graph}

################### RFP graph ########################

ID0034.graph.RFP <- graph.adjacency(as.matrix(cmat.RFPt), mode = "undirected", weighted = TRUE, diag = FALSE)
```
```{r theshold edges}
# Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
# Set weight threshold (set to 0.30 as per literature: Avitan et al., 2017 http://dx.doi.org/10.1016/j.cub.2017.06.056)
ID0034.graph.RFP <- delete.edges(ID0034.graph.RFP, which(E(ID0034.graph.RFP)$weight <0.30))
```

```{r palettes to ignore now}
##### Plot network
# g.palette.Sync <- c("TRUE" = "green","FALSE" = "grey")
# g.sizes.Sync <- c("TRUE" = 2.5,"FALSE" = 1)
# g.shapes.Sync <- c("TRUE" = 23, "FALSE" = 21)

# g.palette.RFP <- c("TRUE" = "red","FALSE" = "grey")
# g.sizes.RFP <- c("TRUE" = 6)
# g.shapes.RFP <- c("TRUE" = 25)
```

```{r hubs score}
# Hubs
# ID0034.hub_score.RFP <- (round(hub_score(ID0034.graph.RFP)$value))/nrow(cmat.RFP) #normalized on absolute adj matrix!


# Hub score of each node --------------------------------------------------
# ID0034.hub.score.RFP <- hub_score(ID0034.graph.RFP, scale = TRUE)
# ID0034.posXY.RFP$`Hub score` <- round(ID0034.hub.score.RFP$vector, digits = 2)
# ID0034.posXY.RFP$HScore <- (ID0034.posXY.RFP$`Hub score`)*100
```

```{r Robustness of the whole graph}
# Robustness
ID0034.cohesion <- cohesion(ID0034.graph.RFP)
```

```{r Communities detection}
# Communities detection ---------------------------------------------------
# greedy method (hierarchical, fast method)
ID0034.graph.clusters.RFP = cluster_leading_eigen(ID0034.graph.RFP)
ID0034.posXY.RFP$Community <- ID0034.graph.clusters.RFP$membership

# ID0034.posXY.RFP$Member <- duplicated(ID0034.posXY.RFP$Community) to be fixed
# ID0034.posXY.RFP$degree <- ordered(degree(ID0034.graph.RFP))
```

```{r actual graph plot}
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

ID0034.graph.RFP.plt <- ggraph(ID0034.graph.RFP, 
                                          layout = as.matrix(ID0034.posXY.RFP)[, c("X", "Y")]) +
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
                                          geom_node_point(aes(fill = ordered(cluster_leading_eigen(ID0034.graph.RFP)$membership),
                                                              size = degree(ID0034.graph.RFP)),
                                                          shape = 21)+
                                          geom_node_text(aes(label = ID0034.posXY.RFP$Cell), 
                                                         colour = "red",
                                                          repel = TRUE,
                                                         size = 2.5)+
                                          geom_node_text(aes(label = ordered(cluster_leading_eigen(ID0034.graph.RFP)$membership)),
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
                                          #          label = ID0034.cohesion)+
                                          # geom_node_point(aes(fill = as.factor(ID0034.posXY$synchron),
                                          #                     size = as.factor(ID0034.posXY$synchron),
                                          #                     shape = as.factor(ID0034.posXY$synchron)))+
                                          # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
                                          # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
                                          # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
                                          # geom_node_label(aes(label = ID0034.posXY$Cell), repel = TRUE)+
                                          
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
                                          ggtitle("ID0034")+
                                          scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
```

```{r histogram count of degrees/node}
# Histogram count of degrees ----------------------------------------------
ID0034.posXY.RFP$Degree <- degree(ID0034.graph.RFP)
max.y.degree <- length(ID0034.RFPcells)
ID0034.degree.RFP.hist <- ggplot(ID0034.posXY.RFP, aes(x = Degree))+ 
                            stat_bin(binwidth = 1, center = 0)+
                            scale_y_continuous(breaks = seq(0,max.y.degree,1))+
                            scale_x_continuous(breaks = seq(0,20,1))+
                            theme_bw()

# Mean degree
ID0034.degree.mean.RFP <- mean(degree(ID0034.graph.RFP))
```

```{r PCA with scree plot}
# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(T.RFPt, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0034.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0034.scree.RFP <- ggpar(ID0034.scree.RFP, title = element_blank())
```

```{r final plots together, include=FALSE}
# Viz ---------------------------------------------------------------------
gs = c(ID0034.graph.RFP.plt, ID0034.degree.RFP.hist,
      ID0034.RFP.grid, ID0034.RFPsum2.plt, ID0034.scree.RFP)

lay <- rbind(c(1,1,3,3),
             c(1,1,3,3),
             c(2,5,4,4))
ID0034.arranged <- grid.arrange(ID0034.graph.RFP.plt, ID0034.degree.RFP.hist,
                     ID0034.RFPt.grid, ID0034.RFPsum2.plt, ID0034.scree.RFP,
                     layout_matrix = lay)
```

```{r save plots}
#Save whole graph + raster + activity plot + coactive cells/time
ggsave(plot = ID0034.arranged, file = "ID0034.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
ggsave(plot = ID0034.arranged, file = "/Users/Simo/calcium-clustering/plots/ID0034.whole.png", 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
```



# plot communities with shaded regions
# plot(c2, ID0034.graph.RFP, layout=as.matrix(ID0034.posXY.RFP)[, c("X", "Y")])
