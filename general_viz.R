raster.hc <- ggplot(meltPeaks, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  geom_line(aes(color = RFP), alpha = .2)+
  scale_fill_gradientn(colours=c("white", "black"))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "white"))+
  theme_pubr()+
  ylab("cell ID")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(colour = "black", hjust = .5))+
  ggtitle("HRASV12")
# ggtitle("ID0042 hclust", subtitle = sprintf("Mean frequency is: %s events/min", round(frequency, digits = 3)))


spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
  geom_line()+ 
  ylab("Coactive")+
  xlab("Time (s)")+
  theme_pubr(base_size = 8)
# GRID raster/sums
plots <- align_plots(raster.hc, spksSUM.plt, align = 'v', axis = 'l')
HRASV12.grid <- plot_grid(plots[[1]], spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))



# RFP

RFP.raster <- ggplot(meltPeaks.RFP, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
  theme_pubr()+
  ylab("cell ID")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle("HRASV12 RFP")


RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+ 
  ylab("Coactive")+
  xlab("Time (s)")+
  theme_pubr(base_size = 8)
# GRID raster/sums
plots <- align_plots(RFP.raster, RFPsum.plt, align = 'v', axis = 'l')
HRASV12.RFP.grid <- plot_grid(plots[[1]], RFPsum.plt, ncol = 1, rel_heights = c(3.5,1))



#  Graphs
graph.HRAS.RFP.plt <- ggraph(graph.RFP, 
                        layout = as.matrix(posXY.RFP)[, c("X", "Y")]) +
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
                        geom_node_point(aes(fill = ordered(cluster_leading_eigen(graph.RFP)$membership),
                                            size = degree(graph.RFP)),
                                        shape = 21)+
                        # geom_node_text(aes(label = posXY.RFP$Cell), 
                        #                colour = "red",
                        #                 repel = TRUE,
                        #                size = 2.5)+
                        geom_node_text(aes(label = ordered(cluster_leading_eigen(graph.RFP)$membership)),
                                       colour = "black",
                                       fontface = 1,
                                       size = 3)+
                        scale_fill_manual(values = colorz,
                                          guide = "none")+
                        scale_size_continuous(range = c(5, 12),
                                              guide = "none")+
                      theme_graph(background = "white",
                                  plot_margin = margin(5, 5, 5, 5))+
                        theme(plot.title = element_text(hjust = 0.5),
                              legend.position = "right",
                              legend.margin	= margin(1,1,1,1),
                              legend.key.size = unit(0.5, 'cm'), #change legend key size
                              # legend.key.height = unit(1, 'pt'), #change legend key height
                              # legend.key.width = unit(1, 'pt'), #change legend key width
                              # legend.title = element_text(size=5), #change legend title font size
                              # legend.text = element_text(size=4),
                              # legend.text.align = 0
                        )+
                        ggtitle("HRAS RFP")+
                        scale_y_reverse()

