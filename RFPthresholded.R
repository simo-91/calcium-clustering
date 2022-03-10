# RFPcells for activity/hc/raster/dendrogram using thresholded data  -------------------------------
# Raster ggplot
dfpeaks.RFPt <- as.data.frame(t(RFPt))
# colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
dfpeaks.RFPt$time <- 0:(nrow(dfpeaks.RFPt)-1)
meltPeaks.RFPt <- melt(dfpeaks.RFPt, id = "time")
colnames(meltPeaks.RFPt) <- c('time','cell','Ca2+')

# Hierarchical clustering
hc.RFPt <- hclust(dist(RFPt, method = "euclidean"), method = "ward.D2")
dhc.RFPt <- as.dendrogram(hc.RFPt)

# Dendrogram
RFPt.dendro <- ggdendrogram(dhc.RFPt, rotate = TRUE, labels = FALSE)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# GRID to put together dendrograms and rasters
RFPt.order <- order.dendrogram(dhc.RFPt)

## Order the levels according to their position in the cluster
RFPt.rows <- rownames(RFPt)
RFPt.rows <- as.data.frame(RFPt.rows)
meltPeaks.RFPt$cell <- factor(x = meltPeaks.RFPt$cell,
                             levels = RFPt.rows$RFPt.rows[RFPt.order], 
                             ordered = TRUE)

# Ggplot rasterwith dendro order
ID0032.RFPt.raster <- ggplot(meltPeaks.RFPt, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_text(face=ID0032.posXY.RFP$Cell[which(ID0032.posXY.RFP$Member=="TRUE"),], "bold"),
        plot.title = element_text(colour = "red", hjust = .5))
# ggtitle("ID0032 RFP+")

# GRID raster/sums
plots <- align_plots(ID0032.RFPt.raster, ID0032.RFPsum.plt, align = 'v', axis = 'l')
ID0032.RFPt.grid <- plot_grid(plots[[1]], ID0032.RFPsum.plt, ncol = 1, rel_heights = c(3.5,1))
