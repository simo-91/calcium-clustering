# # Synchronicity
# sync <- apply(spksthresholded, MARGIN = 2, FUN = function(x) sum(x)/nrow(spksthresholded))
# # For 2 timepoints (2sec)?
# for (j in 1:ncol(spksthresholded)) {
#   sync[j] <- sum(spksthresholded[,j], spksthresholded[,j+1]) / nrow(spksthresholded)
# }

cmat.hi <- cmat
cmat.hi[lower.tri(cmat, diag=TRUE)] = NA  #Prepare to drop duplicates and meaningless information

sync.cells <- which(cmat.hi > 0.50, arr.ind = T) # select only highly correlated cells; the result of the which function is now in rows & columns
sync.cells <- sort(unique(sync.cells[,1])) # cells no. in one array

# With thresholding
dfpeaks.thresh <- apply(dfpeaks, 2, cutoff)
sync.raster <- as.data.frame(dfpeaks.thresh[, sync.cells])
sync.raster$time <- 1:nrow(sync.raster)

sync.raster.melt <- melt(sync.raster, id = "time")
colnames(sync.raster.melt) <- c('time','cell','Ca2+')

ggplot(sync.raster.melt, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# No thresholding
sync.raster2 <- as.data.frame(dfpeaks[, sync.cells])
# Hierarchical clustering
hc <- hclust(dist(sync.raster2, method = "euclidean"), method = "ward.D2")
dhc <- as.dendrogram(hc)
#
sync.raster2$time <- 1:nrow(sync.raster2)
sync.raster2.melt <- melt(sync.raster2, id = "time")
colnames(sync.raster2.melt) <- c('time','cell','Ca2+')

# Dendrogram
sync.dendro <- ggdendrogram(dhc, rotate = TRUE, labels = FALSE)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# GRID to put together dendrograms and rasters
sync.order <- order.dendrogram(dhc)

## Order the levels according to their position in the cluster
sync.rows <- rownames(sync.raster2.melt)
sync.rows <- as.data.frame(sync.rows)
sync.raster2.melt$cell <- factor(x = sync.raster2.melt$cell,
                         levels = sync.rows$sync.rows[sync.order], 
                         ordered = TRUE)
#Rasterplot
AKT1_hindbrain_1_sync.raster.hc.plt <- ggplot(sync.raster2.melt, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle("AKT1 hindbrain 1")

# # Plot total calcium activity/time ONLY highly corr cells
syncSUM2 <- rowSums(sync.raster2[,-ncol(sync.raster2)])
syncSUM2 <- as.data.frame(syncSUM2)
syncSUM2$Time <- 1:nrow(syncSUM2)

syncSUM2.plt <- ggplot(syncSUM2, aes(Time, syncSUM2))+
  geom_line()+ 
  theme_pubr()+
  ylab("Ca2+")

# Calculating percentage of active cells over time after extracting highly corr cells
hiSUM <- rowSums(sync.raster[,-ncol(sync.raster)])
hiSUM <- as.data.frame(hiSUM)
hiSUM$Time <- 1:nrow(hiSUM)
hiSUM$Perc <- hiSUM$hiSUM/ncol(sync.raster)*100
AKT1_hindbrain_1_hiSUM.plt <- ggplot(hiSUM, aes(Time, Perc))+
  geom_line()+ 
  theme_pubr()


# GRID raster/sums
sync.plots <- align_plots(AKT1_hindbrain_1_sync.raster.hc.plt, hiSUM.plt, align = 'v', axis = 'l')
sync.AKT1hindbrain1.grid <- plot_grid(sync.plots[[1]], AKT1_hindbrain_1_hiSUM.plt, ncol = 1, rel_heights = c(4.5,1))




hiSUM.thresh <- rowSums(sync.raster[,-ncol(sync.raster)])
