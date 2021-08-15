# Sorting by k-means instead of hclust
kmeans.spks <- eclust(spks, "kmeans", k = 3, nstart = 25)
kmeans.spks.df <- kmeans.spks[["clust_plot"]][["data"]]
kmeans.spks.df <- kmeans.spks.df[,c(1,5)]

kmeans.spks.df <- kmeans.spks.df[with(kmeans.spks.df, order(kmeans.spks.df$cluster)),]
kmeans.order <- as.integer(rownames(kmeans.spks.df))


# # Raster+dendro all cells/time ggplot ------------------------------------------
dfpeaks.k <- as.data.frame(t(spks))  # Doing this coercion will apply +1 to all cells numbers
# colnames(dfpeaks) <- 1:ncol(dfpeaks)
dfpeaks.k$time <- 1:nrow(dfpeaks.k)
meltPeaks.k <- melt(dfpeaks.k, id = "time")
colnames(meltPeaks.k) <- c('time','cell','Ca2+')


## Order the levels according to their position in the cluster
peaks.rows <- rownames(spks)
peaks.rows <- as.data.frame(peaks.rows)
meltPeaks.k$cell <- factor(x = meltPeaks.k$cell,
                         levels = peaks.rows$peaks.rows[kmeans.order], 
                         ordered = TRUE)


AKT14dpfhi2.raster.k <- ggplot(meltPeaks.k, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  # geom_line(aes(color = RFP), alpha = .2)+
  #scale_y_discrete(breaks = levels(meltPeaks$RFP))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle("AKT1 4dpf hi 2 k-means (k = 3)")