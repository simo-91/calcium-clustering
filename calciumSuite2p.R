################ CALCIUM ANALYSIS WITHOUT THRESHOLDING (ANALOGIC)

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)
library(heatmaply)
library(factoextra)
library(ggdendro)
library(grid)
library(RcppCNPy)
# load reticulate and use it to load numpy
use_condaenv("/home/simo/anaconda3/bin/python")
library(reticulate)

np <-import("numpy")
# load suite2p numpy arrays outputs
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks
spks <- spks[,2:ncol(spks)]

#Normalize each cell
spks <- sapply(spks, function(x) (x - min(x))/(max(x)-min(x)))

dfpeaks <- as.data.frame(t(spks))
colnames(dfpeaks) <- 1:ncol(dfpeaks)
dfpeaks$time <- 1:nrow(dfpeaks)
meltPeaks <- melt(dfpeaks, id = "time")
colnames(meltPeaks) <- c('time','cell','Ca2+')

#ggplot(meltPeaks, aes(time, cell)) +
 # geom_raster(aes(fill = `Ca2+`))


# Hierarchical clustering
hc <- hclust(dist(spks, method = "euclidean"), method = "ward.D2")
dhc <- as.dendrogram(hc)


# Dendrogram
peaks.dendro <- ggdendrogram(dhc, rotate = TRUE, labels = FALSE)+
                  theme(panel.grid = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank())

# GRID to put together dendrograms and rasters
peaks.order <- order.dendrogram(dhc)

## Order the levels according to their position in the cluster
peaks.rows <- rownames(meltPeaks)
peaks.rows <- as.data.frame(peaks.rows)
meltPeaks$cell <- factor(x = meltPeaks$cell,
                         levels = peaks.rows$peaks.rows[peaks.order], 
                         ordered = TRUE)

# Ggplot rasterwith dendro order
peaks.raster <- ggplot(meltPeaks, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# GRID
grid.newpage()
print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(peaks.dendro, vp = viewport(x = 0.90, y = 0.455, width = 0.2, height = 0.94))









stat <- np$load("stat.npy", allow_pickle = TRUE) #stats containing ROIs XY

# Loop over stat.npy to access "med" (ROIs coordinates) to create list of cells coordinates
posXY <- data.frame()
for (i in 1:length(stat)) {
  posXY <- rbind(posXY, stat[[i]][["med"]])
}

posXY$Cell <- 1:nrow(posXY)
colnames(posXY) <- c('Y','X','Cell')
ggplot(posXY, aes(X, Y, color = Cell, label = Cell))+
  geom_point()+
  geom_label()+
  scale_y_reverse()





# PCA?
pca <-prcomp(spks, center = TRUE, scale = TRUE)
fviz_pca_ind(pca, col.ind = spks, geom.ind = "point", axes =c(1, 2))
