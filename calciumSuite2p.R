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
library(cowplot)
library(ggpubr)
# load reticulate and use it to load numpy
library(reticulate)
use_condaenv("/home/simo/anaconda3/bin/python")

np <-import("numpy")

# Start with stat.npy
stat <- np$load("stat.npy", allow_pickle = TRUE) #stats containing ROIs XY

# Loop over stat.npy to access "med" (ROIs coordinates) to create list of cells coordinates. ROIs from suite2p starts with index 0
posXY <- data.frame()
for (i in 1:length(stat)) {
  posXY <- rbind(posXY, stat[[i]][["med"]])
}

posXY$Cell <- as.numeric(0:(nrow(posXY)-1))
colnames(posXY) <- c('Y','X','Cell')

#iscell.npy to select only ROIs that are recognized as cells
iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
iscell$Cell <- as.numeric(0:(nrow(iscell)-1))
posXY$Positive <- iscell$V1
posXY <- subset(posXY, Positive == 1, select = c(Y,X,Cell))
positives <- posXY$Cell
positivesPLUSone <- positives+1 ##this is needed because suite2p starts counting ROIs with the number 0 (python..)

# load suite2p numpy arrays outputs
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks
spks[is.na(spks)] <- 0 #NAs replaced with 0
#need to clean it from first 0 and select best window
spks <- spks[,25:ncol(spks)]

#Normalize each cell
spks <- sapply(spks, function(x) (x - min(x))/(max(x)-min(x)))
spks <- spks[positivesPLUSone,] #select only positives
rownames(spks) <- positives #fix rownames with actual cells numbers



# Cutoff function <- anything below row sd/cell is 0, anything above is 1
cutoff <- function(x) {
  th <- sd(x)
  x[x < th] <- 0
  x[x > th] <- 1
  return(x)
}

spksthresholded <- t(apply(spks, 1, cutoff))

# Calculating percentage of active RFP cells over time
RFPcells <- unique(c(68,189,385,190,257,190,275,93,26,215,36,911,96,13,123,38,47,
                     73,59,374,204,133,1023,406,245,375,119,92,50,46,7,492,452,
                     139,1,240,223,133,20,139,600,1482,16,39,93,30,174,385,189,662,19,13,96,32))
RFP <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells

##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFP))
RFPsum$Time <- 1:nrow(RFPsum)
RFPsum$Perc <- RFPsum$`colSums(RFP)`/nrow(RFP)*100
RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
                geom_line()+
                ylim(0,100)+
                geom_smooth(method = "loess")



## CALCULATE ALL ACTIVE CELLS OVER TIME
# Threshold to calculate ALL active cells perc
library(mmand)
# Calculating percentage of active cells over time
spksSUM <- colSums(spksthresholded)
spksSUM <- as.data.frame(spksSUM)
spksSUM$Time <- 1:nrow(spksSUM)
spksSUM$Perc <- spksSUM$spksSUM/nrow(spksthresholded)*100
spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
                  geom_line()+
                  ylim(0,100)+
                  geom_smooth(method = "loess")



# # Plot total calcium activity/time --------------------------------------
spksSUM2 <- colSums(spks)
spksSUM2 <- as.data.frame(spksSUM2)
spksSUM2$Time <- 1:nrow(spksSUM2)

spksSUM2.plt <- ggplot(spksSUM2, aes(Time, spksSUM2))+
  geom_line()+ 
  theme_pubr()+
  ylab(NULL)+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())



# # Raster+dendro all cells/time ggplot ------------------------------------------
dfpeaks <- as.data.frame(t(spks))  # Doing this coercion will apply +1 to all cells numbers
colnames(dfpeaks) <- 1:ncol(dfpeaks)
dfpeaks$time <- 1:nrow(dfpeaks)
meltPeaks <- melt(dfpeaks, id = "time")
colnames(meltPeaks) <- c('time','cell','Ca2+')


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
meltPeaks$RFP <- meltPeaks$cell %in% RFPcells #highlight RFP cells

peaks.raster <- ggplot(meltPeaks, aes(time, cell))+
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
        axis.text.y = element_blank())

# GRID
# grid.newpage()
# print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
# print(peaks.dendro, vp = viewport(x = 0.90, y = 0.455, width = 0.2, height = 0.94))

# GRID raster/sums
plots <- align_plots(peaks.raster, spksSUM2.plt, align = 'v', axis = 'l')
CTRL5dpfhi1.grid <- plot_grid(plots[[1]], spksSUM2.plt, ncol = 1, rel_heights = c(4.5,1))


########################################### RFP ####################################
# RFPcells for raster/dendrogram. Take care of using already normalized spks array
RFPnothresh <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells
rownames(RFPnothresh) <- unique(c(68,189,385,190,257,190,275,93,26,215,36,911,96,13,123,38,47,73,59,374,204,133,1023,406,245,375,119,92,50,46,7,492,452,139,1,240,223,133,20,139,600,1482,16,39,93,30,174,385,189,662,19,13,96,32))

# Raster ggplot
dfpeaks <- as.data.frame(t(RFPnothresh))
colnames(dfpeaks) <- 1:ncol(dfpeaks)
dfpeaks$time <- 1:nrow(dfpeaks)
meltPeaks <- melt(dfpeaks, id = "time")
colnames(meltPeaks) <- c('time','cell','Ca2+')

#ggplot(meltPeaks, aes(time, cell)) +
# geom_raster(aes(fill = `Ca2+`))


# Hierarchical clustering
hc <- hclust(dist(RFPnothresh, method = "euclidean"), method = "ward.D2")
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
print(peaks.dendro, vp = viewport(x = 0.90, y = 0.453, width = 0.2, height = 0.89))
########################################################################################
# Simple ggplot cells on coordinates
ggplot(posXY, aes(X, Y, color = Cell, label = Cell))+
  geom_point()+
  # geom_label()+
  scale_y_reverse()


# Average calcium levels over time of each RFP cell
# For this I use the raw fluorescence
F <- as.data.frame(np$load("F.npy", allow_pickle = TRUE)) #raw calcium levels
Fneu <- as.data.frame(np$load("Fneu.npy", allow_pickle = TRUE)) #neuropil (background)
Fraw <- F-(Fneu*0.7) #neuropil removed
#need to clean it from first 0 and select best window
Fraw <- Fraw[,25:ncol(Fraw)]
# Fraw <- sapply(Fraw, function(x) (x - min(x))/(max(x)-min(x))) #normalize
FrawRFP <- Fraw[RFPcells+1,]
rownames(FrawRFP) <- RFPcells

#Averaging per timepoint
FrawRFPx <- as.data.frame(colSums(FrawRFP)/nrow(FrawRFP))
FrawRFPx$Time <- 0:(nrow(FrawRFPx)-1)
#plot
ggplot(FrawRFPx, aes(Time, `colSums(FrawRFP)/nrow(FrawRFP)`))+
  geom_line()+
  ylab("Average Ca2+")+
  geom_smooth(method = "loess")


#Averaging per cell
Frawsinglecell <- as.data.frame(rowSums(Fraw)/ncol(Fraw))
Frawsinglecell$Cell <- 0:(nrow(Frawsinglecell)-1)


posXY2 <- data.frame()
for (i in 1:length(stat)) {
  posXY2 <- rbind(posXY2, stat[[i]][["med"]])
}

posXY2$Cell <- as.numeric(0:(nrow(posXY2)-1))
colnames(posXY2) <- c('Y','X','Cell')
posXY2$Calcium <- Frawsinglecell$`rowSums(Fraw)/ncol(Fraw)`
# Spatial plot with average activity/cell
ggplot(Frawsinglecell, aes(posXY$X, posXY$Y, color = `rowSums(Fraw)/ncol(Fraw)`, label = Cell))+
  geom_point()+
  # geom_label()+
  scale_y_reverse()








#RFP XY
posXY.RFP <- subset(posXY, posXY$Cell %in% RFPcells)

# Plot RFP deconvolved curves all together






# PCA?
pcaRFP <- matrix(NA, nrow(spks), 2)
pcaRFP[,1] <- rownames(spks)
pcaRFP[,2] <- pcaRFP[,1] %in% RFPcells

pca <-prcomp(spks, center = TRUE, scale = TRUE)
fviz_pca_ind(pca, col.ind = pcaRFP[,2], geom.ind = "point", axes =c(3, 10))

#k-means
km <-kmeans(pca$x, 2)
fviz_pca_ind(pca, col.ind =as.factor(km$cluster), geom.ind = pcaRFP[,2])

             



