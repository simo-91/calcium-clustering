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
spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x))))
spks <- spks[positivesPLUSone,] #select only positives
rownames(spks) <- positives #fix rownames with actual cells numbers



# Cutoff function <- anything below 1*(row sd/cell) is 0, anything above is 1
cutoff <- function(x) {
  th <- sd(x)
  x[x < th] <- 0
  x[x > th] <- 1
  return(x)
}

spksthresholded <- t(apply(spks, 1, cutoff))


## CALCULATE ALL ACTIVE CELLS OVER TIME
# Threshold to calculate ALL active cells perc
library(mmand)
# Calculating percentage of active cells over time
spksSUM <- colSums(spksthresholded)
spksSUM <- as.data.frame(spksSUM)
spksSUM$Time <- 1:nrow(spksSUM)
spksSUM$Perc <- spksSUM$spksSUM/nrow(spksthresholded)*100
AKT14dpflo2.spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
                  geom_line()+ 
                  theme_pubr()



# # Plot total calcium activity/time --------------------------------------
spksSUM2 <- colSums(spks)
spksSUM2 <- as.data.frame(spksSUM2)
spksSUM2$Time <- 1:nrow(spksSUM2)

AKT14dpflo2.spksSUM2.plt <- ggplot(spksSUM2, aes(Time, spksSUM2))+
  geom_line()+ 
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
AKT14dpflo2.spksSUM2.ylim <- layer_scales(AKT14dpflo2.spksSUM2.plt)$y$get_limits()


# # Raster+dendro all cells/time ggplot ------------------------------------------
dfpeaks <- as.data.frame(t(spks))  # Doing this coercion will apply +1 to all cells numbers
# colnames(dfpeaks) <- 1:ncol(dfpeaks)
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
peaks.rows <- rownames(spks)
peaks.rows <- as.data.frame(peaks.rows)
meltPeaks$cell <- factor(x = meltPeaks$cell,
                         levels = peaks.rows$peaks.rows[peaks.order], 
                         ordered = TRUE)

# Ggplot rasterwith dendro order
RFPcells <- unique(c(669,1163,1011,394,113,871,148,1162,27,147,102,253,1231,257,117,63,840,326,950,1272,548,327,292,131,1334,208,103,666,517,68,128,1088,611,94,609,0,354,821))
meltPeaks$RFP <- meltPeaks$cell %in% RFPcells #highlight RFP cells

AKT14dpflo2.raster <- ggplot(meltPeaks, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  # geom_line(aes(color = RFP), alpha = .2)+
  #scale_y_discrete(breaks = levels(meltPeaks$RFP))+
  scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle("AKT1 4dpf lo 2")

# GRID
# grid.newpage()
# print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
# print(peaks.dendro, vp = viewport(x = 0.90, y = 0.455, width = 0.2, height = 0.94))

# GRID raster/sums
plots <- align_plots(AKT14dpflo2.raster, AKT14dpflo2.spksSUM.plt, align = 'v', axis = 'l')
AKT14dpflo2.grid <- plot_grid(plots[[1]], AKT14dpflo2.spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))


########################################### RFP ####################################
# Calculating percentage of active RFP cells over time

RFP <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells

##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFP))
RFPsum$Time <- 1:nrow(RFPsum)
RFPsum$Perc <- RFPsum$`colSums(RFP)`/nrow(RFP)*100
AKT14dpflo2_RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()

# RFPcells for raster/dendrogram. Take care of using already normalized spks array
RFPnothresh <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells
rownames(RFPnothresh) <- unique(c(669,1163,1011,394,113,871,148,1162,27,147,102,253,1231,257,117,63,840,326,950,1272,548,327,292,131,1334,208,103,666,517,68,128,1088,611,94,609,0,354,821))

# Raster ggplot
dfpeaks.RFP <- as.data.frame(t(RFPnothresh))
colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
dfpeaks.RFP$time <- 1:nrow(dfpeaks.RFP)
meltPeaks.RFP <- melt(dfpeaks.RFP, id = "time")
colnames(meltPeaks.RFP) <- c('time','cell','Ca2+')

#ggplot(meltPeaks, aes(time, cell)) +
# geom_raster(aes(fill = `Ca2+`))


# Hierarchical clustering
hc.RFP <- hclust(dist(RFPnothresh, method = "euclidean"), method = "ward.D2")
dhc.RFP <- as.dendrogram(hc.RFP)


# Dendrogram
RFP.dendro <- ggdendrogram(dhc.RFP, rotate = TRUE, labels = FALSE)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

# GRID to put together dendrograms and rasters
RFP.order <- order.dendrogram(dhc.RFP)

## Order the levels according to their position in the cluster
RFP.rows <- rownames(meltPeaks.RFP)
RFP.rows <- as.data.frame(RFP.rows)
meltPeaks.RFP$cell <- factor(x = meltPeaks.RFP$cell,
                         levels = RFP.rows$RFP.rows[RFP.order], 
                         ordered = TRUE)

# Ggplot rasterwith dendro order
RFP.raster <- ggplot(meltPeaks.RFP, aes(time, cell))+
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
  ggtitle("AKT1 4dpf hi 2 RFP+")

# # GRID
# grid.newpage()
# print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
# print(peaks.dendro, vp = viewport(x = 0.90, y = 0.453, width = 0.2, height = 0.89))

# GRID raster/sums
plots <- align_plots(RFP.raster, AKT14dpflo2_RFPsum.plt, align = 'v', axis = 'l')
AKT14dpflo2.RFP.grid <- plot_grid(plots[[1]], AKT14dpflo2_RFPsum.plt, ncol = 1, rel_heights = c(4.5,1))

########################################################################################
# Simple ggplot cells on coordinates
# ggplot(posXY, aes(X, Y, color = Cell, label = Cell))+
#   geom_point()+
#   # geom_label()+
#   scale_y_reverse()


# Average calcium levels over time. Here by using raw fluorescence traces, but this is probably
# not the best approach, as they have to be detrended first. Better sticking to deconvolved traces (spksSUM2)
F <- as.data.frame(np$load("F.npy", allow_pickle = TRUE)) #raw calcium levels
Fneu <- as.data.frame(np$load("Fneu.npy", allow_pickle = TRUE)) #neuropil (background)
Fraw <- F-(Fneu*0.7) #neuropil removed
#need to clean it from first 0 and select best window
Fraw <- Fraw[,25:ncol(Fraw)]

#Averaging per timepoint (all cells)
FrawPOS <- Fraw[positivesPLUSone,]
rownames(FrawPOS) <- positives

FrawPOSx <- as.data.frame(colSums(FrawPOS)/nrow(FrawPOS))
FrawPOSx$Time <- 0:(nrow(FrawPOSx)-1)
#plot
AKT14dpflo2.POS.aveF <- ggplot(FrawPOSx, aes(Time, `colSums(FrawPOS)/nrow(FrawPOS)`))+
  geom_line()+
  theme_pubr()+
  ylab("Average Ca2+")+
  geom_smooth(method = "loess")



#Averaging per timepoint (RFP only)
# Fraw <- sapply(Fraw, function(x) (x - min(x))/(max(x)-min(x))) #normalize?
FrawRFP <- Fraw[RFPcells+1,]
rownames(FrawRFP) <- RFPcells


FrawRFPx <- as.data.frame(colSums(FrawRFP)/nrow(FrawRFP))
FrawRFPx$Time <- 0:(nrow(FrawRFPx)-1)
#plot
AKT14dpflo2.RFP.aveF <- ggplot(FrawRFPx, aes(Time, `colSums(FrawRFP)/nrow(FrawRFP)`))+
                              geom_line()+
                              theme_pubr()+
                              ylab("Average Ca2+ (RFP+)")+
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

             



