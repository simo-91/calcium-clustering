################ CALCIUM ANALYSIS WITHOUT THRESHOLDING
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
use_condaenv("/Users/Simo/suite2p/")

np <-import("numpy")

# Start with stat.npy
stat <- np$load("stat.npy", allow_pickle = TRUE) #stats containing ROIs XY

# Loop over stat.npy to access "med" (ROIs coordinates) to create list of cells coordinates. ROIs from suite2p starts with index 0
HRAS5dpf_hind2_15min.posXY <- data.frame()
for (i in 1:length(stat)) {
  HRAS5dpf_hind2_15min.posXY <- rbind(HRAS5dpf_hind2_15min.posXY, stat[[i]][["med"]])
}

HRAS5dpf_hind2_15min.posXY$Cell <- as.numeric(0:(nrow(HRAS5dpf_hind2_15min.posXY)-1))
colnames(HRAS5dpf_hind2_15min.posXY) <- c('Y','X','Cell')

#iscell.npy to select only ROIs that are recognized as cells
iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
iscell$Cell <- as.numeric(0:(nrow(iscell)-1))
HRAS5dpf_hind2_15min.posXY$Positive <- iscell$V1
HRAS5dpf_hind2_15min.posXY <- subset(HRAS5dpf_hind2_15min.posXY, Positive == 1, select = c(Y,X,Cell))
positives <- HRAS5dpf_hind2_15min.posXY$Cell
positivesPLUSone <- positives+1 ##tlos is needed because suite2p starts counting ROIs with the number 0 (python..)

# load suite2p numpy arrays outputs
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks
spks[is.na(spks)] <- 0 #NAs replaced with 0

#need to clean it from first 0 and select best window
spks <- spks[,50:ncol(spks)]

#Normalize each cell
spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x))))
spks <- spks[positivesPLUSone,] #select only positives
rownames(spks) <- positives #fix rownames with actual cells numbers

spks[is.na(spks)] <- 0 #NAs replaced with 0



# Cutoff function <- anytlong below 1*(row sd/cell) is 0, anytlong above is 1
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
HRAS5dpf_hind2_15min.spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
                  geom_line()+ 
                  theme_pubr()



# # Plot total calcium activity/time --------------------------------------
spksSUM2 <- colSums(spks)
spksSUM2 <- as.data.frame(spksSUM2)
spksSUM2$Time <- 1:nrow(spksSUM2)

HRAS5dpf_hind2_15min.spksSUM2.plt <- ggplot(spksSUM2, aes(Time, spksSUM2))+
  geom_line()+ 
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
HRAS5dpf_hind2_15min.spksSUM2.ylim <- layer_scales(HRAS5dpf_hind2_15min.spksSUM2.plt)$y$get_limits()


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
# RFPcells <- unique(c(150, 48, 398, 118, 223, 20, 3, 224, 24, 130, 199, 320,184,176,97, 13, 2, 735, 1, 216, 74, 16, 79, 266, 85, 6, 200, 576, 257, 21, 56, 18, 502, 204, 64, 93, 31, 0, 399, 122))
# meltPeaks$RFP <- meltPeaks$cell %in% RFPcells #highlight RFP cells

HRAS5dpf_hind2_15min.raster.hc <- ggplot(meltPeaks, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  # geom_line(aes(color = RFP), alpha = .2)+
  # scale_y_discrete(breaks = levels(meltPeaks$RFP))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle("HRAS5dpf_hind2_15min hclust")

# GRID
# grid.newpage()
# print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
# print(peaks.dendro, vp = viewport(x = 0.90, y = 0.455, width = 0.2, height = 0.94))

# GRID raster/sums
plots <- align_plots(HRAS5dpf_hind2_15min.raster.hc, HRAS5dpf_hind2_15min.spksSUM.plt, align = 'v', axis = 'l')
HRAS5dpf_hind2_15min.grid <- plot_grid(plots[[1]], HRAS5dpf_hind2_15min.spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))


########################################### RFP ####################################
RFPcells <- scan("RFPcells.R", sep = ",")
# Calculating percentage of active RFP cells over time
RFP <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells
##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFP))
RFPsum$Time <- 1:nrow(RFPsum)
RFPsum$Perc <- RFPsum$`colSums(RFP)`/nrow(RFP)*100
HRAS5dpf_hind2_15min_RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()

# # Sum of all activity over time RFP+
RFP <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells
RFPsum2 <- as.data.frame(colSums(RFP))
RFPsum2$Time <- 1:nrow(RFPsum2)

HRAS5dpf_hind2_15min.RFPsum2.plt <- ggplot(RFPsum2, aes(Time, `colSums(RFP)`))+
  geom_line()+
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
HRAS5dpf_hind2_15min.RFPsum2.ylim <- layer_scales(HRAS5dpf_hind2_15min.RFPsum2.plt)$y$get_limits()


# RFPcells for activity/raster/dendrogram. Take care of using already normalized spks array
# RFPnothresh <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells

# Raster ggplot
dfpeaks.RFP <- as.data.frame(t(RFPnothresh))
# colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
dfpeaks.RFP$time <- 1:nrow(dfpeaks.RFP)
meltPeaks.RFP <- melt(dfpeaks.RFP, id = "time")
colnames(meltPeaks.RFP) <- c('time','cell','Ca2+')

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
RFP.rows <- rownames(RFPnothresh)
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
  ggtitle("HRAS5dpf_hind2_15min RFP+")

# GRID raster/sums
plots <- align_plots(RFP.raster, HRAS5dpf_hind2_15min_RFPsum.plt, align = 'v', axis = 'l')
HRAS5dpf_hind2_15min.RFP.grid <- plot_grid(plots[[1]], HRAS5dpf_hind2_15min_RFPsum.plt, ncol = 1, rel_heights = c(4.5,1))

########################################################################################
######################################### dF/F #########################################
########################################################################################

# Average calcium levels over time. Here by using dF traces after some coding in python suite2p-environment
dF <- as.data.frame(np$load("dF.npy", allow_pickle = TRUE)) #delta F calcium levels of all cells
dF <- dF[,50:ncol(dF)] #select window
# dF <- t(apply(dF, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize?
dFPOS <- dF[positivesPLUSone,]
rownames(dFPOS) <- positives

#Averaging per timepoint (all cells)

dFx <- as.data.frame(colSums(dFPOS)/nrow(dFPOS))
dFx$Time <- 0:(nrow(dFx)-1)
#plot
HRAS5dpf_hind2_15min.POS.aveF <- ggplot(dFx, aes(Time, `colSums(dFPOS)/nrow(dFPOS)`))+
  geom_line()+
  theme_pubr()+
  ylab("Average dF/F")+
  geom_smooth(method = "loess")

#Averaging per timepoint (RFP only)


FrawRFPx <- as.data.frame(colSums(FrawRFP)/nrow(FrawRFP))
FrawRFPx$Time <- 0:(nrow(FrawRFPx)-1)
#plot
HRAS5dpf_hind2_15min.RFP.aveF <- ggplot(FrawRFPx, aes(Time, `colSums(FrawRFP)/nrow(FrawRFP)`))+
                              geom_line()+
                              theme_pubr()+
                              ylab("Average Ca2+ (RFP+)")+
                              geom_smooth(method = "loess")



# Calculating percentage of active RFP cells over time
dF.RFP <- subset(spksthresholded, rownames(dFPOS) %in% RFPcells) #select only RFP cells
##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFP))
RFPsum$Time <- 1:nrow(RFPsum)
RFPsum$Perc <- RFPsum$`colSums(RFP)`/nrow(RFP)*100
HRAS5dpf_hind2_15min_RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()













#RFP XY
HRAS5dpf_hind2_15min.posXY$RFP <- HRAS5dpf_hind2_15min.posXY$Cell %in% RFPcells




# Plot RFP deconvolved curves all together

# POSITION ANALYSIS
cut3 <- cutree(hc, k = 3)

HRAS5dpf_hind2_15min.posXY$Cluster <- cut3

ggplot(HRAS5dpf_hind2_15min.posXY, aes(X, Y, color = as.factor(Cluster), shape = as.factor(Cluster)))+
  geom_point(size = 2)+
  # scale_color_manual(values=c('red','blue','green'))+
  theme_graph()+
  scale_y_reverse()


# Average activity per cell (dF peaks)
HRAS5dpf_hind2_15min.posXY$Mean.dF <- rowMeans(dFPOS)




# Average activity per cell (deconvolved peaks)
# then plot on graph
HRAS5dpf_hind2_15min.posXY$Mean <- rowMeans(spks)




# Color clusters
# put on graph