################ CALCIUM ANALYSIS WITHOUT THRESHOLDING (ANALOGIC)
setwd("/media/simo/Seagate/880 NBTGCaMP6s_nacre--/25042021/25042021_NBTGCaMP6s_nacre__CTRL_lexOPRFP_5dpf/CTRLfish2/midbrain/suite2p/plane0")
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

#iscell.npy to select only ROIs that are cells
iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
iscell$Cell <- as.numeric(0:(nrow(iscell)-1))
posXY$Positive <- iscell$V1
posXY <- subset(posXY, Positive == 1, select = c(Y,X,Cell))
positives <- posXY$Cell
positives <- positives+1 ##this is needed because suite2p starts counting ROIs with the number 0 (python..)

# load suite2p numpy arrays outputs
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks
#need to clean it from first 0
spks <- spks[,2:ncol(spks)]

#Normalize each cell
spks <- sapply(spks, function(x) (x - min(x))/(max(x)-min(x)))

## CALCULATE ACTIVE RFP CELLS OVER TIME
library(mmand)
spksthresh <- sd(spks)

# Cutoff function <- anything below 2*(row sd/cell) is 0, anything above is 1
cutoff <- function(x){
  x[x < 2*sd(x)] <- 0
  x[x > 2*sd(x)] <- 1
  return(x)
} 

spksthresholded <- t(apply(spks, 1, cutoff))

# Calculating percentage of active RFP cells over time
RFP <- spks[unique(c(68,189,385,190,257,190,275,93,26,215,36,911,96,13,123,38,
                     47,73,59,374,204,133,1023,406,245,375,119,92,50,46,7,492,
                     452,139,1,240,223,133,20,139,600,1482,16,39,93,30,174,385,
                     189,662,19,13,96,32)),] #select only RFP cells
rownames(RFP) <- unique(c(68,189,385,190,257,190,275,93,26,215,36,911,96,13,123,38,
                          47,73,59,374,204,133,1023,406,245,375,119,92,50,46,7,492,
                          452,139,1,240,223,133,20,139,600,1482,16,39,93,30,174,385,
                          189,662,19,13,96,32))
RFPthresholded <- threshold(RFP[,1:ncol(RFP)], spksthresh, method = c("literal"), binarise = TRUE)
########################################### done with this s**t
##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFPthresholded))
RFPsum$Time <- 1:nrow(RFPsum)
RFPsum$Perc <- RFPsum$`colSums(RFPthresholded)`/nrow(RFPsum) *100



spks <- spks[positives,] #select only positives
rownames(spks) <- positives #fix rownames with actual cells numbers

## CALCULATE ALL ACTIVE CELLS OVER TIME
# Threshold to calculate ALL active cells perc
# library(mmand)
# spksthresh <- 2*sd(spks)
# spksthresholded <- threshold(spks[,1:ncol(spks)], spksthresh, method = c("literal"), binarise = TRUE)
# # Calculating percentage of active cells over time
# spksSUM <- colSums(spksthresholded)
# spksSUM <- as.data.frame(spksSUM)
# spksSUM$Time <- 1:nrow(spksSUM)
# spksSUM$spksSUM <- spksSUM$spksSUM/nrow(spksthresholded)*100
# ggplot(spksSUM, aes(Time, spksSUM))+
#   geom_line()+
#   ylim(0,100)






# Raster ggplot
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












ggplot(posXY, aes(X, Y, color = Cell, label = Cell))+
  geom_point()+
  geom_label()+
  scale_y_reverse()





# PCA?
pca <-prcomp(spks, center = TRUE, scale = TRUE)
fviz_pca_ind(pca, col.ind = spks, geom.ind = "point", axes =c(1, 2))





