################ CALCIUM ANALYSIS WITHOUT THRESHOLDING
# sampleID <- readline('Please input ID number (e.g. "ID0123")')

# Libraries ---------------------------------------------------------------
library(utils)
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
library(mmand)
library(rstudioapi)
# Load raw Suite2p npy arrays using reticulate
library(reticulate)
np <-import("numpy")

workdir <- selectDirectory(caption = "Select suite2p/plane folder", 
                           label = "Select")
setwd(workdir)
# Stat.npy ----------------------------------------------------------------
stat <- np$load("stat.npy", allow_pickle = TRUE) #stats containing ROIs XY
# Loop over stat.npy to access "med" (ROIs coordinates) to create list of cells coordinates. ROIs from suite2p starts with index 0
ID0013.posXY <- data.frame()
for (i in 1:length(stat)) {
  ID0013.posXY <- rbind(ID0013.posXY, stat[[i]][["med"]])
}

ID0013.posXY$Cell <- as.numeric(0:(nrow(ID0013.posXY)-1))
colnames(ID0013.posXY) <- c('Y','X','Cell')

# iscell.npy --------------------------------------------------------------
#iscell.npy to select only ROIs that are recognized as cells
iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
iscell$Cell <- as.numeric(0:(nrow(iscell)-1))
ID0013.posXY$Positive <- iscell$V1
ID0013.posXY <- subset(ID0013.posXY, Positive == 1, select = c(Y,X,Cell))
positives <- ID0013.posXY$Cell
positivesPLUSone <- positives+1 ##this is needed because suite2p starts counting ROIs with the number 0 (python..)

# spks.npy ----------------------------------------------------------------
# load suite2p numpy arrays outputs
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks
spks[is.na(spks)] <- 0 #NAs replaced with 0

#need to clean it from first 0 and select best window
spks <- spks[,50:ncol(spks)]


# Normalize each cell -----------------------------------------------------
spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x))))

# 
spks <- spks[positivesPLUSone,] #select only positives
rownames(spks) <- positives #fix rownames with actual cells numbers

spks[is.na(spks)] <- 0 #NAs replaced with 0

saveRDS(spks, file = "ID0013_spks.rds")
write.csv(spks, "ID0013_spks.csv")


# Cutoff function <- anything below 1*(row sd/cell) is 0, anytlong above is 1
cutoff <- function(x) {
  th <- 2*sd(x)
  x[x < th] <- 0
  x[x > th] <- 1
  return(x)
}

spksthresholded <- t(apply(spks, 1, cutoff))
saveRDS(spksthresholded, file = "ID0013_spksthresholded.rds")
write.csv(spksthresholded, "ID0013_spksthresholded.csv")
## CALCULATE ALL ACTIVE CELLS OVER TIME
# Calculating percentage of active cells over time -------------------------
spksSUM <- colSums(spksthresholded)
spksSUM <- as.data.frame(spksSUM)
spksSUM$Time <- 0:(nrow(spksSUM)-1)
spksSUM$Perc <- spksSUM$spksSUM/nrow(spksthresholded)*100
saveRDS(spksSUM, file = "ID0013_spksSUM.rds")
write.csv(spksSUM, file = "ID0013_spksSUM.csv")
ID0013.spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
  geom_line()+ 
  theme_pubr()

# # Plot total calcium activity/time --------------------------------------
spksSUM2 <- colSums(spks)
spksSUM2 <- as.data.frame(spksSUM2)
spksSUM2$Time <- 0:(nrow(spksSUM2)-1)
spksSUM2$Mean <- spksSUM2$spksSUM2/nrow(spksSUM2)
saveRDS(spksSUM2, file = "ID0013_spksSUM2.rds")
write.csv(spksSUM2, file = "ID0013_spksSUM2.csv")
ID0013.spksSUM2.plt <- ggplot(spksSUM2, aes(Time, Mean))+
  geom_line()+ 
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
ID0013.spksSUM2.ylim <- layer_scales(ID0013.spksSUM2.plt)$y$get_limits()

# Average activity PER CELL (deconvolved peaks) ---------------------------
ID0013.posXY$Mean <- rowMeans(spks)


# # Raster+dendro all cells/time ggplot ------------------------------------------
dfpeaks <- as.data.frame(t(spks))  # Doing this coercion will apply +1 to all cells numbers
# colnames(dfpeaks) <- 1:ncol(dfpeaks)
dfpeaks$time <- 0:(nrow(dfpeaks)-1)
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

# Ggplot raster with dendro order
ID0013.raster.hc <- ggplot(meltPeaks, aes(time, cell))+
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
  ggtitle("ID0013 hclust")

# GRID
# grid.newpage()
# print(peaks.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
# print(peaks.dendro, vp = viewport(x = 0.90, y = 0.455, width = 0.2, height = 0.94))

# GRID raster/sums
plots <- align_plots(ID0013.raster.hc, ID0013.spksSUM.plt, align = 'v', axis = 'l')
ID0013.grid <- plot_grid(plots[[1]], ID0013.spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))


########################################### RFP ####################################
ID0013.RFPcells <- scan("RFPcells.R", sep = ",")

# Calculating percentage of active RFP cells over time --------------------
RFPt <- subset(spksthresholded, rownames(spksthresholded) %in% ID0013.RFPcells) #select only RFP cells
##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFPt))
RFPsum$Time <- 0:(nrow(RFPsum)-1)
RFPsum$Perc <- RFPsum$`colSums(RFPt)`/nrow(RFPt)*100
saveRDS(RFPsum, file = "ID0013_RFPsum.rds")
write.csv(RFPsum, file = "ID0013_RFPsum.csv")

# Plot
ID0013.RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()



# Sum of all activity over time RFP+ --------------------------------------
RFP <- subset(spks, rownames(spks) %in% ID0013.RFPcells) #select only RFP cells
RFPsum2 <- as.data.frame(colSums(RFP)/nrow(RFP)) #normalized by number of RFP cells
RFPsum2$Time <- 0:(nrow(RFPsum2)-1)
saveRDS(RFPsum2, file = "ID0013_RFPsum2.rds")
write.csv(RFPsum2, file = "ID0013_RFPsum2.csv")

ID0013.RFPsum2.plt <- ggplot(RFPsum2, aes(Time, `colSums(RFP)/nrow(RFP)`))+
  geom_line()+
  theme_pubr()+
  geom_smooth()+
  ylab("Deconvolved activity")+
  ylim(0, NA)
ID0013.RFPsum2.plt.ylim <- layer_scales(ID0013.RFPsum2.plt)$y$get_limits()


# RFPcells for activity/hc/raster/dendrogram.  -------------------------------
## Take care of using already normalized spks array
# Raster ggplot
dfpeaks.RFP <- as.data.frame(t(RFP))
# colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
dfpeaks.RFP$time <- 0:(nrow(dfpeaks.RFP)-1)
meltPeaks.RFP <- melt(dfpeaks.RFP, id = "time")
colnames(meltPeaks.RFP) <- c('time','cell','Ca2+')

# Hierarchical clustering
hc.RFP <- hclust(dist(RFP, method = "euclidean"), method = "ward.D2")
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
RFP.rows <- rownames(RFP)
RFP.rows <- as.data.frame(RFP.rows)
meltPeaks.RFP$cell <- factor(x = meltPeaks.RFP$cell,
                         levels = RFP.rows$RFP.rows[RFP.order], 
                         ordered = TRUE)

# Ggplot rasterwith dendro order
ID0013.RFP.raster <- ggplot(meltPeaks.RFP, aes(time, cell))+
  geom_raster(aes(fill = `Ca2+`))+
  scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
  theme_pubr()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_text(face=ID0013.posXY.RFP$Cell[which(ID0013.posXY.RFP$Member=="TRUE"),], "bold"),
        plot.title = element_text(colour = "red", hjust = .5))
  # ggtitle("ID0013 RFP+")

# GRID raster/sums
plots <- align_plots(ID0013.RFP.raster, ID0013.RFPsum.plt, align = 'v', axis = 'l')
ID0013.RFP.grid <- plot_grid(plots[[1]], ID0013.RFPsum.plt, ncol = 1, rel_heights = c(3.5,1))

######################################### dF/F #########################################
# Average calcium levels over time ----------------------------------------
## Here by using dF traces after executing "dF.py" to extract them
dF <- as.data.frame(np$load("dF.npy", allow_pickle = TRUE)) #delta F calcium levels of all cells
dF <- dF[,50:ncol(dF)] #select window
# dF <- t(apply(dF, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize?
dFPOS <- dF[positivesPLUSone,]
rownames(dFPOS) <- positives


# Average activity per RFP+ cell (dF peaks) -------------------------------
ID0013.posXY$Mean.dF <- rowMeans(dFPOS)
ID0013.posXY$Mean.dF.N <- apply(as.matrix(ID0013.posXY$Mean.dF), 2,
                                function(x) (x - min(x))/(max(x)-min(x)))



# Calculating percentage of active RFP+ cells over time --------------------
dF.RFP <- subset(spksthresholded, rownames(dFPOS) %in% ID0013.RFPcells) #select only RFP cells
##ggplot to show percentage of RPF+ cells over time
RFPsum <- as.data.frame(colSums(RFP))
RFPsum$Time <- 0:(nrow(RFPsum)-1)
RFPsum$Perc <- RFPsum$`colSums(RFP)`/nrow(RFP)*100
ID0013.RFPsum.dF.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()



# Isolate RFP+ XY from posXY dataframe ------------------------------------
ID0013.posXY$RFP <- ID0013.posXY$Cell %in% ID0013.RFPcells

ID0013.posXY.RFP <- ID0013.posXY[which(ID0013.posXY$RFP=="TRUE"),]

saveRDS(ID0013.posXY.RFP, file = "ID0013.posXY.RFP.rds")
write.csv(ID0013.posXY.RFP, file = "ID0013.posXY.RFP.csv")

#Averaging per timepoint (all cells)
# dFx <- as.data.frame(colSums(dFPOS)/nrow(dFPOS))
# dFx$Time <- 0:(nrow(dFx)-1)
# #plot
# ID0013.POS.aveF <- ggplot(dFx, aes(Time, `colSums(dFPOS)/nrow(dFPOS)`))+
#   geom_line()+
#   theme_pubr()+
#   ylab("Average dF/F")+
#   geom_smooth(method = "loess")
# 
# 
# 



# Averaging per timepoint (RFP only)
# FrawRFPx <- as.data.frame(colSums(FrawRFP)/nrow(FrawRFP))
# FrawRFPx$Time <- 0:(nrow(FrawRFPx)-1)
# #plot
# ID0013.RFP.aveF <- ggplot(FrawRFPx, aes(Time, `colSums(FrawRFP)/nrow(FrawRFP)`))+
#                               geom_line()+
#                               theme_pubr()+
#                               ylab("Average Ca2+ (RFP+)")+
#                               geom_smooth(method = "loess")



















# Plot RFP deconvolved curves all together

# POSITION ANALYSIS
# cut3 <- cutree(hc, k = 3)
# 
# ID0013.posXY$Cluster <- cut3
# 
# ggplot(ID0013.posXY, aes(X, Y, color = as.factor(Cluster), shape = as.factor(Cluster)))+
#   geom_point(size = 2)+
#   # scale_color_manual(values=c('red','blue','green'))+
#   theme_graph()+
#   scale_y_reverse()







# Color clusters
# put on graph