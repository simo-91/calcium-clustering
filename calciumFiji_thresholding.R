################### CALCIUM ANALYSIS WITH THRESHOLDING AND DIGITIZATION

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
library(mmand)
# Choose Fiji raw csv to read
calcium <- read.csv(file.choose())

# Extrapolate Time and Integrated Density values [Why Integrated Density?
#                                                  Integrated density sums all of the pixels within a region and gives you a total value. Mean fluorescent intensity gives you just that, a mean (or average) intensity.
#                                                  Integrated density will capture very bright and very dim pixels in the same object more accurately in accordance with their contribution to the biological phenomenon versus an average which will just chop the bright pixels down and raise the dim pixels up in their contribution. If you have a few very bright pixels in a sea of dim pixels, then integrated density will capture that, whereas an average will likely just chop those bright pixels down to close to no change.
#                                                  In the most basic terms, an integrated density allows a pixel to be what it actually is (either dim or bright), while an average forces each pixel to be what every other pixel.]
#
calcium <- calcium %>%
  select(X, starts_with("IntDen"))

# Labels
calcium <- calcium %>%
  rename(
    "Time" = X,
  )

colnames(calcium) <- gsub(x = colnames(calcium), pattern = "IntDen", replacement = "Cell ")
# Subtract background (last ROI-cell)
##Store last ROI-cell
background <- calcium[, ncol(calcium)]

##subtracts last column from all others (apart from first one)
calcium <- calcium[, 2:ncol(calcium)] - background

##cancels last column, now NA
calcium <- calcium[, -ncol(calcium)]

# Normalizing Intensities 0-1, considering the SINGLE CELL
calcium <- sapply(calcium[ , 1:ncol(calcium)], function(x) (x - min(x))/(max(x)-min(x)))

## Store threshold using 3SD * background ROI/Cell
background <- (background - min(background))/(max(background)-min(background))
calcium.threshold <- sd(background) * 2

##Apply threshold to dataframe
calcium <- threshold(calcium[,1:ncol(calcium)], calcium.threshold, method = c("literal"), binarise = TRUE)



### Rasterplot
#### Reshape for coming ggplot
calciumMELT <- melt(calcium)
####Rename columns
colnames(calciumMELT) <- c("Time", "Cell", "Intensity")


ggplot(calciumMELT, aes(Time, Cell))+
  geom_raster(aes(fill = Intensity))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme(legend.position = "top")

################################################################################
###SEPARATING THE CELLULAR CLUSTERS AND MATRIX TRANSPOSITION####################
################################################################################
#CLUSTER1
cluster1 <- subset(calcium, select = c("Cell 1", "Cell 2", "Cell 3", "Cell 4", "Cell 5",
                                       "Cell 6", "Cell 7", "Cell 8", "Cell 9", "Cell 10", "Cell 11",
                                       "Cell 34", "Cell 36"))

tcluster1 <- t(cluster1) #transposed for HCLUST

#### Reshape for ggplot
cluster1.long <- melt(cluster1)
####Rename columns
colnames(cluster1.long) <- c("Time", "Cell", "Intensity")

#CLUSTER2
cluster2 <- subset(calcium, select = c("Cell 12", "Cell 13", "Cell 14", "Cell 15", "Cell 22",
                                       "Cell 23"))
tcluster2 <- t(cluster2)

### Rasterplot
#### Reshape for coming ggplot
cluster2.long <- melt(cluster2)
####Rename columns
colnames(cluster2.long) <- c("Time", "Cell", "Intensity")

#CLUSTER3
cluster3 <- subset(calcium, select = c("Cell 37", "Cell 38", "Cell 17", "Cell 18", "Cell 19",
                                       "Cell 20", "Cell 21", "Cell 24", "Cell 25", "Cell 26",
                                       "Cell 39"))
tcluster3 <- t(cluster3)

### Rasterplot
#### Reshape for coming ggplot
cluster3.long <- melt(cluster3)
####Rename columns
colnames(cluster3.long) <- c("Time", "Cell", "Intensity")

#CLUSTER4
cluster4 <- subset(calcium, select = c(`Cell 40`:`Cell 46`))
tcluster4 <- t(cluster4)

### Rasterplot
#### Reshape for coming ggplot
cluster4.long <- melt(cluster4)
####Rename columns
colnames(cluster4.long) <- c("Time", "Cell", "Intensity")

#CLUSTER5
cluster5 <- subset(calcium, select = c(`Cell 27`:`Cell 33`))
tcluster5 <- t(cluster5)

### Rasterplot
#### Reshape for coming ggplot
cluster5.long <- melt(cluster5)
####Rename columns
colnames(cluster5.long) <- c("Time", "Cell", "Intensity")

# PCA?
##pca <-prcomp(tcalcium, center = TRUE, scale = TRUE)
##fviz_pca_ind(pca, col.ind = tcalcium, geom.ind = "point", axes =c(1, 2))

# HCLUSTERING
##ALL CLUSTERS
tcalcium <- t(calcium)
hccalcium <- hclust(dist(tcalcium, method = "euclidean"), method = "ward.D2")
dhcc <- as.dendrogram(hccalcium)
calcium.long <- melt(calcium)
####Rename columns
colnames(calcium.long) <- c("Time", "Cell", "Intensity")

##SINGLE CLUSTERS
hc1 <- hclust(dist(tcluster1, method = "euclidean"), method = "ward.D2")
dhc1 <- as.dendrogram(hc1)
hc2 <- hclust(dist(tcluster2, method = "euclidean"), method = "ward.D2")
dhc2 <- as.dendrogram(hc2)
hc3 <- hclust(dist(tcluster3, method = "euclidean"), method = "ward.D2")
dhc3 <- as.dendrogram(hc3)
hc4 <- hclust(dist(tcluster4, method = "euclidean"), method = "ward.D2")
dhc4 <- as.dendrogram(hc4)
hc5 <- hclust(dist(tcluster5, method = "euclidean"), method = "ward.D2")
dhc5 <- as.dendrogram(hc5)

# Dendrogram
calcium.dendro <- ggdendrogram(dhcc, rotate = TRUE)

# GRID to put together dendrograms and rasters
calcium.order <- order.dendrogram(dhcc)

## Order the levels according to their position in the cluster
calcium.rows <- rownames(tcalcium)
calcium.rows <- as.data.frame(calcium.rows)
calcium.long$Cell <- factor(x = calcium.long$Cell,
                             levels = calcium.rows$calcium.rows[calcium.order], 
                             ordered = TRUE)

# Ggplot rasterwith dendro order
calcium.raster <- ggplot(calcium.long, aes(Time, Cell))+
  geom_raster(aes(fill = Intensity))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme(legend.position = "top")

# GRID
grid.newpage()
print(calcium.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(calcium.dendro, vp = viewport(x = 0.90, y = 0.45, width = 0.2, height = 0.95))



