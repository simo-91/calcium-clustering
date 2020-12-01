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

# Choose Fiji raw csv to read
calcium <- read.csv(file.choose())

# Extrapolate Time and Integrated Density values [Why Integrated Density?
#                                                  Integrated density sums all of the pixels within a region and gives you a total value. Mean fluorescent intensity gives you just that, a mean (or average) intensity.
#                                                  Integrated density will capture very bright and very dim pixels in the same object more accurately in accordance with their contribution to the biological phenomenon versus an average which will just chop the bright pixels down and raise the dim pixels up in their contribution. If you have a few very bright pixels in a sea of dim pixels, then integrated density will capture that, whereas an average will likely just chop those bright pixels down to close to no change.
#                                                  In the most basic terms, an integrated density allows a pixel to be what it actually is (either dim or bright), while an average forces each pixel to be what every other pixel.]
#
calcium <- calcium %>%
          select(X, starts_with("IntDen"))

# Reshape wide matrix to long
#meltCalcium <- melt(calcium, id = "X")

# Labels
calcium <- calcium %>%
  rename(
    "Time" = X,
  )


# Subtract background (last ROI-cell)
##subtracts last column from all others (apart from first one)
calcium <- calcium[, 2:ncol(calcium)] - calcium[, ncol(calcium)]
##cancels last column, now NA
calcium <- calcium[, -ncol(calcium)]

# Normalizing Intensities 0-1, considering the SINGLE CELL
calcium <- sapply(calcium, function(x) (x - min(x))/(max(x)-min(x)))


###SEPARATING THE CELLULAR CLUSTERS AND MATRIX TRANSPOSITION

#CLUSTER1
cluster1 <- subset(calcium, select = c(IntDen1, IntDen2, IntDen3, IntDen4, IntDen5,
                   IntDen6, IntDen7, IntDen8, IntDen9, IntDen10, IntDen11,
                   IntDen34, IntDen36))

tcluster1 <- t(cluster1) #transposed for HCLUST

#### Reshape for ggplot
cluster1.long <- melt(cluster1)
####Rename columns
colnames(cluster1.long) <- c("Time", "Cell", "Intensity")

#CLUSTER2
cluster2 <- subset(calcium, select = c(IntDen12, IntDen13, IntDen14, IntDen15, IntDen22,
                                       IntDen23))
tcluster2 <- t(cluster2)

### Rasterplot
#### Reshape for coming ggplot
cluster2.long <- melt(cluster2)
####Rename columns
colnames(cluster2.long) <- c("Time", "Cell", "Intensity")

#CLUSTER3
cluster3 <- subset(calcium, select = c(IntDen37, IntDen38, IntDen17, IntDen18, IntDen19,
                                       IntDen20, IntDen21, IntDen24, IntDen25, IntDen26, IntDen37,
                                       IntDen38, IntDen39))
tcluster3 <- t(cluster3)

### Rasterplot
#### Reshape for coming ggplot
cluster3.long <- melt(cluster3)
####Rename columns
colnames(cluster3.long) <- c("Time", "Cell", "Intensity")

#CLUSTER4
cluster4 <- subset(calcium, select = c(IntDen40:IntDen46))
tcluster4 <- t(cluster4)

### Rasterplot
#### Reshape for coming ggplot
cluster4.long <- melt(cluster4)
####Rename columns
colnames(cluster4.long) <- c("Time", "Cell", "Intensity")

#CLUSTER5
cluster5 <- subset(calcium, select = c(IntDen27:IntDen33))
tcluster5 <- t(cluster5)

### Rasterplot
#### Reshape for coming ggplot
cluster5.long <- melt(cluster5)
####Rename columns
colnames(cluster5.long) <- c("Time", "Cell", "Intensity")

# PCA?
pca <-prcomp(tcalcium, center = TRUE, scale = TRUE)
fviz_pca_ind(pca, col.ind = tcalcium, geom.ind = "point", axes =c(1, 2))

# HCLUSTERING
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
cluster2.dendro <- ggdendrogram(dhc2, rotate = TRUE)

# GRID to put together dendrograms and rasters
cluster2.order <- order.dendrogram(dhc2)

## Order the levels according to their position in the cluster
cluster2.rows <- rownames(tcluster2)
cluster2.rows <- as.data.frame(cluster2.rows)
cluster2.long$Cell <- factor(x = cluster2.long$Cell,
                             levels = cluster2.rows$cluster2.rows[cluster2.order], 
                             ordered = TRUE)

# Ggplot rasterwith dendro order
cluster2.raster <- ggplot(cluster2.long, aes(Time, Cell))+
  geom_raster(aes(fill = Intensity))+
  scale_fill_gradientn(colours=c("#fee0d2", "#5ab4ac", "#de2d26"))+
  theme(legend.position = "top")

# GRID
grid.newpage()
print(cluster2.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(cluster2.dendro, vp = viewport(x = 0.90, y = 0.43, width = 0.2, height = 0.85))











calciumXY <- read.csv(file.choose(), skip = 2)

calciumXY <- calciumXY %>%
  select(Position.X, Position.Y, TrackID)

XYplot <- ggplot(calciumXY, aes(Position.X, Position.Y, color = TrackID, size = 4))+
  geom_point()

ggplotly(XYplot)