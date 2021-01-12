################### CALCIUM ANALYSIS WITH THRESHOLDING AND DIGITIZATION
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(factoextra)
library(ggdendro)
library(grid)
library(mmand)
library(dendextend)
library(dendextendRcpp)
library(gplots)

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

## Store threshold using 2.5 * SD background ROI
background <- (background - min(background))/(max(background)-min(background))
calcium.threshold <- sd(background) * 2.5

##Apply threshold to dataframe
calcium <- threshold(calcium[,1:ncol(calcium)], calcium.threshold, method = c("literal"), binarise = TRUE)




# HCLUSTERING
##ALL CLUSTERS
tcalcium <- t(calcium)
hccalcium <- hclust(dist(tcalcium, method = "euclidean"), method = "ward.D2")
dhcc <- as.dendrogram(hccalcium)




calcium.long <- melt(calcium)
####Rename columns
colnames(calcium.long) <- c("Time", "Cell", "Intensity")


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

# Ggplot raster with dendro order
calcium.raster <- ggplot(calcium.long, aes(Time, Cell))+
  geom_raster(aes(fill = Intensity))+
  scale_fill_gradientn(colours=c("white", "black"))+
  theme(legend.position = "top")

# GRID
grid.newpage()
print(calcium.raster, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(calcium.dendro, vp = viewport(x = 0.90, y = 0.435, width = 0.2, height = 0.92))

# # Calculating percentage of active cells over time
# calciumSUM <- rowSums(calcium5)
# calciumSUM <- as.data.frame(calciumSUM)
# calciumSUM$Time <- rownames(calciumSUM)
# calciumSUM$calciumSUM <- calciumSUM$calciumSUM/ncol(calcium5)*100
# calciumACTIVE_cells <- ggplot(calciumSUM, aes(Time, calciumSUM))+
#   geom_segment()+
#   ylim(0,100)




# Divide in clusters
# plot(dhcc, horiz = TRUE)
# color8 <- color_branches(dhcc, k = 8, horiz = TRUE)
# plot(color5, horiz = TRUE)
# rect5 <- rect.dendrogram(dhcc, k = 5, horiz = TRUE)




# POSITION ANALYSIS
calciumXY <- read.csv(file.choose())
cut5 <- cutree(hccalcium, k = 5)

calciumXY$Cluster <- cut5
calciumXY <- calciumXY %>%
  rename(
    "Cell" = X.1,
  )

ggplot(calciumXY, aes(X, Y, color = as.factor(Cluster)))+
  geom_point(size = 3)+
  scale_color_manual(values=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'))+
  theme(legend.title = element_blank())

