library(pacman)
p_load(utils, dplyr, tidyverse, ggplot2, plotly, tidyr, reshape2, factoextra, ggdendro,
       grid, RcppCNPy, cowplot, ggpubr, mmand, rstudioapi, reticulate, tcltk, ggfortify,
       ggpubr, factoextra, parallel, ggpattern, ggsignif, car)

# Change id_num: ID0143

np <- import("numpy")
stat <- np$load("stat.npy", allow_pickle = TRUE)
redcell <- np$load("redcell.npy", allow_pickle = TRUE)
iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE)) #deconvolved peaks

## Extract the ROI positions of positive cells and save the output files
posXY <- data.frame()
for (i in 1:length(stat)) {
  posXY <- rbind(posXY, stat[[i]][["med"]])
}
posXY$Cell <- as.numeric(0:(nrow(posXY)-1))
colnames(posXY) <- c('Y','X','Cell')
posXY$redcell <- redcell[,1]
iscell$Cell <- as.numeric(0:(nrow(iscell)-1))
posXY$Positive <- iscell$V1
posXY <- subset(posXY, Positive == 1, select = c(Y,X,Cell,redcell))
positives <- posXY$Cell
positivesPLUSone <- positives+1
# 
# # Regression from 1sec/vol to 2sec/vol time resolution -----
# regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
# for (i in 1:(ncol(spks)/2)) {
#   regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
# }
# rownames(regr_spks) <- rownames(spks)
# spks <- regr_spks
# # -----

spks[is.na(spks)] <- 0
spks <- spks[, -c(1:(ncol(spks) - 371))] #need to clean it from first 0 and select best window
spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize each cell
spks <- spks[positivesPLUSone,]  #select only positives (real cells)
rownames(spks) <- positives #fix rownames with actual cells numbers
spks[is.na(spks)] <- 0 #NAs replaced with 0

## Sum of all activity over time RFP+
RFP <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells

# PSD starts here
fft <- apply(spks, 1, fft)

# Calculate power spectrum for each time series
psd <- abs(fft)^2/ncol(spks) # normalized by length of acquisition
fs = 0.5 #sampling freq in Hertz; we take a sample every two seconds (1 sample/2 seconds = 0.5)
nyquist <- fs/2

psd <- round(psd, digits = 1)
lowest_freq <- 1/371
freq <- seq(lowest_freq, nyquist, length.out=nrow(psd)) # calculate frequency range
psd.melt <- as.data.frame(psd)
psd.melt$frequency <- freq
psd.melt <- melt(psd.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")

psd_mean_colname <- paste0(id_str, " PSD mean")
psd.mean <- summarise(psd.melt, "PSD mean" = mean(PSD), .by = "frequency")
assign(paste0("ID0143.psd.mean"), psd.mean)

# RFP only
fft.RFP <- apply(RFP, 1, fft)

# Calculate power spectrum for each time series
psd.RFP <- abs(fft.RFP)^2/ncol(RFP) # normalized by length of acquisition

psd.RFP <- round(psd.RFP, digits = 1)
psd.RFP.melt <- as.data.frame(psd.RFP)
psd.RFP.melt$frequency <- freq
psd.RFP.melt <- melt(psd.RFP.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")

psd.RFP.mean <- summarise(psd.RFP.melt, "PSD mean" = mean(PSD), .by = "frequency")
assign(paste0("ID0143.psd.RFP.mean"), psd.RFP.mean)
