# PSD only subdir loop
library(pacman)
p_load(utils, dplyr, tidyverse, ggplot2, plotly, tidyr, reshape2, factoextra, ggdendro,
       grid, RcppCNPy, cowplot, ggpubr, mmand, rstudioapi, reticulate, tcltk, ggfortify,
       ggpubr, factoextra, parallel, ggpattern, ggsignif, car)

# Function to choose a directory with platform-independent GUI
choose_directory = function(caption = 'Select data directory') {
  if (exists('utils::choose.dir')) {
    choose.dir(caption = caption) 
  } else {
    tk_choose.dir(caption = caption)
  }
}

# Select the main directory containing the subdirectories
main.dir <- choose_directory()

# Loop over each subdirectory in the main directory
subdirs <- list.files(path = main.dir, recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
subdirs_stat <- subdirs[grep("stat.npy", subdirs)]



id_num <- 0118 #starting ID number minus 1

for (subdir in subdirs_stat) {
  id_num <- id_num + 1
  id_str <- sprintf("ID%04d", id_num)
  
  ## Navigate to the subdirectory and load the necessary files
  setwd((dirname(subdir)))
  final_subdir <- basename(dirname(dirname(dirname(dirname(subdir)))))
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
  
  # Regression from 1sec/vol to 2sec/vol time resolution -----
  regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
  for (i in 1:(ncol(spks)/2)) {
    regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
  }
  rownames(regr_spks) <- rownames(spks)
  spks <- regr_spks
  # -----
  
  spks[is.na(spks)] <- 0
  spks <- spks[, (ncol(spks) - 370):ncol(spks)]#need to clean it from first 0 and select best window
  spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize each cell
  spks <- spks[positivesPLUSone,]  #select only positives (real cells)
  rownames(spks) <- positives #fix rownames with actual cells numbers
  spks[is.na(spks)] <- 0 #NAs replaced with 0
  saveRDS(spks, file = paste0("~/calcium-clustering/data/", id_str, "_spks.rds"))
  write.csv(spks, file = paste0("~/calcium-clustering/data/", id_str, "_spks.csv"))
  
  ## Cutoff function <- anything below 2*(row sd/cell) is 0, anytlong above is 1
  cutoff <- function(x) {
    th <- 2*sd(x)
    x[x < th] <- 0
    x[x > th] <- 1
    return(x)
  }
  
  spksthresholded <- t(apply(spks, 1, cutoff))
  
  ## Frequency -cell thresholded events divided by 60 sec -----
  posXY$frequency <- rowSums(spksthresholded)/60 #events per minute
  frequency <- mean(rowSums(spksthresholded)/60)
  
  ## Mark RFP+ cells
  RFPcells <- posXY %>% 
    subset(redcell==1) %>%
    .$Cell # extract redcell indexes
  meltPeaks$RFP <- meltPeaks$cell %in% RFPcells
  
  # RFP redcells only ------------------------------------------------
  # Calculating percentage of active RFP cells over time (using deconvolved peaks)--------------------
  RFPt <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells
  
  
  # Isolate RFP+ XY from posXY dataframe ------------------------------------
  posXY$RFP <- posXY$Cell %in% RFPcells
  
  posXY.RFP <- posXY %>%
    subset(redcell==1)
  
  
  # PSD analysis for acquisition rate of 2sec/vol--------------------------------------
  fft <- apply(spksthresholded, 1, fft)
  
  # Calculate power spectrum for each time series
  psd <- abs(fft)^2/ncol(spksthresholded) # normalized by length of acquisition
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
  assign(paste0(id_str, ".psd.mean"), psd.mean)
  
  # RFP only
  fft.RFP <- apply(RFPt, 1, fft)
  
  # Calculate power spectrum for each time series
  psd.RFP <- abs(fft.RFP)^2/ncol(RFP) # normalized by length of acquisition
  
  psd.RFP <- round(psd.RFP, digits = 1)
  psd.RFP.melt <- as.data.frame(psd.RFP)
  psd.RFP.melt$frequency <- freq
  psd.RFP.melt <- melt(psd.RFP.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")
  
  psd.RFP.mean <- summarise(psd.RFP.melt, "PSD mean" = mean(PSD), .by = "frequency")
  assign(paste0(id_str, ".psd.RFP.mean"), psd.RFP.mean)
  
  
  setTxtProgressBar(pb_general, subdir)
}
close(pb_general)