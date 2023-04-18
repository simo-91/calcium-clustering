library(tcltk)

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

id_num <- 1000 #starting ID number
for (subdir in subdirs_stat) {
  id_num <- id_num + 1
  id_str <- sprintf("ID%04d", id_num)
  
  ## Navigate to the subdirectory and load the necessary files
  setwd((dirname(subdir)))
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
  
  ## Regression from 1sec/vol to 2sec/vol time resolution
  regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
  for (i in 1:(ncol(spks)/2)) {
    regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
  }
  rownames(regr_spks) <- rownames(spks)
  spks <- regr_spks
  # 
  
  spks[is.na(spks)] <- 0
  spks <- spks[,50:ncol(spks)] #need to clean it from first 0 and select best window
  spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize each cell
  spks <- spks[positivesPLUSone,]  #select only positives (real cells)
  rownames(spks) <- positives #fix rownames with actual cells numbers
  spks[is.na(spks)] <- 0 #NAs replaced with 0
  saveRDS(spks, file = paste0(id_str, "_spks.rds"))
  write.csv(spks, file = paste0(id_str, "_spks.csv"))
  
  ## Cutoff function <- anything below 2*(row sd/cell) is 0, anytlong above is 1
  cutoff <- function(x) {
    th <- 2*sd(x)
    x[x < th] <- 0
    x[x > th] <- 1
    return(x)
  }
  
  spksthresholded <- t(apply(spks, 1, cutoff))
  saveRDS(spksthresholded, file = paste0(id_str, "_spksthresholded.rds"))
  write.csv(spksthresholded, file = paste0(id_str, "_spksthresholded.csv"))
  
  ## Calculating percentage of active cells over time -------------------------
  spksSUM <- colSums(spksthresholded)
  spksSUM <- as.data.frame(spksSUM)
  spksSUM$Time <- 0:(nrow(spksSUM)-1)
  spksSUM$Perc <- spksSUM$spksSUM/nrow(spksthresholded)*100
  saveRDS(spksSUM, file = paste0(id_str, "_percentage_of_coactive_cells.rds"))
  write.csv(spksSUM, file = paste0(id_str, "_percentage_of_coactive_cells.csv"))
  spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
    geom_line()+ 
    theme_pubr()
  
  
  ## Frequency -cell thresholded events divided by 60 sec -----
  posXY$frequency <- rowSums(spksthresholded)/60 #events per minute
  frequency <- mean(rowSums(spksthresholded)/60)
  
  saveRDS(frequency, file = paste0(id_str, "_mean_frequency.rds"))
  write.csv(frequency, file = paste0(id_str, "_mean_frequency.csv"))
  ### Compare number of events/min across samples?
  
  ## Plot total calcium activity/time --------------------------------------
  spksSUM2 <- colSums(spks)
  spksSUM2 <- as.data.frame(spksSUM2)
  spksSUM2$Time <- 0:(nrow(spksSUM2)-1)
  spksSUM2$Mean <- spksSUM2$spksSUM2/nrow(spksSUM2)
  saveRDS(spksSUM2, file = paste0(id_str, "_total_activity.rds"))
  write.csv(spksSUM2, file = paste0(id_str, "_total_activity.csv"))
  spksSUM2.plt <- ggplot(spksSUM2, aes(Time, Mean))+
    geom_line()+ 
    theme_pubr()+
    geom_smooth()+
    ylab("Ca2+")+
    ylim(0, NA)
  spksSUM2.ylim <- layer_scales(spksSUM2.plt)$y$get_limits()
  ggsave(plot = spksSUM2.plt, file = paste0(id_str, "_total_activity.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  ## Average activity PER CELL (deconvolved peaks) ---------------------------
  posXY$Mean <- rowMeans(spks)
  
  ## Raster+dendro all cells/time ggplot ------------------------------------------
  dfpeaks <- as.data.frame(t(spks))  # Doing this coercion will apply +1 to all cells numbers
  # colnames(dfpeaks) <- 1:ncol(dfpeaks)
  dfpeaks$time <- 0:(nrow(dfpeaks)-1)
  meltPeaks <- melt(dfpeaks, id = "time")
  colnames(meltPeaks) <- c('time','cell','Ca2+')
  
  
  ## Hierarchical clustering
  hc <- hclust(dist(spks, method = "euclidean"), method = "ward.D2")
  dhc <- as.dendrogram(hc)
  
  ## Dendrogram
  peaks.dendro <- ggdendrogram(dhc, rotate = TRUE, labels = FALSE)+
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  ## GRID to put together dendrograms and rasters
  peaks.order <- order.dendrogram(dhc)
  
  ## Order the levels according to their position in the cluster
  peaks.rows <- rownames(spks)
  peaks.rows <- as.data.frame(peaks.rows)
  meltPeaks$cell <- factor(x = meltPeaks$cell,
                           levels = peaks.rows$peaks.rows[peaks.order], 
                           ordered = TRUE)
  ## Mark RFP+ cells
  RFPcells <- posXY %>% 
    subset(redcell==1) %>%
    .$Cell # extract redcell indexes
  meltPeaks$RFP <- meltPeaks$cell %in% RFPcells
  
  ## Ggplot raster with dendro order
  raster.hc <- ggplot(meltPeaks, aes(time, cell))+
    geom_raster(aes(fill = `Ca2+`))+
    geom_line(aes(color = RFP), alpha = .2)+
    scale_fill_gradientn(colours=c("white", "black"))+
    scale_color_manual(values = c("TRUE" = "red",
                                  "FALSE" = "white"))+
    theme_pubr()+
    ylab("cell ID")+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(colour = "red", hjust = .5))+
  ggtitle(paste0(id_str, " hclust"), subtitle = sprintf("Mean frequency is: %s events/min", round(frequency, digits = 3)))
  
  ## GRID raster/sums
  plots <- align_plots(raster.hc, spksSUM.plt, align = 'v', axis = 'l')
  grid <- plot_grid(plots[[1]], spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))
  
  ## Save data
  ggsave(plot = grid, file = paste0(id_str, "_grid-allcells.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  saveRDS(posXY, file = paste0(id_str, "_posXY.rds"))
  write.csv(posXY, file = paste0(id_str, "_posXY.csv"))
  
  # RFP redcells only ------------------------------------------------
  # Calculating percentage of active RFP cells over time (using deconvolved peaks)--------------------
  
  RFPt <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells
  
  # Isolate RFP+ XY from posXY dataframe ------------------------------------
  posXY$RFP <- posXY$Cell %in% RFPcells
  
  posXY.RFP <- posXY %>%
    subset(redcell==1)
  
  # Frequency -cell thresholded events divided by 60 sec -----
  posXY.RFP$frequency <- rowSums(RFPt)/60 #events per minute
  frequency.RFP <- mean(rowSums(RFPt)/60)
  
  saveRDS(posXY.RFP, file = paste0(id_str, "_dataposXY.RFP.rds"))
  write.csv(posXY.RFP, file = paste0(id_str, "_dataposXY.RFP.csv"))
  
  ##ggplot to show percentage of coactive RPF+ cells over time
  RFPsum <- as.data.frame(colSums(RFPt))
  RFPsum$Time <- 0:(nrow(RFPsum)-1)
  RFPsum$Perc <- RFPsum$`colSums(RFPt)`/nrow(RFPt)*100
  saveRDS(RFPsum, file = paste0(id_str, "_RFPsum.rds"))
  write.csv(RFPsum, file = paste0(id_str, "_RFPsum.csv"))
  
}
