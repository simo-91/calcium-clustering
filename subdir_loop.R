# Libraries ----
library(pacman)
p_load(utils, dplyr, tidyverse, ggplot2, plotly, tidyr, reshape2, factoextra, ggdendro,
       grid, RcppCNPy, cowplot, ggpubr, mmand, rstudioapi, reticulate, tcltk, ggfortify,
       ggpubr, factoextra, parallel, ggpattern, ggsignif, car, gtools, igraph, ggraph, 
       emmeans, FSA, rstatix,tcltk)
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
subdirs_stat <- mixedsort(subdirs_stat)


id_num <- 177 #starting ID number minus 1

# Define a function to calculate event frequency
calculate_event_frequency <- function(spksthresholded, frame_rate) {
  event_counts <- rowSums(spksthresholded > 0)
  total_time <- ncol(spksthresholded) / frame_rate / 60  # Total time in minutes
  return(event_counts / total_time)
}
# Frame rate calculation
frame_rate <- 1 / 2  # 1 frame every 2 seconds

# Initialize a progress bar
total_subdirs <- length(subdirs_stat)
pb_main <- txtProgressBar(min = 0, max = total_subdirs, style = 3)

for (subdir in subdirs_stat) {
  setTxtProgressBar(pb_main, id_num)
  id_num <- id_num + 1
  id_str <- sprintf("ID%04d", id_num)
  
  ## Navigate to the subdirectory and load the necessary files
  setwd((dirname(subdir)))
  path_parts <- strsplit(subdir, "/")[[1]]
  final_subdir <- path_parts[4]
  
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
  
  # # Regression from 1sec/vol to 2sec/vol time resolution -----
  # regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
  # for (i in 1:(ncol(spks)/2)) {
  #   regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
  # }
  # rownames(regr_spks) <- rownames(spks)
  # spks <- regr_spks
  # #
  
  spks[is.na(spks)] <- 0
  spks <- spks[,50:ncol(spks)] #need to clean it from first 0 and select best window
  spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize each cell
  spks <- spks[positivesPLUSone,]  #select only positives (real cells)
  rownames(spks) <- positives #fix rownames with actual cells numbers
  spks[is.na(spks)] <- 0 #NAs replaced with 0
  saveRDS(spks, file = paste0("~/calcium-clustering/data/", id_str, "_spks.rds"))
  write.csv(spks, file = paste0("~/calcium-clustering/data/", id_str, "_spks.csv"))
  assign(paste0(id_str, "_spks"), spks)
  
  ## Cutoff function <- anything below 2*(row sd/cell) is 0, anytlong above is 1
  cutoff <- function(x) {
    th <- 2*sd(x)
    x[x < th] <- 0
    x[x > th] <- 1
    return(x)
  }
  
  spksthresholded <- t(apply(spks, 1, cutoff))
  saveRDS(spksthresholded, file = paste0("~/calcium-clustering/data/", id_str, "_spksthresholded.rds"))
  write.csv(spksthresholded, file = paste0("~/calcium-clustering/data/", id_str, "_spksthresholded.csv"))
  assign(paste0(id_str, "_spksthresholded"), spksthresholded)
  
  
  ## Calculating percentage of active cells over time -------------------------
  spksSUM <- colSums(spksthresholded)
  spksSUM <- as.data.frame(spksSUM)
  spksSUM$Time <- 0:(nrow(spksSUM)-1)
  spksSUM$Perc <- spksSUM$spksSUM/nrow(spksthresholded)*100
  saveRDS(spksSUM, file = paste0("~/calcium-clustering/data/", id_str, "_percentage_of_coactive_cells.rds"))
  write.csv(spksSUM, file = paste0("~/calcium-clustering/data/", id_str, "_percentage_of_coactive_cells.csv"))
  spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
    geom_line()+ 
    theme_pubr()
  mean_perc_active_cells <- mean(spksSUM$spksSUM)
  assign(paste0(id_str, "_mean_perc_active_cells"), frequency)

  
  # Calculate frequency for all cells ----
  event_frequency_all <- calculate_event_frequency(spksthresholded, frame_rate)
  mean_frequency_all <- mean(event_frequency_all)
  frequencies_df <- data.frame(Cell = rownames(spksthresholded), Frequency = event_frequency_all)
  assign(paste0(id_str, ".frequencies.df"), frequencies_df)
  assign(paste0(id_str, ".frequency"), mean_frequency_all)
  
  # Calculate frequency for RFP/redcell only cells
  RFPt <- subset(spksthresholded, rownames(spksthresholded) %in% positives[posXY$redcell == 1])
  event_frequency_RFP <- calculate_event_frequency(RFPt, frame_rate)
  mean_frequency_RFP <- mean(event_frequency_RFP)
  frequencies_RFP_df <- data.frame(Cell = rownames(RFPt), Frequency = event_frequency_RFP)
  assign(paste0(id_str, ".frequencies.RFP.df"), frequencies_RFP_df)
  assign(paste0(id_str, ".frequency.RFP"), mean_frequency_RFP)
  
  ## Plot total calcium activity/time --------------------------------------
  spksSUM2 <- colSums(spks)
  spksSUM2 <- as.data.frame(spksSUM2)
  spksSUM2$Time <- 0:(nrow(spksSUM2)-1)
  spksSUM2$Mean <- spksSUM2$spksSUM2/nrow(spksSUM2)
  saveRDS(spksSUM2, file = paste0("~/calcium-clustering/data/", id_str, "_total_activity.rds"))
  write.csv(spksSUM2, file = paste0("~/calcium-clustering/data/", id_str, "_total_activity.csv"))
  spksSUM2.plt <- ggplot(spksSUM2, aes(Time, Mean))+
    geom_line()+ 
    theme_pubr()+
    geom_smooth()+
    ylab("Ca2+")+
    ylim(0, NA)
  spksSUM2.ylim <- layer_scales(spksSUM2.plt)$y$get_limits()
  ggsave(plot = spksSUM2.plt, file = paste0("~/calcium-clustering/plots/", id_str, "_total_activity.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  ## Average activity PER CELL (deconvolved peaks) ---------------------------
  posXY$Mean <- rowMeans(spks)
  mean_Ca <- rowMeans(spks)
  assign(paste0(id_str, "_mean_Ca"), mean_Ca)
  
  
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
  ggtitle(paste0(id_str, final_subdir, " hclust"), subtitle = sprintf("Mean frequency is: %s events/min", round(mean_frequency_all, digits = 3)))
  
  ## GRID raster/sums
  plots <- align_plots(raster.hc, spksSUM.plt, align = 'v', axis = 'l')
  grid <- plot_grid(plots[[1]], spksSUM.plt, ncol = 1, rel_heights = c(4.5,1))
  
  ## Save data
  ggsave(plot = grid, file = paste0("~/calcium-clustering/plots/", id_str, "_grid-allcells.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  saveRDS(posXY, file = paste0("~/calcium-clustering/data/", id_str, "_posXY.rds"))
  write.csv(posXY, file = paste0("~/calcium-clustering/data/", id_str, "_posXY.csv"))
  
  # RFP redcells only ------------------------------------------------
  # Calculating percentage of active RFP cells over time (using deconvolved peaks)--------------------
  RFPt <- subset(spksthresholded, rownames(spksthresholded) %in% RFPcells) #select only RFP cells
  
  # Isolate RFP+ XY from posXY dataframe ------------------------------------
  posXY$RFP <- posXY$Cell %in% RFPcells
  
  posXY.RFP <- posXY %>%
    subset(redcell==1)
  
  # Frequency RFP -cell thresholded events divided by 120 sec (2sec per volume) -----
  posXY.RFP$frequency <- rowSums(RFPt)/120 #events per minute
  frequency.RFP <- mean(rowSums(RFPt)/120)
  saveRDS(posXY.RFP, file = paste0("~/calcium-clustering/data/", id_str, "_dataposXY.RFP.rds"))
  write.csv(posXY.RFP, file = paste0("~/calcium-clustering/data/", id_str, "_dataposXY.RFP.csv"))
  
  assign(paste0(id_str, "_frequency.RFP"), frequency.RFP)
  
  ## ggplot to show percentage of coactive RPF+ cells over time -----
  RFPsum <- as.data.frame(colSums(RFPt))
  RFPsum$Time <- 0:(nrow(RFPsum)-1)
  RFPsum$Perc <- RFPsum$`colSums(RFPt)`/nrow(RFPt)*100
  saveRDS(RFPsum, file = paste0("~/calcium-clustering/data/", id_str, "_percentage_of_coactive_cells-RFP.rds"))
  write.csv(RFPsum, file = paste0("~/calcium-clustering/data/", id_str, "_percentage_of_coactive_cells-RFP.csv"))
  
  RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
    geom_line()+
    theme_pubr()
  ggsave(plot = RFPsum.plt, file = paste0("~/calcium-clustering/data/", id_str, "_percentage_of_coactive_cells-RFP.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  ## Sum of all activity over time RFP+
  RFP <- subset(spks, rownames(spks) %in% RFPcells) #select only RFP cells
  RFPsum2 <- as.data.frame(colSums(RFP)/nrow(RFP)) #normalized by number of RFP cells
  RFPsum2$Time <- 0:(nrow(RFPsum2)-1)
  saveRDS(RFPsum2, file = paste0("~/calcium-clustering/data/", id_str, "_total_activity_RFP.rds"))
  write.csv(RFPsum2, file = paste0("~/calcium-clustering/data/", id_str, "_total_activity_RFPsum2.csv"))
  
  RFPsum2.plt <- ggplot(RFPsum2, aes(Time, `colSums(RFP)/nrow(RFP)`))+
    geom_line()+
    theme_pubr()+
    geom_smooth()+
    ylab("Ca2+")+
    ylim(0, NA)
  RFPsum2.plt.ylim <- layer_scales(RFPsum2.plt)$y$get_limits()
  ggsave(plot = RFPsum2.plt, file = paste0("~/calcium-clustering/plots/", id_str, "_total_activity-RFP.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  ## RFPcells for activity/hc/raster/dendrogram
  ### Take care of using already normalized spks array
  dfpeaks.RFP <- as.data.frame(t(RFP))
  # colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
  dfpeaks.RFP$time <- 0:(nrow(dfpeaks.RFP)-1)
  meltPeaks.RFP <- melt(dfpeaks.RFP, id = "time")
  colnames(meltPeaks.RFP) <- c('time','cell','Ca2+')
  
  ## Hierarchical clustering
  hc.RFP <- hclust(dist(RFP, method = "euclidean"), method = "ward.D2")
  dhc.RFP <- as.dendrogram(hc.RFP)
  
  ## Dendrogram
  RFP.dendro <- ggdendrogram(dhc.RFP, rotate = TRUE, labels = FALSE)+
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  ## GRID to put together dendrograms and rasters
  RFP.order <- order.dendrogram(dhc.RFP)
  
  ## Order the levels according to their position in the cluster
  RFP.rows <- rownames(RFP)
  RFP.rows <- as.data.frame(RFP.rows)
  meltPeaks.RFP$cell <- factor(x = meltPeaks.RFP$cell,
                               levels = RFP.rows$RFP.rows[RFP.order], 
                               ordered = TRUE)
  
  ## Ggplot raster with dendro order
  RFP.raster <- ggplot(meltPeaks.RFP, aes(time, cell))+
    geom_raster(aes(fill = `Ca2+`))+
    scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
    theme_pubr()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          # axis.ticks.y = element_blank(),
          # axis.text.y = element_text(face=posXY.RFP$Cell[which(posXY.RFP$Member=="TRUE"),], "bold"),
          plot.title = element_text(colour = "red", hjust = .5))+
    ggtitle(paste0(id_str, " hclust RFP+", final_subdir), subtitle = sprintf("Mean frequency is: %s events/min", round(frequency.RFP, digits = 3)))
  
  ## GRID raster/sums
  plots <- align_plots(RFP.raster, RFPsum.plt, align = 'v', axis = 'l')
  RFP.grid <- plot_grid(plots[[1]], RFPsum.plt, ncol = 1, rel_heights = c(3.5,1))
  ggsave(plot = RFP.grid, file = paste0("~/calcium-clustering/plots/", id_str, "_grid-RFP.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  # RFPcells for activity/hc/raster/dendrogram using thresholded data
  
  ## Raster ggplot
  dfpeaks.RFPt <- as.data.frame(t(RFPt))
  # colnames(dfpeaks.RFP) <- 1:ncol(dfpeaks.RFP)
  dfpeaks.RFPt$time <- 0:(nrow(dfpeaks.RFPt)-1)
  meltPeaks.RFPt <- melt(dfpeaks.RFPt, id = "time")
  colnames(meltPeaks.RFPt) <- c('time','cell','Ca2+')
  
  ## Hierarchical clustering
  hc.RFPt <- hclust(dist(RFPt, method = "euclidean"), method = "ward.D2")
  dhc.RFPt <- as.dendrogram(hc.RFPt)
  
  ## Dendrogram
  RFPt.dendro <- ggdendrogram(dhc.RFPt, rotate = TRUE, labels = FALSE)+
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  
  ## GRID to put together dendrograms and rasters
  RFPt.order <- order.dendrogram(dhc.RFPt)
  
  ## Order the levels according to their position in the cluster
  RFPt.rows <- rownames(RFPt)
  RFPt.rows <- as.data.frame(RFPt.rows)
  meltPeaks.RFPt$cell <- factor(x = meltPeaks.RFPt$cell,
                                levels = RFPt.rows$RFPt.rows[RFPt.order], 
                                ordered = TRUE)
  
  ## Ggplot rasterwith dendro order
  RFPt.raster <- ggplot(meltPeaks.RFPt, aes(time, cell))+
    geom_raster(aes(fill = `Ca2+`))+
    scale_fill_gradientn(colours=c("white", "grey20", "grey10", "black"))+
    theme_pubr()+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          # axis.ticks.y = element_blank(),
          # axis.text.y = element_text(face=posXY.RFP$Cell[which(posXY.RFP$Member=="TRUE"),], "bold"),
          plot.title = element_text(colour = "red", hjust = .5))+
    ggtitle(paste0(id_str, " binarized, RFP+"), subtitle = sprintf("Mean frequency is: %s events/min", round(frequency.RFP, digits = 3)))
  
  ## GRID raster/sums
  plots <- align_plots(RFPt.raster, RFPsum.plt, align = 'v', axis = 'l')
  RFPt.grid <- plot_grid(plots[[1]], RFPsum.plt, ncol = 1, rel_heights = c(3.5,1))
  ggsave(plot = RFPt.grid, file = paste0("~/calcium-clustering/plots/", id_str, "_binarized_grid-RFP.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)

  # Graph analysis -------------------------------------------------------
  ## Using thresholded values
  # Create empty matrix that will host corr coefficents. 
  T.allcellst <- t(spksthresholded)
  ## Function to find max CCF between a and b in ±1 lag range
  max_CCF<- function(a,b)
  {
    d <- ccf(a, b, plot = FALSE, lag.max = 1)
    cor = d$acf[,,1]
    return(max(cor))
  } 
  p_load(parallel)
  
  # Set up parallel processing
  n_cores <- detectCores()
  cl <- makeCluster(n_cores)
  clusterExport(cl, "T.allcellst")  # Export T.allcellst to the cluster
  clusterExport(cl, "max_CCF")  # Export max_CCF function to the cluster
  clusterEvalQ(cl, {
    library(matrixStats)  # Load the matrixStats package on the cluster
  })
  
  # Define a function to compute max_CCF
  max_CCF_parallel <- function(i, j) {
    max_CCF(T.allcellst[, i], T.allcellst[, j])
  }
  
  # Initialize the result matrix
  n_cols <- ncol(T.allcellst)
  cmat.allcellst <- matrix(0, n_cols, n_cols)
  
  # Loop through upper triangular part of the matrix in parallel
  pb <- txtProgressBar(min = 0, max = n_cols, style = 3)
  for (i in 1:(n_cols - 1)) {
    res <- parLapply(cl, (i + 1):n_cols, max_CCF_parallel, i = i)
    cmat.allcellst[i, (i + 1):n_cols] <- unlist(res)
    setTxtProgressBar(pb, i)
  }
  
  # Mirror the upper triangular part to the lower triangular part
  cmat.allcellst[lower.tri(cmat.allcellst)] <- t(cmat.allcellst)[lower.tri(cmat.allcellst)]
  
  diag(cmat.allcellst) <- 1 # Set diagonal to 1
  cmat.allcellst[is.na(cmat.allcellst)] <- 0 #(Temporary fix) NaN replaced with 0. This happens when manually adding a ROI on Suite2p mistakenly results in a constant time-series of zeroes (hence ccf() tries to divide by 0)
  
  # Clean up parallel processing
  stopCluster(cl)
  close(pb)
  
  # Label cells back with their actual numbers
  rownames(cmat.allcellst) <- rownames(spksthresholded)
  colnames(cmat.allcellst) <- rownames(spksthresholded)
  
  # General graph ----------------------------
  p_load("ggplot2", "igraph", "ggraph", "visNetwork", "tidyverse", "tidygraph",
         "ggiraph", "ggnewscale", "grid", "gridExtra", "RColorBrewer", "pals")
  
  graph <- graph.adjacency(as.matrix(cmat.allcellst), mode = "undirected", weighted = TRUE, diag = FALSE) # using thresholded values
  
  # Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
  # Set weight threshold (set to 0.30 as per literature: Avitan et al., 2017 http://dx.doi.org/10.1016/j.cub.2017.06.056)
  graph <- delete.edges(graph, which(E(graph)$weight <0.30))
  id_str.graph <- paste0(id_str, ".graph")
  assign(id_str.graph, graph)
  
  ## Robustness
  # cohesion <- cohesion(graph)
  
  ## Communities detection ---------------------------------------------------
  # greedy method (hierarchical, fast method)
  graph.clusters = leading.eigenvector.community(graph, options = list(maxiter = 100000000))
  id_str.graph.clusters <- paste0(id_str, ".graph.clusters")
  assign(id_str.graph.clusters, graph.clusters)
  
  posXY$Community <- graph.clusters$membership
  
  # Get the size of each community
  community_sizes <- sizes(graph.clusters)
  
  # Identify "big" communities (more than 20 members)
  big_communities <- community_sizes[community_sizes > 20]
  
  # Count the number of "big" communities
  number_of_big_communities <- length(big_communities)
  id_str.big_communities <- paste0(id_str, ".big_communities")
  assign(id_str.big_communities, number_of_big_communities)
  
  # Extract big communities
  big_communities <- as.numeric(names(big_communities))
  posXY.big_communities <- posXY %>% filter(Community %in% big_communities)
  id_str.posXY.big_communities <- paste0(id_str, ".posXY.big_communities")
  assign(id_str.posXY.big_communities, posXY.big_communities)
  
  
  # Proportion of redcells involved in big communities
  redcells_in_big_communities <- sum(posXY.big_communities$redcell)/nrow(posXY.big_communities)
  id_str.redcells_in_big_communities <- paste0(id_str, ".redcells_in_big_communities")
  assign(id_str.redcells_in_big_communities, redcells_in_big_communities)
  
  
  # Clustering coefficient
  clustcoeff <- transitivity(graph)
  id_str.clustcoeff <- paste0(id_str, ".clustcoeff")
  assign(id_str.clustcoeff, clustcoeff)
  # Global efficiency
  globaleff <- global_efficiency(graph)
  id_str.globaleff <- paste0(id_str, ".globaleff")
  assign(id_str.globaleff, globaleff)
  
  # Network plot ------------------------------------------------------------
  # Make composite palette
  colorz = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colorz.invert = rev(colorz)
  communities.palette.big <- c(colorz, brewer.pal(9,'Set1'),
                               brewer.pal(8,'Set2'),
                               brewer.pal(12,'Set3'),
                               brewer.pal(8, 'Dark2'),
                               brewer.pal(8, 'Accent'),
                               brewer.pal(9,'Pastel1'),
                               colorz.invert, colorz)
  
  communities.guide <- guide_legend(title = element_text("Communities"),
                                    label = FALSE,
                                    keywidth = 0.1)
  
  graph.plt <- ggraph(graph, 
                      layout = as.matrix(posXY)[, c("X", "Y")]) +
    geom_edge_link(aes(colour = weight, alpha = weight))+
    scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
    scale_edge_color_viridis(name = "F. Corr",
                             alpha = 1,
                             begin = 0.3,
                             end = 1,
                             discrete = FALSE,
                             option = "inferno",
                             direction = 1,
                             guide = guide_colourbar(available_aes = "edge_colour")
    )+
    # Memberships and degrees
    geom_node_point(aes(fill = ordered(leading.eigenvector.community(graph, options = list(maxiter = 100000000))$membership),
                        size = degree(graph)),
                    shape = 21)+
    # geom_node_text(aes(label = posXY$Cell), 
    #                colour = "red",
    #                 repel = TRUE,
    #                size = 2.5)+
    geom_node_text(aes(label = ordered(leading.eigenvector.community(graph, options = list(maxiter = 100000000))$membership)),
                   colour = "black",
                   fontface = 1,
                   size = 3)+
    scale_fill_manual(values = communities.palette.big,
                      guide = "none")+
    scale_size_continuous(range = c(5, 12),
                          guide = "none")+
    theme_graph(background = "white",
                plot_margin = margin(5, 5, 5, 5))+
    theme(legend.position = "right",
          legend.margin	= margin(1,1,1,1),
          legend.key.size = unit(0.5, 'cm'))+
    ggtitle(paste0(id_str, "-", final_subdir))+
    scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
  
  # Save graph
  ggsave(plot = graph.plt, file = paste0("~/calcium-clustering/plots/", id_str, "_graph.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  
  ## RFP-redcells only --------------------
  T.RFPt <- t(RFPt)
  nc <- nrow(RFPt)
  cmat.RFPt <- matrix(NA,nc,nc)
  
  pb <- txtProgressBar(min = 0, max = nc, style = 3)
  for (i in 1:nc) {
    for (j in 1:nc) {
      cmat.RFPt[i,j] <- max_CCF(T.RFPt[,i],T.RFPt[,j])
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  ##
  
  # Label cells back with their actual numbers
  rownames(cmat.RFPt) <- rownames(RFPt)
  colnames(cmat.RFPt) <- rownames(RFPt)
  
  graph.RFP <- graph.adjacency(as.matrix(cmat.RFPt), mode = "undirected", weighted = TRUE, diag = FALSE) # using thresholded values
  
  ## Threshold correlation degree. An interval is chosen because the Pearson correlation coeff goes -1 to 1, BUT -1 means anti-correlation.. so one neuron is active when the other isn't)
  ## Set weight threshold (set to 0.30 as per literature: Avitan et al., 2017 http://dx.doi.org/10.1016/j.cub.2017.06.056)
  graph.RFP <- delete.edges(graph.RFP, which(E(graph.RFP)$weight <0.05))
  
  id_str.graph.RFP <- paste0(id_str, ".graph.RFP")
  assign(id_str.graph.RFP, graph.RFP)
  
  ## Robustness
  ## cohesion <- cohesion(graph.RFP)
  
  # Communities detection ---------------------------------------------------
  ## greedy method (hierarchical, fast method)
  graph.clusters.RFP = cluster_leading_eigen(graph.RFP)
  posXY.RFP$Community <- graph.clusters.RFP$membership
  
  # Number of degrees per community?
  
  # test <- posXY.RFP %>% 
  #     group_by(Community) %>%
  #     summarise(Degree = sum(Degree))
  
  ## Clustering coefficient RFP
  clustcoeff.RFP <- transitivity(graph.RFP)
  id_str.clustcoeff.RFP <- paste0(id_str, ".clustcoeff.RFP")
  assign(id_str.clustcoeff.RFP, clustcoeff.RFP)
  
  ## Global efficiency RFP (NaN if there are no connected triples in the graph)
  globaleff.RFP <- global_efficiency(graph.RFP)
  id_str.globaleff.RFP <- paste0(id_str, ".globaleff.RFP")
  assign(id_str.globaleff.RFP, globaleff.RFP)
  
  # Network plots ------------------------------------------------------------
  # Make composite palette
  colorz = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  communities.guide <- guide_legend(title = element_text("Communities"),
                                    label = FALSE,
                                    keywidth = 0.1)
  
  graph.RFP.plt <- ggraph(graph.RFP, 
                          layout = as.matrix(posXY.RFP)[, c("X", "Y")]) +
    geom_edge_link(aes(colour = weight, alpha = weight))+
    scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
    scale_edge_color_viridis(name = "F. Corr",
                             alpha = 1,
                             begin = 0.3,
                             end = 1,
                             discrete = FALSE,
                             option = "inferno",
                             direction = 1,
                             guide = guide_colourbar(available_aes = "edge_colour")
    )+
    # Calcium levels and degrees
    geom_node_point(aes(fill = ordered(leading.eigenvector.community(graph.RFP, options = list(maxiter = 1000000000))$membership),
                        size = degree(graph.RFP)),
                    shape = 21)+
    # geom_node_text(aes(label = posXY.RFP$Cell), 
    #                colour = "red",
    #               ID0040   repel = TRUE,
    #                size = 2.5)+
    geom_node_text(aes(label = ordered(leading.eigenvector.community(graph.RFP, options = list(maxiter = 1000000000))$membership)),
                    colour = "black",
                    fontface = 1,
                    size = 3)+
    scale_fill_manual(values = colorz,
                      guide = "none")+
    scale_size_continuous(range = c(5, 12),
                          guide = "none")+
    # scale_shape_manual(values = c("TRUE" = 25, "FALSE" = 21))+
    # scale_colour_manual(values = c("TRUE" = "#fc9272", "FALSE" = "black"))+
    # labs(fill = "Ca")+
    # Hubs
    # annotate("text", x=45, y=20, 
    #           label = "W. thresh. = 0.50")+
    # annotate("text", x=40, y=35,
    #          label = "Cohesion = ")+
    # annotate("text", x=70, y=35,
    #          label = cohesion)+
    # geom_node_point(aes(fill = as.factor(posXY$synchron),
  #                     size = as.factor(posXY$synchron),
  #                     shape = as.factor(posXY$synchron)))+
  # scale_size_manual(values = g.sizes.Sync, name = "Synchronous")+
  # scale_fill_manual(values = g.palette.Sync, name = "Synchronous")+
  # scale_shape_manual(values = g.shapes.Sync, name = "Synchronous")+
  # geom_node_label(aes(label = posXY$Cell), repel = TRUE)+
  
  theme_graph(background = "white",
              plot_margin = margin(5, 5, 5, 5))+
    theme(legend.position = "right",
          legend.margin	= margin(1,1,1,1),
          legend.key.size = unit(0.5, 'cm'), #change legend key size
          # legend.key.height = unit(1, 'pt'), #change legend key height
          # legend.key.width = unit(1, 'pt'), #change legend key width
          # legend.title = element_text(size=5), #change legend title font size
          # legend.text = element_text(size=4),
          # legend.text.align = 0
    )+
    ggtitle(paste0(id_str, " RFP+", final_subdir))+
    scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
  
  ## Histogram count of degrees
  posXY.RFP$Degree <- degree(graph.RFP)
  max.y.degree <- length(RFPcells)
  
  degree.RFP.hist <- gghistogram(posXY.RFP, x = "Degree", y = "..count..",
                                 binwidth = 1)
  ## Mean degree
  degree.mean.RFP <- mean(degree(graph.RFP))
  assign(paste0(id_str, ".degree.mean.RFP"), degree.mean.RFP)

  # PCA  ----
  ## General population
  pca <- prcomp(spksthresholded, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
  scree <- fviz_eig(pca, ncp = 100)
  scree <- ggpar(scree, title = element_blank())
  pca.eigenvalues <- get_eigenvalue(pca)
  assign(paste0(id_str, ".pca.eigenvalues"), pca.eigenvalues)
  
  ## for RFP
  pca.RFP <- prcomp(RFPt, center = TRUE, scale = FALSE, rank. = 10) # change to scale = FALSE if not normalised data
  scree.RFP <- fviz_eig(pca.RFP, ncp = 50)
  scree.RFP <- ggpar(scree.RFP, title = element_blank())
  pca.RFP.eigenvalues <- get_eigenvalue(pca.RFP)
  assign(paste0(id_str, ".pca.RFP.eigenvalues"), pca.RFP.eigenvalues)
  
  # Viz ---------
  gs = c(graph.RFP.plt, degree.RFP.hist,
         RFP.grid, RFPsum2.plt)
  
  lay <- rbind(c(1,1,3,3),
               c(1,1,3,3),
               c(2,5,4,4))
  
  arranged <- grid.arrange(graph.RFP.plt, degree.RFP.hist,
                           RFP.grid, RFPsum2.plt, scree.RFP,
                           layout_matrix = lay)
  
  #Save whole graph + raster + activity plot + coactive cells/time
  ggsave(plot = arranged, file = paste0("~/calcium-clustering/plots/", id_str, "_whole.RFP.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  

  # Connections between normal and RFP+ cells ----
  # graph.btw.plt <- ggraph(graph, 
  #                         layout = as.matrix(posXY)[, c("X", "Y")]) +
  #   # geom_edge_link(aes(colour = weight, alpha = weight))+
  #   # scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
  #   # Calcium levels and degrees
  #   geom_node_point(aes(color = as.factor(posXY$redcell),
  #                       size = as.factor(posXY$redcell),
  #                       fill = posXY$frequency,
  #                       shape = 21))+
  #   geom_node_text(aes(label = posXY$redcell),
  #                   repel = TRUE,
  #                  size = 2.5)+
  #   scale_size_manual(values = c("0" = 5,
  #                                "1" = 8),
  #                     guide = "none")+
  #   theme_graph(background = "white",
  #               plot_margin = margin(5, 5, 5, 5))+
  #   theme(legend.position = "right",
  #         legend.margin	= margin(1,1,1,1),
  #         legend.key.size = unit(0.5, 'cm'))+
  #   ggtitle(paste0(id_str, " RFP+ highlighted", final_subdir))+
  #   scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
  # 
  # # Save graph
  # ggsave(plot = graph.btw.plt, file = paste0("~/calcium-clustering/plots/", id_str, "_graph.btw.plt.png"), 
  #        device = "png",  bg = "white",
  #        width = 20, height = 15, units = "cm", dpi = 320,
  #        scale = 2)
  
  ## Histogram count of degrees
  posXY$Degree <- degree(graph)
  max.y.degree <- length(nrow(spks))
  
  degree.hist <- gghistogram(posXY, x = "Degree", y = "..count..",
                                 binwidth = 1)
  ## Mean degree
  degree.mean <- mean(degree(graph))
  assign(paste0(id_str, ".degree.mean"), degree.mean)
  
  # Viz
  gs = c(graph.plt, degree.hist,
         grid, spksSUM2.plt)
  
  lay <- rbind(c(1,1,3,3),
               c(1,1,3,3),
               c(2,5,4,4))
  
  arranged <- grid.arrange(graph.plt, degree.hist,
                           grid, spksSUM2.plt, scree,
                           layout_matrix = lay)
  
  #Save whole graph + raster + activity plot + coactive cells/time
  ggsave(plot = arranged, file = paste0("~/calcium-clustering/plots/", id_str, "_whole.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
  
  # PSD analysis for acquisition rate of 2sec/vol--------------------------------------
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
  assign(paste0(id_str, ".psd.mean"), psd.mean)

  # RFP only
  fft.RFP <- apply(RFP, 1, fft)

  # Calculate power spectrum for each time series
  psd.RFP <- abs(fft.RFP)^2/ncol(RFP) # normalized by length of acquisition

  psd.RFP <- round(psd.RFP, digits = 1)
  psd.RFP.melt <- as.data.frame(psd.RFP)
  psd.RFP.melt$frequency <- freq
  psd.RFP.melt <- melt(psd.RFP.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")

  psd.RFP.mean <- summarise(psd.RFP.melt, "PSD mean" = mean(PSD), .by = "frequency")
  assign(paste0(id_str, ".psd.RFP.mean"), psd.RFP.mean)
  
  
  
  
  assign(paste0(id_str, "_posXY"), posXY)
  
  
}
close(pb_main)