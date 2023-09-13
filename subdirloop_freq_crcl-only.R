library(pacman)
p_load(utils, dplyr, tidyverse, ggplot2, plotly, tidyr, reshape2, factoextra, ggdendro,
       grid, RcppCNPy, cowplot, ggpubr, mmand, rstudioapi, reticulate, tcltk, ggfortify,
       ggpubr, factoextra, parallel, ggpattern, ggsignif, car, igraph, ggraph)



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



id_num <- 0138 #starting ID number

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
  
  # # Regression from 1sec/vol to 2sec/vol time resolution -----
  # regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
  # for (i in 1:(ncol(spks)/2)) {
  #   regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
  # }
  # rownames(regr_spks) <- rownames(spks)
  # spks <- regr_spks
  # # -----
  
  spks[is.na(spks)] <- 0
  spks <- spks[,50:ncol(spks)] #need to clean it from first 0 and select best window
  spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x)))) # Normalize each cell
  spks <- spks[positivesPLUSone,]  #select only positives (real cells)
  rownames(spks) <- positives #fix rownames with actual cells numbers
  spks[is.na(spks)] <- 0 #NAs replaced with 0

  
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
  
  assign(paste0(id_str, "_frequency"), frequency)

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
  ## Average activity PER CELL (deconvolved peaks) ---------------------------
  posXY$Mean <- rowMeans(spks)
  mean_Ca <- rowMeans(spks)
  assign(paste0(id_str, "_mean_Ca"), mean_Ca)
  
  # General graph ----------------------------
  graph <- graph.adjacency(as.matrix(cmat.allcellst), mode = "undirected", weighted = TRUE, diag = FALSE) # using thresholded values
  graph <- delete.edges(graph, which(E(graph)$weight <0.30))
  
  graph.freqcrcl.plt <- ggraph(graph, 
                          layout = as.matrix(posXY)[, c("X", "Y")]) +
    # geom_edge_link(aes(colour = weight, alpha = weight))+
    # scale_edge_alpha_continuous(range = c(0.1, 1), guide = "none")+
    # Calcium levels and degrees
    geom_node_point(aes(color = as.factor(posXY$redcell),
                        size = as.factor(posXY$redcell),
                        fill = posXY$frequency,
                        shape = as.factor(posXY$redcell)))+
    scale_fill_viridis(option = "C",
                       direction = 1,
                       name = "Events/min")+
    scale_color_manual(values = c("0" = alpha("transparent"),
                                  "1" = "red"),
                       name = "Preneoplastic")+
    scale_shape_manual(values = c("0" = 21,
                                  "1" = 25),
                       name = "Preneoplastic")+
    scale_size_manual(values = c("0" = 5,
                                 "1" = 14),
                      guide = "none")+
    theme_graph(background = "white",
                plot_margin = margin(5, 5, 5, 5))+
    theme(legend.position = "right",
          legend.margin	= margin(1,1,1,1),
          legend.key.size = unit(0.5, 'cm'))+
    ggtitle(paste0(id_str, " freq circles ", final_subdir))+
    scale_y_reverse() #this is because in images/movies y axis in coordinates is reversed
  
  # Save graph
  ggsave(plot = graph.freqcrcl.plt, file = paste0("~/calcium-clustering/plots/", id_str, "_graph.freqcrcl.plt.png"), 
         device = "png",  bg = "white",
         width = 20, height = 15, units = "cm", dpi = 320,
         scale = 2)
}