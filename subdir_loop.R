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
  
  # Navigate to the subdirectory and load the necessary files
  setwd((dirname(subdir)))
  np <- import("numpy")
  stat <- np$load("stat.npy", allow_pickle = TRUE)
  redcell <- np$load("redcell.npy", allow_pickle = TRUE)
  iscell <- as.data.frame(np$load("iscell.npy", allow_pickle = TRUE))
  spks <- as.data.frame(np$load("spks.npy", allow_pickle = TRUE))
  
  # Process the data and save the output files
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
  regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
  for (i in 1:(ncol(spks)/2)) {
    regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
  }
  rownames(regr_spks) <- rownames(spks)
  spks <- regr_spks
  spks[is.na(spks)] <- 0
  spks <- spks[,50:ncol(spks)]
  spks <- t(apply(spks, 1, function(x) (x - min(x))/(max(x)-min(x))))
  spks <- spks[positivesPLUSone,]
  rownames(spks) <- positives
  spks[is.na(spks)] <- 0
  saveRDS(spks, file = paste0(id_str, "_spks.rds"))
  write.csv(spks, file = paste0(id_str, "_spks.csv"))
  
}
