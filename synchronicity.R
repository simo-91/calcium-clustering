# Synchronicity
sync <- apply(spksthresholded, MARGIN = 2, FUN = function(x) sum(x)/nrow(spksthresholded))
# For 2 timepoints (2sec)?
for (j in 1:ncol(spksthresholded)) {
  sync[j] <- sum(spksthresholded[,j], spksthresholded[,j+1]) / nrow(spksthresholded)
}

cmat.hi <- cmat
cmat.hi[lower.tri(cmat, diag=TRUE)] = NA  #Prepare to drop duplicates and meaningless information

sync.cells <- which(cmat.hi > 0.60, arr.ind = T) # the result of the which function is now in rows & columns
sync.cells <- sort(unique(sync.cells[,1]))
sync.raster <- dfpeaks[, sync.cells]
sync.raster$time <- 1:nrow(sync.raster)
sync.raster.melt <- melt(sync.raster, id = "time")
