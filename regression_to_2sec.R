# Regression from 1sec/vol to 2sec/vol time resolution
regr_spks <- matrix(0, nrow = nrow(spks), ncol = ncol(spks)/2)
for (i in 1:(ncol(spks)/2)) {
  regr_spks[, i] <- rowMeans(spks[, (2*i - 1):(2*i)])
}

rownames(regr_spks) <- rownames(spks)
