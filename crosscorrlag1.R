# CROSS-COVARIANCE MATRIX CONSIDERING ±1 LAG AND TAKING ONLY THE HIGHEST VALUE
# 
# e.g. Autocorrelations of series ‘X’, by lag
# 
# -1     0     1 
# 0.174 0.786 0.161 
# will take 0.786 as corr coefficient
##########################################

# Create empty matrix that will host corr coefficents. 
tspks <- t(spks)

nc <- nrow(spks)
cmat <- matrix(NA,nc,nc)

# Function to find max CCF between a and b in ±1 lag range
max_CCF<- function(a,b)
  {
  d <- ccf(a, b, plot = FALSE, lag.max = 1)
  cor = d$acf[,,1]
  return(max(cor))
} 


# Actual crosscorrelation; loop for every combination of column pairs (one column - one curve)
for (i in 1:nc) {
  for (j in 1:nc) {
    cmat[i,j] <- max_CCF(tspks[,i],tspks[,j])
  }
}
cmat[is.na(cmat)] <- 0 #NaN replaced with 0. This happens when two timeseries issue originates from Suite2p, as it doesn't generate the  deconvolved signal for a manually added ROI


# Display the resultant matrix
library(gplots)
Colors=c("blue","yellow","red")
Colors=colorRampPalette(Colors)(100)
heatmap.2(as.matrix(cmat), dendrogram = "none",
          col = Colors, trace = "none", density.info =  "none", keysize = 0.50,
          cexRow = 0.5, cexCol=0.5, key.par = list(cex=0.4), na.rm = TRUE)



## Synchronicity
# Create empty matrix that will host corr coefficents. 
tRFP <- t(RFP)

nc <- nrow(RFP)
cmat.RFP <- matrix(NA,nc,nc)

# Function to find max CCF between a and b in ±1 lag range
max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, lag.max = 1)
  cor = d$acf[,,1]
  return(max(cor))
} 


# Actual crosscorrelation; loop for every combination of column pairs (one column - one curve)
for (i in 1:nc) {
  for (j in 1:nc) {
    cmat.RFP[i,j] <- max_CCF(tRFP[,i],tRFP[,j])
  }
}

