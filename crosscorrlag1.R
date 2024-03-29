```{r  CROSS-COVARIANCE MATRIX CONSIDERING ±1 LAG AND TAKING ONLY THE HIGHEST VALUE}
# 
# e.g. Autocorrelations of series ‘X’, by lag
# 
# -1     0     1 
# 0.174 0.786 0.161 
# will take 0.786 as corr coefficient
##########################################
# 
# # Create empty matrix that will host corr coefficients. 
# tspks <- t(spks)
# 
# nc <- nrow(spks)
# cmat <- matrix(NA,nc,nc)
# 
```



```{r max_CCF}
# Function to find max CCF between a and b in ±1 lag range
max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, lag.max = 1)
  cor = d$acf[,,1]
  return(max(cor))
} 

# 
# pb <- txtProgressBar(min = 0, max = nc, style = 3)
# # Actual crosscorrelation; loop for every combination of column pairs (one column - one curve)
# for (i in 1:nc) {
#   for (j in i:nc) {
#     cmat[i,j] <- max_CCF(tspks[,i],tspks[,j])
#     setTxtProgressBar(pb, i)
#   }
# }
# close(pb)
# 
# colnames(cmat) <- colnames(tspks)
# rownames(cmat) <- colnames(tspks)
# 
# cmat[is.na(cmat)] <- 0 #(Temporary fix) NaN replaced with 0. This happens when manually adding a ROI on Suite2p mistakenly results in a constant time-series of zeroes (hence ccf() tries to divide by 0)
# 
# cmat[lower.tri(cmat)] <- t(cmat)[lower.tri(cmat)] 
# 
# 
# 
# 
```
```{r Display the resultant heatmap matrix}
library(gplots)
heatmap.2(as.matrix(cmat.RFPt)), dendrogram = "none",
                                 trace = "none", density.info =  "none", keysize = 0.25,
          cexRow = 0.1, cexCol=0.1, labRow = "none", labCol = "none", main = NULL,
          xlab = NULL,
          ylab = NULL, na.rm = TRUE)
```




```{r RFP}
# RFP ---------------------------------------------------------------------
# Create empty matrix that will host corr coefficents. 
tRFP <- t(RFP)

nc <- nrow(RFP)
cmat.RFP <- matrix(NA,nc,nc)


# Actual crosscorrelation; loop for every combination of column pairs (one column - one curve)
for (i in 1:nc) {
  for (j in 1:nc) {
    cmat.RFP[i,j] <- max_CCF(tRFP[,i],tRFP[,j])
  }
}
```




```{r allcells Using thresholded matrix}
# Using thresholded values ------------------------------------------------
# # Function to find max CCF between a and b in ±1 lag range
max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, lag.max = 1)
  cor = d$acf[,,1]
  return(max(cor))
} 
# Create empty matrix that will host corr coefficents. 
T.allcellst <- t(spksthresholded)

nc <- nrow(spksthresholded)
cmat.allcellst <- matrix(NA,nc,nc)


pb <- txtProgressBar(min = 0, max = nc, style = 3)
for (i in 1:nc) {
  for (j in 1:nc) {
    cmat.allcellst[i,j] <- max_CCF(T.allcellst[,i],T.allcellst[,j])
    setTxtProgressBar(pb, i)
  }
}
close(pb)
```



```{r RFP Using thresholded matrix}
# Using thresholded values ------------------------------------------------
# # Function to find max CCF between a and b in ±1 lag range
max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, lag.max = 1)
  cor = d$acf[,,1]
  return(max(cor))
} 
# Create empty matrix that will host corr coefficents. 
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
```