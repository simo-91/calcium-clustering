# CROSS-COVARIANCE MATRIX CONSIDERING ±1 LAG AND TAKING ONLY THE HIGHEST VALUE
# 
# e.g. Autocorrelations of series ‘X’, by lag
# 
# -1     0     1 
# 0.174 0.786 0.161 
# will take 0.786 as corr coefficient
##########################################

# Create empty matrix that will host corr coefficents
nc <- ncol(calcium)
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
    cmat[i,j] <- max_CCF(calcium[,i],calcium[,j])
  }
}


# Display the resultant matrix
library(gplots)
Colors=c("blue","yellow","red")
Colors=colorRampPalette(Colors)(100)
heatmap.2(as.matrix(cmat), dendrogram = "none",
          col = Colors, trace = "none", density.info =  "none", keysize = 0.50,
          cexRow = 0.5, cexCol=0.5, key.par = list(cex=0.4))
