# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(tRFP, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0009.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0009.scree.RFP <- ggpar(ID0009.scree.RFP, title = element_blank())




#  Normalizing for number of total dimensions?
variance.percentage.mean.CTRL.RFP <- c(sum(ID0040.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0041.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0042.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0043.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0044.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0045.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0046.pca.RFP.eigenvalues[1:3, 2]))

variance.percentage.mean.HRASV12.RFP <- c(sum(ID0031.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0032.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0033.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0034.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0036.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0037.pca.RFP.eigenvalues[1:3, 2]),
                                          sum(ID0038.pca.RFP.eigenvalues[1:3, 2]))
