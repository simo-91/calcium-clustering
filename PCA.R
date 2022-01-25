# libs --------------------------------------------------------------------
library(factoextra)
library(ggpubr)
# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(tRFP, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
ID0009.scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
ID0009.scree.RFP <- ggpar(ID0009.scree.RFP, title = element_blank())
