# libs --------------------------------------------------------------------
library(factoextra)

# PCA ---------------------------------------------------------------------
pca.RFP <- prcomp(tRFP, center = TRUE, scale = FALSE) # change to scale = FALSE if not normalised data
scree.RFP <- fviz_eig(pca.RFP, ncp = 100)
