# Clustering coefficient comparison

HRASV12.clustcoeff.RFP <- c(ID0031.clustcoeff.RFP,ID0032.clustcoeff.RFP,
                            ID0033.clustcoeff.RFP,ID0034.clustcoeff.RFP,
                            ID0036.clustcoeff.RFP,ID0037.clustcoeff.RFP,
                            ID0038.clustcoeff.RFP)
HRASV12.clustcoeff.RFP[is.nan(HRASV12.clustcoeff.RFP)] <- 0


CTRL.clustcoeff.RFP <- c(ID0040.clustcoeff.RFP,ID0041.clustcoeff.RFP,
                            ID0042.clustcoeff.RFP,ID0043.clustcoeff.RFP,
                            ID0044.clustcoeff.RFP,ID0045.clustcoeff.RFP,
                            ID0046.clustcoeff.RFP)

clustcoeff.df <- as.data.frame((cbind(CTRL.clustcoeff.RFP, HRASV12.clustcoeff.RFP)))
clustcoeff.df <- melt(clustcoeff.df)
colnames(clustcoeff.df) <- c("Condition", "Clustering Coefficient")


ggboxplot(clustcoeff.df, x = "Condition", y = "Clustering Coefficient",
          color = "Condition", add = "jitter")+
  stat_compare_means()


# Global efficiency comparison