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

clustcoeff.df.RFP <- as.data.frame((cbind(CTRL.clustcoeff.RFP, HRASV12.clustcoeff.RFP)))
clustcoeff.df.RFP <- melt(clustcoeff.df.RFP)
colnames(clustcoeff.df.RFP) <- c("Condition", "Clustering Coefficient")
levels(clustcoeff.df.RFP$Condition) <- c("CTRL RFP", "HRASV12 RFP")

ggboxplot(clustcoeff.df.RFP, x = "Condition", y = "Clustering Coefficient",
          color = "Condition", add = "jitter")+
          # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
          geom_signif(comparisons = list(c("CTRL RFP", "HRASV12 RFP")), map_signif_level = TRUE)


# Global efficiency comparison