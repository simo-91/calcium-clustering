# Clustering coefficient comparison
# in RFP cells
HRASV12.clustcoeff.df.RFP <- c(ID0031.clustcoeff.RFP,ID0032.clustcoeff.RFP,
                            ID0033.clustcoeff.RFP,ID0034.clustcoeff.RFP,
                            ID0036.clustcoeff.RFP,ID0037.clustcoeff.RFP,
                            ID0038.clustcoeff.RFP)
HRASV12.clustcoeff.df.RFP[is.nan(HRASV12.clustcoeff.df.RFP)] <- 0


CTRL.clustcoeff.df.RFP <- c(ID0040.clustcoeff.RFP,ID0041.clustcoeff.RFP,
                            ID0042.clustcoeff.RFP,ID0043.clustcoeff.RFP,
                            ID0044.clustcoeff.RFP,ID0045.clustcoeff.RFP,
                            ID0046.clustcoeff.RFP)

clustcoeff.df.RFP <- as.data.frame((cbind(CTRL.clustcoeff.df.RFP, HRASV12.clustcoeff.df.RFP)))
clustcoeff.df.RFP <- melt(clustcoeff.df.RFP)
colnames(clustcoeff.df.RFP) <- c("Condition", "Clustering Coefficient")
levels(clustcoeff.df.RFP$Condition) <- c("CTRL RFP", "HRASV12 RFP")

clustcoeff.plot.RFP <- ggboxplot(clustcoeff.df.RFP, x = "Condition", y = "Clustering Coefficient",
                            color = "Condition", add = "jitter", palette = c("CTRL RFP" = "#fdae6b", "HRASV12 RFP" = "#ca0020"))+
                            geom_signif(comparisons = list(c("CTRL RFP", "HRASV12 RFP")), map_signif_level = TRUE)+
                            theme(axis.title.x = element_blank())


# in all cells

HRASV12.clustcoeff.df <- c(ID0031.clustcoeff,ID0032.clustcoeff,
                            ID0033.clustcoeff,ID0034.clustcoeff,
                            ID0036.clustcoeff,ID0037.clustcoeff,
                            ID0038.clustcoeff)
HRASV12.clustcoeff.df[is.nan(HRASV12.clustcoeff.df)] <- 0


CTRL.clustcoeff.df <- c(ID0040.clustcoeff,ID0041.clustcoeff,
                         ID0042.clustcoeff,ID0043.clustcoeff,
                         ID0044.clustcoeff,ID0045.clustcoeff,
                         ID0046.clustcoeff)

clustcoeff.df <- as.data.frame((cbind(CTRL.clustcoeff.df, HRASV12.clustcoeff.df)))
clustcoeff.df <- melt(clustcoeff.df)
colnames(clustcoeff.df) <- c("Condition", "Clustering Coefficient")
levels(clustcoeff.df$Condition) <- c("CTRL", "HRASV12")

clustcoeff.plot <- ggboxplot(clustcoeff.df, x = "Condition", y = "Clustering Coefficient",
                        color = "Condition", add = "jitter", palette = c("CTRL" = "#3182bd", "HRASV12" = "#ef8a62"))+
                        # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
                        geom_signif(comparisons = list(c("CTRL", "HRASV12")), map_signif_level = TRUE)+
                        theme(axis.title.x = element_blank())


# RFP-only vs general population (clustd coeff)
RFPdiff.HRASV12.clustcoeff.df <- rbind(clustcoeff.df[8:14,], clustcoeff.df.RFP[8:14,])
RFPdiff.HRASV12.clustcoeff.plot <- ggboxplot(RFPdiff.HRASV12.clustcoeff.df, x = "Condition", y = "Clustering Coefficient",
                                      color = "Condition", add = "jitter", palette = c("HRASV12" = "#ef8a62", "HRASV12 RFP" = "#ca0020"))+
                                      geom_signif(comparisons = list(c("HRASV12", "HRASV12 RFP")), map_signif_level = TRUE)+
                                      theme(axis.title.x = element_blank())


RFPdiff.CTRL.clustcoeff.df <- rbind(clustcoeff.df[1:7,], clustcoeff.df.RFP[1:7,])
RFPdiff.CTRL.clustcoeff.plot <- ggboxplot(RFPdiff.CTRL.clustcoeff.df, x = "Condition", y = "Clustering Coefficient",
                                             color = "Condition", add = "jitter", palette = c("CTRL" = "#3182bd", "CTRL RFP" = "#fdae6b"))+
                                              geom_signif(comparisons = list(c("CTRL", "CTRL RFP")), map_signif_level = TRUE)+
                                              theme(axis.title.x = element_blank())

# Viz clustering coefficients
palette.conditions <- c("CTRL" = "#3182bd", "CTRL RFP" = "#fdae6b",
                        "HRASV12" = "#4daf4a", "HRASV12 RFP" = "#ca0020")


clustcoeff.global.eff <- full_join(clustcoeff.df, clustcoeff.df.RFP)
clustcoeff.RFP.plot <- ggboxplot(clustcoeff.global.eff %>% filter(Condition == "CTRL RFP" | Condition == "HRASV12 RFP"),
                                 x = "Condition", y = "Clustering Coefficient",
                                 color = "Condition", add = "jitter", palette = palette.conditions,
                                 ggtheme = labs_pubr(base_size = 12))+
                                theme(legend.position = "none",
                                      axis.title.x = element_blank(),
                                      legend.title = element_blank())+
                                geom_signif(comparisons = list(c("CTRL RFP", "HRASV12 RFP")), map_signif_level = TRUE)

clustcoeff.gen.plot <- ggboxplot(clustcoeff.global.eff %>% filter(Condition == "CTRL" | Condition == "HRASV12"),
                                 x = "Condition", y = "Clustering Coefficient",
                                 color = "Condition", add = "jitter", palette = palette.conditions,
                                 ggtheme = labs_pubr(base_size = 12))+
                                  theme(legend.position = "none",
                                        axis.title.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        legend.title = element_blank())+
                                  geom_signif(comparisons = list(c("CTRL", "HRASV12")), map_signif_level = TRUE)

clustcoeff.RFPdiff.CTRL.plot <- ggboxplot(clustcoeff.global.eff %>% filter(Condition == "CTRL" | Condition == "CTRL RFP"),
                                 x = "Condition", y = "Clustering Coefficient",
                                 color = "Condition", add = "jitter", palette = palette.conditions,
                                 ggtheme = labs_pubr(base_size = 12))+
                                  theme(legend.position = "none",
                                        axis.title.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        legend.title = element_blank())+
                                  geom_signif(comparisons = list(c("CTRL", "CTRL RFP")), map_signif_level = TRUE)


clustcoeff.RFPdiff.HRASV12.plot <- ggboxplot(clustcoeff.global.eff %>% filter(Condition == "HRASV12" | Condition == "HRASV12 RFP"),
                                          x = "Condition", y = "Clustering Coefficient",
                                          color = "Condition", add = "jitter", palette = palette.conditions,
                                          ggtheme = labs_pubr(base_size = 12))+
                                          theme(legend.position = "none",
                                                axis.title.x = element_blank(),
                                                legend.title = element_blank())+
                                          geom_signif(comparisons = list(c("HRASV12", "HRASV12 RFP")), map_signif_level = TRUE)

clustcoeff.all.plot <- ggarrange(clustcoeff.RFP.plot, NULL, clustcoeff.gen.plot, clustcoeff.RFPdiff.HRASV12.plot, NULL, clustcoeff.RFPdiff.CTRL.plot,
                          common.legend = TRUE, legend = "bottom", labels = c("A","B"," ","C", "", "D"), widths = c(1, 0.05, 1))










# Global efficiency comparison

clustcoeff.df$`Global Efficiency` <- rbind(ID0040.globaleff,ID0041.globaleff,
                                           ID0042.globaleff,ID0043.globaleff,
                                           ID0044.globaleff,ID0045.globaleff,
                                           ID0046.globaleff,
                                           ID0031.globaleff, ID0032.globaleff,
                                           ID0033.globaleff, ID0034.globaleff,
                                           ID0036.globaleff, ID0037.globaleff,
                                           ID0038.globaleff)


global.efficiency.plot <- ggboxplot(clustcoeff.df, x = "Condition", y = "Global Efficiency",
                              color = "Condition", add = "jitter")+
                              # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
                              geom_signif(comparisons = list(c("CTRL", "HRASV12")), map_signif_level = TRUE)+
                              theme(axis.title.x = element_blank(),
                                    legend.position = "none")


clustcoeff.df.RFP$`Global Efficiency` <- rbind(ID0040.globaleff.RFP,ID0041.globaleff.RFP,
                                           ID0042.globaleff.RFP,ID0043.globaleff.RFP,
                                           ID0044.globaleff.RFP,ID0045.globaleff.RFP,
                                           ID0046.globaleff.RFP,
                                           ID0031.globaleff.RFP, ID0032.globaleff.RFP,
                                           ID0033.globaleff.RFP, ID0034.globaleff.RFP,
                                           ID0036.globaleff.RFP, ID0037.globaleff.RFP,
                                           ID0038.globaleff.RFP)

global.efficiency.RFP.plot <- ggboxplot(clustcoeff.df.RFP, x = "Condition", y = "Global Efficiency",
                                    color = "Condition", add = "jitter")+
                                    # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
                                    geom_signif(comparisons = list(c("CTRL RFP", "HRASV12 RFP")), map_signif_level = TRUE)+
                                    theme(axis.title.x = element_blank(),
                                          legend.position = "none")

# RFP-only vs general population (Global eff)
RFPdiff.CTRL.globaleff <- rbind(ID0040.globaleff,ID0041.globaleff,
                                ID0042.globaleff,ID0043.globaleff,
                                ID0044.globaleff,ID0045.globaleff,
                                ID0046.globaleff,
                                ID0040.globaleff.RFP,ID0041.globaleff.RFP,
                                ID0042.globaleff.RFP,ID0043.globaleff.RFP,
                                ID0044.globaleff.RFP,ID0045.globaleff.RFP,
                                ID0046.globaleff.RFP)

RFPdiff.CTRL.globaleff.plot <- ggboxplot(RFPdiff.CTRL.globaleff, x = "Condition", y = "Global Efficiency",
                                        color = "Condition", add = "jitter", 
                                        palette = c("CTRL" = "#abdda4", "CTRL RFP" = "#fc8d59"))+
                                      # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
                                      geom_signif(comparisons = list(c("CTRL", "CTRL RFP")), map_signif_level = TRUE)+
                                      theme(axis.title.x = element_blank(),
                                            legend.position = "none")

# HRASV12
RFPdiff.HRASV12.globaleff <- rbind(ID0031.globaleff, ID0032.globaleff,
                                   ID0033.globaleff, ID0034.globaleff,
                                   ID0036.globaleff, ID0037.globaleff,
                                   ID0038.globaleff,
                                   ID0031.globaleff.RFP, ID0032.globaleff.RFP,
                                   ID0033.globaleff.RFP, ID0034.globaleff.RFP,
                                   ID0036.globaleff.RFP, ID0037.globaleff.RFP,
                                   ID0038.globaleff.RFP)

RFPdiff.HRASV12.globaleff.plot <- ggboxplot(RFPdiff.HRASV12.globaleff, x = "Condition", y = "Global Efficiency",
                                         color = "Condition", add = "jitter", 
                                         palette = c("HRASV12" = "#ef8a62", "HRASV12 RFP" = "#ca0020"))+
                                       # stat_compare_means(label = "p.format", label.y = 1, label.x.npc = "centre", paired = FALSE)+
                                          geom_signif(comparisons = list(c("HRASV12", "HRASV12 RFP")), map_signif_level = TRUE)+
                                          theme(axis.title.x = element_blank(),
                                                legend.position = "none")

