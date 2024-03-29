layoutgrobs <- cbind(c(1,2,3), 
                     c(4,5,6),
                     c(7,8,9))

library(gridExtra)
grid.arrange(AKT1hindbrain1.grid, AKT1hindbrain2.grid, AKT1hindbrain22.grid, 
             AKT1hindbrain3.grid, AKT1midbrain.grid, AKT1midbrain3.grid, 
             CTRLhindbrain2.grid, CTRLhindbrain3.grid, CTRLmidbrain3.grid, ncol = 3)
             layout_matrix = cbind(c(1,1,1), c(2,3,4))


ggarrange(AKT1hindbrain1.grid, AKT1hindbrain2.grid, AKT1hindbrain22.grid, 
          AKT1hindbrain3.grid, AKT1midbrain1.grid, AKT1midbrain3.grid, 
          CTRLhindbrain2.grid, CTRLhindbrain3.grid, CTRLmidbrain3.grid,
          ncol = 3, nrow = 3)

# 30 min movies
ggarrange(CTRL4dpfhi1.grid, CTRL5dpfhi1.grid, AKT14dpfhi2.grid, AKT14dpflo2.grid)


# Comparing all cells against highly corr ones to highlight oscillations
AKT1hindbrain1_vs_sync <- ggarrange(AKT1hindbrain1.grid, sync.AKT1hindbrain1.grid, AKT1hindbrain1.POS.aveF, AKT1hindbrain1.RFP.aveF, AKT1hindbrain1.RFP.grid, AKT1_hindbrain_1.graph)

AKT1midbrain1_vs_sync <- ggarrange(AKT1midbrain1.grid, sync.AKT1midbrain1.grid, AKT1midbrain1.POS.aveF, AKT1midbrain1.RFP.aveF, AKT1midbrain1.RFP.grid, AKT1_midbrain1.graph)

AKT1hindbrain2_vs_sync <- ggarrange(AKT1hindbrain2.grid, sync.AKT1hindbrain2.grid, AKT1hindbrain2.POS.aveF, AKT1hindbrain2.RFP.aveF, AKT1hindbrain2.RFP.grid, AKT1_hindbrain_2.graph)

AKT1hindbrain2b_vs_sync <- ggarrange(AKT1hindbrain2b.grid, sync.AKT1hindbrain2b.grid, AKT1hindbrain2b.POS.aveF, AKT1hindbrain2b.RFP.aveF, AKT1hindbrain2b.RFP.grid, AKT1_hindbrain2b.graph)

AKT1hindbrain3_vs_sync <- ggarrange(AKT1hindbrain3.grid, sync.AKT1hindbrain3.grid, AKT1hindbrain3.POS.aveF, AKT1hindbrain3.RFP.aveF, AKT1hindbrain3.RFP.grid, AKT1_hindbrain3.graph)

AKT1midbrain3_vs_sync <- ggarrange(AKT1midbrain3.grid, sync.AKT1midbrain3.grid, AKT1midbrain3.POS.aveF, AKT1midbrain3.RFP.aveF, AKT1midbrain3.RFP.grid, AKT1_midbrain3.graph)



CTRLhindbrain2_vs_sync <- ggarrange(CTRLhindbrain2.grid, sync.CTRLhindbrain2.grid, CTRLhindbrain2.POS.aveF, CTRLhindbrain2.RFP.aveF, CTRLhindbrain2.RFP.grid, CTRL_hindbrain2.graph)

CTRLhindbrain3_vs_sync <- ggarrange(CTRLhindbrain3.grid, sync.CTRLhindbrain3.grid, CTRLhindbrain3.POS.aveF, CTRLhindbrain3.RFP.aveF, CTRLhindbrain3.RFP.grid, CTRL_hindbrain3.graph)

CTRLmidbrain3_vs_sync <- ggarrange(CTRLmidbrain3.grid, sync.CTRLmidbrain3.grid, CTRLmidbrain3.POS.aveF, CTRLmidbrain3.RFP.aveF, CTRLmidbrain3.RFP.grid, CTRL_midbrain3.graph)


hindbrains <- ggarrange(sync.AKT1hindbrain1.grid,sync.AKT1hindbrain2.grid, sync.AKT1hindbrain3.grid, sync.CTRLhindbrain2.grid, sync.CTRLhindbrain3.grid)

midbrains <- ggarrange(sync.AKT1midbrain1.grid,sync.AKT1midbrain3.grid, sync.CTRLmidbrain3.grid)


# grid structure: (1) raster+ratio (2) sync.raster+ratio (3) (deconvolved)activity/time (4) sync.(deconvolved)activity/time #
# 30min single planes whole brain -----------------------------------------
CTRL4dpfhi1_vs_sync <- ggarrange(CTRL4dpfhi1.grid, sync.CTRL4dpfhi1.grid, CTRL4dpfhi1.POS.aveF) #larva moves too much!
# , CTRL4dpfhi1.RFP.aveF, CTRL4dpfhi1.RFP.grid, CTRL_4dpfhi1.graph)

CTRL4dpflo1_vs_sync <- ggarrange(CTRL4dpflo1.grid, sync.CTRL4dpflo1.grid, CTRL4dpflo1.spksSUM2.plt, CTRL4dpflo1.syncSUM2.plt)

AKT14dpfhi2_vs_sync <- ggarrange(AKT14dpfhi2.grid, sync.AKT14dpfhi2.grid, AKT14dpfhi2.spksSUM2.plt, AKT14dpfhi2.syncSUM2.plt)

AKT14dpflo2_vs_sync <- ggarrange(AKT14dpflo2.grid, sync.AKT14dpflo2.grid, AKT14dpflo2.spksSUM2.plt, AKT14dpflo2.syncSUM2.plt)

# Wholebrains
ggarrange(CTRL4dpflo1.grid, AKT14dpflo2.grid, AKT14dpfhi2.grid,
          CTRL5dpfhi1.grid)

## Compare levels of calcium activity
ggarrange(AKT1hindbrain1.spksSUM2.plt+ylim(0,100), AKT1midbrain1.spksSUM2.plt+ylim(0,100),AKT1hindbrain2.spksSUM2.plt+ylim(0,100),AKT1hindbrain22.spksSUM2.plt+ylim(0,100),
          AKT1hindbrain3.spksSUM2.plt+ylim(0,100), AKT1midbrain3.spksSUM2.plt+ylim(0,100), CTRLhindbrain2.spksSUM2.plt+ylim(0,100), CTRLhindbrain3.spksSUM2.plt+ylim(0,100),
          CTRLmidbrain3.spksSUM2.plt+ylim(0,100), labels = c("AKT1hindbrain1", "AKT1midbrain1", "AKT1hindbrain2", "AKT1hindbrain22", "AKT1hindbrain3", "AKT1midbrain3", "CTRLhindbrain2","CTRLhindbrain3", "CTRLmidbrain3"))


ggarrange(AKT1hindbrain1.RFPsum2.plt+ylim(0,100), AKT1midbrain1.RFPsum2.plt+ylim(0,100),AKT1hindbrain2.RFPsum2.plt+ylim(0,100),AKT1hindbrain22.RFPsum2.plt+ylim(0,100),
          AKT1hindbrain3.RFPsum2.plt+ylim(0,100), AKT1midbrain3.RFPsum2.plt+ylim(0,100), CTRLhindbrain2.RFPsum2.plt+ylim(0,100), CTRLhindbrain3.RFPsum2.plt+ylim(0,100),
          CTRLmidbrain3.RFPsum2.plt+ylim(0,100), labels = c("AKT1hindbrain1", "AKT1midbrain1", "AKT1hindbrain2", "AKT1hindbrain22", "AKT1hindbrain3", "AKT1midbrain3", "CTRLhindbrain2","CTRLhindbrain3", "CTRLmidbrain3"))




ggplot(spksSUM2.all.melt, aes(Time, value))+
  geom_line(aes(color = variable))+ 
  facet_wrap(vars(variable))+
  theme_pubr()

ggplot(RFPsum2.all.melt, aes(Time, `Ca2+`))+
  geom_line(aes(color = Condition))+
  facet_wrap(vars(Condition))+
  theme_pubr()

distributions of calcium levels??

ggarrange(CTRL4dpflo1.grid, CTRL4dpflo1.graph)

ggarrange(CTRL4dpflo1.grid,CTRL4dpflo1.graph)
CTRL5dpfhi1_grid_graph <- ggarrange(CTRL5dpfhi1.grid, CTRL5dpfhi1.graph+labs(title = NULL))
AKT15dpflo2_grid_graph <- ggarrange(AKT15dpflo2.grid, AKT15dpflo2.graph+labs(title = NULL))



CTRL_AKT1_Ca_ave <- ggarrange(CTRL5dpfhi1.spksSUM2.plt, AKT15dpflo2.spksSUM2.plt+
            ylim(CTRL5dpfhi1.spksSUM2.ylim)+
            labs(y = NULL))

ggsave(plot = CTRL5dpfhi1_grid_graph, file = "CTRL5dpfhi1_grid_graph.png", 
       device = "png",  bg = "transparent",
      units = "cm", dpi = 800)


ggarrange(HRAS5dpf_hind2_15min_RFPsum.plt, HRAS5dpf_hind2_15min.graph.plt)
ggarrange(HRAS5dpf_mid2_15min_RFPsum.plt, HRAS5dpf_mid2_15min.graph.plt)
