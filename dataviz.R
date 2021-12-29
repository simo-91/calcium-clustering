#  Data viz

#  percentage of active cells over time
ID0025.spksSUM.plt <- ggplot(spksSUM, aes(Time, Perc))+
  geom_line()+ 
  theme_pubr()

# # Plot total calcium activity/time --------------------------------------
ID0025.spksSUM2.plt <- ggplot(spksSUM2, aes(Time, Mean))+
  geom_line()+ 
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
ID0025.spksSUM2.ylim <- layer_scales(ID0025.spksSUM2.plt)$y$get_limits()



# RFP isolate------------------------------------------------------------
ID0025_RFPsum.plt <- ggplot(RFPsum, aes(Time, Perc))+
  geom_line()+
  theme_pubr()

ID0025.RFPsum2.plt <- ggplot(RFPsum2, aes(Time, `colSums(RFP)`))+
  geom_line()+
  theme_pubr()+
  geom_smooth()+
  ylab("Ca2+")+
  ylim(0, NA)
ID0025.RFPsum2.ylim <- layer_scales(ID0025.RFPsum2.plt)$y$get_limits()
