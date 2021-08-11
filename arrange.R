layoutgrobs <- cbind(c(1,2,3), 
                     c(4,5,6),
                     c(7,8,9))

library(gridExtra)
grid.arrange(AKT1hindbrain1.grid, AKT1hindbrain2.grid, AKT1hindbrain22.grid, 
             AKT1hindbrain3.grid, AKT1midbrain.grid, AKT1midbrain3.grid, 
             CTRLhindbrain2.grid, CTRLhindbrain3.grid, CTRLmidbrain3.grid, ncol = 3)
             layout_matrix = cbind(c(1,1,1), c(2,3,4)))


ggarrange(AKT1hindbrain1.grid, AKT1hindbrain2.grid, AKT1hindbrain22.grid, 
          AKT1hindbrain3.grid, AKT1midbrain.grid, AKT1midbrain3.grid, 
          CTRLhindbrain2.grid, CTRLhindbrain3.grid, CTRLmidbrain3.grid,
          ncol = 3, nrow = 3)

# 30 min movies
ggarrange(CTRL4dpfhi1.grid, CTRL5dpfhi1.grid, AKT14dpfhi2.grid, AKT14dpflo2.grid)



# Calculate Inter-spike Intervals?
