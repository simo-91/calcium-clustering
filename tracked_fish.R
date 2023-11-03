AKT1.df <- data.frame(
  Fish_no = rep("fish1", times = 4),
  Age = c("4dpf", "5dpf", "6dpf", "8dpf"),
  Mean_Ca = c(mean(ID0152_mean_Ca), 
              mean(ID0161_mean_Ca), 
              mean(ID0170_mean_Ca), 
              mean(ID0178_mean_Ca)),
  Events_min = c(ID0152_frequency, 
                 ID0161_frequency, 
                 ID0170_frequency, 
                 ID0178_frequency),
  Clust_Coeff = c(ID0152.clustcoeff, 
                  ID0161.clustcoeff, 
                  ID0170.clustcoeff, 
                  ID0178.clustcoeff),
  Global_Eff = c(ID0152.globaleff, 
                 ID0161.globaleff, 
                 ID0170.globaleff, 
                 ID0178.globaleff)
)


# Plots
# Mean_Ca
fish1.AKT1_mean_Ca.plt <- ggline(AKT1.df, x = "Age", y = "Mean_Ca", 
                                 add = "line",  # Add a line
                                 type = "p",    # Add points
                                 palette = "jco") + # Color palette
                                  ggtitle("Mean Ca vs. Age") + # Add a title
                                  xlab("Age (days post-fertilization)") + # X-axis label
                                  ylab("Mean Calcium Level") # Y-axis label

# Frequency events/min
fish1.AKT1_freq.plt <- ggline(AKT1.df, x = "Age", y = "Events_min", 
                          add = "line",  # Add a line
                          type = "p",    # Add points
                          palette = "jco") + # Color palette
                          ggtitle("Events/min vs. Age") + # Add a title
                          xlab("Age (days post-fertilization)") + # X-axis label
                          ylab("mean Events/min") # Y-axis label

# Clustering coefficient
fish1.AKT1_clustcoeff.plt <- ggline(AKT1.df, x = "Age", y = "Clust_Coeff", 
                                    add = "line",  # Add a line
                                    type = "p",    # Add points
                                    palette = "jco") + # Color palette
                                    ggtitle("Clustering Coefficient vs. Age") + # Add a title
                                    xlab("Age (days post-fertilization)") + # X-axis label
                                    ylab("C(g)") # Y-axis label

# Global efficiency
fish1.AKT1_globaleff.plt <- ggline(AKT1.df, x = "Age", y = "Global_Eff", 
                                   add = "line",  # Add a line
                                   type = "p",    # Add points
                                   palette = "jco") + # Color palette
                                    ggtitle("Global Efficiency vs. Age") + # Add a title
                                    xlab("Age (days post-fertilization)") + # X-axis label
                                    ylab("G(g)") # Y-axis label
