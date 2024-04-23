```{r background code, include=FALSE}
# AKT1

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



# Unique fish identifiers
unique_fish <- unique(tracked_AKT1.df$Fish_no)

# Loop over each fish to create and arrange plots
for (fish in unique_fish) {
  # Filter data for the current fish
  fish_data <- tracked_AKT1.df[tracked_AKT1.df$Fish_no == fish, ]
  
  # Plot Mean Ca Vs Age
  mean_ca_plot <- ggline(fish_data, x = "Age", y = "Mean_Ca", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Mean Ca vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Mean Calcium Level")
  
  # Plot Events/min vs Age
  events_plot <- ggline(fish_data, x = "Age", y = "Events_min", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Events/min vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Events/min")
  
  # Plot Clustering Coefficient vs Age
  clust_coeff_plot <- ggline(fish_data, x = "Age", y = "Clust_Coeff", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Clustering Coefficient vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Clustering Coefficient")
  
  # Plot Global Efficiency vs Age
  global_eff_plot <- ggline(fish_data, x = "Age", y = "Global_Eff", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Global Efficiency vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Global Efficiency")
  
  # Arrange the four plots into a 2x2 grid and save the combined plot
  combined_plot <- ggarrange(mean_ca_plot, events_plot, clust_coeff_plot, global_eff_plot,
                             ncol = 2, nrow = 2, 
                             labels = c("A", "B", "C", "D"))  # Add labels to the plots if desired
  
  # Save the combined plot to a file
  ggsave(paste0("Combined_Plots_", fish, ".png"), combined_plot, width = 16, height = 12)
  
  # Print the combined plot to the R console (optional)
  print(combined_plot)
}


# Color-blind friendly plots---
# Load the required libraries
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

# Your dataframe 'tracked_AKT1.df' goes here
# ...

# Define a color-blind-friendly palette
color_palette <- brewer.pal(n = length(unique(tracked_AKT1.df$Fish_no)), name = "Dark2")

# Set the color palette in ggplot
scale_color_manual(values = color_palette)

# Plot Mean Ca Vs Age for all fish
mean_ca_plot_all <- ggplot(tracked_AKT1.df, aes(x = Age, y = Mean_Ca, group = Fish_no, color = Fish_no)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "right") +
  ggtitle("Mean Ca vs Age - All Fish") +
  xlab("Age (days post-fertilization)") +
  ylab("Mean Calcium Level")

# Plot Events/min vs Age for all fish
events_plot_all <- ggplot(tracked_AKT1.df, aes(x = Age, y = Events_min, group = Fish_no, color = Fish_no)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "right") +
  ggtitle("Events/min vs Age - All Fish") +
  xlab("Age (days post-fertilization)") +
  ylab("Events/min")

# Plot Clustering Coefficient vs Age for all fish
clust_coeff_plot_all <- ggplot(tracked_AKT1.df, aes(x = Age, y = Clust_Coeff, group = Fish_no, color = Fish_no)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme(legend.position = "right") +
  ggtitle("Clustering Coefficient vs Age - All Fish") +
  xlab("Age (days post-fertilization)") +
  ylab("Clustering Coefficient")

# Plot Global Efficiency vs Age for all fish
global_eff_plot_all <- ggplot(tracked_AKT1.df, aes(x = Age, y = Global_Eff, group = Fish_no, color = Fish_no)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +  
  theme(legend.position = "right") +
  ggtitle("Global Efficiency vs Age - All Fish") +
  xlab("Age (days post-fertilization)") +
  ylab("Global Efficiency")

# Arrange the four plots into a 2x2 grid and save the combined plot
combined_plot_all <- ggarrange(mean_ca_plot_all, events_plot_all, clust_coeff_plot_all, global_eff_plot_all,
                               ncol = 2, nrow = 2, 
                               common.legend = TRUE, legend = "bottom") # Common legend for all plots

# Save the combined plot to a file
ggsave("Combined_Plots_All_Fish_Color_Blind.png", combined_plot_all, width = 16, height = 12)

# Print the combined plot to the R console (optional)
print(combined_plot_all)
# ---


# CTRL

CTRL.df <- data.frame(
  Fish_no = c(rep("fish1", times = 2), rep("fish3", times = 2)),
  Age = c("4dpf", "5dpf"),
  Mean_Ca = c(mean(ID0144_mean_Ca), 
              mean(ID0146_mean_Ca), 
              mean(ID0145_mean_Ca), 
              mean(ID0148_mean_Ca)),
  Events_min = c(ID0144_frequency, 
                 ID0146_frequency, 
                 ID0145_frequency, 
                 ID0148_frequency),
  Clust_Coeff = c(ID0144.clustcoeff, 
                  ID0146.clustcoeff, 
                  ID0145.clustcoeff, 
                  ID0148.clustcoeff),
  Global_Eff = c(ID0144.globaleff, 
                 ID0146.globaleff, 
                 ID0145.globaleff, 
                 ID0148.globaleff)
)


# CTRL.mean_ca.df <- data.frame(
#   Mean_Ca_4dpf = c(mean(ID0144_mean_Ca), mean(ID0145_mean_Ca), mean(ID0186_mean_Ca), mean(ID0187_mean_Ca), mean(ID0188_mean_Ca), mean(ID0189_mean_Ca), mean(ID0190_mean_Ca), mean(ID0191_mean_Ca), mean(ID0192_mean_Ca), mean(ID0193_mean_Ca), mean(ID0194_mean_Ca), mean(ID0195_mean_Ca)),
#   Mean_Ca_5dpf = c(mean(ID0146_mean_Ca), mean(ID0147_mean_Ca), mean(ID0148_mean_Ca), mean(ID0146_mean_Ca), mean(ID0196_mean_Ca), mean(ID0197_mean_Ca), mean(ID0198_mean_Ca), mean(ID0199_mean_Ca), mean(ID0200_mean_Ca), mean(ID0201_mean_Ca), mean(ID0202_mean_Ca), mean(ID0203_mean_Ca), mean(ID0204_mean_Ca), mean(ID0205_mean_Ca)),
#   Mean_Ca_6dpf = c(mean(ID0206_mean_Ca), mean(ID0207_mean_Ca), mean(ID0208_mean_Ca), mean(ID0209_mean_Ca), mean(ID0210_mean_Ca), mean(ID0211_mean_Ca), mean(ID0212_mean_Ca), mean(ID0213_mean_Ca), mean(ID0214_mean_Ca)),
#   Mean_Ca_8dpf = c(mean(ID0215_mean_Ca), mean(ID0216_mean_Ca), mean(ID0217_mean_Ca), mean(ID0218_mean_Ca), mean(ID0219_mean_Ca), mean(ID0220_mean_Ca), mean(ID0221_mean_Ca))
# )


# Unique fish identifiers
unique_fish <- unique(CTRL.df$Fish_no)

# Loop over each fish to create and arrange plots
for (fish in unique_fish) {
  # Filter data for the current fish
  fish_data <- CTRL.df[CTRL.df$Fish_no == fish, ]
  
  # Plot Mean Ca Vs Age
  mean_ca_plot <- ggline(fish_data, x = "Age", y = "Mean_Ca", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Mean Ca vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Mean Calcium Level")
  
  # Plot Events/min vs Age
  events_plot <- ggline(fish_data, x = "Age", y = "Events_min", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Events/min vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Events/min")
  
  # Plot Clustering Coefficient vs Age
  clust_coeff_plot <- ggline(fish_data, x = "Age", y = "Clust_Coeff", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Clustering Coefficient vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Clustering Coefficient")
  
  # Plot Global Efficiency vs Age
  global_eff_plot <- ggline(fish_data, x = "Age", y = "Global_Eff", add = "line", type = "p", palette = "jco") +
    ggtitle(paste("Global Efficiency vs Age -", fish)) +
    xlab("Age (days post-fertilization)") +
    ylab("Global Efficiency")
  
  # Arrange the four plots into a 2x2 grid and save the combined plot
  combined_plot <- ggarrange(mean_ca_plot, events_plot, clust_coeff_plot, global_eff_plot,
                             ncol = 2, nrow = 2, 
                             labels = c("A", "B", "C", "D"))  # Add labels to the plots if desired
  
  # Save the combined plot to a file
  ggsave(paste0("Combined_Plots_CTRL4_5dpf", fish, ".png"), combined_plot, width = 16, height = 12)
  
  # Print the combined plot to the R console (optional)
  print(combined_plot)
}



# CTRL tracked
  # Create a list of fish names
  fish_names <- paste0("fish", 1:10)

# Create vectors for each day post-fertilization (dpf) with the corresponding variable names
dpf_4 <- c(mean(ID0186_mean_Ca), mean(ID0187_mean_Ca), mean(ID0188_mean_Ca), mean(ID0189_mean_Ca), 
           mean(ID0190_mean_Ca), mean(ID0191_mean_Ca), mean(ID0192_mean_Ca), mean(ID0193_mean_Ca), 
           mean(ID0194_mean_Ca), mean(ID0195_mean_Ca))
dpf_5 <- c(mean(ID0196_mean_Ca), mean(ID0197_mean_Ca), mean(ID0198_mean_Ca), mean(ID0199_mean_Ca), 
           mean(ID0200_mean_Ca), mean(ID0201_mean_Ca), mean(ID0202_mean_Ca), mean(ID0203_mean_Ca), 
           mean(ID0204_mean_Ca), mean(ID0205_mean_Ca))
dpf_6 <- c(mean(ID0206_mean_Ca), mean(ID0207_mean_Ca), mean(ID0208_mean_Ca), mean(ID0209_mean_Ca), 
           mean(ID0210_mean_Ca), mean(ID0211_mean_Ca), mean(ID0212_mean_Ca), NA, 
           mean(ID0213_mean_Ca), mean(ID0214_mean_Ca))
dpf_8 <- c(mean(ID0215_mean_Ca), NA, mean(ID0216_mean_Ca), mean(ID0217_mean_Ca), mean(ID0218_mean_Ca), 
           mean(ID0219_mean_Ca), mean(ID0220_mean_Ca), NA, NA, mean(ID0221_mean_Ca))

# Combine into a dataframe
tracked.CTRL_mean_Ca <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4,
  "5dpf" = dpf_5,
  "6dpf" = dpf_6,
  "8dpf" = dpf_8
)
colnames(tracked.CTRL_mean_Ca) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format
tracked.CTRL_mean_Ca <- pivot_longer(tracked.CTRL_mean_Ca, 
                                          cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                          names_to = "Age", 
                                          values_to = "Mean Ca")

# Plotting
ggline(tracked.CTRL_mean_Ca_long, 
       x = "Age", 
       y = "Mean Ca", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "Control",
       x = "Age (dpf)",
       y = "Mean Ca2+")+
  theme(legend.position = "none")


# Frequency as events/min
# Create vectors for each day post-fertilization (dpf) with the corresponding frequency variable names
dpf_4_freq <- c(ID0186_frequency, ID0187_frequency, ID0188_frequency, ID0189_frequency, 
                ID0190_frequency, ID0191_frequency, ID0192_frequency, ID0193_frequency, 
                ID0194_frequency, ID0195_frequency)
dpf_5_freq <- c(ID0196_frequency, ID0197_frequency, ID0198_frequency, ID0199_frequency, 
                ID0200_frequency, ID0201_frequency, ID0202_frequency, ID0203_frequency, 
                ID0204_frequency, ID0205_frequency)
dpf_6_freq <- c(ID0206_frequency, ID0207_frequency, ID0208_frequency, ID0209_frequency, 
                ID0210_frequency, ID0211_frequency, ID0212_frequency, NA, 
                ID0213_frequency, ID0214_frequency)
dpf_8_freq <- c(ID0215_frequency, NA, ID0216_frequency, ID0217_frequency, ID0218_frequency, 
                ID0219_frequency, ID0220_frequency, NA, NA, ID0221_frequency)

# Combine into a dataframe
tracked.CTRL_frequency <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4_freq,
  "5dpf" = dpf_5_freq,
  "6dpf" = dpf_6_freq,
  "8dpf" = dpf_8_freq
)

colnames(tracked.CTRL_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")
# Reshape the data to long format for plotting or analysis
tracked.CTRL_frequency_long <- pivot_longer(tracked.CTRL_frequency, 
                                            cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                            names_to = "Age", 
                                            values_to = "Frequency")

ggline(tracked.CTRL_frequency_long, 
       x = "Age", 
       y = "Frequency", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "Control events/min",
       x = "Age (dpf)",
       y = "Frequency (events/minute)") +
  theme(legend.position = "none")



# Clustering coefficient
# Create vectors for each day post-fertilization (dpf) with the corresponding clustering coefficient variable names
dpf_4_clustcoeff <- c(ID0186.clustcoeff, ID0187.clustcoeff, ID0188.clustcoeff, ID0189.clustcoeff, 
                      ID0190.clustcoeff, ID0191.clustcoeff, ID0192.clustcoeff, ID0193.clustcoeff, 
                      ID0194.clustcoeff, ID0195.clustcoeff)
dpf_5_clustcoeff <- c(ID0196.clustcoeff, ID0197.clustcoeff, ID0198.clustcoeff, ID0199.clustcoeff, 
                      ID0200.clustcoeff, ID0201.clustcoeff, ID0202.clustcoeff, ID0203.clustcoeff, 
                      ID0204.clustcoeff, ID0205.clustcoeff)
dpf_6_clustcoeff <- c(ID0206.clustcoeff, ID0207.clustcoeff, ID0208.clustcoeff, ID0209.clustcoeff, 
                      ID0210.clustcoeff, ID0211.clustcoeff, ID0212.clustcoeff, NA, 
                      ID0213.clustcoeff, ID0214.clustcoeff)
dpf_8_clustcoeff <- c(ID0215.clustcoeff, NA, ID0216.clustcoeff, ID0217.clustcoeff, ID0218.clustcoeff, 
                      ID0219.clustcoeff, ID0220.clustcoeff, NA, NA, ID0221.clustcoeff)

# Combine into a dataframe
tracked.CTRL_clustcoeff <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4_clustcoeff,
  "5dpf" = dpf_5_clustcoeff,
  "6dpf" = dpf_6_clustcoeff,
  "8dpf" = dpf_8_clustcoeff
)

# Set column names
colnames(tracked.CTRL_clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.CTRL_clustcoeff_long <- pivot_longer(tracked.CTRL_clustcoeff, 
                                             cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                             names_to = "Age", 
                                             values_to = "ClustCoeff")

ggboxplot(tracked.CTRL_clustcoeff_long, 
       x = "Age", 
       y = "ClustCoeff",
       fill = "cyan") +
  theme_minimal() +
  labs(title = "Clustering Coefficienc CTRL",
       x = "Age (dpf)",
       y = "ClustCoeff") +
  theme(legend.position = "none")


means <- aggregate(ClustCoeff ~ Age, data = tracked.CTRL_clustcoeff_long, mean)

# Create the boxplot and line plot
tracked.CTRL_clustcoeff_long.plt <- ggplot(tracked.CTRL_clustcoeff_long, aes(x = Age, y = ClustCoeff, group = Age)) +
  geom_boxplot(aes(fill = "blue")) +
  geom_line(data = means, aes(y = ClustCoeff, group = 1), color = "black") +
  geom_point(data = means, aes(y = ClustCoeff), color = "black", size = 3) +
  scale_fill_identity() +
  theme_pubr() +
  labs(title = "Clustering Coefficient CTRL",
       x = "Age (dpf)",
       y = "Clustering Coefficient") +
  theme(legend.position = "none")



#  Global efficiency
# Global Efficiency
# Create vectors for each day post-fertilization (dpf) with the corresponding global efficiency variable names
dpf_4_globaleff <- c(ID0186.globaleff, ID0187.globaleff, ID0188.globaleff, ID0189.globaleff, 
                     ID0190.globaleff, ID0191.globaleff, ID0192.globaleff, ID0193.globaleff, 
                     ID0194.globaleff, ID0195.globaleff)
dpf_5_globaleff <- c(ID0196.globaleff, ID0197.globaleff, ID0198.globaleff, ID0199.globaleff, 
                     ID0200.globaleff, ID0201.globaleff, ID0202.globaleff, ID0203.globaleff, 
                     ID0204.globaleff, ID0205.globaleff)
dpf_6_globaleff <- c(ID0206.globaleff, ID0207.globaleff, ID0208.globaleff, ID0209.globaleff, 
                     ID0210.globaleff, ID0211.globaleff, ID0212.globaleff, NA, 
                     ID0213.globaleff, ID0214.globaleff)
dpf_8_globaleff <- c(ID0215.globaleff, NA, ID0216.globaleff, ID0217.globaleff, ID0218.globaleff, 
                     ID0219.globaleff, ID0220.globaleff, NA, NA, ID0221.globaleff)

# Combine into a dataframe
tracked.CTRL_globaleff <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4_globaleff,
  "5dpf" = dpf_5_globaleff,
  "6dpf" = dpf_6_globaleff,
  "8dpf" = dpf_8_globaleff
)

# Set column names
colnames(tracked.CTRL_globaleff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.CTRL_globaleff_long <- pivot_longer(tracked.CTRL_globaleff, 
                                            cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                            names_to = "Age", 
                                            values_to = "GlobalEff")

# Plotting
plot222 <- ggline(tracked.CTRL_globaleff_long, 
       x = "Age", 
       y = "GlobalEff", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "Control Global Efficiency by Age",
       x = "Age (dpf)",
       y = "Global Efficiency") +
  theme(legend.position = "none")



# AKT1

#  Mean_Ca
# AKT1 Mean Calcium
# Create vectors for each day post-fertilization (dpf) with the corresponding mean calcium variable names
dpf_4_mean_Ca_AKT1 <- c(mean(ID0152_mean_Ca), mean(ID0153_mean_Ca), mean(ID0154_mean_Ca), mean(ID0155_mean_Ca), 
                        mean(ID0156_mean_Ca), mean(ID0157_mean_Ca), NA, mean(ID0159_mean_Ca), mean(ID0160_mean_Ca))
dpf_5_mean_Ca_AKT1 <- c(mean(ID0161_mean_Ca), mean(ID0162_mean_Ca), mean(ID0163_mean_Ca), mean(ID0164_mean_Ca), 
                        mean(ID0165_mean_Ca), mean(ID0166_mean_Ca), NA, mean(ID0168_mean_Ca), mean(ID0169_mean_Ca))
dpf_6_mean_Ca_AKT1 <- c(mean(ID0170_mean_Ca), mean(ID0171_mean_Ca), mean(ID0172_mean_Ca), mean(ID0173_mean_Ca), 
                        mean(ID0174_mean_Ca), NA, NA, mean(ID0176_mean_Ca), mean(ID0177_mean_Ca))
dpf_8_mean_Ca_AKT1 <- c(mean(ID0178_mean_Ca), mean(ID0179_mean_Ca), NA, mean(ID0181_mean_Ca), mean(ID0182_mean_Ca), 
                        NA, NA, mean(ID0184_mean_Ca), mean(ID0185_mean_Ca))

# Combine into a dataframe
tracked.AKT1_mean_Ca <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_mean_Ca_AKT1,
  "5dpf" = dpf_5_mean_Ca_AKT1,
  "6dpf" = dpf_6_mean_Ca_AKT1,
  "8dpf" = dpf_8_mean_Ca_AKT1
)

# Set column names
colnames(tracked.AKT1_mean_Ca) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1_mean_Ca_long <- pivot_longer(tracked.AKT1_mean_Ca, 
                                          cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                          names_to = "Age", 
                                          values_to = "Mean_Ca")

# Plotting
ggline(tracked.AKT1_mean_Ca_long, 
       x = "Age", 
       y = "Mean_Ca", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "AKT1 Mean Calcium Levels by Age",
       x = "Age (dpf)",
       y = "Mean Calcium Level") +
  theme(legend.position = "none")

# AKT1 Frequency
# Create vectors for each day post-fertilization (dpf) with the corresponding frequency variable names
dpf_4_freq_AKT1 <- c(ID0152_frequency, ID0153_frequency, ID0154_frequency, ID0155_frequency, 
                     ID0156_frequency, ID0157_frequency, NA, ID0159_frequency, ID0160_frequency)
dpf_5_freq_AKT1 <- c(ID0161_frequency, ID0162_frequency, ID0163_frequency, ID0164_frequency, 
                     ID0165_frequency, ID0166_frequency, NA, ID0168_frequency, ID0169_frequency)
dpf_6_freq_AKT1 <- c(ID0170_frequency, ID0171_frequency, ID0172_frequency, ID0173_frequency, 
                     ID0174_frequency, NA, NA, ID0176_frequency, ID0177_frequency)
dpf_8_freq_AKT1 <- c(ID0178_frequency, ID0179_frequency, NA, ID0181_frequency, ID0182_frequency, 
                     NA, NA, ID0184_frequency, ID0185_frequency)

# Combine into a dataframe
tracked.AKT1_frequency <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_freq_AKT1,
  "5dpf" = dpf_5_freq_AKT1,
  "6dpf" = dpf_6_freq_AKT1,
  "8dpf" = dpf_8_freq_AKT1
)

colnames(tracked.AKT1_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1_frequency_long <- pivot_longer(tracked.AKT1_frequency, 
                                            cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                            names_to = "Age", 
                                            values_to = "Frequency")

# Plotting
ggline(tracked.AKT1_frequency_long, 
                  x = "Age", 
                  y = "Frequency", 
                  group = "fish no", 
                  color = "fish no") +
  theme_minimal() +
  labs(title = "AKT1 events/min by Age",
       x = "Age (dpf)",
       y = "Events/min") +
  theme(legend.position = "none")


# AKT1 Clustering Coefficient
# Create vectors for each day post-fertilization (dpf) with the corresponding clustering coefficient variable names
dpf_4_clustcoeff_AKT1 <- c(ID0152.clustcoeff, ID0153.clustcoeff, ID0154.clustcoeff, ID0155.clustcoeff, 
                           ID0156.clustcoeff, ID0157.clustcoeff, NA, ID0159.clustcoeff, ID0160.clustcoeff)
dpf_5_clustcoeff_AKT1 <- c(ID0161.clustcoeff, ID0162.clustcoeff, ID0163.clustcoeff, ID0164.clustcoeff, 
                           ID0165.clustcoeff, ID0166.clustcoeff, NA, ID0168.clustcoeff, ID0169.clustcoeff)
dpf_6_clustcoeff_AKT1 <- c(ID0170.clustcoeff, ID0171.clustcoeff, ID0172.clustcoeff, ID0173.clustcoeff, 
                           ID0174.clustcoeff, NA, NA, ID0176.clustcoeff, ID0177.clustcoeff)
dpf_8_clustcoeff_AKT1 <- c(ID0178.clustcoeff, ID0179.clustcoeff, NA, ID0181.clustcoeff, ID0182.clustcoeff, 
                           NA, NA, ID0184.clustcoeff, ID0185.clustcoeff)

# Combine into a dataframe
tracked.AKT1_clustcoeff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_clustcoeff_AKT1,
  "5dpf" = dpf_5_clustcoeff_AKT1,
  "6dpf" = dpf_6_clustcoeff_AKT1,
  "8dpf" = dpf_8_clustcoeff_AKT1
)

# Set column names
colnames(tracked.AKT1_clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1_clustcoeff_long <- pivot_longer(tracked.AKT1_clustcoeff, 
                                             cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                             names_to = "Age", 
                                             values_to = "ClustCoeff")

# Plotting
ggline(tracked.AKT1_clustcoeff_long, 
       x = "Age", 
       y = "ClustCoeff", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "AKT1 Clustering Coefficient by Age",
       x = "Age (dpf)",
       y = "Clustering Coefficient") +
  theme(legend.position = "none")



# AKT1 Global efficiency G
# Create vectors for each day post-fertilization (dpf) with the corresponding clustering coefficient variable names
dpf_4_globaleff_AKT1 <- c(ID0152.globaleff, ID0153.globaleff, ID0154.globaleff, ID0155.globaleff, 
                           ID0156.globaleff, ID0157.globaleff, NA, ID0159.globaleff, ID0160.globaleff)
dpf_5_globaleff_AKT1 <- c(ID0161.globaleff, ID0162.globaleff, ID0163.globaleff, ID0164.globaleff, 
                           ID0165.globaleff, ID0166.globaleff, NA, ID0168.globaleff, ID0169.globaleff)
dpf_6_globaleff_AKT1 <- c(ID0170.globaleff, ID0171.globaleff, ID0172.globaleff, ID0173.globaleff, 
                           ID0174.globaleff, NA, NA, ID0176.globaleff, ID0177.globaleff)
dpf_8_globaleff_AKT1 <- c(ID0178.globaleff, ID0179.globaleff, NA, ID0181.globaleff, ID0182.globaleff, 
                           NA, NA, ID0184.globaleff, ID0185.globaleff)

# Combine into a dataframe
tracked.AKT1_globaleff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_globaleff_AKT1,
  "5dpf" = dpf_5_globaleff_AKT1,
  "6dpf" = dpf_6_globaleff_AKT1,
  "8dpf" = dpf_8_globaleff_AKT1
)

# Set column names
colnames(tracked.AKT1_globaleff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1_globaleff_long <- pivot_longer(tracked.AKT1_globaleff, 
                                             cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                             names_to = "Age", 
                                             values_to = "globaleff")

# Plotting
ggline(tracked.AKT1_globaleff_long, 
       x = "Age", 
       y = "globaleff", 
       group = "fish no", 
       color = "fish no") +
  theme_minimal() +
  labs(title = "AKT1 Global Efficiency by Age",
       x = "Age (dpf)",
       y = "Global Efficiency") +
  theme(legend.position = "none")
```

```{r Mean Ca 4dpf, echo=FALSE, warning=FALSE}
# Comparisons of CTRL vs AKT1
# Mean Calcium levels ----
# Extracting 4dpf data for CTRL and AKT1, omitting NAs
CTRL_4dpf <- na.omit(tracked.CTRL_mean_Ca$`4dpf`)
AKT1_4dpf <- na.omit(tracked.AKT1_mean_Ca$`4dpf`)

# Find the maximum length of the two vectors
max_length <- max(length(CTRL_4dpf), length(AKT1_4dpf))

# Extend both vectors to the maximum length by adding NAs
CTRL_4dpf_extended <- c(CTRL_4dpf, rep(NA, max_length - length(CTRL_4dpf)))
AKT1_4dpf_extended <- c(AKT1_4dpf, rep(NA, max_length - length(AKT1_4dpf)))

# Creating a combined dataframe with equal length vectors
data_4dpf <- data.frame(
  Mean_Ca = c(CTRL_4dpf_extended, AKT1_4dpf_extended),
  Condition = rep(c("CTRL", "AKT1"), each = max_length)
)

# Plotting the boxplot
CTRLvsAKT1.4dpf_mean_Ca.plt <- ggplot(data_4dpf, aes(x = Condition, y = Mean_Ca, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 1) +  # Set alpha to 1 for dots
  labs(title = "4dpf",
       x = "Condition",
       y = "Mean Calcium Level") +
  theme_pubr()+
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab(expression(Ca^"2+"))
CTRLvsAKT1.4dpf_mean_Ca.plt
```

```{r Mean Ca 5dpf, echo=FALSE, warning=FALSE}
# Extracting 5dpf data for CTRL and AKT1, omitting NAs
CTRL_5dpf <- na.omit(tracked.CTRL_mean_Ca$`5dpf`)
AKT1_5dpf <- na.omit(tracked.AKT1_mean_Ca$`5dpf`)

# Perform Mann-Whitney U test
mw_test_result <- wilcox.test(CTRL_5dpf, AKT1_5dpf)

# Extract the p-value
p_value <- mw_test_result$p.value

# Create a combined dataframe
max_length <- max(length(CTRL_5dpf), length(AKT1_5dpf))
data_5dpf <- data.frame(
  Mean_Ca = c(CTRL_5dpf, rep(NA, max_length - length(CTRL_5dpf)), 
              AKT1_5dpf, rep(NA, max_length - length(AKT1_5dpf))),
  Condition = factor(rep(c("CTRL", "AKT1"), each = max_length))
)

# Plotting the boxplot with p-value
CTRLvsAKT1.5dpf_mean_Ca.plt <- ggplot(data_5dpf, aes(x = Condition, y = Mean_Ca, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 1) +  # Set alpha to 1 for dots
  labs(title = "5dpf",
       x = "Condition",
       y = "Mean Calcium Level") +
  theme_pubr() +
  geom_text(aes(label = paste("p =", format(p_value, digits = 2)), 
                x = 1.5, y = max(data_5dpf$Mean_Ca, na.rm = TRUE)), 
            vjust = 2.5)+
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab(expression(Ca^"2+"))
CTRLvsAKT1.5dpf_mean_Ca.plt
```

```{r Mean Ca 6dpf, warning=FALSE echo = FALSE}
# Extracting 6dpf data for CTRL and AKT1, omitting NAs
CTRL_6dpf <- na.omit(tracked.CTRL_mean_Ca$`6dpf`)
AKT1_6dpf <- na.omit(tracked.AKT1_mean_Ca$`6dpf`)

# Perform Mann-Whitney U test for 6dpf
mw_test_result_6dpf <- wilcox.test(CTRL_6dpf, AKT1_6dpf)

# Extract the p-value for 6dpf
p_value_6dpf <- mw_test_result_6dpf$p.value

# Create a combined dataframe for 6dpf
max_length_6dpf <- max(length(CTRL_6dpf), length(AKT1_6dpf))
data_6dpf <- data.frame(
  Mean_Ca = c(CTRL_6dpf, rep(NA, max_length_6dpf - length(CTRL_6dpf)), 
              AKT1_6dpf, rep(NA, max_length_6dpf - length(AKT1_6dpf))),
  Condition = factor(rep(c("CTRL", "AKT1"), each = max_length_6dpf))
)

# Plotting the boxplot with p-value for 6dpf
CTRLvsAKT1.6dpf_mean_Ca.plt <- ggplot(data_6dpf, aes(x = Condition, y = Mean_Ca, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 1) +  # Set alpha to 1 for dots
  labs(title = "6dpf",
       x = "Condition",
       y = "Mean Calcium Level") +
  theme_pubr() +
  geom_text(aes(label = paste("p =", format(p_value_6dpf, digits = 2)), 
                x = 1.5, y = max(data_6dpf$Mean_Ca, na.rm = TRUE)), 
            vjust = 2.5)+
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab(expression(Ca^"2+"))
```

```{r Mean Ca 8dpf, echo=FALSE, warning=FALSE}
# Extracting 8dpf data for CTRL and AKT1, omitting NAs
CTRL_8dpf <- na.omit(tracked.CTRL_mean_Ca$`8dpf`)
AKT1_8dpf <- na.omit(tracked.AKT1_mean_Ca$`8dpf`)

# Equalize the length of both vectors by padding with NAs
max_length <- max(length(CTRL_8dpf), length(AKT1_8dpf))
length(CTRL_8dpf) <- max_length
length(AKT1_8dpf) <- max_length

# Perform Mann-Whitney U test
mw_test_8dpf <- wilcox.test(CTRL_8dpf, AKT1_8dpf, exact = FALSE)

# Extract the p-value
p_value_8dpf <- mw_test_8dpf$p.value

# Create a combined dataframe
data_8dpf <- data.frame(
  Mean_Ca = c(CTRL_8dpf, AKT1_8dpf),
  Condition = rep(c("CTRL", "AKT1"), each = max_length)
)

# Plotting the boxplot for 8dpf with p-value
CTRLvsAKT1.8dpf_mean_Ca.plt <- ggplot(data_8dpf, aes(x = Condition, y = Mean_Ca, fill = Condition)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 1) +  # Set alpha to 1 for dots
  labs(title = "8dpf",
       x = "Condition",
       y = "Mean Calcium Level") +
  theme_pubr() +
  geom_text(aes(label = paste("p =", format(p_value_8dpf, digits = 2)), 
                x = 1.5, y = max(data_8dpf$Mean_Ca, na.rm = TRUE)), 
            vjust = 3.5)+
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  ylab(expression(Ca^"2+"))
```

Frequency (as events/min)
```{r Frequency 4dpf, echo=FALSE, warning=FALSE}
# FREQUENCY ----------------
# Extracting 4dpf frequency data for CTRL and AKT1, omitting NAs
CTRL_4dpf_freq <- na.omit(tracked.CTRL_frequency$`4dpf`)
AKT1_4dpf_freq <- na.omit(tracked.AKT1_frequency$`4dpf`)

# Equalize the length of both vectors by padding with NAs
max_length <- max(length(CTRL_4dpf_freq), length(AKT1_4dpf_freq))
CTRL_4dpf_freq_extended <- c(CTRL_4dpf_freq, rep(NA, max_length - length(CTRL_4dpf_freq)))
AKT1_4dpf_freq_extended <- c(AKT1_4dpf_freq, rep(NA, max_length - length(AKT1_4dpf_freq)))

# Perform Mann-Whitney U test
mw_test_4dpf_freq <- wilcox.test(CTRL_4dpf_freq_extended, AKT1_4dpf_freq_extended, exact = FALSE)

# Extract the p-value
p_value_4dpf_freq <- mw_test_4dpf_freq$p.value

# Create combined dataframe for 4dpf frequency
data_4dpf_freq <- data.frame(
  Frequency = c(CTRL_4dpf_freq_extended, AKT1_4dpf_freq_extended),
  Condition = rep(c("CTRL", "AKT1"), each = max_length)
)


ggplot(data_4dpf_freq, aes(x = Condition, y = Frequency, fill = Condition)) +
  geom_boxplot() +
  labs(title = "4dpf Frequency Comparison: CTRL vs AKT1",
       x = "Condition",
       y = "Frequency") +
  theme_minimal() +
  geom_text(aes(label = paste("p =", format(p_value_4dpf_freq, digits = 2)), 
                x = 1.5, y = max(data_4dpf_freq$Frequency, na.rm = TRUE)), 
            vjust = -1)


# Equalize the length of both vectors by padding with NAs
max_length_4dpf <- max(length(CTRL_4dpf_freq), length(AKT1_4dpf_freq))
length(CTRL_4dpf_freq) <- max_length_4dpf
length(AKT1_4dpf_freq) <- max_length_4dpf

# Perform Mann-Whitney U test
mw_test_4dpf_freq <- wilcox.test(CTRL_4dpf_freq, AKT1_4dpf_freq, exact = FALSE)

# Extract the p-value
p_value_4dpf_freq <- mw_test_4dpf_freq$p.value

# Create combined dataframe and plot for 4dpf frequency
data_4dpf_freq <- data.frame(
  Frequency = c(CTRL_4dpf_freq, AKT1_4dpf_freq),
  Condition = rep(c("CTRL", "AKT1"), each = max_length_4dpf)
)

CTRLvsAKT1.4dpf_frequency.plt <- ggplot(data_4dpf_freq, aes(x = Condition, y = Frequency, fill = Condition)) +
  geom_boxplot() +
  geom_jitter()+
  labs(title = "4dpf Frequency Comparison: CTRL vs AKT1",
       x = "",
       y = "Events/min") +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = paste("p =", format(p_value_4dpf_freq, digits = 2)), 
                x = 1.5, y = max(data_4dpf_freq$Frequency, na.rm = TRUE)), 
            vjust = 2.5)

```

```{r Frequency 5dpf, echo=FALSE, warning=FALSE}
# Extracting 5dpf frequency data for CTRL and AKT1, omitting NAs
CTRL_5dpf_freq <- na.omit(tracked.CTRL_frequency$`5dpf`)
AKT1_5dpf_freq <- na.omit(tracked.AKT1_frequency$`5dpf`)

# Equalize the length of both vectors by padding with NAs
max_length_5dpf <- max(length(CTRL_5dpf_freq), length(AKT1_5dpf_freq))
length(CTRL_5dpf_freq) <- max_length_5dpf
length(AKT1_5dpf_freq) <- max_length_5dpf

# Perform Mann-Whitney U test
mw_test_5dpf_freq <- wilcox.test(CTRL_5dpf_freq, AKT1_5dpf_freq, exact = FALSE)

# Extract the p-value
p_value_5dpf_freq <- mw_test_5dpf_freq$p.value

# Create combined dataframe and plot for 5dpf frequency
data_5dpf_freq <- data.frame(
  Frequency = c(CTRL_5dpf_freq, AKT1_5dpf_freq),
  Condition = rep(c("CTRL", "AKT1"), each = max_length_5dpf)
)

CTRLvsAKT1.5dpf_frequency.plt <- ggplot(data_5dpf_freq, aes(x = Condition, y = Frequency, fill = Condition)) +
  geom_boxplot() +
  geom_jitter()+
  labs(title = "5dpf Frequency Comparison: CTRL vs AKT1",
       x = "",
       y = "Events/min") +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = paste("p =", format(p_value_5dpf_freq, digits = 2)), 
                x = 1.75, y = max(data_5dpf_freq$Frequency, na.rm = TRUE)), 
            vjust = 2.5)

```

```{r Frequency 6dpf, echo=FALSE, warning=FALSE}
# Extracting 6dpf frequency data for CTRL and AKT1, omitting NAs
CTRL_6dpf_freq <- na.omit(tracked.CTRL_frequency$`6dpf`)
AKT1_6dpf_freq <- na.omit(tracked.AKT1_frequency$`6dpf`)

# Equalize the length of both vectors by padding with NAs
max_length_6dpf <- max(length(CTRL_6dpf_freq), length(AKT1_6dpf_freq))
length(CTRL_6dpf_freq) <- max_length_6dpf
length(AKT1_6dpf_freq) <- max_length_6dpf

# Perform Mann-Whitney U test
mw_test_6dpf_freq <- wilcox.test(CTRL_6dpf_freq, AKT1_6dpf_freq, exact = FALSE)

# Extract the p-value
p_value_6dpf_freq <- mw_test_6dpf_freq$p.value

# Create combined dataframe and plot for 6dpf frequency
data_6dpf_freq <- data.frame(
  Frequency = c(CTRL_6dpf_freq, AKT1_6dpf_freq),
  Condition = rep(c("CTRL", "AKT1"), each = max_length_6dpf)
)

CTRLvsAKT1.6dpf_frequency.plt <- ggplot(data_6dpf_freq, aes(x = Condition, y = Frequency, fill = Condition)) +
  geom_boxplot() +
  geom_jitter()+
  labs(title = "6dpf Frequency Comparison: CTRL vs AKT1",
       x = "",
       y = "Events/min") +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = paste("p =", format(p_value_6dpf_freq, digits = 2)), 
                x = 1.75, y = max(data_6dpf_freq$Frequency, na.rm = TRUE)), 
            vjust = 2.5)
```

```{r Frequency 8dpf, echo=FALSE, warning=FALSE}
# Extracting 8dpf frequency data for CTRL and AKT1, omitting NAs
CTRL_8dpf_freq <- na.omit(tracked.CTRL_frequency$`8dpf`)
AKT1_8dpf_freq <- na.omit(tracked.AKT1_frequency$`8dpf`)

# Equalize the length of both vectors by padding with NAs
max_length_8dpf <- max(length(CTRL_8dpf_freq), length(AKT1_8dpf_freq))
length(CTRL_8dpf_freq) <- max_length_8dpf
length(AKT1_8dpf_freq) <- max_length_8dpf

# Perform Mann-Whitney U test
mw_test_8dpf_freq <- wilcox.test(CTRL_8dpf_freq, AKT1_8dpf_freq, exact = FALSE)

# Extract the p-value
p_value_8dpf_freq <- mw_test_8dpf_freq$p.value

# Create combined dataframe and plot for 8dpf frequency
data_8dpf_freq <- data.frame(
  Frequency = c(CTRL_8dpf_freq, AKT1_8dpf_freq),
  Condition = rep(c("CTRL", "AKT1"), each = max_length_8dpf)
)

CTRLvsAKT1.8dpf_frequency.plt <- ggplot(data_8dpf_freq, aes(x = Condition, y = Frequency, fill = Condition)) +
  geom_boxplot() +
  geom_jitter()+
  labs(title = "8dpf Frequency Comparison: CTRL vs AKT1",
       x = "",
       y = "Events/min") +
  theme_pubr() +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(label = paste("p =", format(p_value_8dpf_freq, digits = 2)), 
                x = 1.75, y = max(data_8dpf_freq$Frequency, na.rm = TRUE)), 
            vjust = 2.5)

```

# Comparisons -----

CTRLvsAKT1_mean_Ca.plt <- ggarrange(CTRLvsAKT1.4dpf_mean_Ca.plt, CTRLvsAKT1.5dpf_mean_Ca.plt,
                                CTRLvsAKT1.6dpf_mean_Ca.plt, CTRLvsAKT1.8dpf_mean_Ca.plt)

CTRLvsAKT1_frequency.plt <- ggarrange(CTRLvsAKT1.4dpf_frequency.plt, CTRLvsAKT1.5dpf_frequency.plt,
                                  CTRLvsAKT1.6dpf_frequency.plt, CTRLvsAKT1.8dpf_frequency.plt)
# Condition during time
CTRL_frequency_over_days.plt <- ggboxplot(data = tracked.CTRL_frequency_long2, x = "Age", y = "Frequency", 
                                          fill = "lightblue", # Change the fill color
                                          palette = "jco",    # Use a predefined color palette
                                          add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                          title = "CTRL Events/min by Age",  # Add a title
                                          xlab = "Age",       # Label for the x-axis
                                          ylab = "Events/min")+
                                    theme_pubr()


AKT1_frequency_over_days.plt <- ggboxplot(data = tracked.AKT1_frequency_long2, x = "Age", y = "Frequency", 
                                          fill = "red", # Change the fill color
                                          palette = "jco",    # Use a predefined color palette
                                          add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                          title = "AKT1 Events/min by Age",  # Add a title
                                          xlab = "Age",       # Label for the x-axis
                                          ylab = "Events/min") + 
  theme_pubr() + # Using a predefined theme
  ylim(0, 1.5)   # Set the y-axis limits


# Both AKT1 and CTRL overlapping over time
# Merge the two dataframes
CTRL_AKT1_frequency_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_frequency_long2),
  data.frame(Group = "AKT1", tracked.AKT1_frequency_long2)
)

# Create the combined plot with overlapping data
CTRL_AKT1_frequency_merged.plt <- ggboxplot(CTRL_AKT1_frequency_merged_data.df, x = "Age", y = "Frequency",
                                            fill = "Group",
                                            palette = c("CTRL" = "blue", "AKT1" = "red"),
                                            add = "boxplot",
                                            title = "Frequency Comparison: CTRL vs AKT1",
                                            xlab = "Age",
                                            ylab = "Events/min") +
  theme(legend.position = "none") + # Using a predefined theme
  ylim(0, 1.5) 


ggboxplot(CTRL_AKT1_frequency_merged_data.df, x = "Age", y = "Frequency",
          fill = "Group",
          palette = c("CTRL" = "blue", "AKT1" = "red"),
          add = "boxplot",
          title = "Frequency Comparison: CTRL vs AKT1",
          xlab = "Age",
          ylab = "Events/min") +
  theme(legend.position = "none") + # Remove legend
  ylim(0, 1.5) +
  stat_compare_means(method = "wilcox.test", 
                     comparisons = list(c("CTRL", "AKT1")), 
                     label = "p.format",
                     method.args = list(alternative = "two.sided", exact = FALSE))




# Clustering Coefficient comparisons ----
CTRL_AKT1_clustcoeff.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_clustcoeff_long),
  data.frame(Group = "AKT1", tracked.AKT1_clustcoeff_long)
)


CTRL_AKT1_clustcoeff.df <- na.omit(CTRL_AKT1_clustcoeff.df)

CTRL_AKT1_clustcoeff.plt <- ggboxplot(CTRL_AKT1_clustcoeff.df, x = "Age", y = "ClustCoeff",
                                                                  fill = "Group",
                                                                  palette = c("CTRL" = "blue", "AKT1" = "red"),
                                                                  add = "boxplot",
                                                                  title = "Clustering Coefficients Comparison: CTRL vs AKT1",
                                                                  xlab = "Age",
                                                                  ylab = "C(g)") +
                          theme(legend.position = "none") + # Remove legend
                          ylim(0, 1.5) +
                          stat_compare_means(data = CTRL_AKT1_clustcoeff.df, method = "wilcox.test", 
                                             comparisons = list(c("CTRL", "AKT1")), label = "p.format")


# Global Efficiency comparisons ---------
# Create a data frame for global efficiency
CTRL_AKT1_globaleff.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_globaleff_long), # Replace with your actual data frame for CTRL global efficiency
  data.frame(Group = "AKT1", tracked.AKT1_globaleff_long)  # Replace with your actual data frame for AKT1 global efficiency
)

# Remove NA values
CTRL_AKT1_globaleff.df <- na.omit(CTRL_AKT1_globaleff.df)

# Create the boxplot with statistical comparison
CTRL_AKT1_globaleff.plt <- ggboxplot(CTRL_AKT1_globaleff.df, x = "Age", y = "GlobalEff", # Make sure to replace 'GlobalEff' with your actual column name
                                     fill = "Group",
                                     palette = c("CTRL" = "blue", "AKT1" = "red"),
                                     add = "boxplot",
                                     title = "Global Efficiency Comparison: CTRL vs AKT1",
                                     xlab = "Age",
                                     ylab = "Global Efficiency") +
  theme(legend.position = "centre") + # Remove legend
  ylim(0, 1.5) +
  geom_signif()

# CTRL
mean_data <- tracked.CTRL_globaleff_long %>%
  group_by(Age) %>%
  summarise(MeanGlobalEff = mean(GlobalEff))

mean_sd_data <- tracked.CTRL_globaleff_long %>%
  group_by(Age) %>%
  summarise(MeanGlobalEff = mean(GlobalEff),
            SdGlobalEff = sd(GlobalEff))

mean_sd_data <- tracked.CTRL_globaleff_long %>%
                group_by(Age) %>%
                summarise(MeanGlobalEff = mean(GlobalEff), 
                          SdGlobalEff = sd(GlobalEff))

# Create the plot
tracked.CTRL_globaleff_long.plt <- ggplot(tracked.CTRL_globaleff_long, aes(x = Age, y = GlobalEff)) +
                                      geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7, outlier.shape = NA) +
                                      geom_jitter(position = position_jitter(width = 0.2), color = "black", size = 2, alpha = 1) +
                                      labs(
                                        title = "Global Efficiency by Age in CTRL",
                                        y = "Global Efficiency"
                                      ) +
                                      theme_pubr() +
                                      theme(
                                        plot.title = element_text(hjust = 0.5),
                                        axis.title.x = element_blank(),
                                        axis.title.y = element_text(margin = margin(r = 10)),
                                        axis.text = element_text(size = 12),
                                        axis.title = element_text(size = 14),
                                        legend.position = "none"  # Remove legend
  )

  
tracked.AKT1_globaleff_long.plt <- ggplot(tracked.AKT1_globaleff_long, aes(x = Age, y = GlobalEff)) +
  geom_boxplot(fill = "#f03b20", color = "black", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), color = "black", size = 2, alpha = 1) +
  labs(
    title = "Global Efficiency by Age in AKT1",
    y = "Global Efficiency"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Remove legend
  )



# HRASV12 ----
# Create a list of fish names for HRASV12 condition
fish_names_hrasv12 <- paste0("fish", 1:17)

# Define NA vector for missing values in each dpf column
na_vec_4dpf <- rep(NA, 17)
na_vec_5dpf <- rep(NA, 17)
na_vec_6dpf <- rep(NA, 17)
na_vec_8dpf <- rep(NA, 17)

# Fill the 4dpf column
dpf_4_hrasv12_mean_Ca <- c(mean(ID0222_mean_Ca), mean(ID0223_mean_Ca), mean(ID0224_mean_Ca), mean(ID0225_mean_Ca), 
                   mean(ID0226_mean_Ca), mean(ID0227_mean_Ca), mean(ID0228_mean_Ca), mean(ID0229_mean_Ca), 
                   mean(ID0230_mean_Ca), mean(ID0231_mean_Ca), mean(ID0247_mean_Ca), mean(ID0248_mean_Ca), 
                   mean(ID0249_mean_Ca), mean(ID0250_mean_Ca), mean(ID0251_mean_Ca), mean(ID0252_mean_Ca), 
                   mean(ID0253_mean_Ca))
na_vec_4dpf[1:17] <- dpf_4_hrasv12_mean_Ca

# Fill the 5dpf column
dpf_5_hrasv12_mean_Ca <- c(mean(ID0232_mean_Ca), mean(ID0233_mean_Ca), NA, mean(ID0234_mean_Ca), mean(ID0235_mean_Ca), 
                   mean(ID0236_mean_Ca), NA, NA, mean(ID0237_mean_Ca), mean(ID0238_mean_Ca), NA, mean(ID0254_mean_Ca), 
                   mean(ID0255_mean_Ca), NA, mean(ID0256_mean_Ca), mean(ID0257_mean_Ca), mean(ID0258_mean_Ca))
na_vec_5dpf[1:17] <- dpf_5_hrasv12_mean_Ca

# Fill the 6dpf column
dpf_6_hrasv12_mean_Ca <- c(mean(ID0239_mean_Ca), NA, NA, mean(ID0240_mean_Ca), mean(ID0241_mean_Ca), NA, NA, NA, 
                   mean(ID0242_mean_Ca), mean(ID0243_mean_Ca), NA, mean(ID0259_mean_Ca), mean(ID0260_mean_Ca), NA, 
                   NA, mean(ID0261_mean_Ca), mean(ID0262_mean_Ca))
na_vec_6dpf[1:17] <- dpf_6_hrasv12_mean_Ca

# Fill the 8dpf column
dpf_8_hrasv12_mean_Ca <- c(mean(ID0244_mean_Ca), NA, NA, mean(ID0245_mean_Ca), mean(ID0246_mean_Ca), NA, NA, NA, NA, NA, 
                   NA, mean(ID0263_mean_Ca), mean(ID0264_mean_Ca), NA, NA, NA, mean(ID0265_mean_Ca))
na_vec_8dpf[1:17] <- dpf_8_hrasv12_mean_Ca

# Combine into a dataframe
tracked.HRASV12_mean_Ca <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12_mean_Ca) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")


# HRASV12 frequency ----

# Fill the 4dpf column
dpf_4_hrasv12_frequency <- c(ID0222_frequency, ID0223_frequency, ID0224_frequency, ID0225_frequency, 
                           ID0226_frequency, ID0227_frequency, ID0228_frequency, ID0229_frequency, 
                           ID0230_frequency, ID0231_frequency, ID0247_frequency, ID0248_frequency, 
                           ID0249_frequency, ID0250_frequency, ID0251_frequency, ID0252_frequency, 
                           ID0253_frequency)
na_vec_4dpf[1:17] <- dpf_4_hrasv12_frequency

# Fill the 5dpf column
dpf_5_hrasv12_frequency <- c(ID0232_frequency, ID0233_frequency, NA, ID0234_frequency, ID0235_frequency, 
                           ID0236_frequency, NA, NA, ID0237_frequency, ID0238_frequency, NA, ID0254_frequency, 
                           ID0255_frequency, NA, ID0256_frequency, ID0257_frequency, ID0258_frequency)
na_vec_5dpf[1:17] <- dpf_5_hrasv12_frequency

# Fill the 6dpf column
dpf_6_hrasv12_frequency <- c(ID0239_frequency, NA, NA, ID0240_frequency, ID0241_frequency, NA, NA, NA, 
                           ID0242_frequency, ID0243_frequency, NA, ID0259_frequency, ID0260_frequency, NA, 
                           NA, ID0261_frequency, ID0262_frequency)
na_vec_6dpf[1:17] <- dpf_6_hrasv12_frequency

# Fill the 8dpf column
dpf_8_hrasv12_frequency <- c(ID0244_frequency, NA, NA, ID0245_frequency, ID0246_frequency, NA, NA, NA, NA, NA, 
                           NA, ID0263_frequency, ID0264_frequency, NA, NA, NA, ID0265_frequency)
na_vec_8dpf[1:17] <- dpf_8_hrasv12_frequency

# Combine into a dataframe
tracked.HRASV12_frequency <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# HRASV12 Clustering Coefficient ----

# Fill the 4dpf column
dpf_4_hrasv12.clustcoeff <- c(ID0222.clustcoeff, ID0223.clustcoeff, ID0224.clustcoeff, ID0225.clustcoeff, 
                             ID0226.clustcoeff, ID0227.clustcoeff, ID0228.clustcoeff, ID0229.clustcoeff, 
                             ID0230.clustcoeff, ID0231.clustcoeff, ID0247.clustcoeff, ID0248.clustcoeff, 
                             ID0249.clustcoeff, ID0250.clustcoeff, ID0251.clustcoeff, ID0252.clustcoeff, 
                             ID0253.clustcoeff)
na_vec_4dpf[1:17] <- dpf_4_hrasv12.clustcoeff

# Fill the 5dpf column
dpf_5_hrasv12.clustcoeff <- c(ID0232.clustcoeff, ID0233.clustcoeff, NA, ID0234.clustcoeff, ID0235.clustcoeff, 
                             ID0236.clustcoeff, NA, NA, ID0237.clustcoeff, ID0238.clustcoeff, NA, ID0254.clustcoeff, 
                             ID0255.clustcoeff, NA, ID0256.clustcoeff, ID0257.clustcoeff, ID0258.clustcoeff)
na_vec_5dpf[1:17] <- dpf_5_hrasv12.clustcoeff

# Fill the 6dpf column
dpf_6_hrasv12.clustcoeff <- c(ID0239.clustcoeff, NA, NA, ID0240.clustcoeff, ID0241.clustcoeff, NA, NA, NA, 
                             ID0242.clustcoeff, ID0243.clustcoeff, NA, ID0259.clustcoeff, ID0260.clustcoeff, NA, 
                             NA, ID0261.clustcoeff, ID0262.clustcoeff)
na_vec_6dpf[1:17] <- dpf_6_hrasv12.clustcoeff

# Fill the 8dpf column
dpf_8_hrasv12.clustcoeff <- c(ID0244.clustcoeff, NA, NA, ID0245.clustcoeff, ID0246.clustcoeff, NA, NA, NA, NA, NA, 
                             NA, ID0263.clustcoeff, ID0264.clustcoeff, NA, NA, NA, ID0265.clustcoeff)
na_vec_8dpf[1:17] <- dpf_8_hrasv12.clustcoeff

# Combine into a dataframe
tracked.HRASV12.clustcoeff <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12.clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# HRASV12 Global Efficiency ----
# Fill the 4dpf column
dpf_4_hrasv12.globaleff <- c(ID0222.globaleff, ID0223.globaleff, ID0224.globaleff, ID0225.globaleff, 
                              ID0226.globaleff, ID0227.globaleff, ID0228.globaleff, ID0229.globaleff, 
                              ID0230.globaleff, ID0231.globaleff, ID0247.globaleff, ID0248.globaleff, 
                              ID0249.globaleff, ID0250.globaleff, ID0251.globaleff, ID0252.globaleff, 
                              ID0253.globaleff)
na_vec_4dpf[1:17] <- dpf_4_hrasv12.globaleff

# Fill the 5dpf column
dpf_5_hrasv12.globaleff <- c(ID0232.globaleff, ID0233.globaleff, NA, ID0234.globaleff, ID0235.globaleff, 
                              ID0236.globaleff, NA, NA, ID0237.globaleff, ID0238.globaleff, NA, ID0254.globaleff, 
                              ID0255.globaleff, NA, ID0256.globaleff, ID0257.globaleff, ID0258.globaleff)
na_vec_5dpf[1:17] <- dpf_5_hrasv12.globaleff

# Fill the 6dpf column
dpf_6_hrasv12.globaleff <- c(ID0239.globaleff, NA, NA, ID0240.globaleff, ID0241.globaleff, NA, NA, NA, 
                              ID0242.globaleff, ID0243.globaleff, NA, ID0259.globaleff, ID0260.globaleff, NA, 
                              NA, ID0261.globaleff, ID0262.globaleff)
na_vec_6dpf[1:17] <- dpf_6_hrasv12.globaleff

# Fill the 8dpf column
dpf_8_hrasv12.globaleff <- c(ID0244.globaleff, NA, NA, ID0245.globaleff, ID0246.globaleff, NA, NA, NA, NA, NA, 
                              NA, ID0263.globaleff, ID0264.globaleff, NA, NA, NA, ID0265.globaleff)
na_vec_8dpf[1:17] <- dpf_8_hrasv12.globaleff

# Combine into a dataframe
tracked.HRASV12.globaleff <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12.globaleff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")


# Data viz for HRASV12 ----
# Mean_Ca
# Convert the dataframe from wide to long format
tracked.HRASV12_mean_Ca_long <- tracked.HRASV12_mean_Ca %>% 
  gather(key = "Age", value = "Mean_Ca", - "fish no")

# Create the boxplot
HRASV12_mean_Ca_over_days.plt <- ggboxplot(data = tracked.HRASV12_mean_Ca_long, x = "Age", y = "Mean_Ca", 
                                           fill = "purple", # Change the fill color
                                           palette = "jco",    # Use a predefined color palette
                                           add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                           title = "HRASV12 Mean Ca by Age",  # Add a title
                                           xlab = "Age",       # Label for the x-axis
                                           ylab = "Mean Ca") +
                                            theme_pubr()

# Frequency
# Convert the frequency dataframe from wide to long format
tracked.HRASV12_frequency_long <- tracked.HRASV12_frequency %>% 
  gather(key = "Age", value = "Frequency", - "fish no")

# Create the boxplot for Frequency
HRASV12_frequency_over_days.plt <- ggboxplot(data = tracked.HRASV12_frequency_long, x = "Age", y = "Frequency", 
                                             fill = "purple", # Change the fill color
                                             palette = "jco",    # Use a predefined color palette
                                             add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                             title = "HRASV12 Events/min by Age",  # Add a title
                                             xlab = "Age",       # Label for the x-axis
                                             ylab = "Events/min") +
                                           theme_pubr()

# Clustering Coefficient FOR HRASV12
# Convert the clustcoeff dataframe from wide to long format
tracked.HRASV12.clustcoeff_long <- tracked.HRASV12.clustcoeff %>% 
  gather(key = "Age", value = "ClustCoeff", - "fish no")

# Create the boxplot for .clustcoeff
HRASV12.clustcoeff_over_days.plt <- ggboxplot(data = tracked.HRASV12.clustcoeff_long, x = "Age", y = "ClustCoeff", 
                                              fill = "lightblue", # Change the fill color
                                              palette = "jco",    # Use a predefined color palette
                                              add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                              title = "HRASV12 Clustering Coefficient by Age",  # Add a title
                                              xlab = "Age",       # Label for the x-axis
                                              ylab = "C(g)") +
                                            theme_pubr()

# Global Efficiency for HRASV12
tracked.HRASV12.globaleff_long <- tracked.HRASV12.globaleff %>% 
  gather(key = "Age", value = "GlobalEff", - "fish no")

# Create the boxplot for .clustcoeff
HRASV12.globaleff_over_days.plt <- ggboxplot(data = tracked.HRASV12.globaleff_long, x = "Age", y = "GlobalEff", 
                                             fill = "lightblue", # Change the fill color
                                             palette = "jco",    # Use a predefined color palette
                                             add = c("mean_bar", "jitter"),    # Add mean and standard deviation
                                             title = "HRASV12 Global Efficiency by Age",  # Add a title
                                             xlab = "Age",       # Label for the x-axis
                                             ylab = "G(g)") +
                                            theme_pubr()

# Comparisons CTRL vs AKT1 vs HRASV12 ----
# Frequency comparison
# Merge the three dataframes for FREQUENCY
CTRL_AKT1_HRASV12_frequency_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_frequency_long2),
  data.frame(Group = "AKT1", tracked.AKT1_frequency_long2),
  data.frame(Group = "HRASV12", tracked.HRASV12_frequency_long)
)
CTRL_AKT1_HRASV12_frequency_merged_data.df <- na.omit(CTRL_AKT1_HRASV12_frequency_merged_data.df)
# Create the combined plot with overlapping data
results <- df %>%
  filter(Group %in% c("CTRL", "AKT1")) %>%
  group_by(Age) %>%
  summarise(p_value = t.test(Frequency[Group == "CTRL"], Frequency[Group == "AKT1"])$p.value) %>%
  mutate(label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ ""
  ),
  ypos = 1.2)
CTRL_AKT1_HRASV12_frequency_merged_data.df.plt <- ggplot(df, aes(x = Age, y = Frequency, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple")) +
  theme_pubr() +
  labs(title = "Frequency Comparison: CTRL vs AKT1 vs HRASV12",
       x = "Age",
       y = "Events/min")+
  ylim(0,1.3)+
  geom_text(data = results, aes(x = Age, y = ypos, label = label), inherit.aes = FALSE)





# Mean Calcium comparison ----
# Merge the three dataframes for Mean Calcium
CTRL_AKT1_HRASV12_mean_Ca_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_mean_Ca_long),
  data.frame(Group = "AKT1", tracked.AKT1_mean_Ca_long),
  data.frame(Group = "HRASV12", tracked.HRASV12_mean_Ca_long)
)

CTRL_AKT1_HRASV12_mean_Ca_merged_data.df <- na.omit(CTRL_AKT1_HRASV12_mean_Ca_merged_data.df)

# Create the combined plot with overlapping data
CTRL_AKT1_HRASV12_mean_Ca_merged_data.df.plt <- ggboxplot(CTRL_AKT1_HRASV12_mean_Ca_merged_data.df, x = "Age", y = "Mean_Ca",
                                                            fill = "Condition",
                                                            palette = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple"),
                                                            add = "boxplot",
                                                            title = "Calcium levels Comparison: CTRL vs AKT1 vs HRASV12",
                                                            xlab = "Age",
                                                            ylab = "Ca2+")+
                                                        theme_pubr()+
                                                        theme(axis.title.x = element_blank())+
                                                        ylim(0.035, 0.11)



# Merge the three dataframes for clustering coefficient C(g) ----
CTRL_AKT1_HRASV12.clustcoeff_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_clustcoeff_long),
  data.frame(Group = "AKT1", tracked.AKT1.clustcoeff_long),
  data.frame(Group = "HRASV12", tracked.HRASV12.clustcoeff_long)
)

CTRL_AKT1_HRASV12.clustcoeff_merged_data.df <- na.omit(CTRL_AKT1_HRASV12.clustcoeff_merged_data.df)

# Create the combined plot with overlapping data
CTRL_AKT1_HRASV12.clustcoeff_merged_data.df.plt <- ggboxplot(CTRL_AKT1_HRASV12.clustcoeff_merged_data.df, x = "Age", y = "ClustCoeff",
                                                          fill = "Group",
                                                          palette = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple"),
                                                          add = "boxplot",
                                                          title = "Clustering Coefficient Comparison: CTRL vs AKT1 vs HRASV12",
                                                          xlab = "Age",
                                                          ylab = "C(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())+
  geom_segment(x = 3.70, xend = 4.30, y = 0.83, yend = 0.83, color = "black", size = 1.5) +
  annotate("text", x = 4, y = 0.850, label = "p = 0.105", vjust = 0, size = 5, color = "black")
  



# Merge the three dataframes for Global Efficiency G(g)
CTRL_AKT1_HRASV12.globaleff_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_globaleff_long),
  data.frame(Group = "AKT1", tracked.AKT1.globaleff_long),
  data.frame(Group = "HRASV12", tracked.HRASV12.globaleff_long)
)

CTRL_AKT1_HRASV12.globaleff_merged_data.df <- na.omit(CTRL_AKT1_HRASV12.globaleff_merged_data.df)

# Create the combined plot with overlapping data
CTRL_AKT1_HRASV12.globaleff_merged_data.df.plt <- ggboxplot(CTRL_AKT1_HRASV12.globaleff_merged_data.df, x = "Age", y = "GlobalEff",
                                                             fill = "Group",
                                                             palette = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple"),
                                                             add = "boxplot",
                                                             title = "Global Efficiency Comparison: CTRL vs AKT1 vs HRASV12",
                                                             xlab = "Age",
                                                             ylab = "G(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())+
  ylim(0,1.0)


#  RFP comparisons ------
# Mean Calcium all vs RFP
ctrl_data <- CTRL_AKT1_HRASV12_mean_Ca.RFP.df[CTRL_AKT1_HRASV12_mean_Ca.RFP.df$Condition == "CTRL", ]

CTRL_mean_Ca.RFP_comparison <- ggplot(ctrl_data, aes(x = Age, y = Mean_Ca, fill = RFP)) +
  geom_boxplot()+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(x = "Age", y = "Mean Ca", title = "Mean Calcium for CTRL") +
  theme_pubr()

# AKT1
akt1_data <- CTRL_AKT1_HRASV12_mean_Ca.RFP.df[CTRL_AKT1_HRASV12_mean_Ca.RFP.df$Condition == "AKT1", ]

AKT1_mean_Ca.RFP_comparison <- ggplot(akt1_data, aes(x = Age, y = Mean_Ca, fill = RFP)) +
  geom_boxplot()+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(x = "Age", y = "Mean Ca", title = "Mean Calcium for AKT1") +
  theme_pubr()

# HRASV12
hrasv12_data <- CTRL_AKT1_HRASV12_mean_Ca.RFP.df[CTRL_AKT1_HRASV12_mean_Ca.RFP.df$Condition == "HRASV12", ]

HRASV12_mean_Ca.RFP_comparison <- ggplot(hrasv12_data, aes(x = Age, y = Mean_Ca, fill = RFP)) +
  geom_boxplot()+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(x = "Age", y = "Mean Ca", title = "Mean Calcium for HRASV12") +
  theme_pubr()

# overall 3 conditions
CTRL_AKT1_HRASV12_mean_Ca.RFP.plt <- ggboxplot(mean_Ca.RFP.df, x = "Age", y = "Mean_Ca", 
                                               fill = "Condition", 
                                               palette = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple"),
                                               add = "jitter", shape = "Condition",
                                               xlab = "Age", ylab = "Mean Ca", title = "Mean Calcium in RFP cells") +
                                            theme_pubr()

# 