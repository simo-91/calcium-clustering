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

# Reshape the data to long format
tracked.CTRL_mean_Ca_long <- pivot_longer(tracked.CTRL_mean_Ca, 
                                          cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                          names_to = "Age", 
                                          values_to = "Mean Ca")

# Plotting
ggline(tracked.CTRL_mean_Ca_long, 
       x = "Age", 
       y = "Mean Ca", 
       group = "fish no", 
       color = "fish no", 
       add = "mean_se") +
  theme_minimal() +
  labs(title = "Control",
       x = "Age (dpf)",
       y = "Mean Ca2+")+
  theme(legend.position = "none")

