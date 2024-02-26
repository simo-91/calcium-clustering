# IRF8null
# Frequency
# CTRL_IRF8null Frequency
# Step 1: Define frequency variables for each dpf and fish number, including NAs for missing data

# Correcting for 6dpf and 8dpf by adding NA for missing fish numbers
dpf_4_freq_CTRL_IRF8null <- c(ID0266_frequency, ID0267_frequency, ID0268_frequency, ID0269_frequency, 
                              ID0270_frequency, ID0271_frequency, ID0272_frequency, ID0273_frequency, ID0274_frequency)

dpf_5_freq_CTRL_IRF8null <- c(ID0275_frequency, ID0276_frequency, ID0277_frequency, ID0278_frequency, 
                              ID0279_frequency, ID0280_frequency, ID0281_frequency, ID0282_frequency, ID0283_frequency)

dpf_6_freq_CTRL_IRF8null <- c(ID0284_frequency, ID0285_frequency, ID0286_frequency, ID0287_frequency, 
                              ID0288_frequency, ID0289_frequency, ID0290_frequency, NA, NA) # Added NAs for missing fish numbers

dpf_8_freq_CTRL_IRF8null <- c(ID0291_frequency, ID0292_frequency, ID0293_frequency, ID0294_frequency, 
                              ID0295_frequency, ID0296_frequency, ID0297_frequency, NA, NA) # Added NAs for missing fish numbers

# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.CTRL_IRF8null_frequency <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_freq_CTRL_IRF8null,
  "5dpf" = dpf_5_freq_CTRL_IRF8null,
  "6dpf" = dpf_6_freq_CTRL_IRF8null,
  "8dpf" = dpf_8_freq_CTRL_IRF8null
)

colnames(tracked.CTRL_IRF8null_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null_frequency_long <- pivot_longer(tracked.CTRL_IRF8null_frequency, 
                                                     cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                     names_to = "Age", 
                                                     values_to = "Frequency")

# Step 4: Plotting
ggplot(tracked.CTRL_IRF8null_frequency_long, aes(x = Age, y = Frequency, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null Frequency by Age",
       x = "Age (dpf)",
       y = "Frequency") +
  theme(legend.position = "none")



# CTRL_IRF8null Mean Calcium
# Create vectors for each day post-fertilization (dpf) with the corresponding mean calcium variable names
dpf_4_mean_Ca_CTRL_IRF8null <- c(mean(ID0266_mean_Ca), mean(ID0267_mean_Ca), mean(ID0268_mean_Ca), mean(ID0269_mean_Ca), 
                                 mean(ID0270_mean_Ca), mean(ID0271_mean_Ca), mean(ID0272_mean_Ca), mean(ID0273_mean_Ca), mean(ID0274_mean_Ca))
dpf_5_mean_Ca_CTRL_IRF8null <- c(mean(ID0275_mean_Ca), mean(ID0276_mean_Ca), mean(ID0277_mean_Ca), mean(ID0278_mean_Ca), 
                                 mean(ID0279_mean_Ca), mean(ID0280_mean_Ca), mean(ID0281_mean_Ca), mean(ID0282_mean_Ca), mean(ID0283_mean_Ca))
dpf_6_mean_Ca_CTRL_IRF8null <- c(mean(ID0284_mean_Ca), mean(ID0285_mean_Ca), mean(ID0286_mean_Ca), mean(ID0287_mean_Ca), 
                                 mean(ID0288_mean_Ca), mean(ID0289_mean_Ca), mean(ID0290_mean_Ca), NA, NA)
dpf_8_mean_Ca_CTRL_IRF8null <- c(mean(ID0291_mean_Ca), mean(ID0292_mean_Ca), mean(ID0293_mean_Ca), mean(ID0294_mean_Ca), 
                                 mean(ID0295_mean_Ca), mean(ID0296_mean_Ca), mean(ID0297_mean_Ca), NA, NA)

# Combine into a dataframe
tracked.CTRL_IRF8null_mean_Ca <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_mean_Ca_CTRL_IRF8null,
  "5dpf" = dpf_5_mean_Ca_CTRL_IRF8null,
  "6dpf" = dpf_6_mean_Ca_CTRL_IRF8null,
  "8dpf" = dpf_8_mean_Ca_CTRL_IRF8null
)

# Set column names
colnames(tracked.CTRL_IRF8null_mean_Ca) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null_mean_Ca_long <- pivot_longer(tracked.CTRL_IRF8null_mean_Ca, 
                                                   cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                   names_to = "Age", 
                                                   values_to = "Mean_Ca")

# Plotting with ggplot2 (adjusted from ggline to ggplot due to previous use of ggline which might not be directly recognized)
ggplot(tracked.CTRL_IRF8null_mean_Ca_long, aes(x = Age, y = Mean_Ca, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null Mean Calcium Levels by Age",
       x = "Age (dpf)",
       y = "Mean Calcium Level") +
  theme(legend.position = "none")


# CTRL_IRF8null Clustering Coefficient ----

# 4dpf
dpf_4_ctrl_irf8null.clustcoeff <- c(ID0266.clustcoeff, ID0267.clustcoeff, ID0268.clustcoeff, ID0269.clustcoeff, 
                                    ID0270.clustcoeff, ID0271.clustcoeff, ID0272.clustcoeff, ID0273.clustcoeff, ID0274.clustcoeff)

# 5dpf
dpf_5_ctrl_irf8null.clustcoeff <- c(ID0275.clustcoeff, ID0276.clustcoeff, ID0277.clustcoeff, ID0278.clustcoeff, 
                                    ID0279.clustcoeff, ID0280.clustcoeff, ID0281.clustcoeff, ID0282.clustcoeff, ID0283.clustcoeff)

# 6dpf
dpf_6_ctrl_irf8null.clustcoeff <- c(ID0284.clustcoeff, ID0285.clustcoeff, ID0286.clustcoeff, ID0287.clustcoeff, 
                                    ID0288.clustcoeff, ID0289.clustcoeff, ID0290.clustcoeff, NA, NA)

# 8dpf
dpf_8_ctrl_irf8null.clustcoeff <- c(ID0291.clustcoeff, ID0292.clustcoeff, ID0293.clustcoeff, ID0294.clustcoeff, 
                                    ID0295.clustcoeff, ID0296.clustcoeff, ID0297.clustcoeff, NA, NA)

# Combine into a dataframe
tracked.CTRL_IRF8null.clustcoeff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_ctrl_irf8null.clustcoeff,
  "5dpf" = dpf_5_ctrl_irf8null.clustcoeff,
  "6dpf" = dpf_6_ctrl_irf8null.clustcoeff,
  "8dpf" = dpf_8_ctrl_irf8null.clustcoeff
)

# Set column names
colnames(tracked.CTRL_IRF8null.clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null.clustcoeff_long <- pivot_longer(tracked.CTRL_IRF8null.clustcoeff, 
                                                      cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                      names_to = "Age", 
                                                      values_to = "ClustCoeff")

# Plotting
ggplot(tracked.CTRL_IRF8null.clustcoeff_long, aes(x = Age, y = ClustCoeff, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null Clustering Coefficient by Age",
       x = "Age (dpf)",
       y = "Clustering Coefficient") +
  theme(legend.position = "none")


# CTRL_IRF8null Global Efficiency ----

# 4dpf
dpf_4_ctrl_irf8null.globaleff <- c(ID0266.globaleff, ID0267.globaleff, ID0268.globaleff, ID0269.globaleff, 
                                   ID0270.globaleff, ID0271.globaleff, ID0272.globaleff, ID0273.globaleff, ID0274.globaleff)

# 5dpf
dpf_5_ctrl_irf8null.globaleff <- c(ID0275.globaleff, ID0276.globaleff, ID0277.globaleff, ID0278.globaleff, 
                                   ID0279.globaleff, ID0280.globaleff, ID0281.globaleff, ID0282.globaleff, ID0283.globaleff)

# 6dpf
dpf_6_ctrl_irf8null.globaleff <- c(ID0284.globaleff, ID0285.globaleff, ID0286.globaleff, ID0287.globaleff, 
                                   ID0288.globaleff, ID0289.globaleff, ID0290.globaleff, NA, NA)

# 8dpf
dpf_8_ctrl_irf8null.globaleff <- c(ID0291.globaleff, ID0292.globaleff, ID0293.globaleff, ID0294.globaleff, 
                                   ID0295.globaleff, ID0296.globaleff, ID0297.globaleff, NA, NA)

# Combine into a dataframe
tracked.CTRL_IRF8null.globaleff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_ctrl_irf8null.globaleff,
  "5dpf" = dpf_5_ctrl_irf8null.globaleff,
  "6dpf" = dpf_6_ctrl_irf8null.globaleff,
  "8dpf" = dpf_8_ctrl_irf8null.globaleff
)

# Set column names
colnames(tracked.CTRL_IRF8null.globaleff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null.globaleff_long <- pivot_longer(tracked.CTRL_IRF8null.globaleff, 
                                                     cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                     names_to = "Age", 
                                                     values_to = "GlobalEff")

# Plotting
ggplot(tracked.CTRL_IRF8null.globaleff_long, aes(x = Age, y = GlobalEff, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null Global Efficiency by Age",
       x = "Age (dpf)",
       y = "Global Efficiency") +
  theme(legend.position = "none")

# Comparisons --------
# Frequency comparison
# Merge the three dataframes for FREQUENCY
CTRL_CTRLnull_frequency_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_frequency_long2),
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null_frequency_long)
)
CTRL_CTRLnull_frequency_merged_data.df <- na.omit(CTRL_CTRLnull_frequency_merged_data.df)

CTRL_CTRLnull_frequency_merged_data.df.plt <- ggboxplot(CTRL_CTRLnull_frequency_merged_data.df, x = "Age", y = "Frequency",
                                                            fill = "Group",
                                                            palette = c("CTRL" = "blue", "CTRL_IRF8null" = "cyan"),
                                                            add = "boxplot",
                                                            title = "Frequency Comparison: CTRL vs CTRL_IRF8mut",
                                                            xlab = "Age",
                                                            ylab = "Events/min")+
  theme_pubr()+
  theme(axis.title.x = element_blank())+
  ylim(0, 0.4)


# Clustering comparison
# Merge the three dataframes for CLUSTCOEFF
CTRL_CTRLnull_clustcoeff_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_clustcoeff_long),
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null.clustcoeff_long)
)
CTRL_CTRLnull_clustcoeff_merged_data.df <- na.omit(CTRL_CTRLnull_clustcoeff_merged_data.df)

CTRL_CTRLnull_clustcoeff_merged_data.df.plt <- ggboxplot(CTRL_CTRLnull_clustcoeff_merged_data.df, x = "Age", y = "ClustCoeff",
                                                        fill = "Group",
                                                        palette = c("CTRL" = "blue", "CTRL_IRF8null" = "cyan"),
                                                        add = "boxplot",
                                                        title = "Clustering Comparison: CTRL vs CTRL_IRF8mut",
                                                        xlab = "Age",
                                                        ylab = "Events/min")+
  theme_pubr()+
  theme(axis.title.x = element_blank())
