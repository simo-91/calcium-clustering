# IRF8null
# CTRL_IRF8null Frequency ----
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


# CTRL_IRF8null.RFP redcell only frequency----
# Correcting for 6dpf and 8dpf by adding NA for missing fish numbers
dpf_4_freq_CTRL_IRF8null.RFP <- c(`ID0266_frequency-RFP`, `ID0267_frequency-RFP`, `ID0268_frequency-RFP`, `ID0269_frequency-RFP`, 
                              `ID0270_frequency-RFP`, `ID0271_frequency-RFP`, `ID0272_frequency-RFP`, `ID0273_frequency-RFP`, `ID0274_frequency-RFP`)

dpf_5_freq_CTRL_IRF8null.RFP <- c(`ID0275_frequency-RFP`, `ID0276_frequency-RFP`, `ID0277_frequency-RFP`, `ID0278_frequency-RFP`, 
                              `ID0279_frequency-RFP`, `ID0280_frequency-RFP`, `ID0281_frequency-RFP`, `ID0282_frequency-RFP`, `ID0283_frequency-RFP`)

dpf_6_freq_CTRL_IRF8null.RFP <- c(`ID0284_frequency-RFP`, `ID0285_frequency-RFP`, `ID0286_frequency-RFP`, `ID0287_frequency-RFP`, 
                              `ID0288_frequency-RFP`, `ID0289_frequency-RFP`, `ID0290_frequency-RFP`, NA, NA) # Added NAs for missing fish numbers

dpf_8_freq_CTRL_IRF8null.RFP <- c(`ID0291_frequency-RFP`, `ID0292_frequency-RFP`, `ID0293_frequency-RFP`, `ID0294_frequency-RFP`, 
                              `ID0295_frequency-RFP`, `ID0296_frequency-RFP`, `ID0297_frequency-RFP`, NA, NA) # Added NAs for missing fish numbers

# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.CTRL_IRF8null.RFP_frequency <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_freq_CTRL_IRF8null.RFP,
  "5dpf" = dpf_5_freq_CTRL_IRF8null.RFP,
  "6dpf" = dpf_6_freq_CTRL_IRF8null.RFP,
  "8dpf" = dpf_8_freq_CTRL_IRF8null.RFP
)

colnames(tracked.CTRL_IRF8null.RFP_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null.RFP_frequency_long <- pivot_longer(tracked.CTRL_IRF8null.RFP_frequency, 
                                                     cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                     names_to = "Age", 
                                                     values_to = "Frequency")

# Step 4: Plotting
ggplot(tracked.CTRL_IRF8null.RFP_frequency_long, aes(x = Age, y = Frequency, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null.RFP Frequency by Age",
       x = "Age (dpf)",
       y = "Frequency") +
  theme(legend.position = "none")



# CTRL_IRF8null Mean Calcium ----
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
dpf_4_ctrl_irf8null.clustcoeff  <- c(ID0266.clustcoeff , ID0267.clustcoeff , ID0268.clustcoeff , ID0269.clustcoeff , 
                                    ID0270.clustcoeff , ID0271.clustcoeff , ID0272.clustcoeff , ID0273.clustcoeff , ID0274.clustcoeff )

# 5dpf
dpf_5_ctrl_irf8null.clustcoeff  <- c(ID0275.clustcoeff , ID0276.clustcoeff , ID0277.clustcoeff , ID0278.clustcoeff , 
                                    ID0279.clustcoeff , ID0280.clustcoeff , ID0281.clustcoeff , ID0282.clustcoeff , ID0283.clustcoeff )

# 6dpf
dpf_6_ctrl_irf8null.clustcoeff  <- c(ID0284.clustcoeff , ID0285.clustcoeff , ID0286.clustcoeff , ID0287.clustcoeff , 
                                    ID0288.clustcoeff , ID0289.clustcoeff , ID0290.clustcoeff , NA, NA)

# 8dpf
dpf_8_ctrl_irf8null.clustcoeff  <- c(ID0291.clustcoeff , ID0292.clustcoeff , ID0293.clustcoeff , ID0294.clustcoeff , 
                                    ID0295.clustcoeff , ID0296.clustcoeff , ID0297.clustcoeff , NA, NA)

# Combine into a dataframe
tracked.CTRL_IRF8null.clustcoeff  <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_ctrl_irf8null.clustcoeff ,
  "5dpf" = dpf_5_ctrl_irf8null.clustcoeff ,
  "6dpf" = dpf_6_ctrl_irf8null.clustcoeff ,
  "8dpf" = dpf_8_ctrl_irf8null.clustcoeff 
)

# Set column names
colnames(tracked.CTRL_IRF8null.clustcoeff ) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

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


# AKT1_IRF8null Frequency ----
# Step 1: Define frequency variables for each dpf and fish number, including NAs for missing data

# Correcting for 6dpf and 8dpf by adding NA for missing fish numbers
dpf_4_freq_AKT1_IRF8null <- c(ID0298_frequency, ID0299_frequency, ID0300_frequency, ID0301_frequency, 
                              ID0302_frequency, ID0303_frequency, ID0304_frequency, ID0305_frequency, ID0306_frequency)

dpf_5_freq_AKT1_IRF8null <- c(ID0307_frequency, ID0308_frequency, ID0309_frequency, ID0310_frequency, 
                              ID0311_frequency, ID0312_frequency, ID0313_frequency, ID0314_frequency, ID0315_frequency)

dpf_6_freq_AKT1_IRF8null <- c(ID0316_frequency, NA, NA, ID0317_frequency, ID0318_frequency, ID0319_frequency, NA,
                              ID0320_frequency, ID0321_frequency) # Added NAs for missing fish numbers

dpf_8_freq_AKT1_IRF8null <- c(ID0322_frequency, NA, NA, ID0323_frequency, NA, ID0324_frequency, NA, ID0325_frequency, 
                              ID0326_frequency) # Added NAs for missing fish numbers

# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.AKT1_IRF8null_frequency <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_freq_AKT1_IRF8null,
  "5dpf" = dpf_5_freq_AKT1_IRF8null,
  "6dpf" = dpf_6_freq_AKT1_IRF8null,
  "8dpf" = dpf_8_freq_AKT1_IRF8null
)

colnames(tracked.AKT1_IRF8null_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.AKT1_IRF8null_frequency_long <- pivot_longer(tracked.AKT1_IRF8null_frequency, 
                                                     cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                     names_to = "Age", 
                                                     values_to = "Frequency")

# Step 4: Plotting
ggplot(tracked.AKT1_IRF8null_frequency_long, aes(x = Age, y = Frequency, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "AKT1_IRF8null Frequency by Age",
       x = "Age (dpf)",
       y = "Frequency") +
  theme(legend.position = "none")


# AKT1_IRF8null.RFP redcell only Frequency ----

dpf_4_freq_AKT1_IRF8null.RFP <- c(`ID0298_frequency-RFP`, `ID0299_frequency-RFP`, `ID0300_frequency-RFP`, `ID0301_frequency-RFP`, 
                              `ID0302_frequency-RFP`, `ID0303_frequency-RFP`, `ID0304_frequency-RFP`, `ID0305_frequency-RFP`, `ID0306_frequency-RFP`)

dpf_5_freq_AKT1_IRF8null.RFP <- c(`ID0307_frequency-RFP`, `ID0308_frequency-RFP`, `ID0309_frequency-RFP`, `ID0310_frequency-RFP`, 
                              `ID0311_frequency-RFP`, `ID0312_frequency-RFP`, `ID0313_frequency-RFP`, `ID0314_frequency-RFP`, `ID0315_frequency-RFP`)

dpf_6_freq_AKT1_IRF8null.RFP <- c(`ID0316_frequency-RFP`, NA, NA, `ID0317_frequency-RFP`, `ID0318_frequency-RFP`, `ID0319_frequency-RFP`, NA,
                              `ID0320_frequency-RFP`, `ID0321_frequency-RFP`) # Added NAs for missing fish numbers

dpf_8_freq_AKT1_IRF8null.RFP <- c(`ID0322_frequency-RFP`, NA, NA, `ID0323_frequency-RFP`, NA, `ID0324_frequency-RFP`, NA, `ID0325_frequency-RFP`, 
                              `ID0326_frequency-RFP`) # Added NAs for missing fish numbers

# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.AKT1_IRF8null.RFP_frequency <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_freq_AKT1_IRF8null.RFP,
  "5dpf" = dpf_5_freq_AKT1_IRF8null.RFP,
  "6dpf" = dpf_6_freq_AKT1_IRF8null.RFP,
  "8dpf" = dpf_8_freq_AKT1_IRF8null.RFP
)

colnames(tracked.AKT1_IRF8null.RFP_frequency) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.AKT1_IRF8null.RFP_frequency_long <- pivot_longer(tracked.AKT1_IRF8null.RFP_frequency, 
                                                     cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                     names_to = "Age", 
                                                     values_to = "Frequency")

# Step 4: Plotting
ggplot(tracked.AKT1_IRF8null.RFP_frequency_long, aes(x = Age, y = Frequency, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "AKT1_IRF8null RFP Frequency by Age",
       x = "Age (dpf)",
       y = "Frequency") +
  theme(legend.position = "none")




# AKT1_IRF8null Mean Calcium ----
# Create vectors for each day post-fertilization (dpf) with the corresponding mean calcium variable names
dpf_4_mean_Ca_AKT1_IRF8null <- c(mean(ID0298_mean_Ca), mean(ID0299_mean_Ca), mean(ID0300_mean_Ca), mean(ID0301_mean_Ca), 
                                 mean(ID0302_mean_Ca), mean(ID0303_mean_Ca), mean(ID0304_mean_Ca), mean(ID0305_mean_Ca), mean(ID0306_mean_Ca))

dpf_5_mean_Ca_AKT1_IRF8null <- c(mean(ID0307_mean_Ca), mean(ID0308_mean_Ca), mean(ID0309_mean_Ca), mean(ID0310_mean_Ca), 
                                 mean(ID0311_mean_Ca), mean(ID0312_mean_Ca), mean(ID0313_mean_Ca), mean(ID0314_mean_Ca), mean(ID0315_mean_Ca))

dpf_6_mean_Ca_AKT1_IRF8null <- c(mean(ID0316_mean_Ca), NA, NA, mean(ID0317_mean_Ca), mean(ID0318_mean_Ca), mean(ID0319_mean_Ca), NA,
                                 mean(ID0320_mean_Ca), mean(ID0321_mean_Ca)) # Added NAs for missing fish numbers

dpf_8_mean_Ca_AKT1_IRF8null <- c(mean(ID0322_mean_Ca), NA, NA, mean(ID0323_mean_Ca), NA, mean(ID0324_mean_Ca), NA, mean(ID0325_mean_Ca), 
                                 mean(ID0326_mean_Ca)) # Added NAs for missing fish numbers


# Combine into a dataframe
tracked.AKT1_IRF8null_mean_Ca <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_mean_Ca_AKT1_IRF8null,
  "5dpf" = dpf_5_mean_Ca_AKT1_IRF8null,
  "6dpf" = dpf_6_mean_Ca_AKT1_IRF8null,
  "8dpf" = dpf_8_mean_Ca_AKT1_IRF8null
)

# Set column names
colnames(tracked.AKT1_IRF8null_mean_Ca) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1_IRF8null_mean_Ca_long <- pivot_longer(tracked.AKT1_IRF8null_mean_Ca, 
                                                   cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                   names_to = "Age", 
                                                   values_to = "Mean_Ca")

# Plotting with ggplot2 (adjusted from ggline to ggplot due to previous use of ggline which might not be directly recognized)
ggplot(tracked.AKT1_IRF8null_mean_Ca_long, aes(x = Age, y = Mean_Ca, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "AKT1_IRF8null Mean Calcium Levels by Age",
       x = "Age (dpf)",
       y = "Mean Calcium Level") +
  theme(legend.position = "none")




# COMPARISONS --------
# Frequency comparison ----
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




# AKT1 vs AKT1_IRF8null
AKT1_AKT1null_frequency_merged_data.df <- rbind(
  data.frame(Group = "AKT1", tracked.AKT1_frequency_long2),
  data.frame(Group = "AKT1_IRF8null", tracked.AKT1_IRF8null_frequency_long)
)

results_AKT1_vs_AKT1_IRF8null <- AKT1_AKT1null_frequency_merged_data.df %>%
  filter(Group %in% c("AKT1", "AKT1_IRF8null")) %>%
  group_by(Age) %>%
  summarise(p_value = t.test(Frequency[Group == "AKT1"], Frequency[Group == "AKT1_IRF8null"])$p.value) %>%
  mutate(label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ ""
  ),
  ypos = 1.2)

AKT1_AKT1null_frequency_merged_data.df.plt <- ggboxplot(AKT1_AKT1null_frequency_merged_data.df, x = "Age", y = "Frequency",
                                                        fill = "Group",
                                                        palette = c("AKT1" = "red", "AKT1_IRF8null" = "orange"),
                                                        add = "boxplot",
                                                        title = "Frequency Comparison: AKT1 vs AKT1 IRF8null",
                                                        xlab = "Age",
                                                        ylab = "Events/min")+
                                                        theme_pubr()+
                                                        theme(axis.title.x = element_blank())+
                                                        ylim(0,1.3)+
                  geom_text(data = results_AKT1_vs_AKT1_IRF8null, aes(x = Age, y = ypos, label = label), inherit.aes = FALSE)






# CTRL vs CTRL_IRF8null vs AKT1 vs AKT1_IRF8null frequency comparison ----
CTRL_CTRLnull_AKT1_AKT1null_frequency_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_frequency_long2),
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null_frequency_long),
  data.frame(Group = "AKT1", tracked.AKT1_frequency_long2),
  data.frame(Group = "AKT1null", tracked.AKT1_IRF8null_frequency_long)
)

CTRL_CTRLnull_AKT1_AKT1null_frequency_merged_data.df.plt <- ggboxplot(CTRL_CTRLnull_AKT1_AKT1null_frequency_merged_data.df, x = "Age", y = "Frequency",
                                                                      fill = "Group",
                                                                      palette = c("CTRL" = "blue", "CTRL_IRF8null" = "cyan", "AKT1" = "red", "AKT1null" = "orange"),
                                                                      add = "boxplot",
                                                                      title = "Frequency Comparison: CTRL vs CTRL_IRF8null vs AKT1 vs AKT1null",
                                                                      xlab = "Age",
                                                                      ylab = "Events/min")+
                                                                    theme_pubr()+
                                                                    theme(axis.title.x = element_blank())+
                                                                    ylim(0,1.25)


# Redcells only frequency comparison ----
CTRL.RFP_CTRLnull.RFP_AKT1.RFP_AKT1null.RFP_frequency_merged_data.df <- rbind(
  data.frame(Group = "CTRL.RFP", tracked.CTRL_frequency.RFP_long2),
  data.frame(Group = "CTRL_IRF8null.RFP", tracked.CTRL_IRF8null.RFP_frequency_long),
  data.frame(Group = "AKT1.RFP", tracked.AKT1.frequency.RFP_long2),
  data.frame(Group = "AKT1_IRF8null.RFP", tracked.AKT1_IRF8null.RFP_frequency_long)
)

CTRL.RFP_CTRLnull.RFP_AKT1.RFP_AKT1null.RFP_frequency_merged_data.df$RFP <- "TRUE"

CTRL.RFP_CTRLnull.RFP_AKT1.RFP_AKT1null.RFP_frequency_merged_data.df.plt <- ggplot(CTRL.RFP_CTRLnull.RFP_AKT1.RFP_AKT1null.RFP_frequency_merged_data.df, aes(x = Age, y = Frequency, fill = Group, pattern = RFP)) +
  geom_boxplot_pattern(aes(pattern = RFP, fill = Group),
    pattern = "stripe", # Applying stripe pattern to all
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.05,
    pattern_color = "red", # All stripes in red
  )+
  scale_fill_manual(values = c("CTRL.RFP" = "blue", "CTRL_IRF8null.RFP" = "cyan", "AKT1.RFP" = "red", "AKT1_IRF8null.RFP" = "orange")) +
  labs(title = "Frequency Comparison (RFP-only): CTRL vs CTRL_IRF8null vs AKT1 vs AKT1null",
       x = "Age",
       y = "Events/min") +
  theme_pubr() +
  theme(axis.title.x = element_blank())



# Mean Calcium comparisons ----
CTRL_CTRLnull_AKT1_AKT1null_calcium_merged_data.df <- rbind(
  data.frame(Group = "CTRL", tracked.CTRL_mean_Ca_long),
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null_mean_Ca_long),
  data.frame(Group = "AKT1", tracked.AKT1_mean_Ca_long),
  data.frame(Group = "AKT1_IRF8null", tracked.AKT1_IRF8null_mean_Ca_long)
)

ggboxplot(CTRL_CTRLnull_AKT1_AKT1null_calcium_merged_data.df, x = "Age", y = "Mean.Ca",
          fill = "Group",
          palette = c("CTRL" = "blue", "CTRL_IRF8null" = "cyan", "AKT1" = "red", "AKT1_IRF8null" = "orange"),
          add = "boxplot",
          title = "Mean Calcium: CTRL vs CTRL_IRF8null vs AKT1 vs AKT1null",
          xlab = "Age",
          ylab = "Ca2+")+
  theme_pubr()+
  theme(axis.title.x = element_blank())



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
                                                        ylab = "C(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())







# Clustering Coefficients ----
# Correcting for 6dpf and 8dpf by adding NA for missing fish numbers
dpf_4_.clustcoeff_AKT1_IRF8null <- c(ID0298.clustcoeff, ID0299.clustcoeff, ID0300.clustcoeff, ID0301.clustcoeff, 
                              ID0302.clustcoeff, ID0303.clustcoeff, ID0304.clustcoeff, ID0305.clustcoeff, ID0306.clustcoeff)

dpf_5_.clustcoeff_AKT1_IRF8null <- c(ID0307.clustcoeff, ID0308.clustcoeff, ID0309.clustcoeff, ID0310.clustcoeff, 
                              ID0311.clustcoeff, ID0312.clustcoeff, ID0313.clustcoeff, ID0314.clustcoeff, ID0315.clustcoeff)

dpf_6_.clustcoeff_AKT1_IRF8null <- c(ID0316.clustcoeff, NA, NA, ID0317.clustcoeff, ID0318.clustcoeff, ID0319.clustcoeff, NA,
                              ID0320.clustcoeff, ID0321.clustcoeff) # Added NAs for missing fish numbers

dpf_8_.clustcoeff_AKT1_IRF8null <- c(ID0322.clustcoeff, NA, NA, ID0323.clustcoeff, NA, ID0324.clustcoeff, NA, ID0325.clustcoeff, 
                              ID0326.clustcoeff) # Added NAs for missing fish numbers


# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.AKT1_IRF8null.clustcoeff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_.clustcoeff_AKT1_IRF8null,
  "5dpf" = dpf_5_.clustcoeff_AKT1_IRF8null,
  "6dpf" = dpf_6_.clustcoeff_AKT1_IRF8null,
  "8dpf" = dpf_8_.clustcoeff_AKT1_IRF8null
)

colnames(tracked.AKT1_IRF8null.clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.AKT1_IRF8null.clustcoeff_long <- pivot_longer(tracked.AKT1_IRF8null.clustcoeff, 
                                                         cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                         names_to = "Age", 
                                                         values_to = "ClustCoeff")

# Step 4: Plotting
ggplot(tracked.AKT1_IRF8null.clustcoeff_long, aes(x = Age, y = ClustCoeff, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "AKT1_IRF8null ClustCoeff by Age",
       x = "Age (dpf)",
       y = "ClustCoeff") +
  theme(legend.position = "none")

# Clustering Coefficients redcells/RFP+ only ----
dpf_4_.clustcoeff.RFP_AKT1_IRF8null <- c(ID0298.clustcoeff.RFP, ID0299.clustcoeff.RFP, ID0300.clustcoeff.RFP, ID0301.clustcoeff.RFP, 
                                     ID0302.clustcoeff.RFP, ID0303.clustcoeff.RFP, ID0304.clustcoeff.RFP, ID0305.clustcoeff.RFP, ID0306.clustcoeff.RFP)

dpf_5_.clustcoeff.RFP_AKT1_IRF8null <- c(ID0307.clustcoeff.RFP, ID0308.clustcoeff.RFP, ID0309.clustcoeff.RFP, ID0310.clustcoeff.RFP, 
                                     ID0311.clustcoeff.RFP, ID0312.clustcoeff.RFP, ID0313.clustcoeff.RFP, ID0314.clustcoeff.RFP, ID0315.clustcoeff.RFP)

dpf_6_.clustcoeff.RFP_AKT1_IRF8null <- c(ID0316.clustcoeff.RFP, NA, NA, ID0317.clustcoeff.RFP, ID0318.clustcoeff.RFP, ID0319.clustcoeff.RFP, NA,
                                     ID0320.clustcoeff.RFP, ID0321.clustcoeff.RFP) # Added NAs for missing fish numbers

dpf_8_.clustcoeff.RFP_AKT1_IRF8null <- c(ID0322.clustcoeff.RFP, NA, NA, ID0323.clustcoeff.RFP, NA, ID0324.clustcoeff.RFP, NA, ID0325.clustcoeff.RFP, 
                                     ID0326.clustcoeff.RFP) # Added NAs for missing fish numbers


# Step 2: Combine into a dataframe ensuring all vectors have the same length
tracked.AKT1_IRF8null.RFP.clustcoeff <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_.clustcoeff.RFP_AKT1_IRF8null,
  "5dpf" = dpf_5_.clustcoeff.RFP_AKT1_IRF8null,
  "6dpf" = dpf_6_.clustcoeff.RFP_AKT1_IRF8null,
  "8dpf" = dpf_8_.clustcoeff.RFP_AKT1_IRF8null
)

colnames(tracked.AKT1_IRF8null.RFP.clustcoeff) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Step 3: Reshape the data to long format for plotting or analysis
tracked.AKT1_IRF8null.RFP.clustcoeff_long <- pivot_longer(tracked.AKT1_IRF8null.RFP.clustcoeff, 
                                                          cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                          names_to = "Age", 
                                                          values_to = "ClustCoeff")

# Step 4: Plotting
ggplot(tracked.AKT1_IRF8null.RFP.clustcoeff_long, aes(x = Age, y = ClustCoeff, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "AKT1_IRF8null RFP ClustCoeff by Age",
       x = "Age (dpf)",
       y = "ClustCoeff") +
  theme(legend.position = "none")

# AKT1.RFP vs AKT1.RFP_IRF8null C(g) ----
tracked.AKT1_vs_AKT1_IRF8null.RFP.clustcoeff <- rbind(
  data.frame(Group = "AKT1_IRF8null", tracked.AKT1_IRF8null.clustcoeff_long),
  data.frame(Group = "AKT1.RFP_IRF8null", tracked.AKT1_IRF8null.RFP.clustcoeff_long)
)

ggboxplot(tracked.AKT1_vs_AKT1_IRF8null.RFP.clustcoeff, x = "Age", y = "ClustCoeff",
          fill = "Group",
          palette = c("AKT1.RFP_IRF8null" = "orange","AKT1_IRF8null" = "red"),
          add = "boxplot",
          title = "ClustCoeff Comparison: CTRL_IRF8null vs AKT1_IRF8null",
          xlab = "Age",
          ylab = "Average C(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())



# Clustering coefficient C(g) comparison with IRF8-/- mutants ----
tracked.CTRL_vs_AKT1_vs_CTRL_IRF8null_AKT1_IRF8null.clustcoeff <- rbind(
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null.clustcoeff_long),
  data.frame(Group = "AKT1_IRF8null", tracked.AKT1_IRF8null.clustcoeff_long),
  data.frame(Group = "AKT1", tracked.AKT1.clustcoeff_long),
  data.frame(Group = "CTRL", tracked.CTRL.clustcoeff_long)
)


tracked.CTRL_vs_AKT1_vs_CTRL_IRF8null_AKT1_IRF8null.clustcoeff.plt <- ggboxplot(tracked.CTRL_vs_AKT1_vs_CTRL_IRF8null_AKT1_IRF8null.clustcoeff, x = "Age", y = "ClustCoeff",
          fill = "Group",
          palette = c("AKT1_IRF8null" = "orange", "CTRL_IRF8null" = "cyan", "AKT1" = "red", "CTRL" = "blue"),
          add = "boxplot",
          title = "ClustCoeff Comparison: CTRL_IRF8null vs AKT1_IRF8null",
          xlab = "Age",
          ylab = "Average C(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())+
  geom_text(aes(x = 4.20, y = 0.9, label = "ns"), color = "black", size = 5)

# C(g) for RFP only
# CTRL IRF8null RFP C(g) ----
# 4dpf
dpf_4_ctrl_irf8.clustcoeff.RFP  <- c(ID0266.clustcoeff.RFP , ID0267.clustcoeff.RFP , ID0268.clustcoeff.RFP , ID0269.clustcoeff.RFP , 
                                       ID0270.clustcoeff.RFP , ID0271.clustcoeff.RFP , ID0272.clustcoeff.RFP , ID0273.clustcoeff.RFP , ID0274.clustcoeff.RFP )

# 5dpf
dpf_5_ctrl_irf8.clustcoeff.RFP  <- c(ID0275.clustcoeff.RFP , ID0276.clustcoeff.RFP , ID0277.clustcoeff.RFP , ID0278.clustcoeff.RFP , 
                                     ID0279.clustcoeff.RFP , ID0280.clustcoeff.RFP , ID0281.clustcoeff.RFP , ID0282.clustcoeff.RFP , ID0283.clustcoeff.RFP )

# 6dpf
dpf_6_ctrl_irf8.clustcoeff.RFP  <- c(ID0284.clustcoeff.RFP , ID0285.clustcoeff.RFP , ID0286.clustcoeff.RFP , ID0287.clustcoeff.RFP , 
                                     ID0288.clustcoeff.RFP , ID0289.clustcoeff.RFP , ID0290.clustcoeff.RFP , NA, NA)

# 8dpf
dpf_8_ctrl_irf8.clustcoeff.RFP  <- c(ID0291.clustcoeff.RFP , ID0292.clustcoeff.RFP , ID0293.clustcoeff.RFP , ID0294.clustcoeff.RFP , 
                                     ID0295.clustcoeff.RFP , ID0296.clustcoeff.RFP , ID0297.clustcoeff.RFP , NA, NA)

# Combine into a dataframe
tracked.CTRL_IRF8null.clustcoeff.RFP <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_ctrl_irf8.clustcoeff.RFP ,
  "5dpf" = dpf_5_ctrl_irf8.clustcoeff.RFP ,
  "6dpf" = dpf_6_ctrl_irf8.clustcoeff.RFP ,
  "8dpf" = dpf_8_ctrl_irf8.clustcoeff.RFP 
)
tracked.CTRL_IRF8null.clustcoeff.RFP[] <- lapply(tracked.CTRL_IRF8null.clustcoeff.RFP, function(x) replace(x, is.na(x), 0))
colnames(tracked.CTRL_IRF8null.clustcoeff.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")
# Reshape the data to long format for plotting or analysis
tracked.CTRL_IRF8null.clustcoeff.RFP_long <- pivot_longer(tracked.CTRL_IRF8null.clustcoeff.RFP, 
                                                      cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                      names_to = "Age", 
                                                      values_to = "ClustCoeff")

# Plotting
ggplot(tracked.CTRL_IRF8null.clustcoeff.RFP_long, aes(x = Age, y = ClustCoeff, group = `fish no`, color = `fish no`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CTRL_IRF8null RFP Clustering Coefficient by Age",
       x = "Age (dpf)",
       y = "Clustering Coefficient") +
  theme(legend.position = "none")

# C(g) comparison among genos between RFP only ----
tracked.CTRL.RFP_vs_AKT1.RFP_vs_CTRL.RFP_IRF8null.RFP_AKT1_IRF8null.RFP.clustcoeff.df <- rbind(
  data.frame(Group = "CTRL_IRF8null", tracked.CTRL_IRF8null.clustcoeff.RFP_long),
  data.frame(Group = "AKT1_IRF8null", tracked.AKT1_IRF8null.clustcoeff.RFP_long),
  data.frame(Group = "AKT1", tracked.AKT1.clustcoeff.RFP_long2),
  data.frame(Group = "CTRL", tracked.CTRL.clustcoeff.RFP_long2)
)

ggboxplot(tracked.CTRL.RFP_vs_AKT1.RFP_vs_CTRL.RFP_IRF8null.RFP_AKT1_IRF8null.RFP.clustcoeff.df, x = "Age", y = "ClustCoeff",
          fill = "Group",
          palette = c("AKT1_IRF8null" = "orange", "CTRL_IRF8null" = "cyan", "AKT1" = "red", "CTRL" = "blue"),
          add = "boxplot",
          title = "C(g) in redcells only",
          xlab = "Age",
          ylab = "Average C(g)")+
  theme_pubr()+
  theme(axis.title.x = element_blank())
