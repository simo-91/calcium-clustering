# Clustering Coefficient RFP
# CTRL Clustering Coefficient ----
dpf_4_clustcoeff.RFP <- c(ID0186.clustcoeff.RFP, ID0187.clustcoeff.RFP, ID0188.clustcoeff.RFP, ID0189.clustcoeff.RFP, 
                     ID0190.clustcoeff.RFP, ID0191.clustcoeff.RFP, ID0192.clustcoeff.RFP, ID0193.clustcoeff.RFP, 
                     ID0194.clustcoeff.RFP, ID0195.clustcoeff.RFP)
dpf_5_clustcoeff.RFP <- c(ID0196.clustcoeff.RFP, ID0197.clustcoeff.RFP, ID0198.clustcoeff.RFP, ID0199.clustcoeff.RFP, 
                     ID0200.clustcoeff.RFP, ID0201.clustcoeff.RFP, ID0202.clustcoeff.RFP, ID0203.clustcoeff.RFP, 
                     ID0204.clustcoeff.RFP, ID0205.clustcoeff.RFP)
dpf_6_clustcoeff.RFP <- c(ID0206.clustcoeff.RFP, ID0207.clustcoeff.RFP, ID0208.clustcoeff.RFP, ID0209.clustcoeff.RFP, 
                     ID0210.clustcoeff.RFP, ID0211.clustcoeff.RFP, ID0212.clustcoeff.RFP, NA, 
                     ID0213.clustcoeff.RFP, ID0214.clustcoeff.RFP)
dpf_8_clustcoeff.RFP <- c(ID0215.clustcoeff.RFP, NA, ID0216.clustcoeff.RFP, ID0217.clustcoeff.RFP, ID0218.clustcoeff.RFP, 
                     ID0219.clustcoeff.RFP, ID0220.clustcoeff.RFP, NA, NA, ID0221.clustcoeff.RFP)

# Combine into a dataframe
tracked.CTRL.clustcoeff.RFP <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4_clustcoeff.RFP,
  "5dpf" = dpf_5_clustcoeff.RFP,
  "6dpf" = dpf_6_clustcoeff.RFP,
  "8dpf" = dpf_8_clustcoeff.RFP
)

# Set column names
colnames(tracked.CTRL.clustcoeff.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

tracked.CTRL.clustcoeff.RFP[sapply(tracked.CTRL.clustcoeff.RFP, is.numeric)] <- lapply(tracked.CTRL.clustcoeff.RFP[sapply(tracked.CTRL.clustcoeff.RFP, is.numeric)], function(x) ifelse(is.nan(x), 0, x))

# Transform the data to long format
tracked.CTRL_clustcoeff.RFP_long <- melt(tracked.CTRL.clustcoeff.RFP, id.vars = "fish no", variable.name = "Age", value.name = "Clustering Coefficient")

tracked.CTRL.clustcoeff.RFP_long$RFP <- "TRUE"
tracked.CTRL.clustcoeff.RFP_comparison.df <- rbind(tracked.CTRL.clustcoeff_long, tracked.CTRL.clustcoeff.RFP_long)


# Create the boxplot
tracked.CTRL_clustcoeff.RFP_comparison.df.plt <- ggplot(tracked.CTRL.clustcoeff.RFP_comparison.df, aes(x = Age, y = `Clustering Coefficient`, fill = RFP, pattern = RFP)) +
  geom_boxplot_pattern() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  scale_pattern_manual(values = c("FALSE" = "none", "TRUE" = "stripe")) + # Define patterns
  labs(x = " ", y = "C(g)", title = "CTRL") +
  theme_pubr() +
  theme(legend.position = "none")


# Compare RFP-only vs all
tracked.HRASV12.clustcoeff.RFP_comparison.df.plt <- ggplot(tracked.HRASV12.clustcoeff.RFP_comparison.df, aes(x = Age, y = `Clustering Coefficient`, fill = RFP)) +
  geom_boxplot() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(x = " ", y = "Clustering Coefficient", title = "Clustering Coefficient RFP vs all in HRASV12") +
  theme_pubr() +
  theme(legend.position = "top")

# HRASV12 Clustering Coefficient ----
# Fill the 4dpf column
dpf_4_hrasv12.clustcoeff.RFP <- c(ID0222.clustcoeff.RFP, ID0223.clustcoeff.RFP, ID0224.clustcoeff.RFP, ID0225.clustcoeff.RFP, 
                              ID0226.clustcoeff.RFP, ID0227.clustcoeff.RFP, ID0228.clustcoeff.RFP, ID0229.clustcoeff.RFP, 
                              ID0230.clustcoeff.RFP, ID0231.clustcoeff.RFP, ID0247.clustcoeff.RFP, ID0248.clustcoeff.RFP, 
                              ID0249.clustcoeff.RFP, ID0250.clustcoeff.RFP, ID0251.clustcoeff.RFP, ID0252.clustcoeff.RFP, 
                              ID0253.clustcoeff.RFP)
na_vec_4dpf[1:17] <- dpf_4_hrasv12.clustcoeff.RFP

# Fill the 5dpf column
dpf_5_hrasv12.clustcoeff.RFP <- c(ID0232.clustcoeff.RFP, ID0233.clustcoeff.RFP, NA, ID0234.clustcoeff.RFP, ID0235.clustcoeff.RFP, 
                              ID0236.clustcoeff.RFP, NA, NA, ID0237.clustcoeff.RFP, ID0238.clustcoeff.RFP, NA, ID0254.clustcoeff.RFP, 
                              ID0255.clustcoeff.RFP, NA, ID0256.clustcoeff.RFP, ID0257.clustcoeff.RFP, ID0258.clustcoeff.RFP)
na_vec_5dpf[1:17] <- dpf_5_hrasv12.clustcoeff.RFP

# Fill the 6dpf column
dpf_6_hrasv12.clustcoeff.RFP <- c(ID0239.clustcoeff.RFP, NA, NA, ID0240.clustcoeff.RFP, ID0241.clustcoeff.RFP, NA, NA, NA, 
                              ID0242.clustcoeff.RFP, ID0243.clustcoeff.RFP, NA, ID0259.clustcoeff.RFP, ID0260.clustcoeff.RFP, NA, 
                              NA, ID0261.clustcoeff.RFP, ID0262.clustcoeff.RFP)
na_vec_6dpf[1:17] <- dpf_6_hrasv12.clustcoeff.RFP

# Fill the 8dpf column
dpf_8_hrasv12.clustcoeff.RFP <- c(ID0244.clustcoeff.RFP, NA, NA, ID0245.clustcoeff.RFP, ID0246.clustcoeff.RFP, NA, NA, NA, NA, NA, 
                              NA, ID0263.clustcoeff.RFP, ID0264.clustcoeff.RFP, NA, NA, NA, ID0265.clustcoeff.RFP)
na_vec_8dpf[1:17] <- dpf_8_hrasv12.clustcoeff.RFP

# Combine into a dataframe
tracked.HRASV12.clustcoeff.RFP <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12.clustcoeff.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")
tracked.HRASV12.clustcoeff.RFP[sapply(tracked.HRASV12.clustcoeff.RFP, is.numeric)] <- lapply(tracked.HRASV12.clustcoeff.RFP[sapply(tracked.HRASV12.clustcoeff.RFP, is.numeric)], function(x) ifelse(is.nan(x), 0, x))

tracked.HRASV12.clustcoeff.RFP_long <- melt(tracked.HRASV12.clustcoeff.RFP, id.vars = "fish no", variable.name = "Age", value.name = "Clustering Coefficient")
tracked.HRASV12.clustcoeff.RFP_long$RFP <- "TRUE"

tracked.HRASV12.clustcoeff.RFP_comparison.df <- rbind(tracked.HRASV12.clustcoeff_long, tracked.HRASV12.clustcoeff.RFP_long)

# Create the boxplot
tracked.HRASV12.clustcoeff.RFP_long.plt <- ggplot(tracked.HRASV12.clustcoeff.RFP_long, aes(x = Age, y = `Clustering Coefficient`, fill = Age)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.7) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Age (dpf)", y = "Clustering Coefficient", title = "Clustering Coefficient RFP-only in HRASV12") +
  theme_pubr() +
  theme(legend.position = "none")

tracked.HRASV12.clustcoeff.RFP_comparison.df.plt <- ggplot(tracked.HRASV12.clustcoeff.RFP_comparison.df, aes(x = Age, y = `Clustering Coefficient`, fill = RFP, pattern = RFP)) +
  geom_boxplot_pattern() +
  scale_fill_manual(values = c("FALSE" = "purple", "TRUE" = "red")) +
  scale_pattern_manual(values = c("FALSE" = "none", "TRUE" = "stripe")) + # Define patterns
  labs(x = " ", y = "C(g)", title = "HRASV12") +
  theme_pubr() +
  theme(legend.position = "none")


# AKT1 Clustering Coefficient ----
dpf_4.clustcoeff.RFP_AKT1 <- c(ID0152.clustcoeff.RFP, ID0153.clustcoeff.RFP, ID0154.clustcoeff.RFP, ID0155.clustcoeff.RFP, 
                     ID0156.clustcoeff.RFP, ID0157.clustcoeff.RFP, NA, ID0159.clustcoeff.RFP, ID0160.clustcoeff.RFP)
dpf_5.clustcoeff.RFP_AKT1 <- c(ID0161.clustcoeff.RFP, ID0162.clustcoeff.RFP, ID0163.clustcoeff.RFP, ID0164.clustcoeff.RFP, 
                     ID0165.clustcoeff.RFP, ID0166.clustcoeff.RFP, NA, ID0168.clustcoeff.RFP, ID0169.clustcoeff.RFP)
dpf_6.clustcoeff.RFP_AKT1 <- c(ID0170.clustcoeff.RFP, ID0171.clustcoeff.RFP, ID0172.clustcoeff.RFP, ID0173.clustcoeff.RFP, 
                     ID0174.clustcoeff.RFP, NA, NA, ID0176.clustcoeff.RFP, ID0177.clustcoeff.RFP)
dpf_8.clustcoeff.RFP_AKT1 <- c(ID0178.clustcoeff.RFP, ID0179.clustcoeff.RFP, NA, ID0181.clustcoeff.RFP, ID0182.clustcoeff.RFP, 
                     NA, NA, ID0184.clustcoeff.RFP, ID0185.clustcoeff.RFP)

# Combine into a dataframe
tracked.AKT1.clustcoeff.RFP <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4.clustcoeff.RFP_AKT1,
  "5dpf" = dpf_5.clustcoeff.RFP_AKT1,
  "6dpf" = dpf_6.clustcoeff.RFP_AKT1,
  "8dpf" = dpf_8.clustcoeff.RFP_AKT1
)

colnames(tracked.AKT1.clustcoeff.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")
tracked.AKT1.clustcoeff.RFP[sapply(tracked.AKT1.clustcoeff.RFP, is.numeric)] <- lapply(tracked.AKT1.clustcoeff.RFP[sapply(tracked.AKT1.clustcoeff.RFP, is.numeric)], function(x) ifelse(is.nan(x), 0, x))

# Reshape the data to long format for plotting or analysis
tracked.AKT1.clustcoeff.RFP_long <- pivot_longer(tracked.AKT1.clustcoeff.RFP, 
                                            cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                            names_to = "Age", 
                                            values_to = "Clustering Coefficient")

tracked.AKT1.clustcoeff.RFP_long.plt <- ggplot(tracked.AKT1.clustcoeff.RFP_long, aes(x = Age, y = `Clustering Coefficient`, fill = Age)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = "black", size = 1.5, alpha = 0.7) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Age (dpf)", y = "Clustering Coefficient", title = "Clustering Coefficient RFP-only in AKT1") +
  theme_pubr() +
  theme(legend.position = "none")

# RFP vs general population in AKT1
tracked.AKT1.clustcoeff.RFP_comparison.df.plt <- ggplot(tracked.AKT1.clustcoeff.RFP_comparison.df, aes(x = Age, y = `Clustering Coefficient`, fill = RFP, pattern = RFP)) +
  geom_boxplot_pattern() + # Use geom_boxplot_pattern for adding patterns
  scale_fill_manual(values = c("FALSE" = "#de2d26", "TRUE" = "red")) +
  scale_pattern_manual(values = c("FALSE" = "none", "TRUE" = "stripe")) + # Define patterns
  labs(x = " ", y = "C(g)", title = "AKT1") +
  theme_pubr() +
  theme(legend.position = "none")



# Across genos
tracked.CTRL.clustcoeff.RFP_long$Condition <- "CTRL"
tracked.AKT1.clustcoeff.RFP_long$Condition <- "AKT1"
tracked.HRASV12.clustcoeff.RFP_long$Condition <- "HRASV12"

# Combine the data frames using rbind
CTRL_AKT1_HRASV12.clustcoeff.RFP.df <- rbind(tracked.CTRL.clustcoeff.RFP_long, 
                                             tracked.AKT1.clustcoeff.RFP_long, 
                                             tracked.HRASV12.clustcoeff.RFP_long)


CTRL_AKT1_HRASV12.clustcoeff.RFP.df.plt <- ggplot(CTRL_AKT1_HRASV12.clustcoeff.RFP.df, aes(x = Age, y = `Clustering Coefficient`, fill = Condition)) +
  geom_boxplot_pattern() +
  scale_fill_manual(values = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple")) +
  labs(x = " ", y = "Clustering Coefficient", title = "Clustering Coefficient RFP-only cells") +
  theme_pubr() +
  theme(legend.position = "top")










# Frequency of events --------
# CTRL Clustering Coefficient ----
dpf_4_frequency.RFP <- c(`ID0186_frequency.RFP`, `ID0187_frequency.RFP`, `ID0188_frequency.RFP`, `ID0189_frequency.RFP`, 
                          `ID0190_frequency.RFP`, `ID0191_frequency.RFP`, `ID0192_frequency.RFP`, `ID0193_frequency.RFP`, 
                          `ID0194_frequency.RFP`, `ID0195_frequency.RFP`)
dpf_5_frequency.RFP <- c(`ID0196_frequency.RFP`, `ID0197_frequency.RFP`, `ID0198_frequency.RFP`, `ID0199_frequency.RFP`, 
                          `ID0200_frequency.RFP`, `ID0201_frequency.RFP`, `ID0202_frequency.RFP`, `ID0203_frequency.RFP`, 
                          `ID0204_frequency.RFP`, `ID0205_frequency.RFP`)
dpf_6_frequency.RFP <- c(`ID0206_frequency.RFP`, `ID0207_frequency.RFP`, `ID0208_frequency.RFP`, `ID0209_frequency.RFP`, 
                          `ID0210_frequency.RFP`, `ID0211_frequency.RFP`, `ID0212_frequency.RFP`, NA, 
                          `ID0213_frequency.RFP`, `ID0214_frequency.RFP`)
dpf_8_frequency.RFP <- c(`ID0215_frequency.RFP`, NA, `ID0216_frequency.RFP`, `ID0217_frequency.RFP`, `ID0218_frequency.RFP`, 
                          `ID0219_frequency.RFP`, `ID0220_frequency.RFP`, NA, NA, `ID0221_frequency.RFP`)

# Combine into a dataframe
tracked.CTRL_frequency.RFP <- data.frame(
  "fish no" = fish_names,
  "4dpf" = dpf_4_frequency.RFP,
  "5dpf" = dpf_5_frequency.RFP,
  "6dpf" = dpf_6_frequency.RFP,
  "8dpf" = dpf_8_frequency.RFP
)

# Set column names
colnames(tracked.CTRL_frequency.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Transform the data to long format
tracked.CTRL_frequency.RFP_long <- melt(tracked.CTRL_frequency.RFP, id.vars = "fish no", variable.name = "Age", value.name = "Events/min")

tracked.CTRL_frequency.RFP_long$RFP <- "TRUE"
tracked.CTRL_frequency_long$RFP <- "FALSE"
tracked.CTRL.frequency.RFP_comparison.df <- rbind(tracked.CTRL_frequency_long, tracked.CTRL_frequency.RFP_long)

# Create the boxplot
tracked.CTRL.frequency.RFP_comparison.df.plt <- ggplot(tracked.CTRL.frequency.RFP_comparison.df, aes(x = Age, y = `Events/min`, fill = RFP, pattern = RFP)) +
  geom_boxplot_pattern() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  scale_pattern_manual(values = c("FALSE" = "none", "TRUE" = "stripe")) + # Define patterns
  labs(x = " ", y = "Events/min", title = "CTRL") +
  theme_pubr() +
  theme(legend.position = "top")


# AKT1 frequency of events/min
dpf_4_frequency.RFP_AKT1 <- c(ID0152_frequency.RFP, ID0153_frequency.RFP, ID0154_frequency.RFP, ID0155_frequency.RFP, 
                               ID0156_frequency.RFP, ID0157_frequency.RFP, NA, ID0159_frequency.RFP, ID0160_frequency.RFP)
dpf_5_frequency.RFP_AKT1 <- c(ID0161_frequency.RFP, ID0162_frequency.RFP, ID0163_frequency.RFP, ID0164_frequency.RFP, 
                               ID0165_frequency.RFP, ID0166_frequency.RFP, NA, ID0168_frequency.RFP, ID0169_frequency.RFP)
dpf_6_frequency.RFP_AKT1 <- c(ID0170_frequency.RFP, ID0171_frequency.RFP, ID0172_frequency.RFP, ID0173_frequency.RFP, 
                               ID0174_frequency.RFP, NA, NA, ID0176_frequency.RFP, ID0177_frequency.RFP)
dpf_8_frequency.RFP_AKT1 <- c(ID0178_frequency.RFP, ID0179_frequency.RFP, NA, ID0181_frequency.RFP, ID0182_frequency.RFP, 
                               NA, NA, ID0184_frequency.RFP, ID0185_frequency.RFP)

# Combine into a dataframe
tracked.AKT1.frequency.RFP <- data.frame(
  "fish no" = c("fish1", "fish2", "fish3", "fish4", "fish5", "fish6", "fish7", "fish8", "fish9"),
  "4dpf" = dpf_4_frequency.RFP_AKT1,
  "5dpf" = dpf_5_frequency.RFP_AKT1,
  "6dpf" = dpf_6_frequency.RFP_AKT1,
  "8dpf" = dpf_8_frequency.RFP_AKT1
)

colnames(tracked.AKT1.frequency.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")

# Reshape the data to long format for plotting or analysis
tracked.AKT1.frequency.RFP_long <- pivot_longer(tracked.AKT1.frequency.RFP, 
                                                 cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                 names_to = "Age", 
                                                 values_to = "Events/min")

tracked.AKT1.frequency.RFP_long$RFP <- "TRUE"
tracked.AKT1_frequency_long$RFP <- "FALSE"
tracked.AKT1.frequency.RFP_comparison.df <- rbind(tracked.AKT1_frequency_long, tracked.AKT1.frequency.RFP_long)

# Create the boxplot
tracked.AKT1.frequency.RFP_comparison.df.plt <- ggplot(tracked.AKT1.frequency.RFP_comparison.df, aes(x = Age, y = `Events/min`, fill = RFP)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(x = NULL, y = "Events/min", title = "AKT1 events/min") +
  ylim(0, 1.5) +
  theme_pubr() +
  theme(legend.position = "top") +
  stat_compare_means(aes(group = interaction(Age, RFP)), method = "t.test", label = "p.signif")


# HRASV12 events/min RFP
# Fill the 4dpf column
dpf_4_hrasv12_frequency.RFP <- c(ID0222_frequency.RFP, ID0223_frequency.RFP, ID0224_frequency.RFP, ID0225_frequency.RFP, 
                                  ID0226_frequency.RFP, ID0227_frequency.RFP, ID0228_frequency.RFP, ID0229_frequency.RFP, 
                                  ID0230_frequency.RFP, ID0231_frequency.RFP, ID0247_frequency.RFP, ID0248_frequency.RFP, 
                                  ID0249_frequency.RFP, ID0250_frequency.RFP, ID0251_frequency.RFP, ID0252_frequency.RFP, 
                                  ID0253_frequency.RFP)
na_vec_4dpf[1:17] <- dpf_4_hrasv12_frequency.RFP

# Fill the 5dpf column
dpf_5_hrasv12_frequency.RFP <- c(ID0232_frequency.RFP, ID0233_frequency.RFP, NA, ID0234_frequency.RFP, ID0235_frequency.RFP, 
                                  ID0236_frequency.RFP, NA, NA, ID0237_frequency.RFP, ID0238_frequency.RFP, NA, ID0254_frequency.RFP, 
                                  ID0255_frequency.RFP, NA, ID0256_frequency.RFP, ID0257_frequency.RFP, ID0258_frequency.RFP)
na_vec_5dpf[1:17] <- dpf_5_hrasv12_frequency.RFP

# Fill the 6dpf column
dpf_6_hrasv12_frequency.RFP <- c(ID0239_frequency.RFP, NA, NA, ID0240_frequency.RFP, ID0241_frequency.RFP, NA, NA, NA, 
                                  ID0242_frequency.RFP, ID0243_frequency.RFP, NA, ID0259_frequency.RFP, ID0260_frequency.RFP, NA, 
                                  NA, ID0261_frequency.RFP, ID0262_frequency.RFP)
na_vec_6dpf[1:17] <- dpf_6_hrasv12_frequency.RFP

# Fill the 8dpf column
dpf_8_hrasv12_frequency.RFP <- c(ID0244_frequency.RFP, NA, NA, ID0245_frequency.RFP, ID0246_frequency.RFP, NA, NA, NA, NA, NA, 
                                  NA, ID0263_frequency.RFP, ID0264_frequency.RFP, NA, NA, NA, ID0265_frequency.RFP)
na_vec_8dpf[1:17] <- dpf_8_hrasv12_frequency.RFP

# Combine into a dataframe
tracked.HRASV12.frequency.RFP <- data.frame(
  "fish no" = fish_names_hrasv12,
  "4dpf" = na_vec_4dpf,
  "5dpf" = na_vec_5dpf,
  "6dpf" = na_vec_6dpf,
  "8dpf" = na_vec_8dpf
)

# Correct the column names if necessary
colnames(tracked.HRASV12.frequency.RFP) <- c("fish no", "4dpf", "5dpf", "6dpf", "8dpf")


# Reshape the data to long format for plotting or analysis
tracked.HRASV12.frequency.RFP_long <- pivot_longer(tracked.HRASV12.frequency.RFP, 
                                                cols = c("4dpf", "5dpf", "6dpf", "8dpf"), 
                                                names_to = "Age", 
                                                values_to = "Events/min")

tracked.HRASV12.frequency.RFP_long$RFP <- "TRUE"
tracked.HRASV12.frequency_long$RFP <- "FALSE"
tracked.HRASV12.frequency.RFP_comparison.df <- rbind(tracked.HRASV12.frequency_long, tracked.HRASV12.frequency.RFP_long)

# Create the boxplot
tracked.HRASV12.frequency.RFP_comparison.df.plt <- ggplot(tracked.HRASV12.frequency.RFP_comparison.df, aes(x = Age, y = `Events/min`, fill = RFP, pattern = RFP)) +
  geom_boxplot_pattern(pattern_spacing = 0.02, outlier.shape = NA) +
  scale_fill_manual(values = c("FALSE" = "purple", "TRUE" = "red")) +
  scale_pattern_manual(values = c("FALSE" = "none", "TRUE" = "stripe")) + # Define patterns
  labs(x = " ", y = "Events/min", title = "HRASV12") +
  # ylim(0, 0.5)+
  theme_pubr() +
  theme(legend.position = "top")


# Comparison between conditions through age
tracked.CTRL.frequency.RFP_long$Condition <- "CTRL"
tracked.AKT1.frequency.RFP_long$Condition <- "AKT1"
tracked.HRASV12.frequency.RFP_long$Condition <- "HRASV12"

# Combine the data frames using rbind
CTRL_AKT1_HRASV12.frequency.RFP.df <- rbind(tracked.CTRL.frequency.RFP_long, 
                                             tracked.AKT1.frequency.RFP_long, 
                                             tracked.HRASV12.frequency.RFP_long)


CTRL_AKT1_HRASV12.frequency.RFP.df.plt <- ggplot(CTRL_AKT1_HRASV12.frequency.RFP.df, aes(x = Age, y = `Events/min`, fill = Condition)) +
  geom_boxplot_pattern() +
  scale_fill_manual(values = c("CTRL" = "blue", "AKT1" = "red", "HRASV12" = "purple")) +
  labs(x = " ", y = "Events/min", title = "Events/min RFP-only cells") +
  theme_pubr() +
  theme(legend.position = "top")


# IRF8null
# CTRL vs CTRL_IRF8null vs AKT1 vs AKT1_IRF8null frequency comparison, redcell only ----



CTRL_CTRLnull_AKT1_AKT1null_frequency.RFP_merged_data.df <- rbind(
  data.frame(Group = "CTRL.RFP", tracked.CTRL_frequency.RFP_long2),
  data.frame(Group = "CTRL_IRF8null.RFP", tracked.CTRL_IRF8null.RFP_frequency_long),
  data.frame(Group = "AKT1.RFP", tracked.AKT1.frequency.RFP_long2),
  data.frame(Group = "AKT1_IRF8null.RFP", tracked.AKT1_IRF8null.RFP_frequency_long)
)


CTRL_CTRLnull_AKT1_AKT1null_frequency.RFP_merged_data.df <- na.omit(CTRL_CTRLnull_AKT1_AKT1null_frequency.RFP_merged_data.df)


ggboxplot(CTRL_CTRLnull_AKT1_AKT1null_frequency.RFP_merged_data.df, x = "Age", y = "Frequency",
          fill = "Group",
          palette = c("CTRL.RFP" = "blue", "CTRL_IRF8null.RFP" = "cyan", "AKT1.RFP" = "red", "AKT1_IRF8null.RFP" = "orange"),
          add = "boxplot",
          title = "Frequency Redcells: CTRL vs CTRL_IRF8null vs AKT1 vs AKT1null",
          xlab = "Age",
          ylab = "Events/min")+
  theme_pubr()+
  theme(axis.title.x = element_blank())


comparison1 <- CTRL_CTRLnull_AKT1_AKT1null_frequency.RFP_merged_data.df %>%
  filter(Group %in% c("AKT1.RFP", "AKT1_IRF8null.RFP")) %>%  # Filter for specific groups
  select(Group, Frequency)  # Select desired columns




