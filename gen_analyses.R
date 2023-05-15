CTRLs_4dpf__freq <- c(ID0040_frequency, ID0041_frequency, ID0042_frequency, ID0043_frequency,
                    ID0044_frequency, ID0045_frequency, ID0046_frequency, ID0047_frequency)
CTRL_4dpf.RFP__freq <- c(`ID0040_frequency-RFP`, `ID0041_frequency-RFP`, `ID0042_frequency-RFP`, `ID0043_frequency-RFP`,
                         `ID0044_frequency-RFP`, `ID0045_frequency-RFP`, `ID0046_frequency-RFP`,`ID0047_frequency-RFP`)

CTRL_5dpf__freq <- c(ID0048_frequency, ID0049_frequency, ID0050_frequency, ID0051_frequency,
                     ID0052_frequency, ID0053_frequency, ID0054_frequency, ID0055_frequency,
                     ID0056_frequency, ID0057_frequency)
CTRL_5dpf.RFP__freq <- c(`ID0048_frequency-RFP`, `ID0049_frequency-RFP`,`ID0050_frequency-RFP`,`ID0051_frequency-RFP`,
                        `ID0052_frequency-RFP`, `ID0053_frequency-RFP`, `ID0054_frequency-RFP`, `ID0055_frequency-RFP`,
                         `ID0056_frequency-RFP`, `ID0057_frequency-RFP`)

HRASV12_4dpf.RFP__freq <- c(`ID0058_frequency-RFP`, `ID0059_frequency-RFP`, `ID0060_frequency-RFP`, `ID0061_frequency-RFP`,
                            `ID0062_frequency-RFP`, `ID0063_frequency-RFP`)






# Create a vector of ID numbers
id_numbers <- 130:132

# Initialize an empty vector for frequencies
AKT1_5dpf__freq <- numeric(length(id_numbers))

# Iterate over each ID number and assign the corresponding frequency value
for (i in seq_along(id_numbers)) {
  id <- sprintf("ID%04d", id_numbers[i])
 AKT1_5dpf__freq[i] <- get(paste0(id, "_frequency"))
}


# Comparisons!
# 4dpf - CTRL vs HRASV12 vs AKT1
# Determine the maximum length among the vectors
max_length <- max(length(CTRLs_4dpf__freq), length(HRASV12_4dpf__freq), length(AKT1_4dpf__freq))

# Create a data frame with NA values
frequency_df <- data.frame(CTRLs_4dpf__freq = rep(NA, max_length),
                           HRASV12_4dpf__freq = rep(NA, max_length),
                           AKT1_4dpf__freq = rep(NA, max_length))

# Assign vectors to the data frame columns
frequency_df[1:length(CTRLs_4dpf__freq), "CTRLs_4dpf__freq"] <- CTRLs_4dpf__freq
frequency_df[1:length(HRASV12_4dpf__freq), "HRASV12_4dpf__freq"] <- HRASV12_4dpf__freq
frequency_df[1:length(AKT1_4dpf__freq), "AKT1_4dpf__freq"] <- AKT1_4dpf__freq

frequency_df <- melt(frequency_df, variable.name = "Genotype", value.name = "Events/minute")
frequency_df <- na.omit(frequency_df)

my_comparisons <- list( c("CTRLs_4dpf__freq", "HRASV12_4dpf__freq"),
                        c("CTRLs_4dpf__freq", "AKT1_4dpf__freq"),
                        c("HRASV12_4dpf__freq", "AKT1_4dpf__freq"))

ggplot(frequency_df, aes(Genotype, `Events/minute`))+
  geom_boxplot(aes(colour = Genotype))+
  stat_compare_means(comparisons = my_comparisons) # Add pairwise comparisons p-value
