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
id_numbers <- 31:37

# Initialize an empty vector for frequencies
HRASV12_4dpf__freq <- numeric(length(id_numbers))

# Iterate over each ID number and assign the corresponding frequency value
for (i in seq_along(id_numbers)) {
  id <- sprintf("ID%04d", id_numbers[i])
 HRASV12_4dpf__freq[i] <- get(paste0(id, "_frequency"))
}


# Create a vector of ID numbers
id_numbers <- c(31:37, 58:63, 112:115)

# Initialize an empty vector for frequencies
HRASV12_4dpf__freq <- numeric(length(id_numbers))

# Iterate over each ID number and assign the corresponding frequency value
for (i in seq_along(id_numbers)) {
  id <- sprintf("ID%04d", id_numbers[i])
  HRASV12_4dpf__freq[i] <- get(paste0(id, "_frequency"))
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

frequency_df <- melt(frequency_df, variable.name = "Genotype", value.name = "events/min")
frequency_df <- na.omit(frequency_df)



# List the variable names and their corresponding column names
variable_names <- c(
  CTRLs_4dpf__freq = "CTRLs_4dpf",
  CTRL_4dpf.RFP__freq = "CTRL_4dpf_RFP",
  CTRL_5dpf__freq = "CTRL_5dpf",
  CTRL_5dpf.RFP__freq = "CTRL_5dpf_RFP",
  HRASV12_4dpf__freq = "HRASV12_4dpf",
  HRASV12_4dpf.RFP__freq = "HRASV12_4dpf_RFP",
  HRASV12_5dpf__freq = "HRASV12_5dpf",
  HRASV12_5dpf.RFP__freq = "HRASV12_5dpf_RFP",
  AKT1_4dpf__freq = "AKT1_4dpf",
  AKT1_4dpf.RFP__freq = "AKT1_4dpf_RFP",
  AKT1_5dpf__freq = "AKT1_5dpf",
  AKT1_5dpf.RFP__freq = "AKT1_5dpf_RFP"
)

# Determine the maximum length among the vectors
max_length <- max(lengths(list(CTRLs_4dpf__freq, CTRL_4dpf.RFP__freq, CTRL_5dpf__freq, CTRL_5dpf.RFP__freq,
                               HRASV12_4dpf__freq, HRASV12_4dpf.RFP__freq, HRASV12_5dpf__freq, HRASV12_5dpf.RFP__freq,
                               AKT1_4dpf__freq, AKT1_4dpf.RFP__freq, AKT1_5dpf__freq, AKT1_5dpf.RFP__freq)))

# Create a data frame with NA values
frequency_df <- data.frame(matrix(NA, nrow = max_length, ncol = length(variable_names)))
colnames(frequency_df) <- names(variable_names)

# Assign vectors to the data frame columns
for (variable in names(variable_names)) {
  variable_vector <- eval(parse(text = variable))
  frequency_df[1:length(variable_vector), variable] <- variable_vector
}




frequency_df <- melt(frequency_df, variable.name = "Condition", value.name = "events/min")
frequency_df <- na.omit(frequency_df)


# Remove "__freq" from "Genotype" column
frequency_df$Condition <- gsub("__freq", "", frequency_df$Condition)





# Perform oneway Welch's test (cause they are parametric but unequal variances)
oneway.frequency.test <- oneway.test(`events/min` ~ Genotype, data = frequency_df, var.equal = FALSE)
# Perform pairwise t.test for unequal variances to compare between groups
pairwise.frequency.test <- pairwise.t.test(frequency_df$`events/min`, frequency_df$Condition, p.adjust.method = "bonferroni", pool.sd = FALSE)



# Create boxplot with significance comparisons
p <- ggplot(frequency_df, aes(x = variable, y = `events/min`)) +
  geom_boxplot() +
  labs(x = "Group", y = "Frequency") +
  theme_minimal()

# Extract significant comparisonscalled Genotype
significant_comparisons <- pairwise_results$p.`events/min` < 0.05
significant_indices <- which(significant_comparisons, arr.ind = TRUE)

# Add significance comparisons to the plot
p <- p + geom_text(aes(x = significant_indices[, 1], y = max(frequency_df$`events/min`), label = "*"), size = 6)

# Display the plot
print(p)




# 
# comparisons <- data.frame(pairwise.frequency.test$p.value)
# comparisons$Genotype1 <- rownames(comparisons)
# comparisons <- subset(comparisons, p.value <= 0.05) # Filter for significant comparisons

# Plot -----
shape_mapping <- data.frame(Genotype = unique(frequency_df$Genotype))
shape_mapping$shape <- ifelse(grepl("^CTRL", shape_mapping$Genotype), 21,
                              ifelse(grepl("^HRASV12", shape_mapping$Genotype), 22,
                                     ifelse(grepl("^AKT1", shape_mapping$Genotype), 23, NA)))


# Create the ggplot
ggplot(frequency_df, aes(x = Genotype, y = `events/min`, fill = Genotype)) +
  geom_boxplot() +
  geom_jitter(data = frequency_df, aes(shape = factor(shape_mapping$shape[match(Genotype, shape_mapping$Genotype)])),
              geom_point(aes(shape = Genotype), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)))+
  labs(x = "Genotype", y = "events/min") +
  guides(fill = "none", shape = "none") +
  theme_minimal()





# Change the order of levels in "Genotype" column
frequency_df$Condition <- factor(frequency_df$Condition, levels = c("CTRL_4dpf", "CTRL_4dpf.RFP", "CTRL_5dpf", "CTRL_5dpf.RFP",
                                                                    "HRASV12_4dpf", "HRASV12_4dpf.RFP", "HRASV12_5dpf", "HRASV12_5dpf.RFP",
                                                                    "AKT1_4dpf", "AKT1_4dpf.RFP", "AKT1_5dpf", "AKT1_5dpf.RFP"))
frequency_df$Genotype <- factor(frequency_df$Genotype, levels = c("CTRL", "HRASV12", "AKT1"))
frequency_df$Age <- factor(frequency_df$Age, levels = c("4dpf", "5dpf"))



library(ggplot2)
library(ggpattern)

# Update the plot code
events_per_min_across_genos.violinplt <- ggplot(frequency_df, aes(x = Condition, y = `events/min`, fill = Genotype, shape = Genotype)) +
  geom_violin_pattern(aes(pattern = RFP),
                      pattern_colour = "red",
                      pattern_density = 0.05,
                      pattern_spacing = 0.025,
                      trim = FALSE) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Events/min across genotypes",
       x = "Genotype", y = "events/min") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +  # Adjust colors as needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)


events_per_min_across_genos.boxplt <- ggplot(frequency_df, aes(x = Condition, y = `events/min`, fill = Genotype, shape = Genotype)) +
  geom_boxplot_pattern(aes(pattern = RFP),
                      pattern_colour = "red",
                      pattern_density = 0.05,
                      pattern_spacing = 0.025) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Events/min across genotypes",
       x = "Genotype", y = "events/min") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +  # Adjust colors as needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)
# NEED TO CHANGE KEY LABELS ("RFP" SHOULD BE "PRENEOPLASTIC" MAYBE)
  
ggsave(events_per_min_across_genos.violinplt,
       filename = paste0("events_per_min_across_genos.violinplt",
                         ".png"),
       path = paste0("~/calcium-clustering/plots/"), 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)

ggsave(events_per_min_across_genos.boxplt,
       filename = paste0(format(Sys.time(), "events_per_min_across_genos.boxplt"),
                         ".png"),
       path = paste0("~/calcium-clustering/plots/"), 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)


# Assuming you have your data in a data frame called 'data'
# 'Condition' is the categorical variable and 'events/min' is the continuous variable

# Perform Welch's ANOVA
welch_anova <- oneway.test(`events/min` ~ Condition, data=frequency_df, var.equal=FALSE)

# Perform post-hoc tests (Tukey's HSD) if the ANOVA result is significant
if (welch_anova$p.value < 0.05) {
  posthoc <- TukeyHSD(aov(`events/min` ~ Condition, data=frequency_df))
  print(posthoc)
}

# Print the ANOVA results
print(welch_anova)

# Hind VS Hind

