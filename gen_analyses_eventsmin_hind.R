# Filter the lists containing the word "hind"
hind_lists <- all_lists[grep("hind", names(all_lists))]

# Initialize an empty list to hold the arrays
all_hind_arrays <- list()

# Loop over the hind_lists
for (list_name in names(hind_lists)) {
  # Get the current list
  curr_list <- hind_lists[[list_name]]
  
  # Find the arrays that end with "_frequency" or "_frequency-RFP"
  freq_names <- grep("_frequency$|_frequency-RFP$", names(curr_list), value = TRUE)
  
  # Add the arrays to the all_hind_arrays list
  for (arr_name in freq_names) {
    all_hind_arrays[[paste(list_name, arr_name, sep = "_")]] <- curr_list[[arr_name]]
  }
}

all_freq_arrays.hind <- all_hind_arrays


df_temp <- data.frame(matrix(unlist(all_freq_arrays.hind), nrow=length(all_freq_arrays.hind), byrow=T))

# Get the names of your list
names_vector <- names(all_freq_arrays.hind)

# Convert names to dataframe
names_df <- data.frame(names_vector)

# Merge the two dataframes
all_freq_arrays.hind.df <- cbind(names_df, df_temp)

# Rename the columns
colnames(all_freq_arrays.hind.df) <- c("name", "events_per_min")

# Separate the 'name' column into multiple columns
all_freq_arrays.hind.df <- all_freq_arrays.hind.df %>% 
  separate(name, into = c("Genotype", "Age", "Region", "ID", "Measurement"), sep = "_", extra = "merge")

# If Measurement ends with '-RFP', create new column "Type" and set it as 'RFP', else 'total'
all_freq_arrays.hind.df <- all_freq_arrays.hind.df %>%
  mutate(Type = ifelse(grepl("-RFP", Measurement), "RFP", "total"))

# Remove the Measurement column
all_freq_arrays.hind.df$Measurement <- NULL

# Convert Age column to be just the number (remove 'dpf')
all_freq_arrays.hind.df$Age <- gsub("dpf", "", all_freq_arrays.hind.df$Age)

# Convert Age to numeric
all_freq_arrays.hind.df$Age <- as.numeric(all_freq_arrays.hind.df$Age)

# Create a new column "Condition" that is a concatenation of Genotype, Age, and Type
all_freq_arrays.hind.df <- all_freq_arrays.hind.df %>%
  mutate(Condition = paste(Genotype, Age, Type, sep="_"))

# Arrange the dataframe
all_freq_arrays.hind.df <- all_freq_arrays.hind.df %>% arrange(Genotype, Age, ID)


# Now fit the model
model <- aov(events_per_min ~ Condition, data = all_freq_arrays.hind.df)

# Check the ANOVA table
summary(model)

# Conduct the post-hoc test
posthoc <- TukeyHSD(model)
print(posthoc)


# Convert the posthoc test result to a dataframe
posthoc_df <- data.frame(posthoc[[1]])

# Add the rownames (i.e., the comparison groups) as a new column
posthoc_df$comparison <- rownames(posthoc_df)

# Filter out only the significant comparisons
significant_comparisons <- posthoc_df[posthoc_df$p.adj < 0.05, ]



ggplot(all_freq_arrays.hind.df, aes(x = Condition, y = events_per_min, fill = Genotype, shape = Genotype)) +
  geom_boxplot_pattern(aes(pattern = Type),
                       pattern_colour = "red",
                       pattern_density = 0.05,
                       pattern_spacing = 0.025,
                       outlier.shape = NULL) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Events/min across genotypes",
       x = "Genotype", y = "events/min") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)
