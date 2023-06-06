# Initialize an empty list to hold the arrays
all_hind_mean_Ca <- list()

# Loop over the hind_lists
for (list_name in names(hind_lists)) {
  # Get the current list
  curr_list <- hind_lists[[list_name]]
  
  # Find the arrays that end with "_mean_Ca" or "RFP_mean_Ca"
  mean_Ca_names <- grep("(RFP_)?_mean_Ca$", names(curr_list), value = TRUE)
  
  # Add the arrays to the all_hind_mean_Ca list
  for (arr_name in mean_Ca_names) {
    all_hind_mean_Ca[[paste(list_name, arr_name, sep = "_")]] <- curr_list[[arr_name]]
  }
}


# Initialize an empty data frame
all_hind_mean_Ca.df <- data.frame()

# Loop over the all_hind_mean_Ca list
for (list_name in names(all_hind_mean_Ca)) {
  
  # Extract the Genotype, Age, ID and Type from the list_name
  split_name <- strsplit(list_name, "_")[[1]]
  
  # Handle the case where ".RFP" is in the array name
  if (grepl("\\.RFP", list_name)) {
    Type <- "RFP"
    split_name <- unlist(strsplit(list_name, "\\."))
    split_ID <- unlist(strsplit(split_name[1], "_"))
  } else {
    Type <- "total"
    split_ID <- split_name
  }
  
  Genotype <- split_ID[1]
  Age <- split_ID[2]
  
  # Calculate mean for each array
  mean_Ca = mean(unlist(all_hind_mean_Ca[[list_name]]))
  
  # Create a data frame from the current array
  curr_df <- data.frame(Genotype = Genotype, Age = Age, ID = list_name, Type = Type,
                        "mean_Ca" = mean_Ca)
  
  # Add the current data frame to the all_hind_mean_Ca.df data frame
  all_hind_mean_Ca.df <- rbind(all_hind_mean_Ca.df, curr_df)
}

# Remove the unwanted portion of text from the ID column
all_hind_mean_Ca.df$ID <- sub("_ID\\d{4}(.RFP)?_mean_Ca$", "", all_hind_mean_Ca.df$ID)

# Rename the ID column to Condition
names(all_hind_mean_Ca.df)[names(all_hind_mean_Ca.df) == "ID"] <- "Condition"

# Load the necessary libraries
library(ggplot2)
library(ggpattern)

# Make the plot
mean_Ca_across_genos.hind.boxplt <- ggplot(all_hind_mean_Ca.df, aes(x = Condition, y = mean_Ca, fill = Genotype, shape = Genotype)) +
  geom_boxplot_pattern(aes(pattern = Type),
                       pattern_colour = "red",
                       pattern_density = 0.05,
                       pattern_spacing = 0.025,
                       outlier.shape = NULL) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Mean Ca concentration across genotypes (hindbrain only)",
       x = "Condition", y = "mean_Ca") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)


ggplot(all_hind_mean_Ca.df, aes(x = Condition, y = mean_Ca, fill = Genotype, shape = Genotype)) +
  geom_violin_pattern(aes(pattern = Type),
                      pattern_colour = "red",
                      pattern_density = 0.05,
                      pattern_spacing = 0.025,
                      trim = FALSE) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Mean Ca concentration across genotypes (hindbrain only)",
       x = "Condition", y = "mean_Ca") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)


ggsave(mean_Ca_across_genos.hind.boxplt,
       filename = paste0(format(Sys.time(), "mean_Ca_across_genos.boxplt"),
                         ".png"),
       path = paste0("~/calcium-clustering/plots/"), 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
