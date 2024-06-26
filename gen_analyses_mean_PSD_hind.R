# Initialize an empty data frame
all_hind_PSD_mean <- data.frame()

# Loop over the hind_lists
for (list_name in names(hind_lists)) {
  
  # Get the current list
  curr_list <- hind_lists[[list_name]]
  
  # Find the arrays that contain "psd.mean" for total type
  total_psd_mean_names <- grep("psd.mean$", names(curr_list), value = TRUE)
  
  # Find the arrays that contain "psd.RFP.mean" for RFP type
  RFP_psd_mean_names <- grep("psd.RFP.mean$", names(curr_list), value = TRUE)
  
  # Combine the arrays
  psd_mean_names <- c(total_psd_mean_names, RFP_psd_mean_names)
  
  # Add the arrays to the all_hind_PSD_mean data frame
  for (arr_name in psd_mean_names) {
    # Extract the Genotype and Age from the list_name
    split_name <- strsplit(list_name, "_")[[1]]
    Genotype <- split_name[1]
    Age <- split_name[2]
    
    # Extract the ID, Condition and Type from the arr_name
    Type <- ifelse(grepl("RFP", arr_name), "RFP", "total")
    split_ID <- strsplit(arr_name, "_")[[1]]
    Condition <- paste(Genotype, Age, Type, sep = " ")
    Condition <- gsub("ID[0-9]+", "", Condition)
    
    # Calculate mean for each array
    mean_psd = mean(unlist(curr_list[[arr_name]]))
    
    # Create a data frame from the current array
    curr_df <- data.frame(Genotype = Genotype, Age = Age, Condition = Condition, Type = Type, "mean_psd" = mean_psd)
    
    # Add the current data frame to the all_hind_PSD_mean data frame
    all_hind_PSD_mean <- rbind(all_hind_PSD_mean, curr_df)
  }
}


# Make the plot
PSD_mean_across_genos.boxplt <- ggplot(all_hind_PSD_mean, aes(x = Condition, y = mean_psd, fill = Genotype, shape = Genotype)) +
  geom_boxplot_pattern(aes(pattern = Type),
                       pattern_colour = "red",
                       pattern_density = 0.05,
                       pattern_spacing = 0.025,
                       outlier.shape = NULL) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "PSD across genotypes",
       x = "Condition", y = "mean_Ca") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE)

ggsave(PSD_mean_across_genos.boxplt,
       filename = paste0(format(Sys.time(), "PSD_mean_across_genos.boxplt"),
                         ".png"),
       path = paste0("~/calcium-clustering/plots/"), 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)
