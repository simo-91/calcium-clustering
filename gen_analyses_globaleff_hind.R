# Initialize an empty data frame
all_hind_globaleff <- data.frame()

# Loop over the hind_lists
for (list_name in names(hind_lists)) {
  
  # Get the current list
  curr_list <- hind_lists[[list_name]]
  
  # Find the arrays that contain "globalcoeff" for total type
  total_globaleff_names <- grep("globalcoeff$", names(curr_list), value = TRUE)
  
  # Find the arrays that contain "globalcoeff.RFP" for RFP type
  RFP_globaleff_names <- grep("globalcoeff.RFP$", names(curr_list), value = TRUE)
  
  # Combine the arrays
  globaleff_names <- c(total_globaleff_names, RFP_globaleff_names)
  
  # Add the arrays to the all_hind_globaleff data frame
  for (arr_name in globaleff_names) {
    # Skip if any element of the array is NA
    if (any(is.na(unlist(curr_list[[arr_name]])))) {
      next
    }
    
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
    globaleff = unlist(curr_list[[arr_name]])
    
    # Create a data frame from the current array
    curr_df <- data.frame(Genotype = Genotype, Age = Age, Condition = Condition, Type = Type, "globaleff" = globaleff)
    
    # Add the current data frame to the all_hind_globaleff data frame
    all_hind_globaleff <- rbind(all_hind_globaleff, curr_df)
  }
}


# Make the plot
global_eff_across_genos.boxplt <- ggplot(all_hind_globaleff, aes(x = Condition, y = globaleff, fill = Genotype, shape = Genotype)) +
  geom_boxplot_pattern(aes(pattern = Type),
                       pattern_colour = "red",
                       pattern_density = 0.05,
                       pattern_spacing = 0.025,
                       outlier.shape = NULL) +
  scale_pattern_manual(values = c(total="none", 
                                  RFP = "stripe"),
                       guide = guide_legend(title = element_blank())) +
  geom_point(aes(shape = Genotype), alpha = 0.5)+
  labs(title = "Global efficiency across genotypes",
       x = "Condition", y = "E(G)") +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(pattern = "none")), shape = FALSE) +
  geom_signif(comparisons = list(c("AKT1 4dpf RFP", "HRASV12 5dpf total"), 
                                 c("HRASV12 4dpf RFP", "HRASV12 5dpf total")),
              step_increase = 0.05,
              map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))


ggsave(global_eff_across_genos.boxplt,
       filename = paste0(format(Sys.time(), "global_eff_across_genos.boxplt"),
                         ".png"),
       path = paste0("~/calcium-clustering/plots/"), 
       device = "png",  bg = "white",
       width = 20, height = 15, units = "cm", dpi = 320,
       scale = 2)

library(FSA)
pairwise.globaleff <- dunnTest(globaleff ~ Condition, data=all_hind_globaleff, method="bonferroni")
pairwise.globaleff.df <- as.data.frame(pairwise.globaleff$res)

pairwise.globaleff.sign <- pairwise.globaleff.df[which(pairwise.globaleff.df$P.adj < 0.06),]
                                      
                                      
ggbetweenstats(
  data = all_hind_globaleff,
  x = Condition,
  y = globaleff,
  plot.type = "box",
  type = "np",   # Non-parametric tests
  pairwise.comparisons = TRUE,   # Pairwise comparisons
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",  # P-value adjustment method
  centrality.type = "trimmed mean",
  test.value = "dunn",   # Type of test
  palette = "Set3",
  ggtheme = theme_minimal()# Specifying the color palette
)


library(ggstatsplot)
library(ggplot2)

ggbetweenstats(
  data = all_hind_globaleff,
  x = Condition,
  y = globaleff,
  plot.type = "box",
  type = "np",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  centrality.type = "trimmed mean",
  test.value = "dunn",
  palette = "Set3",
  ggtheme = theme_minimal()
) +
  geom_boxplot(aes(fill = Genotype), alpha = 0.7) +
  scale_fill_manual(values = c("#8DD3C7", "#FFFFB3", "#BEBADA"))  # Adjust colors as needed

# Convert Genotype to factor
all_hind_globaleff$Genotype <- factor(all_hind_globaleff$Genotype)

# Create the plot with facets using facet_grid
library(ggstatsplot)
library(ggplot2)

ggbetweenstats(
  data = all_hind_globaleff,
  x = Condition,
  y = globaleff,
  plot.type = "box",
  type = "np",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  centrality.type = "trimmed mean",
  test.value = "dunn",
  ggtheme = theme_minimal()
) +
  facet_grid(Genotype ~ ., scales = "free_y")

