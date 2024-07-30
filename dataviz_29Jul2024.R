library(pacman)
p_load(utils, dplyr, tidyverse, ggplot2, plotly, tidyr, reshape2, factoextra, ggdendro,
       grid, RcppCNPy, cowplot, ggpubr, mmand, rstudioapi, reticulate, tcltk, ggfortify,
       ggpubr, factoextra, parallel, ggpattern, ggsignif, car, gtools, igraph, ggraph, 
       emmeans, FSA, rstatix,tcltk, data.table)

# data viz
# Convert Age to a factor for better plotting
results_df <- results_df %>%
  mutate(Age = as.factor(Age))


# Clustcoeff
## CTRL vs AKT1 vs HRASV12 ----
results_ctrl_akt1_hrasv12.df <- results_df %>% filter(Condition %in% c("CTRL", "AKT1", "HRASV12"))
ctrl_akt1_hrasv12_clustcoeff.plt <- ggplot(results_ctrl_akt1_hrasv12.df, aes(x = Condition, y = clustering_coefficient, fill = Condition)) +
  geom_boxplot() +
  facet_wrap(vars(Age), scales = "free_y") +
  theme_pubr() +
  labs(title = "Clustering Coefficient",
       x = "Condition",
       y = "Clustering Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

# Add significance annotations for age 8
for (i in 1:nrow(annotations)) {
  comparison <- annotations$Comparison[i]
  y_pos <- annotations$y_position[i]
  
  if (comparison == "AKT1_vs_CTRL") {
    ctrl_akt1_hrasv12_clustcoeff.plt <- ctrl_akt1_hrasv12_clustcoeff.plt +
      geom_signif(
        comparisons = list(c("AKT1", "CTRL")),
        annotations = "*",
        y_position = y_pos,
        data = subset(results_ctrl_akt1_hrasv12.df, Age == 8),
        tip_length = 0.01
      )
  } else if (comparison == "CTRL_vs_HRASV12") {
    ctrl_akt1_hrasv12_clustcoeff.plt <- ctrl_akt1_hrasv12_clustcoeff.plt +
      geom_signif(
        comparisons = list(c("CTRL", "HRASV12")),
        annotations = "*",
        y_position = y_pos + 0.02,
        data = subset(results_ctrl_akt1_hrasv12.df, Age == 8),
        tip_length = 0.01
      )
  }
}

# Print the plot
print(ctrl_akt1_hrasv12_clustcoeff.plt)


## CTRL vs AKT1 vs HRASV12 IRF8null ----
results_ctrl_akt1_hrasv12_IRF8null.df <- results_df %>% filter(Condition %in% c("CTRL_IRF8null", "AKT1_IRF8null", "HRASV12_IRF8null"))
significant_irf8null_results <- irf8null_results_dt %>% filter(P_value < 0.05 | (Comparison == "AKT1_IRF8null_vs_HRASV12_IRF8null" & Age == 4 & P_value == 0.0667))

# Prepare annotations for ggsignif
annotations_irf8null <- significant_irf8null_results %>% mutate(
  y_position = sapply(Age, function(x) max(results_ctrl_akt1_hrasv12_IRF8null.df %>% filter(Age == x) %>% pull(clustering_coefficient))) + 0.05
)

# Adjust y_position for specific overlapping cases
annotations_irf8null$y_position[annotations_irf8null$Comparison == "CTRL_IRF8null_vs_HRASV12_IRF8null" & annotations_irf8null$Age == 4] <- 
  annotations_irf8null$y_position[annotations_irf8null$Comparison == "CTRL_IRF8null_vs_HRASV12_IRF8null" & annotations_irf8null$Age == 4] + 0.05

# Extend the Y-axis by defining limits
y_max <- max(results_ctrl_akt1_hrasv12_IRF8null.df$clustering_coefficient) + 0.05

ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt <- ggplot(results_ctrl_akt1_hrasv12_IRF8null.df, aes(x = Condition, y = clustering_coefficient, fill = Condition)) +
  geom_boxplot() +
  facet_wrap(vars(Age), scales = "free_y") +
  theme_pubr() +
  labs(title = "Clustering Coefficient in IRF8-/-",
       x = "Condition",
       y = "Clustering Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylim(NA, y_max)  # Extend the y-axis limits

# Add significance annotations ----
for (i in 1:nrow(annotations_irf8null)) {
  comparison <- annotations_irf8null$Comparison[i]
  y_pos <- annotations_irf8null$y_position[i]
  age <- annotations_irf8null$Age[i]
  
  if (comparison == "AKT1_IRF8null_vs_CTRL_IRF8null") {
    ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt <- ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt +
      geom_signif(
        comparisons = list(c("AKT1_IRF8null", "CTRL_IRF8null")),
        annotations = "*",
        y_position = y_pos - 0.01,
        data = subset(results_ctrl_akt1_hrasv12_IRF8null.df, Age == age),
        tip_length = 0.01
      )
  } else if (comparison == "CTRL_IRF8null_vs_HRASV12_IRF8null") {
    ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt <- ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt +
      geom_signif(
        comparisons = list(c("CTRL_IRF8null", "HRASV12_IRF8null")),
        annotations = "*",
        y_position = y_pos + 0.05,
        data = subset(results_ctrl_akt1_hrasv12_IRF8null.df, Age == age),
        tip_length = 0.01
      )
  } else if (comparison == "AKT1_IRF8null_vs_HRASV12_IRF8null" & age == 4 & annotations_irf8null$P_value[i] == 0.0667) {
    ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt <- ctrl_akt1_hrasv12_IRF8null_clustcoeff.plt +
      geom_signif(
        comparisons = list(c("AKT1_IRF8null", "HRASV12_IRF8null")),
        annotations = "p = 0.0667",
        y_position = y_pos + 0.035,
        data = subset(results_ctrl_akt1_hrasv12_IRF8null.df, Age == age),
        tip_length = 0.01
      )
  }
}


# AKT1 vs AKT1_IRF8null ----
# Filter the results for AKT1 vs AKT1_IRF8null
results_akt1_akt1_irf8null.df <- results_df %>% filter(Condition %in% c("AKT1", "AKT1_IRF8null"))

# Prepare annotations for ggsignif
annotations_akt1_akt1_irf8null <- akt1_irf8null_results_dt %>% mutate(
  y_position = sapply(Age, function(x) max(results_akt1_akt1_irf8null.df %>% filter(Age == x) %>% pull(clustering_coefficient))) + 0.05
)

# Adjust y_position for specific overlapping cases if needed
# No adjustments required for this case

# Extend the Y-axis by defining limits
y_max <- max(results_akt1_akt1_irf8null.df$clustering_coefficient) + 0.1

# Create the plot with significance annotations
akt1_vs_akt1_irf8null_clustcoeff.plt <- ggplot(results_akt1_akt1_irf8null.df, aes(x = Condition, y = clustering_coefficient, fill = Condition)) +
  geom_boxplot() +
  facet_wrap(vars(Age), scales = "free_y") +
  theme_pubr() +
  labs(title = "Clustering Coefficient: AKT1 vs AKT1_IRF8null",
       x = "Condition",
       y = "Clustering Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylim(NA, y_max)  # Extend the y-axis limits

# Add significance annotations for 4dpf
akt1_vs_akt1_irf8null_clustcoeff.plt <- akt1_vs_akt1_irf8null_clustcoeff.plt +
  geom_signif(
    comparisons = list(c("AKT1", "AKT1_IRF8null")),
    annotations = "p = 0.0847",
    y_position = annotations_akt1_akt1_irf8null$y_position[annotations_akt1_akt1_irf8null$Age == 4],
    data = subset(results_akt1_akt1_irf8null.df, Age == 4),
    tip_length = 0.01
  )

# Print the plot
print(akt1_vs_akt1_irf8null_clustcoeff.plt)