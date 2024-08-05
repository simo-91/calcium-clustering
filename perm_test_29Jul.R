#   for CTRLvsAKT1vsHRASV12 clustcoeff ----
# Function to calculate the median difference
median_diff <- function(x, y) {
  return(abs(median(x) - median(y)))
}

# Function to perform permutation test
perm_test <- function(group1, group2, num_perm = 10000) {
  observed_diff <- median_diff(group1, group2)
  combined <- c(group1, group2)
  perm_diffs <- replicate(num_perm, {
    perm_sample <- sample(combined)
    perm_group1 <- perm_sample[1:length(group1)]
    perm_group2 <- perm_sample[(length(group1) + 1):length(combined)]
    median_diff(perm_group1, perm_group2)
  })
  p_value <- mean(perm_diffs >= observed_diff)
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(clustering_coefficient)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(clustering_coefficient)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(clustering_coefficient)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

clustering_coefficient_IRF8pos_p_values <- results_dt

#   for CTRLvsAKT1vsHRASV12 IRF8null clustcoeff ----
# Load necessary libraries
library(dplyr)
library(data.table)

# Filter the dataframe to include only _IRF8null conditions
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))

# Unique ages in the dataset
irf8null_ages <- unique(irf8null_conditions$Age)

# Initialize a list to store results
irf8null_results_list <- list()

# Loop through each age group
for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(clustering_coefficient)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(clustering_coefficient)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(clustering_coefficient)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

# Convert irf8null_results_list to data.table
irf8null_results_dt <- data.table(Age = integer(),
                                  Comparison = character(),
                                  Observed_Difference = numeric(),
                                  P_value = numeric())

for (age in names(irf8null_results_list)) {
  irf8null_results_dt <- rbind(irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_results_dt <- rbind(irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_results_dt <- rbind(irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Print the data.table
print(irf8null_results_dt)
clustering_coefficient_IRF8null_p_values <- irf8null_results_dt
clustering_coefficient_all_p_values <- rbind(clustering_coefficient_IRF8pos_p_values, clustering_coefficient_IRF8null_p_values)

# AKT1 vs AKT1_IRF8null Clustcoeff   ----
# Filter the dataframe to include only AKT1 and AKT1_IRF8null conditions
akt1_irf8null_conditions <- results_df %>% filter(Condition %in% c("AKT1", "AKT1_IRF8null"))

# Unique ages in the dataset
akt1_irf8null_ages <- unique(akt1_irf8null_conditions$Age)

# Initialize a list to store results
akt1_irf8null_results_list <- list()

# Loop through each age group
for (age in akt1_irf8null_ages) {
  age_data <- akt1_irf8null_conditions %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(clustering_coefficient)
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(clustering_coefficient)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  
  akt1_irf8null_results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null
  )
}

# Convert akt1_irf8null_results_list to data.table
akt1_irf8null_results_dt <- data.table(Age = integer(),
                                       Comparison = character(),
                                       Observed_Difference = numeric(),
                                       P_value = numeric())

for (age in names(akt1_irf8null_results_list)) {
  akt1_irf8null_results_dt <- rbind(akt1_irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = akt1_irf8null_results_list[[age]]$AKT1_vs_AKT1_IRF8null$observed_diff,
    P_value = akt1_irf8null_results_list[[age]]$AKT1_vs_AKT1_IRF8null$p_value
  ))
}


#   for HRASV12 vs HRASV12_IRF8null Clustcoeff ----
# Filter the dataframe to include only HRASV12 and HRASV12_IRF8null conditions
hrasv12_irf8null_conditions <- results_df %>% filter(Condition %in% c("HRASV12", "HRASV12_IRF8null"))

# Unique ages in the dataset
hrasv12_irf8null_ages <- unique(hrasv12_irf8null_conditions$Age)

# Initialize a list to store results
hrasv12_irf8null_results_list <- list()

# Loop through each age group
for (age in hrasv12_irf8null_ages) {
  age_data <- hrasv12_irf8null_conditions %>% filter(Age == age)
  
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(clustering_coefficient)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(clustering_coefficient)
  
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  
  hrasv12_irf8null_results_list[[as.character(age)]] <- list(
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
  )
}

# Convert hrasv12_irf8null_results_list to data.table
hrasv12_irf8null_results_dt <- data.table(Age = integer(),
                                          Comparison = character(),
                                          Observed_Difference = numeric(),
                                          P_value = numeric())

for (age in names(hrasv12_irf8null_results_list)) {
  hrasv12_irf8null_results_dt <- rbind(hrasv12_irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = hrasv12_irf8null_results_list[[age]]$HRASV12_vs_HRASV12_IRF8null$observed_diff,
    P_value = hrasv12_irf8null_results_list[[age]]$HRASV12_vs_HRASV12_IRF8null$p_value
  ))
}

#   for CTRL vs CTRL_IRF8null Clustcoeff ----
# Filter the dataframe to include only CTRL and CTRL_IRF8null conditions
ctrl_irf8null_conditions <- results_df %>% filter(Condition %in% c("CTRL", "CTRL_IRF8null"))

# Unique ages in the dataset
ctrl_irf8null_ages <- unique(ctrl_irf8null_conditions$Age)

# Initialize a list to store results
ctrl_irf8null_results_list <- list()

# Loop through each age group
for (age in ctrl_irf8null_ages) {
  age_data <- ctrl_irf8null_conditions %>% filter(Age == age)
  
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(clustering_coefficient)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(clustering_coefficient)
  
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  
  ctrl_irf8null_results_list[[as.character(age)]] <- list(
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null
  )
}

# Convert ctrl_irf8null_results_list to data.table
ctrl_irf8null_results_dt <- data.table(Age = integer(),
                                       Comparison = character(),
                                       Observed_Difference = numeric(),
                                       P_value = numeric())

for (age in names(ctrl_irf8null_results_list)) {
  ctrl_irf8null_results_dt <- rbind(ctrl_irf8null_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_CTRL_IRF8null",
    Observed_Difference = ctrl_irf8null_results_list[[age]]$CTRL_vs_CTRL_IRF8null$observed_diff,
    P_value = ctrl_irf8null_results_list[[age]]$CTRL_vs_CTRL_IRF8null$p_value
  ))
}

# Combine all results into a single data.table
clustering_coefficient_all_p_values <- rbind(clustering_coefficient_all_p_values,
                             akt1_irf8null_results_dt, 
                             hrasv12_irf8null_results_dt, 
                             ctrl_irf8null_results_dt)



#   for CTRL vs AKT1 vs HRASV12 clustering_coefficient_rfp ----
ages <- unique(results_df$Age)
results_rfp_clustcoeff_list <- list()

for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(clustering_coefficient_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(clustering_coefficient_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(clustering_coefficient_rfp)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_rfp_clustcoeff_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

results_rfp_clustcoeff_dt <- data.table(Age = integer(),
                                        Comparison = character(),
                                        Observed_Difference = numeric(),
                                        P_value = numeric())

for (age in names(results_rfp_clustcoeff_list)) {
  results_rfp_clustcoeff_dt <- rbind(results_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_rfp_clustcoeff_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_rfp_clustcoeff_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_rfp_clustcoeff_dt <- rbind(results_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_rfp_clustcoeff_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_rfp_clustcoeff_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_rfp_clustcoeff_dt <- rbind(results_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_rfp_clustcoeff_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_rfp_clustcoeff_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

clustering_coefficient_rfp_p_values <- results_rfp_clustcoeff_dt

#   for CTRL vs AKT1 vs HRASV12 IRF8null clustering_coefficient_rfp ----
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))
irf8null_ages <- unique(irf8null_conditions$Age)
irf8null_rfp_clustcoeff_list <- list()

for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(clustering_coefficient_rfp)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(clustering_coefficient_rfp)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(clustering_coefficient_rfp)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_rfp_clustcoeff_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

irf8null_rfp_clustcoeff_dt <- data.table(Age = integer(),
                                         Comparison = character(),
                                         Observed_Difference = numeric(),
                                         P_value = numeric())

for (age in names(irf8null_rfp_clustcoeff_list)) {
  irf8null_rfp_clustcoeff_dt <- rbind(irf8null_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_rfp_clustcoeff_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_rfp_clustcoeff_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_rfp_clustcoeff_dt <- rbind(irf8null_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_rfp_clustcoeff_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_rfp_clustcoeff_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_rfp_clustcoeff_dt <- rbind(irf8null_rfp_clustcoeff_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_rfp_clustcoeff_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_rfp_clustcoeff_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Merge results
clustering_coefficient_rfp_p_values <- rbind(results_rfp_clustcoeff_dt, irf8null_rfp_clustcoeff_dt)

# Print the merged results
print(clustering_coefficient_rfp_p_values)


# Perform permutation tests for AKT1 vs AKT1_IRF8null, HRASV12 vs HRASV12_IRF8null, and CTRL vs CTRL_IRF8null Clustering_coefficient ----
# Filter the dataframe to include the specified conditions
rfp_conditions <- results_df %>% filter(Condition %in% c("AKT1", "AKT1_IRF8null", "HRASV12", "HRASV12_IRF8null", "CTRL", "CTRL_IRF8null"))

# Unique ages in the dataset
rfp_ages <- unique(rfp_conditions$Age)

# Initialize a list to store results
rfp_results_list <- list()

# Loop through each age group for each condition comparison
for (age in rfp_ages) {
  age_data <- rfp_conditions %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(clustering_coefficient_rfp)
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(clustering_coefficient_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(clustering_coefficient_rfp)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(clustering_coefficient_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(clustering_coefficient_rfp)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(clustering_coefficient_rfp)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  
  rfp_results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null,
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null
  )
}

# Convert rfp_results_list to data.table
rfp_results_dt <- data.table(Age = integer(),
                             Comparison = character(),
                             Observed_Difference = numeric(),
                             P_value = numeric())

for (age in names(rfp_results_list)) {
  rfp_results_dt <- rbind(rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = rfp_results_list[[age]]$AKT1_vs_AKT1_IRF8null$observed_diff,
    P_value = rfp_results_list[[age]]$AKT1_vs_AKT1_IRF8null$p_value
  ))
  
  rfp_results_dt <- rbind(rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = rfp_results_list[[age]]$HRASV12_vs_HRASV12_IRF8null$observed_diff,
    P_value = rfp_results_list[[age]]$HRASV12_vs_HRASV12_IRF8null$p_value
  ))
  
  rfp_results_dt <- rbind(rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_CTRL_IRF8null",
    Observed_Difference = rfp_results_list[[age]]$CTRL_vs_CTRL_IRF8null$observed_diff,
    P_value = rfp_results_list[[age]]$CTRL_vs_CTRL_IRF8null$p_value
  ))
}

# Merge the new results with existing clustering_coefficient_rfp_p_values
clustering_coefficient_rfp_p_values <- rbind(clustering_coefficient_rfp_p_values, rfp_results_dt)


# Global efficiency ----
#   for CTRL vs AKT1 vs HRASV12 global_efficiency ----
# Function to calculate the median difference
median_diff <- function(x, y) {
  return(abs(median(x) - median(y)))
}

# Function to perform permutation test
perm_test <- function(group1, group2, num_perm = 10000) {
  observed_diff <- median_diff(group1, group2)
  combined <- c(group1, group2)
  perm_diffs <- replicate(num_perm, {
    perm_sample <- sample(combined)
    perm_group1 <- perm_sample[1:length(group1)]
    perm_group2 <- perm_sample[(length(group1) + 1):length(combined)]
    median_diff(perm_group1, perm_group2)
  })
  p_value <- mean(perm_diffs >= observed_diff)
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_global_efficiency_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(global_efficiency)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(global_efficiency)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(global_efficiency)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_global_efficiency_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_global_efficiency_list to data.table
results_global_efficiency_dt <- data.table(Age = integer(),
                                           Comparison = character(),
                                           Observed_Difference = numeric(),
                                           P_value = numeric())

for (age in names(results_global_efficiency_list)) {
  results_global_efficiency_dt <- rbind(results_global_efficiency_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_global_efficiency_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_global_efficiency_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_global_efficiency_dt <- rbind(results_global_efficiency_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_global_efficiency_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_global_efficiency_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_global_efficiency_dt <- rbind(results_global_efficiency_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_global_efficiency_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_global_efficiency_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

#   for CTRL vs AKT1 vs HRASV12 IRF8 mutants global_efficiency ----
# Filter the dataframe to include only _IRF8null conditions
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))

# Unique ages in the dataset
irf8null_ages <- unique(irf8null_conditions$Age)

# Initialize a list to store results
irf8null_global_efficiency_results_list <- list()

# Loop through each age group
for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(global_efficiency)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(global_efficiency)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(global_efficiency)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_global_efficiency_results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

# Convert irf8null_global_efficiency_results_list to data.table
irf8null_global_efficiency_results_dt <- data.table(Age = integer(),
                                                    Comparison = character(),
                                                    Observed_Difference = numeric(),
                                                    P_value = numeric())

for (age in names(irf8null_global_efficiency_results_list)) {
  irf8null_global_efficiency_results_dt <- rbind(irf8null_global_efficiency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_global_efficiency_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_global_efficiency_results_dt <- rbind(irf8null_global_efficiency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_global_efficiency_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_global_efficiency_results_dt <- rbind(irf8null_global_efficiency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_global_efficiency_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Print the data.table
print(irf8null_global_efficiency_results_dt)


# Merge the two data tables into one
global_efficiency_p_values <- rbind(
  results_global_efficiency_dt,
  irf8null_global_efficiency_results_dt
)

# Print the merged data.table
print(global_efficiency_p_values)

#   for IRF8+/+ vs IRF8-/- ----
# Function to perform permutation test for AKT1 vs AKT1_IRF8null, HRASV12 vs HRASV12_IRF8null, CTRL vs CTRL_IRF8null
perm_test_irf8_comparisons <- function(results_df, condition1, condition2) {
  # Unique ages in the dataset
  ages <- unique(results_df$Age)
  
  # Initialize a list to store results
  results_list <- list()
  
  # Loop through each age group
  for (age in ages) {
    age_data <- results_df %>% filter(Age == age)
    
    data1 <- age_data %>% filter(Condition == condition1) %>% pull(global_efficiency)
    data2 <- age_data %>% filter(Condition == condition2) %>% pull(global_efficiency)
    
    test_result <- perm_test(data1, data2)
    
    results_list[[as.character(age)]] <- list(
      Comparison = paste(condition1, "vs", condition2, sep = "_"),
      Observed_Difference = test_result$observed_diff,
      P_value = test_result$p_value
    )
  }
  
  # Convert results_list to data.table
  results_dt <- data.table(Age = integer(), Comparison = character(), Observed_Difference = numeric(), P_value = numeric())
  
  for (age in names(results_list)) {
    results_dt <- rbind(results_dt, data.table(
      Age = as.integer(age),
      Comparison = results_list[[age]]$Comparison,
      Observed_Difference = results_list[[age]]$Observed_Difference,
      P_value = results_list[[age]]$P_value
    ))
  }
  
  return(results_dt)
}

# Perform permutation tests for AKT1 vs AKT1_IRF8null
akt1_vs_akt1_irf8null_dt <- perm_test_irf8_comparisons(results_df, "AKT1", "AKT1_IRF8null")

# Perform permutation tests for HRASV12 vs HRASV12_IRF8null
hrasv12_vs_hrasv12_irf8null_dt <- perm_test_irf8_comparisons(results_df, "HRASV12", "HRASV12_IRF8null")

# Perform permutation tests for CTRL vs CTRL_IRF8null
ctrl_vs_ctrl_irf8null_dt <- perm_test_irf8_comparisons(results_df, "CTRL", "CTRL_IRF8null")

# Merge all results into the global_efficiency_p_values data.table
global_efficiency_p_values <- rbind(
  global_efficiency_p_values,
  akt1_vs_akt1_irf8null_dt,
  hrasv12_vs_hrasv12_irf8null_dt,
  ctrl_vs_ctrl_irf8null_dt
)

# Print the merged data.table
print(global_efficiency_p_values)


#   for CTRL vs AKT1 vs HRASV12 global_efficiency_rfp ----
# Function to calculate the median difference
median_diff <- function(x, y) {
  return(abs(median(x) - median(y)))
}

# Function to perform permutation test
perm_test <- function(group1, group2, num_perm = 10000) {
  observed_diff <- median_diff(group1, group2)
  combined <- c(group1, group2)
  perm_diffs <- replicate(num_perm, {
    perm_sample <- sample(combined)
    perm_group1 <- perm_sample[1:length(group1)]
    perm_group2 <- perm_sample[(length(group1) + 1):length(combined)]
    median_diff(perm_group1, perm_group2)
  })
  p_value <- mean(perm_diffs >= observed_diff)
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_global_efficiency_rfp_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(global_efficiency_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(global_efficiency_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(global_efficiency_rfp)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_global_efficiency_rfp_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_global_efficiency_rfp_list to data.table
results_global_efficiency_rfp_dt <- data.table(Age = integer(),
                                               Comparison = character(),
                                               Observed_Difference = numeric(),
                                               P_value = numeric())

for (age in names(results_global_efficiency_rfp_list)) {
  results_global_efficiency_rfp_dt <- rbind(results_global_efficiency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_global_efficiency_rfp_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_global_efficiency_rfp_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_global_efficiency_rfp_dt <- rbind(results_global_efficiency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_global_efficiency_rfp_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_global_efficiency_rfp_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_global_efficiency_rfp_dt <- rbind(results_global_efficiency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_global_efficiency_rfp_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_global_efficiency_rfp_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}


#   for CTRL vs AKT1 vs HRASV12 IRF8 mutants global_efficiency_rfp ----
# Filter the dataframe to include only _IRF8null conditions
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))

# Unique ages in the dataset
irf8null_ages <- unique(irf8null_conditions$Age)

# Initialize a list to store results
irf8null_global_efficiency_rfp_results_list <- list()

# Loop through each age group
for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(global_efficiency_rfp)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(global_efficiency_rfp)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(global_efficiency_rfp)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_global_efficiency_rfp_results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

# Convert irf8null_global_efficiency_rfp_results_list to data.table
irf8null_global_efficiency_rfp_results_dt <- data.table(Age = integer(),
                                                        Comparison = character(),
                                                        Observed_Difference = numeric(),
                                                        P_value = numeric())

for (age in names(irf8null_global_efficiency_rfp_results_list)) {
  irf8null_global_efficiency_rfp_results_dt <- rbind(irf8null_global_efficiency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_global_efficiency_rfp_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_rfp_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_global_efficiency_rfp_results_dt <- rbind(irf8null_global_efficiency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_global_efficiency_rfp_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_rfp_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_global_efficiency_rfp_results_dt <- rbind(irf8null_global_efficiency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_global_efficiency_rfp_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_global_efficiency_rfp_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Merge the two data tables into one
global_efficiency_rfp_p_values <- rbind(
  results_global_efficiency_rfp_dt,
  irf8null_global_efficiency_rfp_results_dt
)

# Print the merged data.table
print(global_efficiency_rfp_p_values)

#   for IRF8+/+ vs IRF8-/- global_efficiency_rfp ----
perm_test_irf8_comparisons <- function(results_df, condition1, condition2) {
  # Unique ages in the dataset
  ages <- unique(results_df$Age)
  
  # Initialize a list to store results
  results_list <- list()
  
  # Loop through each age group
  for (age in ages) {
    age_data <- results_df %>% filter(Age == age)
    
    data1 <- age_data %>% filter(Condition == condition1) %>% pull(global_efficiency_rfp)
    data2 <- age_data %>% filter(Condition == condition2) %>% pull(global_efficiency_rfp)
    
    test_result <- perm_test(data1, data2)
    
    results_list[[as.character(age)]] <- list(
      Comparison = paste(condition1, "vs", condition2, sep = "_"),
      Observed_Difference = test_result$observed_diff,
      P_value = test_result$p_value
    )
  }
  
  # Convert results_list to data.table
  results_dt <- data.table(Age = integer(), Comparison = character(), Observed_Difference = numeric(), P_value = numeric())
  
  for (age in names(results_list)) {
    results_dt <- rbind(results_dt, data.table(
      Age = as.integer(age),
      Comparison = results_list[[age]]$Comparison,
      Observed_Difference = results_list[[age]]$Observed_Difference,
      P_value = results_list[[age]]$P_value
    ))
  }
  
  return(results_dt)
}

# Perform permutation tests for AKT1 vs AKT1_IRF8null global_efficiency_rfp
akt1_vs_akt1_irf8null_rfp_dt <- perm_test_irf8_comparisons(results_df, "AKT1", "AKT1_IRF8null")

# Perform permutation tests for HRASV12 vs HRASV12_IRF8null global_efficiency_rfp
hrasv12_vs_hrasv12_irf8null_rfp_dt <- perm_test_irf8_comparisons(results_df, "HRASV12", "HRASV12_IRF8null")

# Perform permutation tests for CTRL vs CTRL_IRF8null global_efficiency_rfp
ctrl_vs_ctrl_irf8null_rfp_dt <- perm_test_irf8_comparisons(results_df, "CTRL", "CTRL_IRF8null")

# Merge all results into the global_efficiency_rfp_p_values data.table
global_efficiency_rfp_p_values <- rbind(
  global_efficiency_rfp_p_values,
  akt1_vs_akt1_irf8null_rfp_dt,
  hrasv12_vs_hrasv12_irf8null_rfp_dt,
  ctrl_vs_ctrl_irf8null_rfp_dt
)

# Print the merged data.table
print(global_efficiency_rfp_p_values)

                         

#   for CTRL vs AKT1 vs HRASV12 frequency ----
# Function to calculate the median difference
median_diff <- function(x, y) {
  return(abs(median(x) - median(y)))
}

# Function to perform permutation test
perm_test <- function(group1, group2, num_perm = 10000) {
  observed_diff <- median_diff(group1, group2)
  combined <- c(group1, group2)
  perm_diffs <- replicate(num_perm, {
    perm_sample <- sample(combined)
    perm_group1 <- perm_sample[1:length(group1)]
    perm_group2 <- perm_sample[(length(group1) + 1):length(combined)]
    median_diff(perm_group1, perm_group2)
  })
  p_value <- mean(perm_diffs >= observed_diff)
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_frequency_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(frequency)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(frequency)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(frequency)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_frequency_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_frequency_list to data.table
results_frequency_dt <- data.table(Age = integer(),
                                   Comparison = character(),
                                   Observed_Difference = numeric(),
                                   P_value = numeric())

for (age in names(results_frequency_list)) {
  results_frequency_dt <- rbind(results_frequency_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_frequency_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_frequency_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_frequency_dt <- rbind(results_frequency_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_frequency_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_frequency_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_frequency_dt <- rbind(results_frequency_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_frequency_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_frequency_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

#   for CTRL vs AKT1 vs HRASV12 IRF8 mutants frequency ----
# Filter the dataframe to include only _IRF8null conditions
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))

# Unique ages in the dataset
irf8null_ages <- unique(irf8null_conditions$Age)

# Initialize a list to store results
irf8null_frequency_results_list <- list()

# Loop through each age group
for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(frequency)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(frequency)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(frequency)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_frequency_results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

# Convert irf8null_frequency_results_list to data.table
irf8null_frequency_results_dt <- data.table(Age = integer(),
                                            Comparison = character(),
                                            Observed_Difference = numeric(),
                                            P_value = numeric())

for (age in names(irf8null_frequency_results_list)) {
  irf8null_frequency_results_dt <- rbind(irf8null_frequency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_frequency_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_frequency_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_frequency_results_dt <- rbind(irf8null_frequency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_frequency_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_frequency_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_frequency_results_dt <- rbind(irf8null_frequency_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_frequency_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_frequency_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Print the data.table
print(irf8null_frequency_results_dt)

# Merge the two data tables into one
frequency_p_values <- rbind(
  results_frequency_dt,
  irf8null_frequency_results_dt
)

# Print the merged data.table
print(frequency_p_values)

#   for IRF8+/+ vs IRF8-/- ----
# Function to perform permutation test for AKT1 vs AKT1_IRF8null, HRASV12 vs HRASV12_IRF8null, CTRL vs CTRL_IRF8null
perm_test_irf8_comparisons <- function(results_df, condition1, condition2) {
  # Unique ages in the dataset
  ages <- unique(results_df$Age)
  
  # Initialize a list to store results
  results_list <- list()
  
  # Loop through each age group
  for (age in ages) {
    age_data <- results_df %>% filter(Age == age)
    
    data1 <- age_data %>% filter(Condition == condition1) %>% pull(frequency)
    data2 <- age_data %>% filter(Condition == condition2) %>% pull(frequency)
    
    test_result <- perm_test(data1, data2)
    
    results_list[[as.character(age)]] <- list(
      Comparison = paste(condition1, "vs", condition2, sep = "_"),
      Observed_Difference = test_result$observed_diff,
      P_value = test_result$p_value
    )
  }
  
  # Convert results_list to data.table
  results_dt <- data.table(Age = integer(), Comparison = character(), Observed_Difference = numeric(), P_value = numeric())
  
  for (age in names(results_list)) {
    results_dt <- rbind(results_dt, data.table(
      Age = as.integer(age),
      Comparison = results_list[[age]]$Comparison,
      Observed_Difference = results_list[[age]]$Observed_Difference,
      P_value = results_list[[age]]$P_value
    ))
  }
  
  return(results_dt)
}

# Perform permutation tests for AKT1 vs AKT1_IRF8null
akt1_vs_akt1_irf8null_frequency_dt <- perm_test_irf8_comparisons(results_df, "AKT1", "AKT1_IRF8null")

# Perform permutation tests for HRASV12 vs HRASV12_IRF8null
hrasv12_vs_hrasv12_irf8null_frequency_dt <- perm_test_irf8_comparisons(results_df, "HRASV12", "HRASV12_IRF8null")

# Perform permutation tests for CTRL vs CTRL_IRF8null
ctrl_vs_ctrl_irf8null_frequency_dt <- perm_test_irf8_comparisons(results_df, "CTRL", "CTRL_IRF8null")

# Merge all results into the frequency_p_values data.table
frequency_p_values <- rbind(
  frequency_p_values,
  akt1_vs_akt1_irf8null_frequency_dt,
  hrasv12_vs_hrasv12_irf8null_frequency_dt,
  ctrl_vs_ctrl_irf8null_frequency_dt
)

# Print the merged data.table
print(frequency_p_values)


#   for CTRL vs AKT1 vs HRASV12 frequency_rfp ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_frequency_rfp_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(frequency_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(frequency_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(frequency_rfp)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_frequency_rfp_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_frequency_rfp_list to data.table
results_frequency_rfp_dt <- data.table(Age = integer(),
                                       Comparison = character(),
                                       Observed_Difference = numeric(),
                                       P_value = numeric())

for (age in names(results_frequency_rfp_list)) {
  results_frequency_rfp_dt <- rbind(results_frequency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_frequency_rfp_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_frequency_rfp_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_frequency_rfp_dt <- rbind(results_frequency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_frequency_rfp_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_frequency_rfp_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_frequency_rfp_dt <- rbind(results_frequency_rfp_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_frequency_rfp_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_frequency_rfp_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

#   for CTRL vs AKT1 vs HRASV12 IRF8 mutants frequency_rfp ----
# Filter the dataframe to include only _IRF8null conditions
irf8null_conditions <- results_df %>% filter(grepl("_IRF8null", Condition))

# Unique ages in the dataset
irf8null_ages <- unique(irf8null_conditions$Age)

# Initialize a list to store results
irf8null_frequency_rfp_results_list <- list()

# Loop through each age group
for (age in irf8null_ages) {
  age_data <- irf8null_conditions %>% filter(Age == age)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(frequency_rfp)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(frequency_rfp)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(frequency_rfp)
  
  test_akt1_ctrl_irf8null <- perm_test(akt1_irf8null_data, ctrl_irf8null_data)
  test_akt1_hrasv12_irf8null <- perm_test(akt1_irf8null_data, hrasv12_irf8null_data)
  test_ctrl_hrasv12_irf8null <- perm_test(ctrl_irf8null_data, hrasv12_irf8null_data)
  
  irf8null_frequency_rfp_results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl_irf8null,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12_irf8null,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12_irf8null
  )
}

# Convert irf8null_frequency_rfp_results_list to data.table
irf8null_frequency_rfp_results_dt <- data.table(Age = integer(),
                                                Comparison = character(),
                                                Observed_Difference = numeric(),
                                                P_value = numeric())

for (age in names(irf8null_frequency_rfp_results_list)) {
  irf8null_frequency_rfp_results_dt <- rbind(irf8null_frequency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = irf8null_frequency_rfp_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$observed_diff,
    P_value = irf8null_frequency_rfp_results_list[[age]]$AKT1_IRF8null_vs_CTRL_IRF8null$p_value
  ))
  
  irf8null_frequency_rfp_results_dt <- rbind(irf8null_frequency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_frequency_rfp_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_frequency_rfp_results_list[[age]]$AKT1_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
  
  irf8null_frequency_rfp_results_dt <- rbind(irf8null_frequency_rfp_results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = irf8null_frequency_rfp_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$observed_diff,
    P_value = irf8null_frequency_rfp_results_list[[age]]$CTRL_IRF8null_vs_HRASV12_IRF8null$p_value
  ))
}

# Merge the two data tables into one
frequency_rfp_p_values <- rbind(
  results_frequency_rfp_dt,
  irf8null_frequency_rfp_results_dt
)

#   for IRF8+/+ vs IRF8-/- frequency_rfp ----
# Function to perform permutation test for AKT1 vs AKT1_IRF8null, HRASV12 vs HRASV12_IRF8null, CTRL vs CTRL_IRF8null
perm_test_irf8_comparisons <- function(results_df, condition1, condition2, metric) {
  # Unique ages in the dataset
  ages <- unique(results_df$Age)
  
  # Initialize a list to store results
  results_list <- list()
  
  # Loop through each age group
  for (age in ages) {
    age_data <- results_df %>% filter(Age == age)
    
    data1 <- age_data %>% filter(Condition == condition1) %>% pull(!!as.symbol(metric))
    data2 <- age_data %>% filter(Condition == condition2) %>% pull(!!as.symbol(metric))
    
    test_result <- perm_test(data1, data2)
    
    results_list[[as.character(age)]] <- list(
      Comparison = paste(condition1, "vs", condition2, sep = "_"),
      Observed_Difference = test_result$observed_diff,
      P_value = test_result$p_value
    )
  }
  
  # Convert results_list to data.table
  results_dt <- data.table(Age = integer(), Comparison = character(), Observed_Difference = numeric(), P_value = numeric())
  
  for (age in names(results_list)) {
    results_dt <- rbind(results_dt, data.table(
      Age = as.integer(age),
      Comparison = results_list[[age]]$Comparison,
      Observed_Difference = results_list[[age]]$Observed_Difference,
      P_value = results_list[[age]]$P_value
    ))
  }
  
  return(results_dt)
}

# Perform permutation tests for frequency_rfp
akt1_vs_akt1_irf8null_frequency_rfp_dt <- perm_test_irf8_comparisons(results_df, "AKT1", "AKT1_IRF8null", "frequency_rfp")
hrasv12_vs_hrasv12_irf8null_frequency_rfp_dt <- perm_test_irf8_comparisons(results_df, "HRASV12", "HRASV12_IRF8null", "frequency_rfp")
ctrl_vs_ctrl_irf8null_frequency_rfp_dt <- perm_test_irf8_comparisons(results_df, "CTRL", "CTRL_IRF8null", "frequency_rfp")

# Merge all results into the frequency_rfp_p_values data.table
frequency_rfp_p_values <- rbind(
  frequency_rfp_p_values,
  akt1_vs_akt1_irf8null_frequency_rfp_dt,
  hrasv12_vs_hrasv12_irf8null_frequency_rfp_dt,
  ctrl_vs_ctrl_irf8null_frequency_rfp_dt
)

# Print the merged data.table
print(frequency_rfp_p_values)

      
#   for CTRL vs AKT1 vs HRASV12 mean_degree ----
# Function to calculate the median difference
median_diff <- function(x, y) {
  return(abs(median(x) - median(y)))
}

# Function to perform permutation test
perm_test <- function(group1, group2, num_perm = 10000) {
  observed_diff <- median_diff(group1, group2)
  combined <- c(group1, group2)
  perm_diffs <- replicate(num_perm, {
    perm_sample <- sample(combined)
    perm_group1 <- perm_sample[1:length(group1)]
    perm_group2 <- perm_sample[(length(group1) + 1):length(combined)]
    median_diff(perm_group1, perm_group2)
  })
  p_value <- mean(perm_diffs >= observed_diff)
  return(list(observed_diff = observed_diff, p_value = p_value))
}

# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(mean_degree)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(mean_degree)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(mean_degree)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

mean_degree_IRF8pos_p_values <- results_dt

#   for CTRL_IRF8null vs AKT1_IRF8null vs HRASV12_IRF8null mean_degree ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(mean_degree)
  ctrl_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(mean_degree)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(mean_degree)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
}

mean_degree_IRF8null_p_values <- results_dt

#   for AKT1 vs AKT1_IRF8null, CTRL vs CTRL_IRF8null, HRASV12 vs HRASV12_IRF8null mean_degree ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(mean_degree)
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(mean_degree)
  
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(mean_degree)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(mean_degree)
  
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(mean_degree)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(mean_degree)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null,
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$p_value
  ))
}

mean_degree_IRF8null_vs_regular_p_values <- results_dt


mean_degree_all_p_values <- rbind(mean_degree_IRF8pos_p_values, mean_degree_IRF8null_p_values, mean_degree_IRF8null_vs_regular_p_values)

#   for CTRLvsAKT1vsHRASV12 mean_degree_rfp ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(mean_degree_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(mean_degree_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(mean_degree_rfp)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

mean_degree_IRF8pos_p_values <- results_dt

#   for CTRL_IRF8null vs AKT1_IRF8null vs HRASV12_IRF8null mean_degree_rfp ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(mean_degree_rfp)
  ctrl_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(mean_degree_rfp)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(mean_degree_rfp)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
}

mean_degree_rfp_IRF8null_p_values <- results_dt

#   for AKT1 vs AKT1_IRF8null, CTRL vs CTRL_IRF8null, HRASV12 vs HRASV12_IRF8null mean_degree_rfp ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(mean_degree_rfp)
  
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(mean_degree_rfp)
  
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(mean_degree_rfp)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(mean_degree_rfp)
  
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(mean_degree_rfp)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(mean_degree_rfp)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null,
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$p_value
  ))
}

mean_degree_rfp_IRF8null_vs_IRF8pos_p_values <- results_dt

mean_degree_all_rfp_p_values <- rbind(mean_degree_rfp_p_values, mean_degree_rfp_IRF8null_p_values, mean_degree_rfp_IRF8null_vs_IRF8pos_p_values)

# for CTRLvsAKT1vsHRASV12 Top5PC_XVariance ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(Top5PC_XVariance)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(Top5PC_XVariance)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(Top5PC_XVariance)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

Top5PC_XVariance_p_values <- results_dt

# for CTRL_IRF8null vs AKT1_IRF8null vs HRASV12_IRF8null Top5PC_XVariance ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(Top5PC_XVariance)
  ctrl_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(Top5PC_XVariance)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(Top5PC_XVariance)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
}

Top5PC_XVariance_IRF8null_p_values <- results_dt

# for AKT1 vs AKT1_IRF8null, CTRL vs CTRL_IRF8null, HRASV12 vs HRASV12_IRF8null Top5PC_XVariance ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(Top5PC_XVariance)
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(Top5PC_XVariance)
  
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(Top5PC_XVariance)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(Top5PC_XVariance)
  
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(Top5PC_XVariance)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(Top5PC_XVariance)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null,
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$p_value
  ))
}

Top5PC_XVariance_IRF8null_vs_regular_p_values <- results_dt

Top5PC_XVariance_all_p_values <- rbind(Top5PC_XVariance_p_values, Top5PC_XVariance_IRF8null_p_values, Top5PC_XVariance_IRF8null_vs_regular_p_values)

# # for CTRLvsAKT1vsHRASV12 Top5PC_XVariance_rfp ----
# # Unique ages in the dataset
# ages <- unique(results_df$Age)
# 
# # Initialize a list to store results
# results_list <- list()
# 
# # Loop through each age group
# for (age in ages) {
#   age_data <- results_df %>% filter(Age == age)
#   
#   akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(Top5PC_XVariance_rfp)
#   ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(Top5PC_XVariance_rfp)
#   hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(Top5PC_XVariance_rfp)
#   
#   test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
#   test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
#   test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
#   
#   results_list[[as.character(age)]] <- list(
#     "AKT1_vs_CTRL" = test_akt1_ctrl,
#     "AKT1_vs_HRASV12" = test_akt1_hrasv12,
#     "CTRL_vs_HRASV12" = test_ctrl_hrasv12
#   )
# }
# 
# # Convert results_list to data.table
# results_dt <- data.table(Age = integer(),
#                          Comparison = character(),
#                          Observed_Difference = numeric(),
#                          P_value = numeric())
# 
# for (age in names(results_list)) {
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "AKT1_vs_CTRL",
#     Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
#     P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "AKT1_vs_HRASV12",
#     Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
#     P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "CTRL_vs_HRASV12",
#     Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
#     P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
#   ))
# }
# 
# Top5PC_XVariance_rfp_p_values <- results_dt
# 
# # for CTRL_IRF8null vs AKT1_IRF8null vs HRASV12_IRF8null Top5PC_XVariance_rfp ----
# # Unique ages in the dataset
# ages <- unique(results_df$Age)
# 
# # Initialize a list to store results
# results_list <- list()
# 
# # Loop through each age group
# for (age in ages) {
#   age_data <- results_df %>% filter(Age == age)
#   
#   akt1_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   ctrl_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   hrasv12_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   
#   test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
#   test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
#   test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
#   
#   results_list[[as.character(age)]] <- list(
#     "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl,
#     "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12,
#     "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12
#   )
# }
# 
# # Convert results_list to data.table
# results_dt <- data.table(Age = integer(),
#                          Comparison = character(),
#                          Observed_Difference = numeric(),
#                          P_value = numeric())
# 
# for (age in names(results_list)) {
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
#     Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
#     Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
#     Observed_Difference = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$p_value
#   ))
# }
# 
# Top5PC_XVariance_rfp_IRF8null_p_values <- results_dt
# 
# #   for AKT1 vs AKT1_IRF8null, CTRL vs CTRL_IRF8null, HRASV12 vs HRASV12_IRF8null Top5PC_XVariance_rfp ----
# # Unique ages in the dataset
# ages <- unique(results_df$Age)
# 
# # Initialize a list to store results
# results_list <- list()
# 
# # Loop through each age group
# for (age in ages) {
#   age_data <- results_df %>% filter(Age == age)
#   
#   akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(Top5PC_XVariance_rfp)
#   akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   
#   ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(Top5PC_XVariance_rfp)
#   ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   
#   hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(Top5PC_XVariance_rfp)
#   hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(Top5PC_XVariance_rfp)
#   
#   test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
#   test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
#   test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
#   
#   results_list[[as.character(age)]] <- list(
#     "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
#     "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null,
#     "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
#   )
# }
# 
# # Convert results_list to data.table
# results_dt <- data.table(Age = integer(),
#                          Comparison = character(),
#                          Observed_Difference = numeric(),
#                          P_value = numeric())
# 
# for (age in names(results_list)) {
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "AKT1_vs_AKT1_IRF8null",
#     Observed_Difference = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "CTRL_vs_CTRL_IRF8null",
#     Observed_Difference = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`CTRL_vs_CTRL_IRF8null`$p_value
#   ))
#   
#   results_dt <- rbind(results_dt, data.table(
#     Age = as.integer(age),
#     Comparison = "HRASV12_vs_HRASV12_IRF8null",
#     Observed_Difference = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$observed_diff,
#     P_value = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$p_value
#   ))
# }
# 
# Top5PC_XVariance_rfp_IRF8null_vs_regular_p_values <- results_dt


# for CTRLvsAKT1vsHRASV12 RFPtoNonRFP_normalized ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(RFPtoNonRFP_normalized)
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(RFPtoNonRFP_normalized)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(RFPtoNonRFP_normalized)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_CTRL" = test_akt1_ctrl,
    "AKT1_vs_HRASV12" = test_akt1_hrasv12,
    "CTRL_vs_HRASV12" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_CTRL",
    Observed_Difference = results_list[[age]]$AKT1_vs_CTRL$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_CTRL$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_HRASV12",
    Observed_Difference = results_list[[age]]$AKT1_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$AKT1_vs_HRASV12$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_vs_HRASV12",
    Observed_Difference = results_list[[age]]$CTRL_vs_HRASV12$observed_diff,
    P_value = results_list[[age]]$CTRL_vs_HRASV12$p_value
  ))
}

RFPtoNonRFP_normalized_p_values <- results_dt

# for CTRL_IRF8null vs AKT1_IRF8null vs HRASV12_IRF8null RFPtoNonRFP_normalized ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  ctrl_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  
  test_akt1_ctrl <- perm_test(akt1_data, ctrl_data)
  test_akt1_hrasv12 <- perm_test(akt1_data, hrasv12_data)
  test_ctrl_hrasv12 <- perm_test(ctrl_data, hrasv12_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_IRF8null_vs_CTRL_IRF8null" = test_akt1_ctrl,
    "AKT1_IRF8null_vs_HRASV12_IRF8null" = test_akt1_hrasv12,
    "CTRL_IRF8null_vs_HRASV12_IRF8null" = test_ctrl_hrasv12
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_CTRL_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_CTRL_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "CTRL_IRF8null_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`CTRL_IRF8null_vs_HRASV12_IRF8null`$p_value
  ))
}

RFPtoNonRFP_normalized_IRF8null_p_values <- results_dt

# for AKT1 vs AKT1_IRF8null, CTRL vs CTRL_IRF8null, HRASV12 vs HRASV12_IRF8null RFPtoNonRFP_normalized ----
# Unique ages in the dataset
ages <- unique(results_df$Age)

# Initialize a list to store results
results_list <- list()

# Loop through each age group
for (age in ages) {
  age_data <- results_df %>% filter(Age == age)
  
  akt1_data <- age_data %>% filter(Condition == "AKT1") %>% pull(RFPtoNonRFP_normalized)
  akt1_irf8null_data <- age_data %>% filter(Condition == "AKT1_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  
  ctrl_data <- age_data %>% filter(Condition == "CTRL") %>% pull(RFPtoNonRFP_normalized)
  ctrl_irf8null_data <- age_data %>% filter(Condition == "CTRL_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  
  hrasv12_data <- age_data %>% filter(Condition == "HRASV12") %>% pull(RFPtoNonRFP_normalized)
  hrasv12_irf8null_data <- age_data %>% filter(Condition == "HRASV12_IRF8null") %>% pull(RFPtoNonRFP_normalized)
  
  test_akt1_vs_akt1_irf8null <- perm_test(akt1_data, akt1_irf8null_data)
  test_ctrl_vs_ctrl_irf8null <- perm_test(ctrl_data, ctrl_irf8null_data)
  test_hrasv12_vs_hrasv12_irf8null <- perm_test(hrasv12_data, hrasv12_irf8null_data)
  
  results_list[[as.character(age)]] <- list(
    "AKT1_vs_AKT1_IRF8null" = test_akt1_vs_akt1_irf8null,
    "CTRL_vs_CTRL_IRF8null" = test_ctrl_vs_ctrl_irf8null,
    "HRASV12_vs_HRASV12_IRF8null" = test_hrasv12_vs_hrasv12_irf8null
  )
}

# Convert results_list to data.table
results_dt <- data.table(Age = integer(),
                         Comparison = character(),
                         Observed_Difference = numeric(),
                         P_value = numeric())

for (age in names(results_list)) {
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "AKT1_vs_AKT1_IRF8null",
    Observed_Difference = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`AKT1_vs_AKT1_IRF8null`$p_value
  ))
  
  results_dt <- rbind(results_dt, data.table(
    Age = as.integer(age),
    Comparison = "HRASV12_vs_HRASV12_IRF8null",
    Observed_Difference = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$observed_diff,
    P_value = results_list[[age]]$`HRASV12_vs_HRASV12_IRF8null`$p_value
  ))
}

RFPtoNonRFP_normalized_IRF8null_vs_regular_p_values <- results_dt

# Combine all results
RFPtoNonRFP_normalized_all_p_values <- rbind(
  RFPtoNonRFP_normalized_p_values, 
  RFPtoNonRFP_normalized_IRF8null_p_values, 
  RFPtoNonRFP_normalized_IRF8null_vs_regular_p_values
)

# View the combined results
print(RFPtoNonRFP_normalized_all_p_values)

    