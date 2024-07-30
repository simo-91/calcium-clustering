# Perm test for CTRLvsAKT1vsHRASV12 clustcoeff ----
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

# Perm test for CTRLvsAKT1vsHRASV12 IRF mutants clustcoeff ----
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

# AKT1 vs AKT1_IRF8null Clustcoeff perm test ----
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


# Perm test for CTRL vs AKT1 vs HRASV12 clustering_coefficient_rfp ----
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

# Perm test for CTRL vs AKT1 vs HRASV12 IRF8null clustering_coefficient_rfp ----
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
perm_test_results_clustcoeff_rfp.df <- rbind(results_rfp_clustcoeff_dt, irf8null_rfp_clustcoeff_dt)

# Print the merged results
print(perm_test_results_clustcoeff_rfp.df)
