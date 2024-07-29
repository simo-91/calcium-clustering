# MFing permutational testing ----
# Define the permutation test function using median as the test statistic
permutation_test_median <- function(data, group_var, value_var, n_permutations = 1000) {
  observed_diff <- median(data[[value_var]][data[[group_var]] == levels(data[[group_var]])[1]]) - 
    median(data[[value_var]][data[[group_var]] == levels(data[[group_var]])[2]])
  
  perm_diffs <- replicate(n_permutations, {
    permuted_data <- data
    permuted_data[[group_var]] <- sample(permuted_data[[group_var]])
    median(permuted_data[[value_var]][permuted_data[[group_var]] == levels(permuted_data[[group_var]])[1]]) - 
      median(permuted_data[[value_var]][permuted_data[[group_var]] == levels(permuted_data[[group_var]])[2]])
  })
  
  p_value <- mean(abs(perm_diffs) >= abs(observed_diff))
  return(list(observed_diff = observed_diff, perm_diffs = perm_diffs, p_value = p_value))
}

# Perform permutation tests for each Age and Condition comparison
permutation_results <- fish_level_data %>%
  group_by(Age) %>%
  do({
    conditions <- unique(.$Condition)
    if (length(conditions) == 2) {
      test_result <- permutation_test_median(., "Condition", "Median_Frequency")
      data.frame(Observed_Difference = test_result$observed_diff, P_Value = test_result$p_value)
    } else {
      data.frame(Observed_Difference = NA, P_Value = NA)
    }
  })


# Ensure permutation_results is in a dataframe format
permutation_results_df <- as.data.frame(permutation_results)

# Add Age and Condition information to permutation_results
permutation_results_df <- fish_level_data %>%
  group_by(Age) %>%
  summarise(Observed_Difference = unique(permutation_results$Observed_Difference),
            P_Value = unique(permutation_results$P_Value))

# Plot the results
ggplot(permutation_results_df, aes(x = Age, y = Observed_Difference, color = P_Value < 0.05)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), size = 1) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Significant") +
  labs(title = "Median Event Frequency Difference by Age and Condition",
       x = "Age",
       y = "Observed Median Difference") +
  theme_minimal()
