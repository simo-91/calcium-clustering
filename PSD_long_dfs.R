# PSD in ggplot objects 
#  CTRL 4dpf ----
CTRL_4dpf <- data.frame(ID = sprintf("ID%04d", 186:195),
                        PSD_Mean = c(ID0186.psd.mean, ID0187.psd.mean, ID0188.psd.mean, ID0189.psd.mean, 
                                     ID0190.psd.mean, ID0191.psd.mean, ID0192.psd.mean, ID0193.psd.mean, 
                                     ID0194.psd.mean, ID0195.psd.mean), # Continue adding all your actual variables
                        Condition = "CTRL",
                        dpf = 4)




# Step 1: Extract the Frequency column from one of the data frames
frequency_column <- ID0186.psd.mean$frequency

# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the IDs and extract the PSD mean column for each
for (ID in 186:195) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}


# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)
# Assume psd_means_df contains only PSD mean columns at this point

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
CTRL_4dpf_PSD.df <- data.frame(Frequency = frequency_column, Row_Mean = row_wise_mean)


#  CTRL 5dpf ----
# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the IDs and extract the PSD mean column for each
for (ID in 196:205) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}


# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)
# Assume psd_means_df contains only PSD mean columns at this point

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
CTRL_5dpf_PSD.df <- data.frame(Frequency = frequency_column, Row_Mean = row_wise_mean)


#  CTRL 6dpf ----
# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the IDs and extract the PSD mean column for each
for (ID in 206:214) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}


# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)
# Assume psd_means_df contains only PSD mean columns at this point

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
CTRL_6dpf_PSD.df <- data.frame(Frequency = frequency_column, Row_Mean = row_wise_mean)



#  CTRL 8dpf ----
# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the IDs and extract the PSD mean column for each
for (ID in 215:221) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}


# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)
# Assume psd_means_df contains only PSD mean columns at this point

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
CTRL_8dpf_PSD.df <- data.frame(Frequency = frequency_column, Row_Mean = row_wise_mean)


# HRASV12 4dpf ----
# Step 1: Combine the two ranges into a single vector
ids <- c(222:231, 247:249, 251:253)

# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the combined IDs and extract the PSD mean column for each
for (ID in ids) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}

# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
HRASV12_4dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df, Row_Mean = row_wise_mean)


# HRASV12 5pf ----
# Step 1: Combine the two ranges into a single vector
ids <- c(232:238, 254:258)

# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the combined IDs and extract the PSD mean column for each
for (ID in ids) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}

# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
HRASV12_5dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df, Row_Mean = row_wise_mean)


# HRASV12 6dpf ----
# Step 1: Combine the two ranges into a single vector
ids <- c(239:243, 259:262)

# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the combined IDs and extract the PSD mean column for each
for (ID in ids) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}

# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
HRASV12_6dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df, Row_Mean = row_wise_mean)


# HRASV12 8dpf ----
# Step 1: Combine the two ranges into a single vector
ids <- c(244:246, 263:264)

# Step 2: Initialize a list to store PSD mean columns
psd_means_list <- list()

# Loop through the combined IDs and extract the PSD mean column for each
for (ID in ids) {
  # Construct the variable name for each ID
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  
  # Evaluate the variable name to get the actual variable
  psd_data <- eval(parse(text = var_name))
  
  # Add the PSD mean column to the list
  psd_means_list[[length(psd_means_list) + 1]] <- psd_data$`PSD mean`
}

# Combine the PSD mean columns into a data frame
psd_means_df <- do.call(cbind, psd_means_list)

# Calculate the row-wise mean of the PSD mean columns
row_wise_mean <- rowMeans(psd_means_df, na.rm = TRUE)

# Combine the Frequency column, PSD means, and Row Mean into a final dataframe
HRASV12_8dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df, Row_Mean = row_wise_mean)


# AKT1 4dpf ----
ids_4dpf <- 152:160

psd_means_list_4dpf <- list()
for (ID in ids_4dpf) {
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  psd_data <- eval(parse(text = var_name))
  psd_means_list_4dpf[[length(psd_means_list_4dpf) + 1]] <- psd_data$`PSD mean`
}
psd_means_df_4dpf <- do.call(cbind, psd_means_list_4dpf)
row_wise_mean_4dpf <- rowMeans(psd_means_df_4dpf, na.rm = TRUE)
AKT1_4dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df_4dpf, Row_Mean = row_wise_mean_4dpf)

# AKT1 5dpf ----
ids_5dpf <- 161:169

psd_means_list_5dpf <- list()
for (ID in ids_5dpf) {
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  psd_data <- eval(parse(text = var_name))
  psd_means_list_5dpf[[length(psd_means_list_5dpf) + 1]] <- psd_data$`PSD mean`
}
psd_means_df_5dpf <- do.call(cbind, psd_means_list_5dpf)
row_wise_mean_5dpf <- rowMeans(psd_means_df_5dpf, na.rm = TRUE)
AKT1_5dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df_5dpf, Row_Mean = row_wise_mean_5dpf)


# AKT1 6dpf ----
ids_6dpf <- 170:177

psd_means_list_6dpf <- list()
for (ID in ids_6dpf) {
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  psd_data <- eval(parse(text = var_name))
  psd_means_list_6dpf[[length(psd_means_list_6dpf) + 1]] <- psd_data$`PSD mean`
}
psd_means_df_6dpf <- do.call(cbind, psd_means_list_6dpf)
row_wise_mean_6dpf <- rowMeans(psd_means_df_6dpf, na.rm = TRUE)
AKT1_6dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df_6dpf, Row_Mean = row_wise_mean_6dpf)

# AKT1 8dpf ----
ids_8dpf <- c(178,179,181:185)

psd_means_list_8dpf <- list()
for (ID in ids_8dpf) {
  var_name <- paste0("ID", sprintf("%04d", ID), ".psd.mean")
  psd_data <- eval(parse(text = var_name))
  psd_means_list_8dpf[[length(psd_means_list_8dpf) + 1]] <- psd_data$`PSD mean`
}
psd_means_df_8dpf <- do.call(cbind, psd_means_list_8dpf)
row_wise_mean_8dpf <- rowMeans(psd_means_df_8dpf, na.rm = TRUE)
AKT1_8dpf_PSD.df <- data.frame(Frequency = frequency_column, PSD_Means = psd_means_df_8dpf, Row_Mean = row_wise_mean_8dpf)


# Create a new dataframe with the Frequency column
combined_PSD_df <- data.frame(Frequency = CTRL_4dpf_PSD.df$Frequency)

# Add the Row_Mean columns from each dataframe
combined_PSD_df$CTRL_4dpf <- CTRL_4dpf_PSD.df$Row_Mean
combined_PSD_df$CTRL_5dpf <- CTRL_5dpf_PSD.df$Row_Mean
combined_PSD_df$CTRL_6dpf <- CTRL_6dpf_PSD.df$Row_Mean
combined_PSD_df$CTRL_8dpf <- CTRL_8dpf_PSD.df$Row_Mean

combined_PSD_df$AKT1_4dpf <- AKT1_4dpf_PSD.df$Row_Mean
combined_PSD_df$AKT1_5dpf <- AKT1_5dpf_PSD.df$Row_Mean
combined_PSD_df$AKT1_6dpf <- AKT1_6dpf_PSD.df$Row_Mean
combined_PSD_df$AKT1_8dpf <- AKT1_8dpf_PSD.df$Row_Mean

combined_PSD_df$HRASV12_4dpf <- HRASV12_4dpf_PSD.df$Row_Mean
combined_PSD_df$HRASV12_5dpf <- HRASV12_5dpf_PSD.df$Row_Mean
combined_PSD_df$HRASV12_6dpf <- HRASV12_6dpf_PSD.df$Row_Mean
combined_PSD_df$HRASV12_8dpf <- HRASV12_8dpf_PSD.df$Row_Mean


# CTRL comparisons across age ----
# Convert from wide to long format
psd_long <- pivot_longer(combined_PSD_df, 
                         cols = starts_with("CTRL"), 
                         names_to = "Age", 
                         values_to = "PSD",
                         names_prefix = "CTRL_")

# Now, plotting with ggplot2
ggplot(data = psd_long, aes(x = Frequency, y = PSD, color = Age)) +
  geom_line() +
  labs(title = "PSD values across different ages for CTRL",
       x = "Frequency", 
       y = "PSD") +
  theme_minimal() +
  scale_color_manual(values = c("4dpf" = "blue", "5dpf" = "red", "6dpf" = "green", "8dpf" = "purple"))



# HRASV12 comparisons across age ----
# Convert from wide to long format
psd_long <- pivot_longer(combined_PSD_df, 
                         cols = starts_with("HRASV12"), 
                         names_to = "Age", 
                         values_to = "PSD",
                         names_prefix = "HRASV12_")

# Now, plotting with ggplot2
ggplot(data = psd_long, aes(x = Frequency, y = PSD, color = Age)) +
  geom_line() +
  labs(title = "PSD values across different ages for HRASV12",
       x = "Frequency", 
       y = "PSD") +
  theme_minimal() +
  scale_color_manual(values = c("4dpf" = "blue", "5dpf" = "red", "6dpf" = "green", "8dpf" = "purple"))


# AKT1 comparisons across age ----
# Convert from wide to long format
psd_long <- pivot_longer(combined_PSD_df, 
                         cols = starts_with("AKT1"), 
                         names_to = "Age", 
                         values_to = "PSD",
                         names_prefix = "AKT1_")

# Now, plotting with ggplot2
ggplot(data = psd_long, aes(x = Frequency, y = PSD, color = Age)) +
  geom_line() +
  labs(title = "PSD values across different ages for AKT1",
       x = "Frequency", 
       y = "PSD") +
  theme_minimal() +
  scale_color_manual(values = c("4dpf" = "blue", "5dpf" = "red", "6dpf" = "green", "8dpf" = "purple"))


# PSD across conditions at 4dpf ----
# Combine the data frames for each condition and add a 'Condition' column
PSD_4dpf_df <- data.frame(
  "Frequency" = combined_PSD_df$Frequency,
  "CTRL" = combined_PSD_df$CTRL_4dpf,
  "HRASV12" = combined_PSD_df$HRASV12_4dpf,
  "AKT1" = combined_PSD_df$AKT1_4dpf
)

PSD_4dpf_long <- PSD_4dpf_df %>% 
  pivot_longer(
    cols = -Frequency, 
    names_to = "Condition", 
    values_to = "PSD",
    names_prefix = "\\w+_"
  ) %>%
  mutate(Condition = factor(Condition, levels = c("CTRL", "HRASV12", "AKT1")))

# Now, plotting with ggplot2
PSD_all_conditions_4dpf.plt <- ggplot(PSD_4dpf_long, aes(x = Frequency, y = PSD, color = Condition)) +
  geom_line() +
  labs(title = "PSD across Conditions at 4dpf",
       x = "Frequency",
       y = "PSD",
       color = "Condition") +
  theme_minimal()+
  ylim(0, 0.025)

# PSD across conditions at 5dpf ----
# Combine the data frames for each condition and add a 'Condition' column
PSD_5dpf_df <- data.frame(
  "Frequency" = combined_PSD_df$Frequency,
  "CTRL" = combined_PSD_df$CTRL_5dpf,
  "HRASV12" = combined_PSD_df$HRASV12_5dpf,
  "AKT1" = combined_PSD_df$AKT1_5dpf
)

PSD_5dpf_long <- PSD_5dpf_df %>% 
  pivot_longer(
    cols = -Frequency, 
    names_to = "Condition", 
    values_to = "PSD",
    names_prefix = "\\w+_"
  ) %>%
  mutate(Condition = factor(Condition, levels = c("CTRL", "HRASV12", "AKT1")))

# Now, plotting with ggplot2
PSD_all_conditions_5dpf.plt <- ggplot(PSD_5dpf_long, aes(x = Frequency, y = PSD, color = Condition)) +
  geom_line() +
  labs(title = "PSD across Conditions at 5dpf",
       x = "Frequency",
       y = "PSD",
       color = "Condition") +
  theme_minimal()+
  ylim(0, 0.025)


# PSD across conditions at 6dpf ----
# Combine the data frames for each condition and add a 'Condition' column
PSD_6dpf_df <- data.frame(
  "Frequency" = combined_PSD_df$Frequency,
  "CTRL" = combined_PSD_df$CTRL_6dpf,
  "HRASV12" = combined_PSD_df$HRASV12_6dpf,
  "AKT1" = combined_PSD_df$AKT1_6dpf
)

PSD_6dpf_long <- PSD_6dpf_df %>% 
  pivot_longer(
    cols = -Frequency, 
    names_to = "Condition", 
    values_to = "PSD",
    names_prefix = "\\w+_"
  ) %>%
  mutate(Condition = factor(Condition, levels = c("CTRL", "HRASV12", "AKT1")))

# Now, plotting with ggplot2
PSD_all_conditions_6dpf.plt <- ggplot(PSD_6dpf_long, aes(x = Frequency, y = PSD, color = Condition)) +
  geom_line() +
  labs(title = "PSD across Conditions at 6dpf",
       x = "Frequency",
       y = "PSD",
       color = "Condition") +
  theme_minimal()+
  ylim(0, 0.025)


# PSD across conditions at 8dpf ----
# Combine the data frames for each condition and add a 'Condition' column
PSD_8dpf_df <- data.frame(
  "Frequency" = combined_PSD_df$Frequency,
  "CTRL" = combined_PSD_df$CTRL_8dpf,
  "HRASV12" = combined_PSD_df$HRASV12_8dpf,
  "AKT1" = combined_PSD_df$AKT1_8dpf
)

PSD_8dpf_long <- PSD_8dpf_df %>% 
  pivot_longer(
    cols = -Frequency, 
    names_to = "Condition", 
    values_to = "PSD",
    names_prefix = "\\w+_"
  ) %>%
  mutate(Condition = factor(Condition, levels = c("CTRL", "HRASV12", "AKT1")))

# Now, plotting with ggplot2
PSD_all_conditions_8dpf.plt <- ggplot(PSD_8dpf_long2, aes(x = Frequency, y = PSD, color = Condition)) +
  geom_line() +
  labs(title = "PSD across Conditions at 8dpf",
       x = "mHz",
       y = "PSD",
       color = "Condition") +
  theme_minimal()+
  ylim(0, 0.025)
