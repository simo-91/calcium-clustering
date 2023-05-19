# Create an empty dataframe
mean_Ca <- data.frame(ID = character(), Mean_Ca = numeric(), stringsAsFactors = FALSE)

# Loop over the variables and populate the dataframe
for (i in 31:143) {
  var_name <- paste0("ID", sprintf("%04d", i), "_mean_Ca")
  if (exists(var_name)) {
    values <- get(var_name)
    if (length(values) > 0) {
      # Extract the ID value without the "_mean_Ca" suffix
      id <- gsub("_mean_Ca", "", var_name)
      df <- data.frame(ID = id, Mean_Ca = values)
      mean_Ca <- rbind(mean_Ca, df)
    } else {
      # Handle case when variable exists but has no values
      cat("Variable", var_name, "exists but has no values.\n")
    }
  } else {
    # Handle case when variable is not found
    cat("Variable", var_name, "not found.\n")
  }
}

# Print the resulting dataframe
print(mean_Ca)

# ----
# Specify the range of ID numbers
id_numbers <- c(130:132, 139:143)

# Initialize an empty vector for storing mean values
AKT1_5dpf.RFP__mean_Ca <- numeric(length(id_numbers))

# Loop over the ID numbers and calculate the mean for each
for (i in seq_along(id_numbers)) {
  id <- sprintf("ID%04d", id_numbers[i])
  data <- get(paste0(id, ".RFP_mean_Ca"))
  mean_value <- mean(data, na.rm = TRUE)
  AKT1_5dpf.RFP__mean_Ca[i] <- mean_value
}



# Comparisons!
# 4dpf - CTRL vs HRASV12 vs AKT1
# Determine the maximum length among the vectors
max_length <- max(length(CTRL_4dpf__mean_Ca), length(HRASV12_4dpf__mean_Ca), length(AKT1_4dpf__mean_Ca),
                  length(CTRL_5dpf__mean_Ca), length(HRASV12_5dpf__mean_Ca), length(AKT1_5dpf__mean_Ca),
                  length(CTRL_4dpf.RFP__mean_Ca), length(HRASV12_4dpf.RFP__mean_Ca), length(AKT1_4dpf.RFP__mean_Ca),
                  length(CTRL_5dpf.RFP__mean_Ca), length(HRASV12_5dpf.RFP__mean_Ca), length(AKT1_5dpf.RFP__mean_Ca))

# Create a data frame with NA values
mean_Ca_df <- data.frame(CTRL_4dpf__mean_Ca = rep(NA, max_length),
                           HRASV12_4dpf__mean_Ca = rep(NA, max_length),
                           AKT1_4dpf__mean_Ca = rep(NA, max_length),
                           CTRL_5dpf__mean_Ca = rep(NA, max_length),
                           HRASV12_5dpf__mean_Ca = rep(NA, max_length),
                           AKT1_5dpf__mean_Ca = rep(NA, max_length),
                           CTRL_4dpf.RFP__mean_Ca = rep(NA, max_length),
                           HRASV12_4dpf.RFP__mean_Ca = rep(NA, max_length),
                           AKT1_4dpf.RFP__mean_Ca = rep(NA, max_length),
                           CTRL_5dpf.RFP__mean_Ca = rep(NA, max_length),
                           HRASV12_5dpf.RFP__mean_Ca = rep(NA, max_length),
                           AKT1_5dpf.RFP__mean_Ca = rep(NA, max_length))

# Assign vectors to the data frame columns
mean_Ca_df[1:length(CTRL_4dpf__mean_Ca), "CTRL_4dpf__mean_Ca"] <- CTRL_4dpf__mean_Ca
mean_Ca_df[1:length(HRASV12_4dpf__mean_Ca), "HRASV12_4dpf__mean_Ca"] <- HRASV12_4dpf__mean_Ca
mean_Ca_df[1:length(AKT1_4dpf__mean_Ca), "AKT1_4dpf__mean_Ca"] <- AKT1_4dpf__mean_Ca

mean_Ca_df[1:length(CTRL_5dpf__mean_Ca), "CTRL_5dpf__mean_Ca"] <- CTRL_5dpf__mean_Ca
mean_Ca_df[1:length(HRASV12_5dpf__mean_Ca), "HRASV12_5dpf__mean_Ca"] <- HRASV12_5dpf__mean_Ca
mean_Ca_df[1:length(AKT1_5dpf__mean_Ca), "AKT1_5dpf__mean_Ca"] <- AKT1_5dpf__mean_Ca

mean_Ca_df[1:length(CTRL_4dpf.RFP__mean_Ca), "CTRL_4dpf.RFP__mean_Ca"] <- CTRL_4dpf.RFP__mean_Ca
mean_Ca_df[1:length(HRASV12_4dpf.RFP__mean_Ca), "HRASV12_4dpf.RFP__mean_Ca"] <- HRASV12_4dpf.RFP__mean_Ca
mean_Ca_df[1:length(AKT1_4dpf.RFP__mean_Ca), "AKT1_4dpf.RFP__mean_Ca"] <- AKT1_4dpf.RFP__mean_Ca

mean_Ca_df[1:length(CTRL_5dpf.RFP__mean_Ca), "CTRL_5dpf.RFP__mean_Ca"] <- CTRL_5dpf.RFP__mean_Ca
mean_Ca_df[1:length(HRASV12_5dpf.RFP__mean_Ca), "HRASV12_5dpf.RFP__mean_Ca"] <- HRASV12_5dpf.RFP__mean_Ca
mean_Ca_df[1:length(AKT1_5dpf.RFP__mean_Ca), "AKT1_5dpf.RFP__mean_Ca"] <- AKT1_5dpf.RFP__mean_Ca

mean_Ca_df <- melt(mean_Ca_df, variable.name = "Genotype", value.name = "events/min")
mean_Ca_df <- na.omit(mean_Ca_df)
