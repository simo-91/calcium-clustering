# Set the directory path
directory <- "~/calcium-clustering/data/"

# Create an empty list to store the arrays
mean_arrays <- list()

# Loop over the files in the directory
for (i in 88:143) {
  # Generate the file name
  file_name <- paste0("ID", sprintf("%04d", i), "_posXY.rds")
  
  # Construct the full file path
  file_path <- file.path(directory, file_name)
  
  # Use tryCatch to handle errors
  tryCatch({
    # Read the data from the file
    data <- readRDS(file_path)
    
    # Check if the "Mean" column exists in the data
    if ("Mean" %in% names(data)) {
      # Extract the "Mean" column and store it in the mean_arrays list
      mean_arrays[[i - 87]] <- data[["Mean"]]
    } else {
      # Print a message if the "Mean" column is not found
      cat("Column 'Mean' not found in", file_name, "\n")
    }
  }, error = function(e) {
    # Print a message for files that encounter errors
    cat("Error occurred while reading", file_name, "\n")
  })
}

# Rename the list elements with the desired variable names
for (i in 88:143) {
  assign(paste0("ID", sprintf("%04d", i), "_mean_Ca"), mean_arrays[[i - 87]])
}


# for RFP
# Set the directory path
directory <- "~/calcium-clustering/data/"

# Create an empty list to store the arrays
mean_arrays <- vector("list", length = 37 - 31 + 1)

# Loop over the files in the directory
for (i in 31:37) {
  # Generate the file name
  file_name <- paste0("ID", sprintf("%04d", i), "_posXY.rds")
  
  # Construct the full file path
  file_path <- file.path(directory, file_name)
  
  # Use tryCatch to handle errors
  tryCatch({
    # Read the data from the file
    data <- readRDS(file_path)
    
    # Check if the "Mean" and "redcell" columns exist in the data
    if ("Mean" %in% names(data) && "redcell" %in% names(data)) {
      # Extract the "Mean" column values where "redcell" column is 1
      mean_values <- data$Mean[data$redcell == 1]
      
      # Store the mean_values in the mean_arrays list
      mean_arrays[[i - 30]] <- mean_values
    } else {
      # Print a message if the columns are not found
      cat("Columns 'Mean' and/or 'redcell' not found in", file_name, "\n")
    }
  }, error = function(e) {
    # Print a message for files that encounter errors
    cat("Error occurred while reading", file_name, "\n")
  })
}

# Rename the list elements with the desired variable names
for (i in 31:37) {
  assign(paste0("ID", sprintf("%04d", i), ".RFP_mean_Ca"), mean_arrays[[i - 87]])
}

