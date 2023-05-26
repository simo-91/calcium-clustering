# Isolate hindbrain and midbrain files
# Create an empty list to hold your variables
HRASV12_4dpf_hind <- list()

# List all variables in global environment
all_vars <- ls()

# Filter variable names containing desired IDs
ids <- c("ID0031", "ID0033", "ID0035", "ID0036", "ID0058", "ID0062", "ID0112", "ID0114")
for (id in ids) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if(length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      HRASV12_4dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Create an empty list to hold your variables
HRASV12_4dpf_mid <- list()

# List all variables in global environment
all_vars <- ls()

# Filter variable names containing desired IDs
ids <- c("ID0032", "ID0034", "ID0037", "ID0059", "ID0060", "ID0061", "ID0063", "ID0113", "ID0115")
for (id in ids) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if(length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      HRASV12_4dpf_mid[[var_name]] <- get(var_name)
    }
  }
}

# Create an empty list to hold your variables
HRASV12_5dpf_hind <- list()

# List all variables in the global environment
all_vars <- ls()

# Filter variable names containing desired IDs
ids <- c("ID0064", "ID0066", "ID0068", "ID0117", "ID0118")
for (id in ids) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      HRASV12_5dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Create an empty list to hold your variables
HRASV12_5dpf_mid <- list()

# List all variables in the global environment
all_vars <- ls()

# Filter variable names containing desired IDs
ids <- c("ID0065", "ID0067", "ID0116")
for (id in ids) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      HRASV12_5dpf_mid[[var_name]] <- get(var_name)
    }
  }
}

# Create empty lists to hold your variables
AKT1_4dpf_hind <- list()
AKT1_4dpf_mid <- list()
AKT1_5dpf_hind <- list()
AKT1_5dpf_mid <- list()

# List all variables in the global environment
all_vars <- ls()

# Filter variable names containing desired IDs for AKT1_4dpf_hind
ids_AKT1_4dpf_hind <- c("ID0119", "ID0121", "ID0122", "ID0124", "ID0126", "ID0128", "ID0133", "ID0134", "ID0136", "ID0137", "ID0138")
for (id in ids_AKT1_4dpf_hind) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      AKT1_4dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for AKT1_4dpf_mid
ids_AKT1_4dpf_mid <- c("ID0120", "ID0123", "ID0125", "ID0127", "ID0129")
for (id in ids_AKT1_4dpf_mid) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      AKT1_4dpf_mid[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for AKT1_5dpf_hind
ids_AKT1_5dpf_hind <- c("ID0130", "ID0131", "ID0140", "ID0141", "ID0143")
for (id in ids_AKT1_5dpf_hind) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      AKT1_5dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for AKT1_5dpf_mid
ids_AKT1_5dpf_mid <- c("ID0132", "ID0142")
for (id in ids_AKT1_5dpf_mid) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      AKT1_5dpf_mid[[var_name]] <- get(var_name)
    }
  }
}


# Create empty lists to hold your variables
CTRL_4dpf_hind <- list()
CTRL_4dpf_mid <- list()
CTRL_5dpf_hind <- list()
CTRL_5dpf_mid <- list()

# Filter variable names containing desired IDs for CTRL_4dpf_hind
ids_CTRL_4dpf_hind <- c("ID0040", "ID0042", "ID0044", "ID0046")
for (id in ids_CTRL_4dpf_hind) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      CTRL_4dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for CTRL_4dpf_mid
ids_CTRL_4dpf_mid <- c("ID0041", "ID0043", "ID0045", "ID0047")
for (id in ids_CTRL_4dpf_mid) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      CTRL_4dpf_mid[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for CTRL_5dpf_hind
ids_CTRL_5dpf_hind <- c("ID0048", "ID0050", "ID0052", "ID0054", "ID0056")
for (id in ids_CTRL_5dpf_hind) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      CTRL_5dpf_hind[[var_name]] <- get(var_name)
    }
  }
}

# Filter variable names containing desired IDs for CTRL_5dpf_mid
ids_CTRL_5dpf_mid <- c("ID0049", "ID0051", "ID0053", "ID0055", "ID0057")
for (id in ids_CTRL_5dpf_mid) {
  matching_vars <- grep(id, all_vars, value = TRUE)
  
  # If any variable names match, add the variables to your list
  if (length(matching_vars) > 0) {
    for (var_name in matching_vars) {
      CTRL_5dpf_mid[[var_name]] <- get(var_name)
    }
  }
}

# Save HRASV12_4dpf_hind list into .rda file
save(HRASV12_4dpf_hind, file = "HRASV12_4dpf_hind.rda")

# Save HRASV12_4dpf_mid list into .rda file
save(HRASV12_4dpf_mid, file = "HRASV12_4dpf_mid.rda")

# Save HRASV12_5dpf_hind list into .rda file
save(HRASV12_5dpf_hind, file = "HRASV12_5dpf_hind.rda")

# Save HRASV12_5dpf_mid list into .rda file
save(HRASV12_5dpf_mid, file = "HRASV12_5dpf_mid.rda")

# Save AKT1_4dpf_hind list into .rda file
save(AKT1_4dpf_hind, file = "AKT1_4dpf_hind.rda")

# Save AKT1_4dpf_mid list into .rda file
save(AKT1_4dpf_mid, file = "AKT1_4dpf_mid.rda")

# Save AKT1_5dpf_hind list into .rda file
save(AKT1_5dpf_hind, file = "AKT1_5dpf_hind.rda")

# Save AKT1_5dpf_mid list into .rda file
save(AKT1_5dpf_mid, file = "AKT1_5dpf_mid.rda")

# Save CTRL_4dpf_hind list into .rda file
save(CTRL_4dpf_hind, file = "CTRL_4dpf_hind.rda")

# Save CTRL_4dpf_mid list into .rda file
save(CTRL_4dpf_mid, file = "CTRL_4dpf_mid.rda")

# Save CTRL_5dpf_hind list into .rda file
save(CTRL_5dpf_hind, file = "CTRL_5dpf_hind.rda")

# Save CTRL_5dpf_mid list into .rda file
save(CTRL_5dpf_mid, file = "CTRL_5dpf_mid.rda")
# Create a list to hold all the lists
all_lists <- list(
  HRASV12_4dpf_hind = HRASV12_4dpf_hind,
  HRASV12_4dpf_mid = HRASV12_4dpf_mid,
  HRASV12_5dpf_hind = HRASV12_5dpf_hind,
  HRASV12_5dpf_mid = HRASV12_5dpf_mid,
  AKT1_4dpf_hind = AKT1_4dpf_hind,
  AKT1_4dpf_mid = AKT1_4dpf_mid,
  AKT1_5dpf_hind = AKT1_5dpf_hind,
  AKT1_5dpf_mid = AKT1_5dpf_mid,
  CTRL_4dpf_hind = CTRL_4dpf_hind,
  CTRL_4dpf_mid = CTRL_4dpf_mid,
  CTRL_5dpf_hind = CTRL_5dpf_hind,
  CTRL_5dpf_mid = CTRL_5dpf_mid
)

# Save the all_lists into .rda file
save(all_lists, file = "data/all_lists.rda")

