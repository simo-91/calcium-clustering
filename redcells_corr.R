# redcell ratios

# CTRL
IDs <- paste0("ID00", 40:57)
CTRL.redcell.ratio <- mget(paste0(IDs, "redcell.ratio"))

# HRASV12
IDs <- c(paste0("ID00", 31:37),
         paste0("ID00", 58:68),
         paste0("ID0", 112:118))
HRASV12.redcell.ratio <- mget(paste0(IDs, "redcell.ratio"))

# AKT1
IDs <- paste0("ID0", 119:143)
AKT1.redcell.ratio <- mget(paste0(IDs, "redcell.ratio"))


# Correlation with..
# # ..clustering coefficient

# # Creating the lists
# Create an empty list to store the extracted arrays
CTRL__clustcoeff <- list()

# Define the list names to include
list_names <- c("CTRL_4dpf_hind", "CTRL_4dpf_mid", "CTRL_5dpf_hind", "CTRL_5dpf_mid")

# Iterate over the list names
for (list_name in list_names) {
  # Extract the arrays "IDXXXX.clustcoeff" and "IDXXXX.clustcoeff.RFP"
  clustcoeff <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff$", names(all_lists[[list_name]]))]
  clustcoeff_rfp <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff\\.RFP$", names(all_lists[[list_name]]))]
  
  # Combine the extracted arrays into a single list
  CTRL__clustcoeff <- c(CTRL__clustcoeff, clustcoeff, clustcoeff_rfp)
}

# Create an empty list to store the extracted arrays
AKT1__clustcoeff <- list()

# Define the list names to include
list_names <- c("AKT1_4dpf_hind", "AKT1_4dpf_mid", "AKT1_5dpf_hind", "AKT1_5dpf_mid")

# Iterate over the list names
for (list_name in list_names) {
  # Extract the arrays "IDXXXX.clustcoeff" and "IDXXXX.clustcoeff.RFP"
  clustcoeff <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff$", names(all_lists[[list_name]]))]
  clustcoeff_rfp <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff\\.RFP$", names(all_lists[[list_name]]))]
  
  # Combine the extracted arrays into a single list
  AKT1__clustcoeff <- c(AKT1__clustcoeff, clustcoeff, clustcoeff_rfp)
}

# Create an empty list to store the extracted arrays
HRASV12__clustcoeff <- list()

# Define the list names to include
list_names <- c("HRASV12_4dpf_hind", "HRASV12_4dpf_mid", "HRASV12_5dpf_hind", "HRASV12_5dpf_mid")

# Iterate over the list names
for (list_name in list_names) {
  # Extract the arrays "IDXXXX.clustcoeff" and "IDXXXX.clustcoeff.RFP"
  clustcoeff <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff$", names(all_lists[[list_name]]))]
  clustcoeff_rfp <- all_lists[[list_name]][grepl("^ID\\d{4}\\.clustcoeff\\.RFP$", names(all_lists[[list_name]]))]
  
  # Combine the extracted arrays into a single list
  HRASV12__clustcoeff <- c(HRASV12__clustcoeff, clustcoeff, clustcoeff_rfp)
}

# CTRL

CTRL__clustcoeff <- lapply(CTRL__clustcoeff, function(x) replace(x, is.nan(x), 0))

# Create a new list to store the extracted elements
CTRL__clustcoeff.RFP <- CTRL__clustcoeff[names(CTRL__clustcoeff) %in% grep("^ID\\d{4}\\.clustcoeff\\.RFP$", names(CTRL__clustcoeff), value = TRUE)]

# Remove the extracted elements from CTRL__clustcoeff
CTRL__clustcoeff <- CTRL__clustcoeff[!names(CTRL__clustcoeff) %in% names(CTRL__clustcoeff.RFP)]


cor(unlist(CTRL__clustcoeff), unlist(CTRL.redcell.ratio))

# HRASV12
HRASV12__clustcoeff <- lapply(HRASV12__clustcoeff, function(x) replace(x, is.nan(x), 0))

# Create a new list to store the extracted elements
HRASV12__clustcoeff.RFP <- HRASV12__clustcoeff[names(HRASV12__clustcoeff) %in% grep("^ID\\d{4}\\.clustcoeff\\.RFP$", names(HRASV12__clustcoeff), value = TRUE)]

# Remove the extracted elements from HRASV12__clustcoeff
HRASV12__clustcoeff <- HRASV12__clustcoeff[!names(HRASV12__clustcoeff) %in% names(HRASV12__clustcoeff.RFP)]


cor(unlist(HRASV12__clustcoeff), unlist(HRASV12.redcell.ratio))

# AKT1

AKT1__clustcoeff <- lapply(AKT1__clustcoeff, function(x) replace(x, is.nan(x), 0))

# Create a new list to store the extracted elements
AKT1__clustcoeff.RFP <- AKT1__clustcoeff[names(AKT1__clustcoeff) %in% grep("^ID\\d{4}\\.clustcoeff\\.RFP$", names(AKT1__clustcoeff), value = TRUE)]

# Remove the extracted elements from AKT1__clustcoeff
AKT1__clustcoeff <- AKT1__clustcoeff[!names(AKT1__clustcoeff) %in% names(AKT1__clustcoeff.RFP)]


cor(unlist(AKT1__clustcoeff), unlist(AKT1.redcell.ratio))
