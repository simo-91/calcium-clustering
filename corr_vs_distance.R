# Cross-correlation coefficient VS Distance (uM) ----
# Extract edge list and weights from the graph
edges <- get.edgelist(ID0181.graph)
weights <- E(ID0181.graph)$weight

# Create a function to calculate the Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Calculate distances for each edge
distances <- numeric(length(weights))
# Calculate distances for each edge
for (i in 1:nrow(edges)) {
  idx1 <- which(posXY$Cell == edges[i, 1])
  idx2 <- which(posXY$Cell == edges[i, 2])
  
  if (length(idx1) == 1 && length(idx2) == 1) {
    distances[i] <- euclidean_distance(posXY$X[idx1], posXY$Y[idx1], posXY$X[idx2], posXY$Y[idx2])
  } else {
    distances[i] <- NA
  }
}

# Combine distances and correlation coefficients into a data frame
distance_correlations <- data.frame(Distance = distances, Correlation = weights)

# Plot
ggplot(data, aes(x = Distance, y = Correlation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Correlation Coefficient vs Distance",
       x = "Distance (micrometers)",
       y = "Correlation Coefficient") +
  theme_minimal()


# Load necessary libraries
library(igraph)
library(ggplot2)
library(progress)
# Define posXY_vars and graphs_vars first containing lists of all IDs

# Initialize an empty data frame to store all results
all_data <- data.frame()

# Define the Euclidean distance function
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Processing graphs [:bar] :percent in :elapsed",
  total = length(graph_vars), clear = FALSE, width = 60
)

# Loop through each graph and corresponding positional data
for (i in 1:length(graph_vars)) {
  graph_name <- graph_vars[i]
  posXY_name <- posXY_vars[i]
  
  # Retrieve the actual graph and posXY objects using get()
  graph <- get(graph_name)
  posXY <- get(posXY_name)
  
  # Extract edge list and weights from the graph
  edges <- get.edgelist(graph)
  weights <- E(graph)$weight
  
  # Calculate distances for each edge
  distances <- numeric(length(weights))
  for (j in 1:nrow(edges)) {
    idx1 <- which(posXY$Cell == edges[j, 1])
    idx2 <- which(posXY$Cell == edges[j, 2])
    
    if (length(idx1) == 1 && length(idx2) == 1) {
      distances[j] <- euclidean_distance(posXY$X[idx1], posXY$Y[idx1], posXY$X[idx2], posXY$Y[idx2])
    } else {
      distances[j] <- NA
    }
  }
  
  # Combine distances and correlation coefficients into a data frame
  distance_correlations <- data.frame(Distance = distances, Correlation = weights)
  
  # Add an identifier column to distinguish different graphs
  distance_correlations$Graph <- graph_name
  
  # Append the results to the all_data data frame
  all_data <- rbind(all_data, distance_correlations)
  
  # Update progress bar
  pb$tick()
}
distance_correlations.df <- all_data
# Remove any rows with NA values (if any)
all_data <- na.omit(distance_correlations.df)


# RFP only cells ----
# Initialize progress bar
pb <- progress_bar$new(
  format = "  Processing graphs [:bar] :percent in :elapsed",
  total = length(graph_vars), clear = FALSE, width = 60
)
# Initialize an empty data frame to store all results
all_data_rfp <- data.frame()
# Loop through each graph and corresponding positional data
for (i in 1:length(graph_vars)) {
  graph_name <- graph_vars[i]
  posXY_name <- posXY_vars[i]
  
  # Retrieve the actual graph and posXY objects using get()
  graph <- get(graph_name)
  posXY <- get(posXY_name)
  
  # Filter posXY to include only RFP-positive cells
  posXY_rfp <- posXY[posXY$RFP == TRUE, ]
  
  # Extract edge list and weights from the graph
  edges <- get.edgelist(graph)
  weights <- E(graph)$weight
  
  # Calculate distances for each edge, considering only RFP-positive cells
  distances <- numeric(length(weights))
  for (j in 1:nrow(edges)) {
    idx1 <- which(posXY_rfp$Cell == edges[j, 1])
    idx2 <- which(posXY_rfp$Cell == edges[j, 2])
    
    if (length(idx1) == 1 && length(idx2) == 1) {
      distances[j] <- euclidean_distance(posXY_rfp$X[idx1], posXY_rfp$Y[idx1], posXY_rfp$X[idx2], posXY_rfp$Y[idx2])
    } else {
      distances[j] <- NA
    }
  }
  
  # Combine distances and correlation coefficients into a data frame
  distance_correlations <- data.frame(Distance = distances, Correlation = weights)
  
  # Add an identifier column to distinguish different graphs
  distance_correlations$Graph <- graph_name
  
  # Append the results to the all_data_rfp data frame
  all_data_rfp <- rbind(all_data_rfp, distance_correlations)
  
  # Update progress bar
  pb$tick()
}

# Remove any rows with NA values (if any)
all_data_rfp <- na.omit(all_data_rfp)



# both ----
# Initialize an empty data frame to store all results
all_data <- data.frame()

# Define the Euclidean distance function
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Processing graphs [:bar] :percent in :elapsed",
  total = length(graph_vars), clear = FALSE, width = 60
)

# Loop through each graph and corresponding positional data
for (i in 1:length(graph_vars)) {
  graph_name <- graph_vars[i]
  posXY_name <- posXY_vars[i]
  
  # Retrieve the actual graph and posXY objects using get()
  graph <- get(graph_name)
  posXY <- get(posXY_name)
  
  # Extract edge list and weights from the graph
  edges <- get.edgelist(graph)
  weights <- E(graph)$weight
  
  # Initialize vectors to store RFP status
  rfp_status <- rep(NA, length(weights))
  
  # Calculate distances for each edge and determine RFP status
  distances <- numeric(length(weights))
  for (j in 1:nrow(edges)) {
    idx1 <- which(posXY$Cell == edges[j, 1])
    idx2 <- which(posXY$Cell == edges[j, 2])
    
    if (length(idx1) == 1 && length(idx2) == 1) {
      distances[j] <- euclidean_distance(posXY$X[idx1], posXY$Y[idx1], posXY$X[idx2], posXY$Y[idx2])
      rfp_status[j] <- ifelse(posXY$RFP[idx1] & posXY$RFP[idx2], "Both RFP", ifelse(posXY$RFP[idx1] | posXY$RFP[idx2], "One RFP", "None RFP"))
    } else {
      distances[j] <- NA
    }
  }
  
  # Combine distances, correlation coefficients, and RFP status into a data frame
  distance_correlations <- data.frame(Distance = distances, Correlation = weights, RFP_Status = rfp_status)
  
  # Add an identifier column to distinguish different graphs
  distance_correlations$Graph <- graph_name
  
  # Append the results to the all_data data frame
  all_data <- rbind(all_data, distance_correlations)
  
  # Update progress bar
  pb$tick()
}

# Remove any rows with NA values (if any)
all_data <- na.omit(all_data)


# with data.table ----
# Initialize an empty data.table to store all results
all_data.table <- data.table()

# Define the Euclidean distance function
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Processing graphs [:bar] :percent in :elapsed",
  total = length(graph_vars), clear = FALSE, width = 60
)

# Loop through each graph and corresponding positional data
for (i in 1:length(graph_vars)) {
  graph_name <- graph_vars[i]
  posXY_name <- posXY_vars[i]
  
  # Retrieve the actual graph and posXY objects using get()
  graph <- get(graph_name)
  posXY <- get(posXY_name)
  
  # Convert posXY to data.table
  posXY <- as.data.table(posXY)
  
  # Extract edge list and weights from the graph
  edges <- as.data.table(get.edgelist(graph))
  setnames(edges, c("V1", "V2"))
  weights <- E(graph)$weight
  
  # Initialize vectors to store distances and RFP status
  distances <- numeric(nrow(edges))
  rfp_status <- character(nrow(edges))
  
  # Calculate distances for each edge and determine RFP status
  for (j in 1:nrow(edges)) {
    idx1 <- which(posXY$Cell == edges[j, V1])
    idx2 <- which(posXY$Cell == edges[j, V2])
    
    if (length(idx1) == 1 && length(idx2) == 1) {
      distances[j] <- euclidean_distance(posXY[idx1, X], posXY[idx1, Y], posXY[idx2, X], posXY[idx2, Y])
      rfp_status[j] <- ifelse(posXY[idx1, RFP] & posXY[idx2, RFP], "Both RFP", 
                              ifelse(posXY[idx1, RFP] | posXY[idx2, RFP], "One RFP", "None RFP"))
    } else {
      distances[j] <- NA
      rfp_status[j] <- NA
    }
  }
  
  # Combine distances, correlation coefficients, and RFP status into a data.table
  distance_correlations <- data.table(Distance = distances, Correlation = weights, RFP_Status = rfp_status, Graph = graph_name)
  
  # Append the results to the all_data.table data.table
  all_data.table <- rbind(all_data.table, distance_correlations, fill = TRUE)
  
  # Update progress bar
  pb$tick()
}

# Remove any rows with NA values (if any)
all_data.table <- na.omit(all_data.table)