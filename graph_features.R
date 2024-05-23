# Graph features library ----
# Load necessary libraries
library(igraph)

# Define the smallworldness function
smallworldness <- function(graph) {
  # Generate an equivalent Erdos Renyi network (same number of edges and nodes, random allocation)
  random_smallworld <- erdos.renyi.game(n = vcount(graph), p = ecount(graph), type = "gnm")
  # Small-worldness as sigma = ( C(g)/C(random-g) ) / ( (L(g)/L(random-g) )
  sigma.graph <- (transitivity(graph)/transitivity(random_smallworld)) / (average.path.length(graph)/average.path.length(random_smallworld))
  return(sigma.graph)
}

# Get all graph variables in the global environment
graph_vars <- ls(pattern = "^ID\\d{4}\\.graph$")

# Filter to start from ID0152
graph_ids <- as.numeric(gsub("ID(\\d{4})\\.graph", "\\1", graph_vars))
filtered_graph_vars <- graph_vars[graph_ids >= 152]

# Debugging: Print filtered graph variables
print("Filtered graph variables starting from ID0152:")
print(filtered_graph_vars)

# Initialize a list to store results
results <- list()

# Loop through each graph variable, get the graph and corresponding metrics, and calculate small-worldness
for (var_name in filtered_graph_vars) {
  # Get the graph from the global environment
  graph <- get(var_name)
  
  # Calculate the small-worldness
  sigma <- smallworldness(graph)
  
  # Get the clustering coefficient
  clustcoeff_var_name <- gsub("\\.graph$", ".clustcoeff", var_name)
  clustcoeff <- get(clustcoeff_var_name)
  
  # Get the clustering coefficient RFP
  clustcoeff_rfp_var_name <- gsub("\\.graph$", ".clustcoeff.RFP", var_name)
  clustcoeff_rfp <- get(clustcoeff_rfp_var_name)
  
  # Get the global efficiency
  globeff_var_name <- gsub("\\.graph$", ".globaleff", var_name)
  globeff <- get(globeff_var_name)
  
  # Get the global efficiency RFP
  globeff_rfp_var_name <- gsub("\\.graph$", ".globaleff.RFP", var_name)
  globeff_rfp <- get(globeff_rfp_var_name)
  
  # Store the results
  results[[var_name]] <- list(
    smallworldness = sigma,
    clustering_coefficient = clustcoeff,
    clustering_coefficient_rfp = clustcoeff_rfp,
    global_efficiency = globeff,
    global_efficiency_rfp = globeff_rfp
  )
}

# Convert the list to a data frame for easier viewing
results_df <- do.call(rbind, lapply(names(results), function(x) c(ID = x, results[[x]])))
results_df <- as.data.frame(results_df)

# Print the results
print("Calculated metrics:")
print(results_df)
