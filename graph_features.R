# Load necessary libraries
library(igraph)
library(data.table)

# Define the smallworldness function with a check for transitivity
smallworldness <- function(graph) {
  # Generate an equivalent Erdos Renyi network (same number of edges and nodes, random allocation)
  random_smallworld <- erdos.renyi.game(n = vcount(graph), p = ecount(graph), type = "gnm")
  trans_random <- transitivity(random_smallworld)
  # Check if transitivity of the random graph is zero
  if (trans_random == 0) {
    sigma.graph <- 0  # Set small-worldness to 0 if the random graph has zero transitivity
  } else {
    # Small-worldness as sigma = ( C(g)/C(random-g) ) / ( (L(g)/L(random-g) )
    sigma.graph <- (transitivity(graph) / transitivity(random_smallworld)) / (average.path.length(graph) / average.path.length(random_smallworld))
  }
  return(sigma.graph)
}

# Load the database file
database_file <- "database_IDs.csv"
database_ids <- fread(database_file)

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
  clustcoeff <- if (exists(clustcoeff_var_name)) get(clustcoeff_var_name) else NA
  
  # Get the clustering coefficient RFP
  clustcoeff_rfp_var_name <- gsub("\\.graph$", ".clustcoeff.RFP", var_name)
  clustcoeff_rfp <- if (exists(clustcoeff_rfp_var_name)) get(clustcoeff_rfp_var_name) else NA
  
  # Get the global efficiency
  globeff_var_name <- gsub("\\.graph$", ".globaleff", var_name)
  globeff <- if (exists(globeff_var_name)) get(globeff_var_name) else NA
  
  # Get the global efficiency RFP
  globeff_rfp_var_name <- gsub("\\.graph$", ".globaleff.RFP", var_name)
  globeff_rfp <- if (exists(globeff_rfp_var_name)) get(globeff_rfp_var_name) else NA
  
  # Extract ID to cross-reference with the database
  id <- gsub("\\.graph$", "", var_name)
  
  # Cross-reference with the database to get Condition and Age
  condition <- database_ids[database_ids$`ID number` == id, Condition]
  age <- database_ids[database_ids$`ID number` == id, dpf]
  
  # Store the results
  results[[var_name]] <- list(
    ID = id,
    Condition = condition,
    Age = age,
    smallworldness = sigma,
    clustering_coefficient = clustcoeff,
    clustering_coefficient_rfp = clustcoeff_rfp,
    global_efficiency = globeff,
    global_efficiency_rfp = globeff_rfp
  )
}

# Convert the list to a data frame for easier viewing
results_df <- do.call(rbind, lapply(results, as.data.frame))
results_df <- as.data.frame(results_df)

# Print the results
print("Calculated metrics:")
print(results_df)


# Data viz -----
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Ensure results_df is available from the previous computations
# Convert Condition and Age to factors for proper plotting
results_df$Condition <- as.factor(results_df$Condition)
results_df$Age <- as.factor(results_df$Age)

# Define the features to plot
features <- c("smallworldness", "clustering_coefficient", "clustering_coefficient_rfp", "global_efficiency", "global_efficiency_rfp")

# Define colors for conditions (adjust as needed for your usual colors)
condition_colors <- c("AKT1" = "red", "CTRL" = "blue", "HRASV12" = "purple", 
                      "AKT1_IRF8null" = "orange", "CTRL_IRF8null" = "cyan")  # Add other conditions as necessary

# Function to create and save boxplots for each feature
create_boxplot <- function(feature, data) {
  ggplot(data, aes(x = Age, y = as.numeric(get(feature)), fill = Condition)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", feature, "by Condition and Age"),
         x = "Days Post Fertilization (dpf)",
         y = feature) +
    scale_fill_manual(values = condition_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create boxplots for each feature
for (feature in features) {
  p <- create_boxplot(feature, results_df)
  print(p)  # Display the plot
  ggsave(filename = paste0("boxplot_", feature, ".png"), plot = p)  # Save the plot
}
