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

# Initialize a list to store results
results <- list()

# Loop through each graph variable, get the graph and corresponding metrics, and calculate small-worldness
for (var_name in filtered_graph_vars) {
  # Get the graph from the global environment
  graph <- get(var_name)
  
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
  
  # Get the frequency per minute
  freq_var_name <- gsub("\\.graph$", ".frequency", var_name)
  frequency <- if (exists(freq_var_name)) get(freq_var_name) else NA
  
  # Get the frequency per minute RFP
  freq_rfp_var_name <- gsub("\\.graph$", ".frequency.RFP", var_name)
  frequency_rfp <- if (exists(freq_rfp_var_name)) get(freq_rfp_var_name) else NA
  
  # Get the frequency data frame and calculate the median frequency
  freq_df_var_name <- gsub("\\.graph$", ".frequencies.df", var_name)
  frequency_median <- if (exists(freq_df_var_name)) {
    freq_df <- get(freq_df_var_name)
    median(freq_df$Frequency, na.rm = TRUE)
  } else {
    NA
  }
  
  # Get the frequency RFP data frame and calculate the median frequency
  freq_rfp_df_var_name <- gsub("\\.graph$", ".frequencies.RFP.df", var_name)
  frequency_rfp_median <- if (exists(freq_rfp_df_var_name)) {
    freq_rfp_df <- get(freq_rfp_df_var_name)
    median(freq_rfp_df$Frequency, na.rm = TRUE)
  } else {
    NA
  }
  
  # Get the mean degree
  mean_degree_var_name <- gsub("\\.graph$", ".degree.mean", var_name)
  mean_degree <- if (exists(mean_degree_var_name)) get(mean_degree_var_name) else NA
  
  # Get the mean degree RFP
  mean_degree_rfp_var_name <- gsub("\\.graph$", ".degree.mean.RFP", var_name)
  mean_degree_rfp <- if (exists(mean_degree_rfp_var_name)) get(mean_degree_rfp_var_name) else NA
  
  # Extract ID to cross-reference with the database
  id <- gsub("\\.graph$", "", var_name)
  
  # Cross-reference with the database to get Condition and Age
  condition <- database_ids[database_ids$ID_number == id, Condition]
  age <- database_ids[database_ids$ID_number == id, dpf]
  
  # Store the results
  results[[var_name]] <- list(
    ID = id,
    Condition = condition,
    Age = age,
    clustering_coefficient = clustcoeff,
    clustering_coefficient_rfp = clustcoeff_rfp,
    global_efficiency = globeff,
    global_efficiency_rfp = globeff_rfp,
    frequency = frequency,
    frequency_rfp = frequency_rfp,
    frequency_median = frequency_median,
    frequency_rfp_median = frequency_rfp_median,
    mean_degree = mean_degree,
    mean_degree_rfp = mean_degree_rfp
  )
}

# Convert the list to a data frame for easier viewing
results_df <- do.call(rbind, lapply(results, as.data.frame))
results_df <- as.data.frame(results_df)



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



# Top 5 principal components (PC) explain this much % of the data..
# Custom function to extract the cumulative variance for the top 5 PCs
get_top5pc_variance <- function(ID) {
  var_name <- paste0(ID, ".pca.eigenvalues")
  eval(parse(text = paste0(var_name, "$cumulative.variance.percent[5]")))
}

# Add the Top5PC_XVariance column to results_df
results_df <- results_df %>%
  mutate(Top5PC_XVariance = sapply(ID, get_top5pc_variance))
