# How many connections do RFP cells do to other non-RFP cells?
# Load necessary library
library(igraph)

# Initialize an empty dataframe to store results
graph_redcells_to_non_connections <- data.frame(Graph = character(), Total_Nodes = integer(), Num_Redcells = integer(), RFP_to_NonRFP_Connections = integer(), stringsAsFactors = FALSE)

# Get all graph objects in the global environment
graph_names <- ls(pattern = "^ID\\d{4}\\.graph$")

# Iterate over each graph object
for (graph_name in graph_names) {
  # Get the corresponding posXY file name
  posXY_filename <- paste0("data/", sub("\\.graph$", "_posXY.csv", graph_name))
  
  # Load the graph and posXY data
  graph <- get(graph_name, envir = .GlobalEnv)
  posXY <- read.csv(posXY_filename)
  
  # Check if number of vertices and rows match
  if (vcount(graph) != nrow(posXY)) {
    cat("Mismatch in the number of vertices and rows for", graph_name, "\n")
    next
  }
  
  # Identify the indices of redcells and non-redcells in the posXY data frame
  redcell_indices <- which(posXY$redcell == 1)
  non_redcell_indices <- which(posXY$redcell == 0)
  
  # # Debug: print the indices and some vertex IDs
  # cat("Graph name:", graph_name, "\n")
  # cat("Total nodes:", vcount(graph), "\n")
  # cat("Redcell indices:", redcell_indices[1:10], "\n")
  # cat("Non-redcell indices:", non_redcell_indices[1:10], "\n")
  
  # Initialize a counter for connections
  red_to_non_red_connections <- 0
  
  # Get the total number of nodes in the graph
  total_nodes <- vcount(graph)
  
  # Iterate over each redcell index and count its connections to non-redcells
  for (redcell_index in redcell_indices) {
    # Ensure the index exists within the graph's vertex range
    if (redcell_index + 1 > total_nodes) {
      cat("Invalid redcell_vertex index:", redcell_index + 1, "for graph", graph_name, "\n")
      next
    }
    
    # Get the actual vertex id in the igraph object (adjust for 1-based indexing)
    redcell_vertex <- redcell_index + 1
    neighbors <- neighbors(graph, redcell_vertex, mode = "all")
    
    # Convert neighbors to their indices in posXY data frame (subtract 1 for 1-based to 0-based index if necessary)
    neighbor_indices <- as.numeric(neighbors) - 1
    
    # Debug: print the neighbors
    cat("Redcell vertex:", redcell_vertex, "neighbors:", neighbor_indices, "\n")
    
    # Count connections to non-redcells
    red_to_non_red_connections <- red_to_non_red_connections + sum(neighbor_indices %in% non_redcell_indices)
  }
  
  # Get the number of redcells
  num_redcells <- length(redcell_indices)
  
  # Add the result to the dataframe
  graph_redcells_to_non_connections <- rbind(graph_redcells_to_non_connections, data.frame(Graph = graph_name, Total_Nodes = total_nodes, Num_Redcells = num_redcells, RFP_to_NonRFP_Connections = red_to_non_red_connections))
}

# Print the results dataframe
print(graph_redcells_to_non_connections)


ggplot(graphs.features_df, aes(Age, RFP_to_nonRFP_normalized_by_RFPedges, color = Condition))+
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75))+
  ylim(0,15)
