# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Prepare the data for k-means (excluding 'id' column)
kmeans_data <- agg_data_share %>%
  select(-id)

# Store centroids for each seed
centroid_results <- data.frame()

# Initialize a data frame to store inertia values
inertia_results <- data.frame(seed = integer(), inertia = numeric())

LL <- as.numeric(readline(prompt = "Set a lower limit: "))
UL <- as.numeric(readline(prompt = "Set an upper limit: "))
Steps <- as.numeric(readline(prompt = "Step size: "))

for (seed in seq(LL, UL, by = Steps)) {
  set.seed(seed)
  kmeans_result <- kmeans(kmeans_data, centers = 2, nstart = 1)
  
  # Extract centroids and add cluster and seed information
  centroids <- as.data.frame(kmeans_result$centers)
  centroids$cluster <- 1:2
  centroids$seed <- seed
  
  
  
  # Before the clusters would change, even though they 
  # would take the same value as with other seeds,
  # Therefore we need to ensure consistent labeling: 
  # Assign cluster 1 to the cluster with the highest 'long' value
  centroids <- centroids %>%
    arrange(desc(long)) %>%
    mutate(cluster = if_else(row_number() == 1, 1, 2)) %>%
    arrange(cluster) # Sort clusters back to 1 and 2
  
  # Append to the results
  centroid_results <- rbind(centroid_results, centroids)
  
  # Extract inertia and store it with the corresponding seed
  inertia_results <- rbind(inertia_results, data.frame(seed = seed, inertia = kmeans_result$tot.withinss))
  
}


# Print the results for verification
print(centroid_results)

# Print inertia results for verification
print(inertia_results)

# Reshape data for plotting
centroid_long <- centroid_results %>%
  pivot_longer(cols = -c(cluster, seed), names_to = "variable", values_to = "value")

# Plot centroids by seed
centroid_graph <- function() {
ggplot(centroid_long, aes(x = variable, y = value, color = factor(seed), shape = factor(cluster))) +
  geom_point(size = 4) +
  geom_line(aes(group = interaction(seed, cluster)), linetype = "dashed") +
  labs(
    title = "Variation in K-Means Centroid Positions Across Different Seeds",
    x = "Variable",
    y = "Centroid Value",
    color = "Seed",
    shape = "Cluster"
  ) +
  theme_minimal()
}

inertia_graph <- function() {
# Plot inertia vs. seed
ggplot(inertia_results, aes(x = seed, y = inertia)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 3) +
  labs(
    title = "K-Means Inertia Across Different Seeds",
    x = "Seed",
    y = "Inertia (Total Within-Cluster Sum of Squares)"
  ) +
  theme_minimal()
}

# The two graphs. Need to hash out the one you don't want to see. might make
# a function to choose from the terminal later

# centroid_graph()
inertia_graph()
  
  
  
  
  
  
