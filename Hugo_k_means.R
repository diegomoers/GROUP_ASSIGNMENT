# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Prepare the data for k-means (excluding 'id' column)
kmeans_data <- agg_data_share %>%
  select(-id)

# Define the range of seeds
seeds <- c(10, 20, 30, 40, 50, 60)

# Store centroids for each seed
centroid_results <- data.frame()

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
}

# Print the results for verification
print(centroid_results)

# Reshape data for plotting
centroid_long <- centroid_results %>%
  pivot_longer(cols = -c(cluster, seed), names_to = "variable", values_to = "value")

# Plot centroids by seed
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
