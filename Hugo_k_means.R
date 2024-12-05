# Load necessary library
library(dplyr)

# Prepare the data for k-means (excluding 'id' column)
kmeans_data <- agg_data_share %>%
  select(-id)

# Set seed for reproducibility
set.seed(60)

# Perform k-means clustering
num_clusters <- 2
kmeans_result <- kmeans(kmeans_data, centers = num_clusters, nstart = 1)

# Extract and display results
centroids <- kmeans_result$centers
labels <- kmeans_result$cluster
inertia <- kmeans_result$tot.withinss

# Print results
print("K-Means centroids:")
print(centroids)

print("K-Means labels (first 10):")
print(labels[1:10])

print(paste("K-Means total within-cluster sum of squares (inertia):", inertia))

