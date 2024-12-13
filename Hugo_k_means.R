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

# Get user input for seed range
LL <- as.numeric(readline(prompt = "Set a lower limit: "))
UL <- as.numeric(readline(prompt = "Set an upper limit: "))
Steps <- as.numeric(readline(prompt = "Step size: "))

# Run k-means for each seed
for (seed in seq(LL, UL, by = Steps)) {
  set.seed(seed)
  kmeans_result <- kmeans(kmeans_data, centers = 2, nstart = 1)
  
  # Extract centroids and add cluster and seed information
  centroids <- as.data.frame(kmeans_result$centers)
  centroids$cluster <- 1:2
  centroids$seed <- seed
  
  # Ensure consistent labeling: Assign cluster 1 to the cluster with the highest 'long' value
  centroids <- centroids %>%
    arrange(desc(long)) %>%
    mutate(cluster = if_else(row_number() == 1, 1, 2)) %>%
    arrange(cluster)
  
  # Store results
  centroid_results <- rbind(centroid_results, centroids)
  inertia_results <- rbind(inertia_results, 
                           data.frame(seed = seed, inertia = kmeans_result$tot.withinss))
}

# Create pattern string for identifying unique solutions
centroid_results <- centroid_results %>%
  group_by(seed) %>%
  mutate(pattern = paste(
    paste(coordinate1, coordinate2, large, long, out, planned, collapse="_"),
    collapse="|"
  )) %>%
  ungroup()

# Find unique patterns and their representative seeds
unique_patterns <- centroid_results %>%
  group_by(pattern) %>%
  slice(1:2) %>%  # Keep both clusters
  ungroup()

# Get the seeds that represent unique patterns
unique_pattern_seeds <- unique_patterns %>%
  select(seed) %>%
  distinct()

# Create long format for centroid plotting
unique_patterns_long <- unique_patterns %>%
  pivot_longer(cols = c(coordinate1, coordinate2, large, long, out, planned), 
               names_to = "variable", values_to = "value")

# Enhance inertia results with pattern information
inertia_results <- inertia_results %>%
  mutate(
    inertia_rounded = round(inertia, 3),
    is_unique_pattern = seed %in% unique_pattern_seeds$seed
  )

# Plot 1: Centroid Patterns
centroid_plot <- ggplot(unique_patterns_long, 
                        aes(x = variable, y = value, 
                            color = factor(seed), 
                            shape = factor(cluster),
                            group = interaction(seed, cluster))) +
  geom_point(size = 4) +
  geom_line(linetype = "dashed") +
  scale_color_discrete(name = "Unique Pattern Seeds") +
  scale_shape_manual(values = c(16, 17)) +  # Circle and triangle
  labs(title = "Unique K-Means Centroid Patterns",
       subtitle = paste("From seeds", LL, "to", UL),
       x = "Variable",
       y = "Centroid Value",
       shape = "Cluster") +
  theme_minimal()

# Plot 2: Enhanced Inertia Values
inertia_plot <- ggplot(inertia_results, aes(x = seed, y = inertia)) +
  geom_point(aes(color = is_unique_pattern, size = is_unique_pattern), alpha = 0.5) +
  scale_color_manual(values = c("steelblue", "red"),
                     name = "Unique Pattern",
                     labels = c("No", "Yes")) +
  scale_size_manual(values = c(2, 3),
                    name = "Unique Pattern",
                    labels = c("No", "Yes")) +
  geom_hline(data = unique_inertias, 
             aes(yintercept = inertia_rounded),
             color = "red", linetype = "dashed", alpha = 0.3) +
  labs(
    title = "K-Means Inertia Values Across Different Seeds",
    subtitle = paste("Seeds with unique centroid patterns highlighted in red"),
    x = "Seed Value",
    y = "Total Within-Cluster Inertia\n(Sum of Squares)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x))

# Print summary statistics
print("Seeds that produce unique patterns:")
print(unique_pattern_seeds)

print("\nUnique inertia values:")
print(unique_inertias)

inertia_counts <- inertia_results %>%
  mutate(inertia_rounded = round(inertia, 3)) %>%
  group_by(inertia_rounded) %>%
  summarise(
    count = n(),
    percentage = round(n()/nrow(inertia_results) * 100, 1),
    unique_pattern_seeds = paste(sort(seed[is_unique_pattern]), collapse = ", ")
  ) %>%
  arrange(inertia_rounded)

print("\nFrequency of each inertia value and associated unique pattern seeds:")
print(inertia_counts)

# Display plots
print(centroid_plot)
print(inertia_plot)