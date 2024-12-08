

# Load necessary libraries
library(dplyr)
library(tidyr)

# Read the CSV file
setwd("/Users/carmencilla/Desktop/Master EUR/Block 2/Data Science and HR Analytics/Replication Assignment/GROUP_ASSIGNMENT/")
ceo_data <- read.csv("survey_response_data.csv")

# Set 'id' as the row identifier
ceo_data <- ceo_data %>%
  filter(level1 == 'interacting',
         type != 'personal_family',
         id != 1115) %>%
  mutate(id = as.character(id))

# Function to create crosstabs
create_crosstab <- function(data, row_var, col_var, value_filter = NULL) {
  if (!is.null(value_filter)) {
    data <- data %>% filter(!!sym(col_var) != value_filter)
  }
  data %>%
    count(!!sym(row_var), !!sym(col_var)) %>%
    pivot_wider(names_from = !!sym(col_var), values_from = n, values_fill = 0)
}

df1 <- create_crosstab(ceo_data, "id", "F_duration")
df2 <- create_crosstab(ceo_data, "id", "F_planned", value_filter = "missing")
df3 <- create_crosstab(ceo_data, "id", "F_participants", value_filter = "missing")

ceo_data <- ceo_data %>%
  mutate(
    ins_alone = ifelse(ins == 1.0 & out == 0.0, 1, 0),
    out_alone = ifelse(ins == 0.0 & out == 1.0, 1, 0),
    ins_out = ifelse(ins == 1.0 & out == 1.0, 1, 0),
    coordinate = ifelse(n_functions > 1, 1, 0),
    activity_dummy = 1
  )

df5a <- create_crosstab(ceo_data, "id", "out")

df_groupcom <- create_crosstab(ceo_data, "id", "groupcom")
df_bunits <- create_crosstab(ceo_data, "id", "bunits")
df_coo <- create_crosstab(ceo_data, "id", "coo")
df_cao <- create_crosstab(ceo_data, "id", "cao")

df6 <- create_crosstab(ceo_data, "id", "coordinate")

# Rename columns before joining
df5a <- df5a %>%
  rename(out = `1`) %>%
  select(id, out)

df6 <- df6 %>%
  rename(coordinate2 = `1`) %>%
  select(id, coordinate2)

df_groupcom <- df_groupcom %>%
  rename(groupcom = `1`) %>%
  select(id, groupcom)

df_bunits <- df_bunits %>%
  rename(bunits = `1`) %>%
  select(id, bunits)

df_coo <- df_coo %>%
  rename(coo = `1`) %>%
  select(id, coo)

df_cao <- df_cao %>%
  rename(cao = `1`) %>%
  select(id, cao)

# Aggregate data
agg_data <- data.frame(id = df1$id) %>%
  left_join(df1, by = "id") %>%
  left_join(df2, by = "id") %>%
  left_join(df3, by = "id") %>%
  left_join(df5a, by = "id") %>%
  left_join(df_groupcom, by = "id") %>%
  left_join(df_bunits, by = "id") %>%
  left_join(df_coo, by = "id") %>%
  left_join(df_cao, by = "id") %>%
  left_join(df6, by = "id") %>%
  replace(is.na(.), 0)

# Rename and mutate to create the desired variables
agg_data <- agg_data %>%
  rename(
    long = `1hrplus`,
    planned = planned,
    large = `two_plus_ppl`
  ) %>%
  mutate(
    coordinate1 = groupcom + bunits + coo + cao
  ) %>%
  select(id, long, planned, large, out, coordinate1, coordinate2)

# Verify the renamed and mutated agg_data
print("Renamed and Mutated agg_data:")
print(head(agg_data))

# Calculate activities by summing 'activity_dummy' for each 'id'
activities <- ceo_data %>%
  group_by(id) %>%
  summarise(activity_dummy = sum(activity_dummy, na.rm = TRUE))

# Verify the activities dataframe
print("Activities:")
print(head(activities))
str(activities)

# Merge activities with agg_data and calculate shares
agg_data_share <- agg_data %>%
  left_join(activities, by = "id") %>%
  mutate(
    long = long / activity_dummy,
    planned = planned / activity_dummy,
    large = large / activity_dummy,
    out = out / activity_dummy,
    coordinate1 = coordinate1 / activity_dummy,
    coordinate2 = coordinate2 / activity_dummy
  ) %>%
  # Remove the 'activity_dummy' column as it's no longer needed
  select(-activity_dummy)

# Verify the agg_data_share dataframe
print("Aggregated Data Share:")
print(head(agg_data_share))
str(agg_data_share)

# Remove 'id' column for PCA as it's an identifier, not a feature
pca_data <- agg_data_share %>%
  select(-id)

# Verify the PCA data
print("PCA Data:")
print(head(pca_data))
str(pca_data)

# Compute the correlation matrix
cor_matrix <- cor(pca_data, use = "complete.obs")

# Perform eigen decomposition on the correlation matrix
eig <- eigen(cor_matrix)
eig_vals <- eig$values
eig_vecs <- eig$vectors

# Print eigenvalues to understand the variance captured by each component
print("Eigenvalues:")
print(eig_vals)

# Determine the number of principal components to retain
# For example, retain components with eigenvalues > 1 or based on cumulative variance
# Here, we'll select the top 2 components as per your original Python code

# Order eigenvalues in decreasing order and get the indices of the top 2
ordered_indices <- order(eig_vals, decreasing = TRUE)[1:2]

# Extract the corresponding eigenvectors (principal components)
pca_components <- eig_vecs[, ordered_indices]

# Project the data onto the first two principal components
pca_scores <- as.matrix(pca_data) %*% pca_components

# Verify PCA scores
print("PCA Scores (First 6 Rows):")
print(head(pca_scores))


##K-MEANS
# Assuming 'agg_data' has been created as per previous steps

# Load necessary libraries
library(dplyr)

# Calculate the total count per CEO
agg_data$total_count <- rowSums(agg_data[, c('long', 'planned', 'large', 'out', 'coordinate1', 'coordinate2')])

# Prepare the data for k-means (exclude 'id' column)
kmeans_data <- agg_data_share %>%
  select(-id)

# Set seed for reproducibility
set.seed(60)

# Perform k-means clustering
num_clusters <- 2
kmeans_result <- kmeans(kmeans_data, centers = num_clusters, nstart = 1)

# Extract the centroids
centroids <- kmeans_result$centers

# Extract the cluster labels
labels <- kmeans_result$cluster

# Extract the total within-cluster sum of squares (inertia)
inertia <- kmeans_result$tot.withinss

# Display the results
print("K-Means centroids:")
print(centroids)

print("K-Means labels (first 10):")
print(labels[1:10])

print(paste("K-Means total within-cluster sum of squares (inertia):", inertia))

################################################################################
################################################################################
####################      PCA EXPERIMENT       #################################
################################################################################
################################################################################

#Original analysis: 6 components (same method and commands as the rest to compare!!)
pca_result_6 <- prcomp(pca_data, scale. = TRUE)
biplot(pca_result_6, scale=0, main = "PCA Biplot (6 Components)") 

######PCA WITH 4 COMPONENTS (from agg_data): long, planned, large, out##########

# Subset for PCA with 4 parameters (e.g., 'long', 'planned', 'large', 'out')
pca_data_4 <- agg_data_share %>%
  select(long, planned, large, out)

# Verify the PCA data with 4 parameters
print("PCA Data with 4 Parameters:")
print(head(pca_data_4))

# Perform PCA
pca_result_4 <- prcomp(pca_data_4, scale. = TRUE)

# Print summary for variance explained
print("PCA with 4 Parameters Summary:")
summary(pca_result_4)

# Print the loadings (contributions of variables to the components)
print("PCA with 4 Parameters Loadings:")
print(pca_result_4$rotation)

# Plot Scree Plot
barplot(pca_result_4$sdev^2 / sum(pca_result_4$sdev^2),
        main = "Scree Plot (4 Parameters)",
        xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        col = "skyblue")

print(pca_result_4)

#Important data about PCA 4 Components: 
explained_variance <- (pca_result_4$sdev)^2
proportion_variance <- explained_variance / sum(explained_variance) 
cumulative_variance <- cumsum(proportion_variance) 

# Cumulative Variance Plot
plot(cumulative_variance,
     type = "b",
     main = "Cumulative Variance Explained (4 Parameters)",
     xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance",
     col = "darkblue", pch = 19)

#Scree plot
barplot(proportion_variance,
        main = "Scree Plot (4 Parameters)",
        xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        col = "skyblue")

#Biplot PC1-PC2 and PC3-PC4
biplot(pca_result_4, scale = 0, choices = c(1, 2), main = "PCA Biplot (PC1 vs PC2)")
biplot(pca_result_4, scale = 0, choices = c(3, 4), main = "PCA Biplot (PC3 vs PC4)")

################PCA WITH 8 COMPONENTS (from agg_data)####################

###Creating New Variable 1: public_relations. We need to create it in 
# ceo_data and the use it for agg_data to perform PCA. 
ceo_data <- ceo_data %>%
  mutate(public_relations = ifelse(type %in% c("public_event", "workrelated_leisure", "business_meal", "site_visit"), 1, 0))

#Group by id so that we can paste it to agg_data_share
public_relations_summary <- ceo_data %>%
  +     group_by(id) %>%
  +     summarise(public_relations = sum(public_relations, na.rm = TRUE) / n())

###Create New Variable 2: fixer
#Check the mean of the functions in the data set: 
print(mean(ceo_data$n_functions)) ## 1.663325

#Therefore, we can establish "intense" for 2 or more functions. 
fixer <- ceo_data %>%
  group_by(id) %>%
  summarise(fixer = sum(n_functions > 2, na.rm = TRUE) / n())
#Decision intensity = proportion of activities that imply more than 2 activities per CEO. 

#Note: ceo_data is a dataset of many observations per CEO. Agg_data_share is a measure per CEO
  # that is, only 1 observation, aggregated/grouped by CEO. We add out new variables to agg_data_share:
agg_data_share <- agg_data_share %>%
  left_join(fixer, by = "id") %>%
  left_join(public_relations_summary, by = "id")

##PCA analysis for 8 variables 
# Subset for PCA with 8 parameters
pca_data_8 <- agg_data_share %>%
  select(long, planned, large, out, coordinate1, coordinate2, public_relations, fixer)

# Verify the PCA data with 8 parameters
print("PCA Data with 8 Parameters:")
print(head(pca_data_8))

# Perform PCA
pca_result_8 <- prcomp(pca_data_8, scale. = TRUE)

#Scree plot for 8 components
barplot(pca_result_8$sdev^2 / sum(pca_result_8$sdev^2),
        main = "Scree Plot (8 Parameters)",
        xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        col = "skyblue")
 

############  Eigenvalues for 8 components: 
# Compute the correlation matrix
cor_matrix_8 <- cor(pca_data_8, use = "complete.obs")
print(cor_matrix_8)

# Perform eigen decomposition on the correlation matrix
eig_8 <- eigen(cor_matrix_8)
eig_vals_8 <- eig_8$values
eig_vecs_8 <- eig_8$vectors

# Print eigenvalues to understand the variance captured by each component
print("Eigenvalues:")
print(eig_vals_8)

# Determine the number of components to retain based on eigenvalues > 1
num_components <- sum(eig_vals_8 > 1)
print(paste("Number of components with eigenvalues > 1:", num_components))

# Extract the indices of components with eigenvalues > 1
indices_to_retain <- which(eig_vals_8 > 1)

# Extract the corresponding eigenvectors (principal components)
pca_components_8 <- eig_vecs_8[, indices_to_retain]

# Project the data onto the retained principal components
pca_scores_8 <- as.matrix(pca_data_8) %*% pca_components_8

# Verify PCA scores
print("PCA Scores (First 6 Rows):")
print(head(pca_scores_8))




############  Eigenvalues for 4 components: 
# Compute the correlation matrix
cor_matrix_4 <- cor(pca_data_4, use = "complete.obs")
print(cor_matrix_4)

# Perform eigen decomposition on the correlation matrix
eig_4 <- eigen(cor_matrix_4)
eig_vals_4 <- eig_4$values
eig_vecs_4 <- eig_4$vectors

# Print eigenvalues to understand the variance captured by each component
print("Eigenvalues:")
print(eig_vals_4)

# Determine the number of components to retain based on eigenvalues > 1
num_components_4 <- sum(eig_vals_4 > 1)
print(paste("Number of components with eigenvalues > 1:", num_components_4))

# Extract the indices of components with eigenvalues > 1
indices_to_retain_4 <- which(eig_vals_4 > 1)

# Extract the corresponding eigenvectors (principal components)
pca_components_4 <- eig_vecs_4[, indices_to_retain_4]

# Project the data onto the retained principal components
pca_scores_4 <- as.matrix(pca_data_4) %*% pca_components_4

# Verify PCA scores
print("PCA Scores (First 6 Rows):")
print(head(pca_scores_4))
