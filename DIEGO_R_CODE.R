instal

# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the CSV file
setwd("/Users/carmencilla/Desktop/Master EUR/Block 2/Data Science and HR Analytics/Replication Assignment/GROUP_ASSIGNMENT/")
ceo_data <- read_csv("survey_response_data.csv")

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
print(head(pca_scores*-1))     
print(pca_components*-1)
##CARMEN

##K-MEANS
# Assuming 'agg_data' has been created as per previous steps

# Load necessary libraries
library(dplyr)

# Calculate the total count per CEO
agg_data$total_count <- rowSums(agg_data[, c('long', 'planned', 'large', 'out', 'coordinate1', 'coordinate2')])

#
#Calculate the shares
#agg_data_share <- agg_data %>%
 # mutate(
  #  long_share = long / total_count,
   # planned_share = planned / total_count,
    #large_share = large / total_count,
    #out_share = out / total_count,
    #coordinate1_share = coordinate1 / total_count,
    #coordinate2_share = coordinate2 / total_count
  #) %>%
  #select(id, ends_with('_share'))


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
print(labels)

print(paste("K-Means total within-cluster sum of squares (inertia):", inertia))


###########################
######### TABLES ##########
###########################
#TABLE1
library(haven)
library(dplyr)

# Load the Stata file
corrdata <- read_dta("diegos_correlation_data.dta")

# Select specific variables for correlation analysis
selected_corrdata <- corrdata %>% select(dshaMeeting, dshaSitevisit, dshaCommunications, planned, part_2more, hours_fumore1, ins, out, mix, top, production, mkting, clients, suppliers, consultants
                                         )

# Compute the correlation matrix
cor_matrix <- cor(selected_corrdata, use = "pairwise.complete.obs")  # Handles missing values pairwise

# Display the correlation matrix
print(cor_matrix)

#TABLE3

# Read the data
data_path <- "/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/2017765data/Regressions/Accounts_matched_collapsed.dta"
data <- read_dta(data_path)

# Create noise controls
noise_basic_collapse <- c("pa", "reliability", 
                          names(data)[grep("^ww|^aa", names(data))])
noise_basic_man <- c("pa", "reliability", 
                     names(data)[grep("^reliability", names(data))])

# Function to print key coefficients for verification
print_key_coefs <- function(model, title) {
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  
  cat("\n", title, "\n")
  cat("----------------------------------------\n")
  key_vars <- c("ceo_behavior", "lemp", "lk", "lm", "zmanagement")
  for(var in key_vars) {
    if(var %in% names(coefs)) {
      cat(sprintf("%s: %.3f (%.3f)\n", 
                  var, coefs[var], ses[var]))
    }
  }
  cat("N =", nobs(model), "\n")
  cat("----------------------------------------\n")
}

# 1. Basic labor productivity
reg1 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 2. Adding capital
reg2 <- feols(ly ~ ceo_behavior + lk + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 3. Adding materials
reg3 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + lk + lm + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 4. Public firms only
reg4 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + lk + lm + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = subset(data, f_public == 1),
              weights = ~r_averagewk,
              cluster = ~sic)

# 5. With management controls
reg5 <- feols(ly ~ ceo_behavior + zmanagement + lemp + lempm + 
                lemp_plant + lemp_plantm + cons + emp_imputed + active + 
                factor(year) + factor(cty) + factor(wave) + 
                factor(sic2) + .[noise_basic_man] + 
                man_reliability + man_duration | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic2)

# Print results for verification
print_key_coefs(reg1, "1. Basic Labor Productivity")
print_key_coefs(reg2, "2. Adding Capital")
print_key_coefs(reg3, "3. Adding Materials")
print_key_coefs(reg4, "4. Public Firms Only")
print_key_coefs(reg5, "5. With Management Controls")

# Save the models for later use if needed
models <- list(reg1, reg2, reg3, reg4, reg5)
saveRDS(models, "ceo_regressions.rds")