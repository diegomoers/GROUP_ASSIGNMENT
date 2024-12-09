# Install and load required packages
if (!require("haven")) install.packages("haven")
if (!require("psych")) install.packages("psych")
if (!require("ggplot2")) install.packages("ggplot2")

library(haven)
library(psych)
library(ggplot2)

# Read the Stata .dta file
data <- read_dta("diegos_correlation_data.dta")
data <- as.data.frame(data)

# Select variables
variables <- c("dshaMeeting", "dshaSitevisit", "dshaCommunications", 
               "planned", "part_2more", "hours_fumore1", "ins", "out", 
               "mix", "top", "production", "mkting", "clients", 
               "suppliers", "consultants")

# Create correlation matrix
corr_matrix <- cor(data[variables], use = "pairwise.complete.obs")

# Calculate eigenvalues
eigen_values <- eigen(corr_matrix)$values

# Create scree plot data
scree_data <- data.frame(
  Factor = 1:length(eigen_values),
  Eigenvalue = eigen_values
)

# Create scree plot using ggplot2
scree_plot <- ggplot(scree_data, aes(x = Factor, y = Eigenvalue)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Scree Plot",
       subtitle = "For All Variables",
       x = "Factor Number",
       y = "Eigenvalue")

# Display the plot
print(scree_plot)


# Determine number of factors (using Kaiser criterion - eigenvalues > 1)
n_factors <- sum(eigen_values > 1)

# Perform EFA
efa_result <- fa(r = corr_matrix, 
                 nfactors = n_factors,
                 rotate = "oblimin",
                 fm = "minres",
                 scores = "regression")

# Print results
cat("\nNumber of factors to extract (based on eigenvalues > 1):", n_factors, "\n\n")

cat("Factor Loadings (loadings > 0.4):\n")
print(loadings(efa_result), cutoff = 0.4)

cat("\nVariance Explained by Each Factor:\n")
print(efa_result$Vaccounted)

# cat("\nModel Fit Statistics:\n")
# print(efa_result$stats)