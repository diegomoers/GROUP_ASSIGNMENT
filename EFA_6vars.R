# Install and load required packages
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("psych")) install.packages("psych")
if (!require("ggplot2")) install.packages("ggplot2")

library(readr)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)

# Read the CSV file
ceo_data <- read_csv("survey_response_data.csv")

# Initial data processing and create coordinate variable
ceo_data <- ceo_data %>%
  filter(level1 == 'interacting',
         type != 'personal_family',
         id != 1115) %>%
  mutate(
    id = as.character(id),
    ins_alone = ifelse(ins == 1.0 & out == 0.0, 1, 0),
    out_alone = ifelse(ins == 0.0 & out == 1.0, 1, 0),
    ins_out = ifelse(ins == 1.0 & out == 1.0, 1, 0),
    coordinate = ifelse(n_functions > 1, 1, 0),  # Create coordinate variable here
    activity_dummy = 1
  )

# Function to create crosstabs
create_crosstab <- function(data, row_var, col_var, value_filter = NULL) {
  if (!is.null(value_filter)) {
    data <- data %>% filter(!!sym(col_var) != value_filter)
  }
  data %>%
    count(!!sym(row_var), !!sym(col_var)) %>%
    pivot_wider(names_from = !!sym(col_var), values_from = n, values_fill = 0)
}

# Create all necessary crosstabs
df1 <- create_crosstab(ceo_data, "id", "F_duration")
df2 <- create_crosstab(ceo_data, "id", "F_planned", value_filter = "missing")
df5a <- create_crosstab(ceo_data, "id", "out")
df5b <- create_crosstab(ceo_data, "id", "ins")
df_groupcom <- create_crosstab(ceo_data, "id", "groupcom")
df_bunits <- create_crosstab(ceo_data, "id", "bunits")
df_coo <- create_crosstab(ceo_data, "id", "coo")
df_cao <- create_crosstab(ceo_data, "id", "cao")
df6 <- create_crosstab(ceo_data, "id", "coordinate")  # Now this should work

# Rename columns
df5a <- df5a %>% rename(out = `1`) %>% select(id, out)
df5b <- df5b %>% rename(ins = `1`) %>% select(id, ins)
df6 <- df6 %>% rename(coordinate2 = `1`) %>% select(id, coordinate2)
df_groupcom <- df_groupcom %>% rename(groupcom = `1`) %>% select(id, groupcom)
df_bunits <- df_bunits %>% rename(bunits = `1`) %>% select(id, bunits)
df_coo <- df_coo %>% rename(coo = `1`) %>% select(id, coo)
df_cao <- df_cao %>% rename(cao = `1`) %>% select(id, cao)

# Aggregate data
agg_data <- data.frame(id = df1$id) %>%
  left_join(df1, by = "id") %>%
  left_join(df2, by = "id") %>%
  left_join(df5a, by = "id") %>%
  left_join(df5b, by = "id") %>%
  left_join(df_groupcom, by = "id") %>%
  left_join(df_bunits, by = "id") %>%
  left_join(df_coo, by = "id") %>%
  left_join(df_cao, by = "id") %>%
  left_join(df6, by = "id") %>%
  replace(is.na(.), 0)

# Create final variables
agg_data <- agg_data %>%
  rename(
    long = `1hrplus`,
    planned = planned
  ) %>%
  mutate(
    coordinate1 = groupcom + bunits + coo + cao
  )

# Calculate activities
activities <- ceo_data %>%
  group_by(id) %>%
  summarise(activity_dummy = sum(activity_dummy, na.rm = TRUE))

# Create final dataset with shares
final_data <- agg_data %>%
  left_join(activities, by = "id") %>%
  mutate(
    long = long / activity_dummy,
    planned = planned / activity_dummy,
    out = out / activity_dummy,
    ins = ins / activity_dummy,
    coordinate1 = coordinate1 / activity_dummy,
    coordinate2 = coordinate2 / activity_dummy
  ) %>%
  select(coordinate1, coordinate2, long, ins, out, planned) %>%
  na.omit()

# Now perform EFA
# Calculate correlation matrix
corr_matrix <- cor(final_data, use = "pairwise.complete.obs")

# Calculate eigenvalues
eigen_values <- eigen(corr_matrix)$values

# Create scree plot
scree_data <- data.frame(
  Factor = 1:length(eigen_values),
  Eigenvalue = eigen_values
)

scree_plot <- ggplot(scree_data, aes(x = Factor, y = Eigenvalue)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Scree Plot",
       subtitle = "For 6 Original Variables",
       x = "Factor Number",
       y = "Eigenvalue")

# Display the plot
print(scree_plot)


# Determine number of factors
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

