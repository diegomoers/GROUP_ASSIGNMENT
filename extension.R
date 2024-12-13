################################################################################
############################### EXTENSION ######################################
################################################################################

#THIS IS THE CODE TO APPEND AND CREATE THE MERGED FILE WITH PR DO NOT RUN IF YOU DONT HAVE TO!
# Load required libraries
library(tidyverse)  
library(haven)    

extensions_data <- read_csv("/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/GROUP_ASSIGNMENT/extension_indeces.csv")
main_data <- read_dta("/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/GROUP_ASSIGNMENT/Accounts_matched_collapsed.dta")

extensions_data <- extensions_data %>%
  rename(company_id = id)

cat("Initial data check:\n")
cat("Number of rows in extensions data:", nrow(extensions_data), "\n")
cat("Number of rows in main data:", nrow(main_data), "\n")

#LEFT-JOIN MERGE
merged_data <- main_data %>%
  left_join(extensions_data, by = "company_id")

cat("\nPost-merge diagnostics:")
cat("\nNumber of rows in merged data:", nrow(merged_data), "\n")
cat("Number of non-missing values for each new variable:\n")
cat("Market relations:", sum(!is.na(merged_data$market_relations)), "\n")
cat("Non-market relations:", sum(!is.na(merged_data$non_market_relations)), "\n")
cat("Fixer:", sum(!is.na(merged_data$fixer)), "\n")

cat("\nFirst few rows of merged data (checking key variables):\n")
print(head(select(merged_data, company_id, market_relations, non_market_relations, fixer)))

# SAVE MERGED FILE
write_dta(merged_data, "/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/GROUP_ASSIGNMENT/Accounts_matched_collapsed_with_extensions.dta")
invisible(merged_data)

# RUN THE EXTENSION
library(tidyverse)    
library(fixest)       
library(haven)    

# USE MERGED FILE
data_path <- "/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/GROUP_ASSIGNMENT/Accounts_matched_collapsed_with_extensions.dta"
data <- read_dta(data_path)

noise_basic_collapse <- c("pa", "reliability", 
                          names(data)[grep("^ww|^aa", names(data))])
noise_basic_man <- c("pa", "reliability", 
                     names(data)[grep("^reliability", names(data))])

print_key_coefs <- function(model, title) {
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  
  cat("\n", title, "\n")
  cat("----------------------------------------\n")
  key_vars <- c("ceo_behavior", "market_relations", "non_market_relations", 
                "lemp", "lk", "lm", "zmanagement")
  for(var in key_vars) {
    if(var %in% names(coefs)) {
      cat(sprintf("%s: %.3f (%.3f)\n", 
                  var, coefs[var], ses[var]))
    }
  }
  cat("N =", nobs(model), "\n")
  cat("----------------------------------------\n")
}

cat("Number of non-missing values:\n")
cat("Market relations:", sum(!is.na(data$market_relations)), "\n")
cat("Non-market relations:", sum(!is.na(data$non_market_relations)), "\n")

# REGRESSIONS:
# 1. Basic labor productivity
reg1 <- feols(ly ~ ceo_behavior + market_relations + non_market_relations + 
                lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 2. Adding capital
reg2 <- feols(ly ~ ceo_behavior + market_relations + non_market_relations + 
                lk + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 3. Adding materials
reg3 <- feols(ly ~ ceo_behavior + market_relations + non_market_relations + 
                lemp + lempm + cons + lk + lm + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data,
              weights = ~r_averagewk,
              cluster = ~sic)

# 4. Public firms only
reg4 <- feols(ly ~ ceo_behavior + market_relations + non_market_relations + 
                lemp + lempm + cons + lk + lm + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = subset(data, f_public == 1),
              weights = ~r_averagewk,
              cluster = ~sic)

# 5. With management controls
reg5 <- feols(ly ~ ceo_behavior + market_relations + non_market_relations + 
                zmanagement + lemp + lempm + 
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

# Save the models for later use
models <- list(reg1, reg2, reg3, reg4, reg5)
saveRDS(models, "ceo_regressions_with_relations.rds")

# Additional diagnostics for our new variables
cat("\nDiagnostics for relations variables:\n")
cat("----------------------------------------\n")
cat("Market Relations:\n")
cat("Mean value:", mean(data$market_relations, na.rm = TRUE), "\n")
cat("Standard deviation:", sd(data$market_relations, na.rm = TRUE), "\n")
cat("Range:", range(data$market_relations, na.rm = TRUE), "\n")
cat("\nNon-Market Relations:\n")
cat("Mean value:", mean(data$non_market_relations, na.rm = TRUE), "\n")
cat("Standard deviation:", sd(data$non_market_relations, na.rm = TRUE), "\n")
cat("Range:", range(data$non_market_relations, na.rm = TRUE), "\n")