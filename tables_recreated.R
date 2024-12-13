################################################################################
################################# TABLE 1 ######################################
################################################################################
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
library(haven)
library(dplyr)
library(writexl)
library(Hmisc)

# Load and prepare data
corrdata <- read_dta("diegos_correlation_data.dta")
selected_corrdata <- corrdata %>% 
  select(dshaMeeting, dshaSitevisit, dshaCommunications, planned, 
         part_2more, hours_fumore1, ins, out, mix, top, production, 
         mkting, clients, suppliers, consultants)

# Get correlation matrix with p-values
data_matrix <- as.matrix(selected_corrdata)
correlation_results <- rcorr(data_matrix)
cor_matrix <- round(correlation_results$r, 4)
p_matrix <- correlation_results$P

# Create dataframe of all correlations with their p-values
significant_cors <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  Correlation = numeric(),
  P_Value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

var_names <- names(selected_corrdata)

# Populate the dataframe
for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i < j) {  # Only take lower triangle to avoid duplicates
      sig_level <- case_when(
        p_matrix[i,j] < 0.01 ~ "1%",
        p_matrix[i,j] < 0.05 ~ "5%",
        p_matrix[i,j] < 0.10 ~ "10%",
        TRUE ~ "Not significant"
      )
      
      if(sig_level != "Not significant") {
        significant_cors <- rbind(significant_cors, data.frame(
          Variable1 = var_names[i],
          Variable2 = var_names[j],
          Correlation = cor_matrix[i,j],
          P_Value = round(p_matrix[i,j], 4),
          Significance = sig_level
        ))
      }
    }
  }
}

# Sort by significance level and correlation magnitude
significant_cors <- significant_cors %>%
  arrange(factor(Significance, levels = c("1%", "5%", "10%")), 
          desc(abs(Correlation)))

# Create separate tables for each significance level
sig_1pct <- significant_cors %>% filter(Significance == "1%")
sig_5pct <- significant_cors %>% filter(Significance == "5%")
sig_10pct <- significant_cors %>% filter(Significance == "10%")

# Create Excel workbook
wb <- createWorkbook()

# Function to write table to worksheet
write_sig_table <- function(wb, data, sheet_name, title) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, title, startRow = 1)
  writeData(wb, sheet_name, data, startRow = 3)
}

# Write each significance level to its own worksheet
write_sig_table(wb, sig_1pct, "1% Level", "Correlations Significant at 1% Level")
write_sig_table(wb, sig_5pct, "5% Level", "Correlations Significant at 5% Level")
write_sig_table(wb, sig_10pct, "10% Level", "Correlations Significant at 10% Level")

# Write all results to one summary sheet
write_sig_table(wb, significant_cors, "All Significant", "All Significant Correlations")

# Save workbook
saveWorkbook(wb, "significant_correlations.xlsx", overwrite = TRUE)

# Print summary to console
cat("\nCorrelations Significant at 1% Level:\n")
print(sig_1pct, row.names = FALSE)

cat("\nCorrelations Significant at 5% Level:\n")
print(sig_5pct, row.names = FALSE)

cat("\nCorrelations Significant at 10% Level:\n")
print(sig_10pct, row.names = FALSE)



################################################################################
################################# TABLE 3 ######################################
################################################################################

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



################################################################################
############################### TABLE D.2 ######################################
################################################################################

# Load required libraries
library(fixest)
library(haven)
library(dplyr)

# Read data
data_collapsed <- read_dta("Accounts_matched_collapsed.dta")
data_yearly <- read_dta("/Users/diegomoers/Desktop/REPLICATION ASSIGNMENT/2017765data/Regressions/Accounts_matched_yearly.dta")

# 1. Baseline regression
cat("\nBASELINE REGRESSION:\n")
reg1 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data_collapsed,
              weights = ~r_averagewk,
              cluster = ~sic)
print(reg1)

# 2. Yearly data regression
cat("\nYEARLY DATA REGRESSION:\n")
reg2 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic2) + .[noise_basic_collapse] | 0,
              data = data_yearly,
              weights = ~r_averagewk,
              cluster = ~company_id)
print(reg2)

# 3. Unweighted regression
cat("\nUNWEIGHTED REGRESSION:\n")
reg3 <- feols(ly ~ ceo_behavior + lemp + lempm + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data_collapsed,
              cluster = ~sic)
print(reg3)

# 4. Alternative specifications
cat("\nALTERNATIVE SPECIFICATIONS:\n")
alt_measures <- c("type_1", "std_pca_type1", "k_means_type")
alt_regs <- lapply(alt_measures, function(measure) {
  formula <- as.formula(paste("ly ~", measure, "+ lemp + lempm + cons + active + 
                             factor(year) + factor(cty) + emp_imputed + 
                             factor(sic) + .[noise_basic_collapse] | 0"))
  
  reg <- feols(formula,
               data = data_collapsed,
               weights = ~r_averagewk,
               cluster = ~sic)
  
  cat("\nResults for", measure, ":\n")
  print(reg)
  
  return(reg)
})

# 5. Controls for hours worked
cat("\nREGRESSION WITH HOURS WORKED CONTROLS:\n")
reg5 <- feols(ly ~ ceo_behavior + ltot_nt + lemp + lempm + cons + active + 
                factor(year) + factor(week):factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data_collapsed,
              weights = ~r_averagewk,
              cluster = ~sic)
print(reg5)

# 6. CEO characteristics controls
cat("\nREGRESSION WITH CEO CHARACTERISTICS:\n")
reg6 <- feols(ly ~ ceo_behavior + lemp + lempm + mba + i_studyworkabroad + 
                exist_coo + lage + male + internal + cons + active + 
                factor(year) + factor(cty) + emp_imputed + 
                factor(sic) + .[noise_basic_collapse] | 0,
              data = data_collapsed,
              weights = ~r_averagewk,
              cluster = ~sic)
print(reg6)

# Export results 
etable(reg1, reg2, reg3, alt_regs[[1]], alt_regs[[2]], alt_regs[[3]], reg5, reg6,
       keep = c("ceo_behavior", "type_1", "mba", "i_studyworkabroad", 
                "lage", "male", "internal", "exist_coo", "ltot_nt",
                "std_pca_type1", "k_means_type"),
       signif.code = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

