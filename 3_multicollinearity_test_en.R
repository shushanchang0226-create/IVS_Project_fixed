setwd("D:/STAT462/nz-tourism-main/mlr")

# ── Auto-install missing packages --------------------------------------------
if (!requireNamespace("car",    quietly = TRUE)) install.packages("car",    repos = "https://cloud.r-project.org")
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet", repos = "https://cloud.r-project.org")

library(car)
library(tidyverse)
library(glmnet)
library(tibble)

# ── Load data ----------------------------------------------------------------
# FIX: model_df.rds may not exist if saveRDS() was commented out in
#      2_train_base_model.R. Auto-generate it from mlr_merged.rds if missing.

if (!file.exists("output/model_df.rds")) {
  cat("model_df.rds not found — generating from mlr_merged.rds...\n")
  model_df <- readRDS("output/mlr_merged.rds")
  df <- model_df |>
    select(-treated_spend, -no_days_in_nz, -response_id)
  saveRDS(df, "output/model_df.rds")
  cat("Saved: output/model_df.rds\n")
} else {
  cat("model_df.rds found — loading...\n")
}

df <- readRDS("output/model_df.rds")
cat("Data loaded:", nrow(df), "rows,", ncol(df), "cols\n")

set.seed(42)

n         <- nrow(df)
train_idx <- sample.int(n, size = floor(0.8 * n), replace = FALSE)
train_df  <- df[train_idx, ]
test_df   <- df[-train_idx, ]

# ============================================================================
# STEP 1: VIF TEST — Detect multicollinearity
# ============================================================================
# VIF > 10 indicates severe multicollinearity
# VIF 5-10 indicates moderate multicollinearity
# VIF < 5 is generally acceptable

ols_full <- lm(log_treated_spend ~ ., data = train_df)

raw_vif <- car::vif(ols_full)

vif_tbl <- if (is.matrix(raw_vif)) {
  tibble(
    term = rownames(raw_vif),
    vif  = raw_vif[, "GVIF"]^(1 / (2 * raw_vif[, "Df"]))
  )
} else {
  tibble(
    term = names(raw_vif),
    vif  = as.numeric(raw_vif)
  )
}

cat("\n── VIF Results (top 50, sorted descending) ──\n")
vif_tbl |>
  arrange(desc(vif)) |>
  print(n = 50)

cat("\nVariables with VIF > 10 (severe)    :", sum(vif_tbl$vif > 10),  "\n")
cat("Variables with VIF > 5  (moderate)  :", sum(vif_tbl$vif > 5),   "\n")
cat("Variables with VIF <= 5 (acceptable):", sum(vif_tbl$vif <= 5),  "\n")

# ============================================================================
# STEP 2: REMOVE high-VIF variables (threshold = 10)
# ============================================================================
high_vif_terms <- vif_tbl |>
  filter(vif > 10) |>
  pull(term)

cat("\nVariables removed due to VIF > 10:\n")
print(high_vif_terms)

train_df_filtered <- train_df |> select(-all_of(high_vif_terms))
test_df_filtered  <- test_df  |> select(-all_of(high_vif_terms))

cat("\nFiltered train cols:", ncol(train_df_filtered), "\n")
cat("Filtered test cols: ", ncol(test_df_filtered),  "\n")

# ============================================================================
# STEP 3: LASSO REGRESSION — Further variable selection
# ============================================================================
X_train <- train_df_filtered |>
  select(-log_treated_spend) |>
  as.matrix()

y_train <- train_df_filtered$log_treated_spend

X_test  <- test_df_filtered |>
  select(-log_treated_spend) |>
  as.matrix()

y_test  <- test_df_filtered$log_treated_spend

set.seed(42)
lasso_cv <- cv.glmnet(
  x      = X_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10
)

cat("\nOptimal lambda (lambda.min):", round(lasso_cv$lambda.min, 6), "\n")
cat("Lambda 1se (more regularized):", round(lasso_cv$lambda.1se, 6), "\n")

# ============================================================================
# STEP 4: Evaluate LASSO model on test set
# ============================================================================
lasso_pred <- predict(lasso_cv, newx = X_test, s = "lambda.min")

ss_res     <- sum((y_test - lasso_pred)^2)
ss_tot     <- sum((y_test - mean(y_test))^2)
lasso_r2   <- 1 - ss_res / ss_tot
lasso_rmse <- sqrt(mean((y_test - lasso_pred)^2))

cat("\n── LASSO Model (Test Set) Performance ──\n")
cat("    R2  :", round(lasso_r2,   4), "\n")
cat("    RMSE:", round(lasso_rmse, 4), "\n")

# ============================================================================
# STEP 5: Extract non-zero LASSO coefficients (selected variables)
# ============================================================================
lasso_coef <- coef(lasso_cv, s = "lambda.min")

lasso_coef_tbl <- tibble(
  term     = rownames(lasso_coef),
  estimate = as.numeric(lasso_coef)
) |>
  filter(estimate != 0, term != "(Intercept)") |>
  arrange(desc(abs(estimate)))

cat("\nNumber of variables selected by LASSO:", nrow(lasso_coef_tbl), "\n")
cat("(Reduced from original 234 predictors)\n")

cat("\n── Top 20 LASSO coefficients (by absolute size) ──\n")
print(head(lasso_coef_tbl, 20))

# ============================================================================
# STEP 6: Extract REMOVED variables (coefficient shrunk to zero by LASSO)
# These are the variables with the least predictive relevance
# ============================================================================
lasso_removed_tbl <- tibble(
  term     = rownames(lasso_coef),
  estimate = as.numeric(lasso_coef)
) |>
  filter(estimate == 0, term != "(Intercept)") |>
  arrange(term)

cat("\n── Variables Removed by LASSO (coefficient = 0) ──\n")
cat("These variables have the least relevance to predicting tourist spend:\n\n")
print(lasso_removed_tbl, n = nrow(lasso_removed_tbl))
cat("\nTotal removed:", nrow(lasso_removed_tbl), "variables\n")

# ── Save outputs -------------------------------------------------------------
write_csv(vif_tbl,           "output/vif_results.csv")
write_csv(lasso_coef_tbl,    "output/lasso_coefficients.csv")
write_csv(lasso_removed_tbl, "output/lasso_removed_variables.csv")

cat("\nSaved: output/vif_results.csv\n")
cat("Saved: output/lasso_coefficients.csv\n")
cat("Saved: output/lasso_removed_variables.csv\n")
