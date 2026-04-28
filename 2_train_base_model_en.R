library(tidyverse)
library(caret)

# setwd("/Users/rujalshrestha/Projects/nz-tourism/mlr")

model_df <- readRDS("output/mlr_merged.rds")

df <- model_df |>
  select(-treated_spend, -no_days_in_nz, -response_id)

# saveRDS(df, "output/model_df.rds")

set.seed(42)

n         <- nrow(df)
train_idx <- sample.int(n, size = floor(0.80 * n), replace = FALSE)
train_df  <- df[train_idx, ]
test_df   <- df[-train_idx, ]

cat("Train rows:", nrow(train_df), "\n")
cat("Test rows: ", nrow(test_df),  "\n")

# ── Train model ---------------------------------------------------------------
ols_full     <- lm(log_treated_spend ~ ., data = train_df)
full_summary <- summary(ols_full)

# ── [A] Out-of-sample metrics (TEST SET) -------------------------------------
# Reflects true predictive performance on unseen data
y_pred   <- predict(ols_full, newdata = test_df)
y_actual <- test_df$log_treated_spend

cat("\n[A] Test Set (out-of-sample) metrics:\n")
print(postResample(pred = y_pred, obs = y_actual))
# Expected: RMSE ~ 0.658, R2 ~ 0.564

# ── [B] In-sample metrics (TRAINING SET) -------------------------------------
# NOTE: Adjusted R2 is always <= R2 within the same dataset.
#       [A] and [B] come from different datasets and cannot be directly compared.
cat("\n[B] Training Set (in-sample) metrics:\n")
cat("    R2          :", round(full_summary$r.squared,     4), "\n")
cat("    Adjusted R2 :", round(full_summary$adj.r.squared, 4), "\n")
cat("    (Adjusted R2 <= R2 always holds within the same dataset)\n")

# ── Coefficient table ---------------------------------------------------------
coef_mat <- coef(full_summary)

coef_table <- tibble(
  term      = rownames(coef_mat),
  estimate  = coef_mat[, 1],
  std_error = coef_mat[, 2],
  t_value   = coef_mat[, 3],
  p_value   = coef_mat[, 4]
)

# Baseline: median spend in training set (NZD), used to convert % effect to dollars
baseline_spend <- median(exp(train_df$log_treated_spend), na.rm = TRUE)
cat("\nBaseline spend (median, training set): NZD", round(baseline_spend, 2), "\n")

# Step 1: add percent_effect and dollar_effect
coef_table <- coef_table |>
  mutate(
    percent_effect = (exp(estimate) - 1) * 100,
    dollar_effect  = baseline_spend * (exp(estimate) - 1)
  )

# Step 2: add interpretation note (separate mutate to avoid forward-reference error)
coef_table <- coef_table |>
  mutate(
    interpretation_note = case_when(
      term == "log_days_in_nz" ~
        "Elasticity variable: 1% longer stay -> ~0.5% more spend; dollar_effect not applicable",
      term == "(Intercept)" ~
        "Intercept term",
      TRUE ~
        "Dummy variable: percent_effect and dollar_effect both applicable"
    )
  )

# ── Export --------------------------------------------------------------------
coef_table_export <- coef_table |>
  arrange(p_value)

coef_top30_export <- coef_table_export |>
  filter(term != "(Intercept)") |>
  slice_head(n = 30) |>
  arrange(desc(dollar_effect))

write_csv(coef_table_export, "output/ols_coefficients_full.csv")
write_csv(coef_top30_export, "output/ols_coefficients_top30.csv")

cat("\nSaved: output/ols_coefficients_full.csv\n")
cat("Saved: output/ols_coefficients_top30.csv\n")

# ── Preview Top 30 ------------------------------------------------------------
cat("\nTop 30 significant coefficients (sorted by dollar_effect):\n")

top30_preview <- coef_table |>
  filter(term != "(Intercept)") |>
  arrange(p_value) |>
  slice_head(n = 30) |>
  arrange(desc(dollar_effect)) |>
  select(term, estimate, percent_effect, dollar_effect, interpretation_note)

print(top30_preview, n = 30)
