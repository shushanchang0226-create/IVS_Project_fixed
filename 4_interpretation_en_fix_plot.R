setwd("D:/STAT462/nz-tourism-main/mlr")

# Install once if needed:
# install.packages("patchwork")
# install.packages("scales")

library(tidyverse)
library(patchwork)
library(scales)

# =============================================================================
# COLORS AND THEME
# =============================================================================

NAVY  <- "#0D2B45"
TEAL  <- "#0C7B93"
RED   <- "#C0392B"
GREEN <- "#27AE60"
GRAY  <- "#8A9BB0"
GOLD  <- "#E8A020"
CREAM <- "#F7F4EF"
LGRAY <- "#EEF2F5"

theme_poster <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = CREAM, color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#D0DAE5", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", color = NAVY, size = 14),
      plot.subtitle    = element_text(color = GRAY, size = 11),
      plot.caption     = element_text(color = GRAY, size = 8, hjust = 0),
      axis.title       = element_text(color = NAVY),
      axis.text        = element_text(color = "#1A2B3C"),
      legend.background = element_rect(fill = "white", color = GRAY,
                                       linewidth = 0.4)
    )
}

if (!dir.exists("output/plots")) dir.create("output/plots", recursive = TRUE)

# =============================================================================
# LOAD DATA
# =============================================================================

df       <- read_csv("output/ols_coefficients_full.csv")
model_df <- readRDS("output/mlr_merged.rds")

# =============================================================================
# PREPARE COEFFICIENTS
# =============================================================================

baseline_spend <- median(model_df$treated_spend, na.rm = TRUE)
cat("Baseline median spend (NZD):", round(baseline_spend, 2), "\n")

# Add percent_effect and dollar_effect if not already in CSV
if (!"percent_effect" %in% colnames(df)) {
  df <- df |> mutate(percent_effect = (exp(estimate) - 1) * 100)
}
if (!"dollar_effect" %in% colnames(df)) {
  df <- df |> mutate(dollar_effect = baseline_spend * (exp(estimate) - 1))
}

# Recompute interpretation notes
df <- df |>
  mutate(
    interpretation_note = case_when(
      term == "(Intercept)"    ~ "Intercept term",
      term == "log_days_in_nz" ~ "Elasticity: 1% longer stay -> ~0.5% more spend",
      str_starts(term, "age_range_") ~
        "Age group dummy — conditional association only, not causal",
      TRUE ~ "Dummy variable"
    )
  )

# Significant variables only (p < 0.05, exclude intercept)
coef_sig <- df |>
  filter(term != "(Intercept)", p_value < 0.05) |>
  select(term, estimate, std_error, p_value,
         percent_effect, dollar_effect, interpretation_note)

top_pos <- coef_sig |>
  filter(term != "log_days_in_nz") |>
  arrange(desc(dollar_effect)) |>
  slice_head(n = 10)

top_neg <- coef_sig |>
  filter(term != "log_days_in_nz") |>
  arrange(dollar_effect) |>
  slice_head(n = 10)

age_coefs     <- coef_sig |> filter(str_starts(term, "age_range_")) |>
                              arrange(desc(dollar_effect))
log_days_coef <- df |> filter(term == "log_days_in_nz")

# =============================================================================
# CONSOLE OUTPUT
# =============================================================================

cat("\n--- Top 10 Spend-Up Drivers ---\n")
cat("Primary metric: percent_effect | dollar_effect is reference only\n\n")
top_pos |> select(term, percent_effect, dollar_effect, p_value) |> print(n = 10)

cat("\n--- Top 10 Spend-Down Drivers ---\n")
top_neg |> select(term, percent_effect, dollar_effect, p_value) |> print(n = 10)

cat("\n--- Age Group Coefficients ---\n")
cat("CAUTION: Conditional associations only, not causal effects.\n\n")
age_coefs |> select(term, percent_effect, dollar_effect, p_value) |>
  print(n = nrow(age_coefs))

cat("\n--- log_days_in_nz Elasticity ---\n")
cat("  Estimate:", round(log_days_coef$estimate, 4), "\n")
cat("  10% longer stay ->", round(log_days_coef$estimate * 10, 2), "% more spend\n")
cat("  7 days -> 14 days (+100%) => spend +~",
    round(log_days_coef$estimate * 100, 1), "%\n")

cat("\n--- Model Performance ---\n")
cat("  [B] Train R2     : 0.5820\n")
cat("  [B] Train Adj R2 : 0.5776  (Adj R2 <= R2 confirmed)\n")
cat("  [A] Test  R2     : 0.5639\n")
cat("  [A] Test  RMSE   : 0.658   (log scale)\n")
cat("  NOTE: [A] and [B] are from different datasets.\n")

# Save CSV outputs
write_csv(top_pos,    "output/top10_positive_drivers.csv")
write_csv(top_neg,    "output/top10_negative_drivers.csv")
write_csv(age_coefs,  "output/age_group_coefficients.csv")
cat("\nCSV files saved to output/\n")

# =============================================================================
# HELPER: clean dollar label (avoids sprintf with $, which R does not support)
# =============================================================================

dollar_label <- function(x) {
  sign <- if_else(x >= 0, "+$", "-$")
  paste0(sign, format(abs(round(x)), big.mark = ","))
}

# =============================================================================
# CHART 1: Coefficient Forest Plot
# =============================================================================
cat("\nGenerating Chart 1: Coefficient Forest Plot...\n")

coef_data <- bind_rows(
  top_pos |> mutate(direction = "positive"),
  top_neg |> mutate(direction = "negative")
) |>
  mutate(
    # Clean up variable names for display
    label = term |>
      str_replace("age_range_",            "Age ")        |>
      str_replace("visit_purpose_",        "Purpose: ")   |>
      str_replace("travel_type_",          "Travel: ")    |>
      str_replace("accomm_",               "Accomm: ")    |>
      str_replace("transport_",            "Transport: ") |>
      str_replace("country_of_residence_", "Country: ")   |>
      str_replace_all("_", " ")                           |>
      str_to_title(),
    label     = fct_reorder(label, percent_effect),
    pct_label = sprintf("%+.1f%%", percent_effect),
    dol_label = paste0("(ref: ", dollar_label(dollar_effect), ")")
  )

has_se <- "std_error" %in% colnames(coef_data)

p1 <- ggplot(coef_data, aes(x = estimate, y = label, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = NAVY, linewidth = 0.8, alpha = 0.6) +
  { if (has_se)
      geom_linerange(aes(xmin = estimate - 1.96 * std_error,
                         xmax = estimate + 1.96 * std_error),
                     linewidth = 1.8, alpha = 0.4) } +
  geom_point(size = 3.5) +
  geom_hline(yintercept = 10.5, linetype = "dotted",
             color = GRAY, linewidth = 0.6) +
  # Primary label: % effect
  geom_text(
    aes(label = pct_label,
        x     = estimate + if_else(estimate > 0, 0.012, -0.012)),
    hjust    = if_else(coef_data$estimate > 0, 0, 1),
    size     = 3.4, fontface = "bold"
  ) +
  # Secondary label: dollar reference
  geom_text(
    aes(label = dol_label,
        x     = estimate + if_else(estimate > 0, 0.012, -0.012),
        y     = as.numeric(label) - 0.32),
    hjust    = if_else(coef_data$estimate > 0, 0, 1),
    size     = 2.8, color = "#999999", fontface = "italic"
  ) +
  scale_color_manual(
    values = c("positive" = TEAL, "negative" = RED),
    labels = c("positive" = "Spend UP (Top 10)",
               "negative" = "Spend DOWN (Top 10)")
  ) +
  scale_x_continuous(breaks = seq(-0.5, 0.7, 0.1)) +
  labs(
    title   = "MLR Coefficient Plot — Drivers of Visitor Expenditure",
    subtitle = paste0("Primary: % effect  |  ",
                      "Reference dollar = median baseline $",
                      format(round(baseline_spend), big.mark = ","),
                      " x (exp(coef)-1)  |  p < 0.05  |  95% CI"),
    x       = "Coefficient (log scale)",
    y       = NULL,
    color   = NULL,
    caption = "Per Frank's feedback: % is the correct primary interpretation for a log-linear model"
  ) +
  theme_poster() +
  theme(legend.position = "bottom")

ggsave("output/plots/chart1_coef_plot.png", p1,
       width = 13, height = 9.5, dpi = 180)
cat("  Saved: output/plots/chart1_coef_plot.png\n")

# =============================================================================
# CHART 2: Model Diagnostics — Actual vs Predicted + Residual Plot
# =============================================================================
cat("Generating Chart 2: Model Diagnostics...\n")

# Rebuild train/test split (same seed as 2_train_base_model_en.R)
model_input <- model_df |> select(-treated_spend, -no_days_in_nz, -response_id)

set.seed(42)
train_idx <- sample.int(nrow(model_input), size = floor(0.8 * nrow(model_input)))
train_df  <- model_input[train_idx, ]
test_df   <- model_input[-train_idx, ]

ols_model <- lm(log_treated_spend ~ ., data = train_df)

preds <- tibble(
  actual    = test_df$log_treated_spend,
  predicted = predict(ols_model, newdata = test_df),
  residual  = predicted - actual
)

r2_test   <- cor(preds$actual, preds$predicted)^2
rmse_test <- sqrt(mean(preds$residual^2))

# Sample 2000 points to avoid overplotting
set.seed(1)
preds_sample <- preds |> slice_sample(n = min(2000, nrow(preds)))

# Left: Actual vs Predicted
p2_left <- ggplot(preds_sample, aes(x = actual, y = predicted)) +
  geom_point(color = TEAL, alpha = 0.3, size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              color = RED, linetype = "dashed", linewidth = 1.2) +
  geom_smooth(method = "lm", color = NAVY, linewidth = 1.5, se = FALSE) +
  annotate("label",
           x = min(preds_sample$actual) + 0.3,
           y = max(preds_sample$predicted) - 0.2,
           label = paste0("[A] TEST SET\n",
                          "R2   = ", round(r2_test,   4), "\n",
                          "RMSE = ", round(rmse_test, 4), "\n",
                          "n    = ", nrow(preds)),
           hjust = 0, vjust = 1, size = 3.5, color = NAVY,
           fill  = LGRAY,
           label.border  = unit(0.3, "lines"),
           label.padding = unit(0.4, "lines")) +
  labs(title    = "[A] Test Set — Actual vs Predicted",
       subtitle = "Out-of-sample",
       x = "Actual log(spend)", y = "Predicted log(spend)") +
  theme_poster()

# Right: Residual Plot
sd_r <- sd(preds$residual)

p2_right <- ggplot(preds_sample, aes(x = predicted, y = residual)) +
  geom_point(color = TEAL, alpha = 0.3, size = 1.2) +
  geom_hline(yintercept = 0,
             color = RED, linetype = "dashed", linewidth = 1.2) +
  geom_hline(yintercept =  1.96 * sd_r,
             color = GOLD, linetype = "dotted", linewidth = 1.0) +
  geom_hline(yintercept = -1.96 * sd_r,
             color = GOLD, linetype = "dotted", linewidth = 1.0) +
  annotate("text",
           x = min(preds_sample$predicted) + 0.1,
           y = max(preds_sample$residual)  - 0.1,
           label = "Mild heteroscedasticity\nat extremes",
           hjust = 0, vjust = 1, size = 3.2, color = GRAY) +
  labs(title    = "[A] Test Set — Residual Plot",
       subtitle = "Residual vs Fitted",
       x = "Predicted log(spend)", y = "Residual") +
  theme_poster()

p2 <- p2_left + p2_right +
  plot_annotation(
    title = paste0(
      "Model Diagnostics — OLS on log(spend)  |  ",
      "[A] Test: R2=0.5639, RMSE=0.6581  |  ",
      "[B] Train: R2=0.5820, Adj R2=0.5776 (<=R2 confirmed)  |  ",
      "[A] and [B] cannot be directly compared"
    ),
    caption = "Within [B] train set: Adj R2 (0.5776) <= R2 (0.5820) is correct. Test set R2 in [A] is a separate evaluation.",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 10, color = NAVY),
      plot.caption    = element_text(color = RED, size = 8),
      plot.background = element_rect(fill = CREAM, color = NA)
    )
  )

ggsave("output/plots/chart2_model_diag.png", p2,
       width = 14, height = 6, dpi = 180)
cat("  Saved: output/plots/chart2_model_diag.png\n")

# =============================================================================
# CHART 3: Log Transform Distribution — Before vs After
# =============================================================================
cat("Generating Chart 3: Log Transform Distribution...\n")

spend_data <- model_df |>
  select(treated_spend, log_treated_spend) |>
  drop_na()

mean_raw   <- mean(spend_data$treated_spend)
median_raw <- median(spend_data$treated_spend)
mean_log   <- mean(spend_data$log_treated_spend)
median_log <- median(spend_data$log_treated_spend)

# Left: original distribution
p3_left <- ggplot(spend_data, aes(x = treated_spend)) +
  geom_histogram(bins = 60, fill = TEAL, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = mean_raw,   color = RED,  linetype = "dashed", linewidth = 1.3) +
  geom_vline(xintercept = median_raw, color = GOLD, linetype = "solid",  linewidth = 1.3) +
  coord_cartesian(xlim = c(0, 30000)) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  annotate("label",
           x = 18000, y = Inf, vjust = 1.2,
           label = "Mean > Median\n-> Right skew\n-> OLS violated",
           color = RED, fill = "#FEF0EF",
           label.border = unit(0.3, "lines"), label.padding = unit(0.4, "lines"),
           size = 3.5) +
  labs(title    = "Original Distribution",
       subtitle = "Right-skewed — OLS assumption violated",
       x = "Visitor Spend (NZD)", y = "Frequency") +
  theme_poster()

# Right: after log transform (x-axis shows NZD equivalent)
log_breaks <- c(5, 6, 7, 8, 9, 10, 11)
log_labels <- paste0(log_breaks, "\n($",
                     format(round(exp(log_breaks)), big.mark = ","), ")")

p3_right <- ggplot(spend_data, aes(x = log_treated_spend)) +
  geom_histogram(bins = 55, fill = GREEN, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = mean_log,   color = RED,  linetype = "dashed", linewidth = 1.3) +
  geom_vline(xintercept = median_log, color = GOLD, linetype = "solid",  linewidth = 1.3) +
  scale_x_continuous(breaks = log_breaks, labels = log_labels) +
  annotate("label",
           x = min(spend_data$log_treated_spend, na.rm = TRUE) + 0.2,
           y = Inf, vjust = 1.2,
           label = "Mean ~ Median\n-> Near-normal\n-> OLS satisfied",
           color = GREEN, fill = "#EDF7F0",
           label.border = unit(0.3, "lines"), label.padding = unit(0.4, "lines"),
           size = 3.5) +
  labs(title    = "After log() Transform",
       subtitle = "Approximately normal — OLS satisfied",
       x = "log(Visitor Spend)  [NZD equivalent shown]",
       y = "Frequency") +
  theme_poster()

p3 <- p3_left + p3_right +
  plot_annotation(
    title = "Why log() Transform? — Expenditure Distribution Before vs After",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 13, color = NAVY),
      plot.background = element_rect(fill = CREAM, color = NA)
    )
  )

ggsave("output/plots/chart3_log_transform.png", p3,
       width = 13, height = 5.5, dpi = 180)
cat("  Saved: output/plots/chart3_log_transform.png\n")

# =============================================================================
# CHART 4: Age Group vs Spend Effect
# =============================================================================
cat("Generating Chart 4: Age Group vs Spend Effect...\n")

age_data <- age_coefs |>
  mutate(
    age_label = str_replace(term, "age_range_", "Age ") |>
                str_replace_all("_", "-"),
    age_label = fct_reorder(age_label, dollar_effect),
    bar_color = case_when(
      str_detect(age_label, "50")       ~ "#075265",
      str_detect(age_label, "55|40|65") ~ "#0A5F73",
      dollar_effect >= 0                ~ TEAL,
      TRUE                              ~ RED
    ),
    dol_label = dollar_label(dollar_effect),
    pct_label = sprintf("%+.1f%%", percent_effect)
  )

p4 <- ggplot(age_data, aes(x = age_label, y = dollar_effect, fill = bar_color)) +
  annotate("rect", xmin = 4.5, xmax = 11.5, ymin = -Inf, ymax = Inf,
           fill = TEAL, alpha = 0.07) +
  geom_col(width = 0.72, color = "white", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = NAVY, linewidth = 0.8, alpha = 0.6) +
  geom_text(aes(label = dol_label,
                vjust = if_else(dollar_effect >= 0, -0.4, 1.3)),
            size = 3.4, fontface = "bold", color = "white") +
  geom_text(aes(label = pct_label, y = dollar_effect * 0.5),
            size = 2.8, color = "white", alpha = 0.85) +
  scale_fill_identity() +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  annotate("text",
           x = 1, y = min(age_data$dollar_effect) * 0.85,
           label = paste0("Conditional association only\n",
                          "Age correlates with: income, retirement status, vacation time\n",
                          "Do not interpret as a direct causal effect of age"),
           hjust = 0, size = 3.0, color = GRAY, fontface = "italic") +
  labs(
    title    = "Age Group vs Estimated Spend Effect",
    subtitle = "vs Reference Group — controlling for trip type, days in NZ, package deal, etc.",
    x        = "Age Group",
    y        = "Estimated Spend Effect (NZD)",
    caption  = "Age group effects are conditional associations, not causal. (Per Frank's feedback)"
  ) +
  theme_poster()

ggsave("output/plots/chart4_age_spend.png", p4,
       width = 13, height = 6.5, dpi = 180)
cat("  Saved: output/plots/chart4_age_spend.png\n")

# =============================================================================
# CHART 5: Stay Length vs Estimated Spend
# =============================================================================
cat("Generating Chart 5: Stay Length vs Spend...\n")

elasticity <- log_days_coef$estimate
ref_days   <- 7
ref_spend  <- baseline_spend

stay_curve <- tibble(
  days  = seq(1, 60, by = 0.5),
  spend = ref_spend * (days / ref_days) ^ elasticity
)

key_pts <- tibble(
  days  = c(7, 14, 30),
  spend = ref_spend * (c(7, 14, 30) / ref_days) ^ elasticity
) |>
  mutate(label = paste0(days, " days\n$",
                        format(round(spend), big.mark = ",")))

gain_7_14 <- dollar_label(key_pts$spend[2] - key_pts$spend[1])

p5 <- ggplot(stay_curve, aes(x = days, y = spend)) +
  geom_area(fill = TEAL, alpha = 0.12) +
  geom_line(color = TEAL, linewidth = 2.5) +
  geom_point(data = key_pts, aes(x = days, y = spend),
             color = NAVY, size = 3.5) +
  geom_label(data = key_pts,
             aes(x = days + 2, y = spend - 300, label = label),
             hjust = 0, size = 3.4, color = NAVY,
             fill = "white",
             label.border  = unit(0.25, "lines"),
             label.padding = unit(0.30, "lines")) +
  geom_vline(xintercept = 7,  color = GRAY, linetype = "dotted", linewidth = 1.0) +
  geom_vline(xintercept = 14, color = GOLD, linetype = "dashed", linewidth = 1.2) +
  annotate("label",
           x = 2, y = max(stay_curve$spend) * 0.92,
           label = paste0("Elasticity = ", round(elasticity, 3),
                          "\n1% longer stay -> +",
                          round(elasticity, 3), "% spend"),
           hjust = 0, size = 3.4, color = NAVY, fill = LGRAY,
           label.border  = unit(0.3, "lines"),
           label.padding = unit(0.4, "lines")) +
  annotate("text",
           x = 15, y = max(stay_curve$spend) * 0.88,
           label = paste0("7->14 days: ", gain_7_14),
           hjust = 0, size = 3.4, color = GOLD, fontface = "bold") +
  scale_x_continuous(breaks = c(1, 7, 14, 21, 30, 45, 60)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title    = "Stay Length vs Estimated Visitor Spend",
    subtitle = paste0("Elasticity = ", round(elasticity, 3),
                      "  |  Each extra day from 7-day baseline ~ +$237 NZD"),
    x        = "Days in New Zealand",
    y        = "Estimated Total Spend (NZD)",
    caption  = paste0("spend = baseline x (days/7)^", round(elasticity, 3),
                      "  |  Baseline median = $",
                      format(round(ref_spend), big.mark = ","))
  ) +
  theme_poster()

ggsave("output/plots/chart5_stay_length.png", p5,
       width = 10, height = 6, dpi = 180)
cat("  Saved: output/plots/chart5_stay_length.png\n")

# =============================================================================
# DONE
# =============================================================================
cat("\nAll charts saved to output/plots/\n")
cat("  chart1_coef_plot.png\n")
cat("  chart2_model_diag.png\n")
cat("  chart3_log_transform.png\n")
cat("  chart4_age_spend.png\n")
cat("  chart5_stay_length.png\n")
