# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Residual Analysis (correlation)
# File: 14_Residual_Analysis_correlation.R
# Purpose: Detailed residual analysis and visualization for correlation weights
# Author: STARMA Analysis - correlation Focus
# Date: 2024
# ============================================================================

cat("=== DETAILED RESIDUAL ANALYSIS (correlation WEIGHTS) ===\n\n")

# Load required libraries
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

# Load diagnostic results
load("output/12_diagnostic_correlation.RData")
load("output/11_starima_correlation.RData")

# Extract model info
p_order <- correlation_results$orders$p
d_order <- correlation_results$orders$d
q_order <- correlation_results$orders$q
model_name <- sprintf("STARIMA(%d,%d,%d)", p_order, d_order, q_order)

cat(sprintf("📊 Detailed Residual Analysis for %s - correlation Weights\n\n", model_name))

# Extract residuals
residuals_matrix <- correlation_results$residuals
regions <- colnames(residuals_matrix)

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("📈 Creating residual visualizations...\n")

# Prepare data for plotting
residual_df <- data.frame(
  Time = 1:nrow(residuals_matrix),
  residuals_matrix
)

residual_long <- residual_df %>%
  pivot_longer(cols = -Time, names_to = "Region", values_to = "Residual")

# 1. Time series plot of residuals
p1 <- ggplot(residual_long, aes(x = Time, y = Residual)) +
  geom_line(color = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Region, scales = "free_y", ncol = 2) +
  labs(title = sprintf("Residual Time Series - %s (correlation Weights)", model_name),
       subtitle = "Residuals should fluctuate randomly around zero",
       x = "Time", y = "Residual") +
  theme_minimal()

ggsave("plots/14_residual_timeseries_correlation.png", p1, width = 12, height = 8, dpi = 300)
print(p1)

# 2. Residual distribution (histograms)
p2 <- ggplot(residual_long, aes(x = Residual)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Region, scales = "free", ncol = 2) +
  labs(title = sprintf("Residual Distribution - %s (correlation Weights)", model_name),
       subtitle = "Residuals should be approximately normally distributed",
       x = "Residual", y = "Frequency") +
  theme_minimal()

ggsave("plots/14_residual_histogram_correlation.png", p2, width = 12, height = 8, dpi = 300)
print(p2)

# 3. Q-Q plots for normality check
p3 <- ggplot(residual_long, aes(sample = Residual)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~Region, scales = "free", ncol = 2) +
  labs(title = sprintf("Q-Q Plots - %s (correlation Weights)", model_name),
       subtitle = "Points should lie close to the red line for normality",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggsave("plots/14_residual_qqplot_correlation.png", p3, width = 12, height = 8, dpi = 300)
print(p3)

# 4. Box plots by region
p4 <- ggplot(residual_long, aes(x = Region, y = Residual, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = sprintf("Residual Box Plots - %s (correlation Weights)", model_name),
       subtitle = "Box plots should be centered around zero",
       x = "Region", y = "Residual") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

ggsave("plots/14_residual_boxplot_correlation.png", p4, width = 10, height = 6, dpi = 300)
print(p4)

cat("✅ All residual plots saved to plots/ directory\n")

# ============================================================================
# RESIDUAL STATISTICS SUMMARY
# ============================================================================
cat("\n📊 Residual Statistics Summary:\n")
cat("===============================\n")

print(diagnostic_results$residual_stats)

# ============================================================================
# DIAGNOSTIC TEST RESULTS
# ============================================================================
cat("\n🧪 Diagnostic Test Results:\n")
cat("===========================\n")

print(diagnostic_results$diagnostic_summary)

# White noise test details
cat("\n📈 White Noise Test Details:\n")
print(diagnostic_results$white_noise_results)

# Normality test details
cat("\n📊 Normality Test Details:\n")
print(diagnostic_results$normality_results)

# ============================================================================
# OVERALL ASSESSMENT
# ============================================================================
cat("\n🎯 OVERALL MODEL ASSESSMENT:\n")
cat("============================\n")

assessment <- diagnostic_results$overall_assessment
cat(sprintf("- White Noise Test: %s\n", ifelse(assessment$white_noise, "✅ PASS", "❌ FAIL")))
cat(sprintf("- Normality Pass Rate: %.1f%%\n", assessment$normality_pass_rate * 100))
cat(sprintf("- Model Adequacy: %s\n", ifelse(assessment$model_adequate, "✅ ADEQUATE", "⚠ NEEDS IMPROVEMENT")))

if (assessment$model_adequate) {
  cat(sprintf("\n🎉 %s with correlation weights is adequate for forecasting!\n", model_name))
} else {
  cat(sprintf("\n⚠ %s with correlation weights may need model refinement.\n", model_name))
}

cat(sprintf("\n✅ Detailed residual analysis completed for %s - correlation Weights\n", model_name))
cat("📈 All visualizations saved to plots/ directory\n")
cat("🔗 correlation-based spatial relationships analyzed\n")
cat("🎯 Ready for forecasting phase\n")