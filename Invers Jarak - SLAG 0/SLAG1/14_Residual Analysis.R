# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Residual Analysis (distance SLAG 1)
# File: 14_Residual_Analysis_distance_SLAG1.R
# Purpose: Detailed residual analysis and visualization for distance weights SLAG 1
# Author: STARMA Analysis - distance Focus SLAG 1
# Date: 2024
# ============================================================================

cat("=== DETAILED RESIDUAL ANALYSIS (distance WEIGHTS SLAG 1) ===\n\n")

# Load required libraries
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

# Load diagnostic results
load("output/12_diagnostic_distance_slag1.RData")
load("output/11_starima_distance_slag1.RData")

# Extract dynamic model info with seasonal parameters
p_order <- distance_results_slag1$orders$p
d_order <- distance_results_slag1$orders$d
q_order <- distance_results_slag1$orders$q
# Extract seasonal parameters if available
P_order <- if (!is.null(distance_results_slag1$orders$P)) distance_results_slag1$orders$P else 0
Q_order <- if (!is.null(distance_results_slag1$orders$Q)) distance_results_slag1$orders$Q else 0
D_order <- if (!is.null(distance_results_slag1$orders$D)) distance_results_slag1$orders$D else 1
seasonal_period <- if (!is.null(distance_results_slag1$orders$s)) distance_results_slag1$orders$s else 12

model_name <- sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)%d", 
                     p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period)

cat(sprintf("📊 Detailed Residual Analysis for %s - distance Weights SLAG 1\n\n", model_name))
cat("🎯 Current Model Orders:\n")
cat(sprintf("   Non-seasonal: AR(%d), I(%d), MA(%d)\n", p_order, d_order, q_order))
cat(sprintf("   Seasonal: AR(%d), I(%d), MA(%d), Period=%d\n", P_order, D_order, Q_order, seasonal_period))
cat("   Spatial Lag: 1 (neighbor effects included)\n\n")

# Extract residuals
residuals_matrix <- distance_results_slag1$residuals
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
  labs(title = sprintf("Residual Time Series - %s (distance Weights SLAG 1)", model_name),
       subtitle = "Residuals should fluctuate randomly around zero",
       x = "Time", y = "Residual") +
  theme_minimal()

ggsave("plots/14_residual_timeseries_distance_slag1.png", p1, width = 12, height = 8, dpi = 300)
print(p1)

# 2. Residual distribution (histograms)
p2 <- ggplot(residual_long, aes(x = Residual)) +
  geom_histogram(bins = 20, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~Region, scales = "free", ncol = 2) +
  labs(title = sprintf("Residual Distribution - %s (distance Weights SLAG 1)", model_name),
       subtitle = "Residuals should be approximately normally distributed",
       x = "Residual", y = "Frequency") +
  theme_minimal()

ggsave("plots/14_residual_histogram_distance_slag1.png", p2, width = 12, height = 8, dpi = 300)
print(p2)

# 3. Q-Q plots for normality check
p3 <- ggplot(residual_long, aes(sample = Residual)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~Region, scales = "free", ncol = 2) +
  labs(title = sprintf("Q-Q Plots - %s (distance Weights SLAG 1)", model_name),
       subtitle = "Points should lie close to the red line for normality",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggsave("plots/14_residual_qqplot_distance_slag1.png", p3, width = 12, height = 8, dpi = 300)
print(p3)

# 4. Box plots by region
p4 <- ggplot(residual_long, aes(x = Region, y = Residual, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = sprintf("Residual Box Plots - %s (distance Weights SLAG 1)", model_name),
       subtitle = "Box plots should be centered around zero",
       x = "Region", y = "Residual") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

ggsave("plots/14_residual_boxplot_distance_slag1.png", p4, width = 10, height = 6, dpi = 300)
print(p4)

cat("✅ All residual plots saved to plots/ directory\n")

# ============================================================================
# RESIDUAL STATISTICS SUMMARY
# ============================================================================
cat("\n📊 Residual Statistics Summary:\n")
cat("===============================\n")

print(diagnostic_results_slag1$residual_stats)

# ============================================================================
# DIAGNOSTIC TEST RESULTS
# ============================================================================
cat("\n🧪 Diagnostic Test Results:\n")
cat("===========================\n")

print(diagnostic_results_slag1$diagnostic_summary)

# White noise test details
cat("\n📈 White Noise Test Details:\n")
print(diagnostic_results_slag1$white_noise_results)

# Normality test details
cat("\n📊 Normality Test Details:\n")
print(diagnostic_results_slag1$normality_results)

# ============================================================================
# OVERALL ASSESSMENT
# ============================================================================
cat("\n🎯 OVERALL MODEL ASSESSMENT:\n")
cat("============================\n")

assessment <- diagnostic_results_slag1$overall_assessment
cat(sprintf("- White Noise Test: %s\n", ifelse(assessment$white_noise, "✅ PASS", "❌ FAIL")))
cat(sprintf("- Normality Pass Rate: %.1f%%\n", assessment$normality_pass_rate * 100))
cat(sprintf("- Model Adequacy: %s\n", ifelse(assessment$model_adequate, "✅ ADEQUATE", "⚠️ NEEDS IMPROVEMENT")))

if (assessment$model_adequate) {
  cat(sprintf("\n🎉 %s with distance weights SLAG 1 is adequate for forecasting!\n", model_name))
  cat("✅ Model orders used: (p,d,q,P,D,Q,s) =", sprintf("(%d,%d,%d,%d,%d,%d,%d)\n", 
                                                        p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
} else {
  cat(sprintf("\n⚠️ %s with distance weights SLAG 1 may need model refinement.\n", model_name))
  cat("⚠️ Model orders used: (p,d,q,P,D,Q,s) =", sprintf("(%d,%d,%d,%d,%d,%d,%d)\n", 
                                                        p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
}

cat(sprintf("\n✅ Detailed residual analysis completed for %s - distance Weights SLAG 1\n", model_name))
cat("📋 Final Model Summary:\n")
cat(sprintf("   - Model: %s\n", model_name))
cat(sprintf("   - Orders: (p,d,q,P,D,Q,s) = (%d,%d,%d,%d,%d,%d,%d)\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat(sprintf("   - Total Parameters: %d\n", distance_results_slag1$fit_statistics$parameters))
cat("   - Spatial Lag: 1 (neighbor effects)\n")
cat("📈 All visualizations saved to plots/ directory\n")
cat("🔗 distance-based spatial relationships analyzed (SLAG 1)\n")
cat("🎯 Ready for forecasting phase\n")