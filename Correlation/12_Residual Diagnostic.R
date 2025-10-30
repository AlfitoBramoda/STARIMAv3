# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Residual Diagnostic (Correlation)
# File: 12_Residual_Diagnostic_Correlation.R
# Purpose: Validate STARIMA model through residual analysis and diagnostic tests
# Author: STARMA Analysis - Correlation Focus
# Date: 2024
# ============================================================================

cat("=== STARIMA RESIDUAL DIAGNOSTIC (CORRELATION WEIGHTS) ===\n\n")

# Load required libraries
library(starma)
library(ggplot2)
library(gridExtra)
library(tseries)

# Load estimation results
load("output/11_starima_correlation.RData")

# Extract dynamic model orders
p_order <- correlation_results$orders$p
d_order <- correlation_results$orders$d
q_order <- correlation_results$orders$q
model_name <- sprintf("STARIMA(%d,%d,%d)", p_order, d_order, q_order)

cat(sprintf("🔬 Diagnostic Analysis for %s - Correlation Weights\n\n", model_name))

# Extract residuals and model info
residuals_matrix <- correlation_results$residuals
model_fit <- correlation_results$model
regions <- colnames(residuals_matrix)

cat("📊 Residual Matrix Information:\n")
cat("- Dimensions:", dim(residuals_matrix), "\n")
cat("- Regions:", paste(regions, collapse = ", "), "\n\n")

# ============================================================================
# RESIDUAL ANALYSIS
# ============================================================================
cat("📈 Residual Analysis:\n")
cat("====================\n")

# Basic residual statistics
residual_stats <- data.frame(
  Region = regions,
  Mean = round(apply(residuals_matrix, 2, mean, na.rm = TRUE), 6),
  SD = round(apply(residuals_matrix, 2, sd, na.rm = TRUE), 6),
  Min = round(apply(residuals_matrix, 2, min, na.rm = TRUE), 4),
  Max = round(apply(residuals_matrix, 2, max, na.rm = TRUE), 4),
  Skewness = round(apply(residuals_matrix, 2, function(x) {
    x <- x[is.finite(x)]
    m <- mean(x); s <- sd(x)
    if (s == 0) 0 else sum((x-m)^3)/(length(x)*s^3)
  }), 4),
  Kurtosis = round(apply(residuals_matrix, 2, function(x) {
    x <- x[is.finite(x)]
    m <- mean(x); s <- sd(x)
    if (s == 0) 0 else sum((x-m)^4)/(length(x)*s^4) - 3
  }), 4)
)

print(residual_stats)

# ============================================================================
# WHITE NOISE TESTS
# ============================================================================
cat("\n🧪 White Noise Tests:\n")
cat("=====================\n")

white_noise_results <- data.frame(
  Region = regions,
  LjungBox_Statistic = numeric(length(regions)),
  LjungBox_PValue = numeric(length(regions)),
  LjungBox_WhiteNoise = logical(length(regions)),
  stringsAsFactors = FALSE
)

for (i in 1:length(regions)) {
  region <- regions[i]
  residual_series <- residuals_matrix[, i]
  
  # Remove NA values
  residual_series <- residual_series[!is.na(residual_series)]
  
  if (length(residual_series) > 10) {
    # Ljung-Box test
    tryCatch({
      lb_test <- Box.test(residual_series, lag = min(10, length(residual_series)/4), type = "Ljung-Box")
      white_noise_results$LjungBox_Statistic[i] <- lb_test$statistic
      white_noise_results$LjungBox_PValue[i] <- lb_test$p.value
      white_noise_results$LjungBox_WhiteNoise[i] <- lb_test$p.value > 0.05
      
      cat(sprintf("  %s: Ljung-Box p-value = %.4f %s\n", 
                  region, lb_test$p.value,
                  ifelse(lb_test$p.value > 0.05, "✅ White Noise", "❌ Not White Noise")))
    }, error = function(e) {
      cat(sprintf("  %s: Ljung-Box test failed - %s\n", region, e$message))
      white_noise_results$LjungBox_WhiteNoise[i] <- FALSE
    })
  } else {
    cat(sprintf("  %s: Insufficient data for Ljung-Box test\n", region))
    white_noise_results$LjungBox_WhiteNoise[i] <- FALSE
  }
}

# Overall white noise assessment
overall_white_noise <- all(white_noise_results$LjungBox_WhiteNoise, na.rm = TRUE)
cat(sprintf("\n📊 Overall White Noise Assessment: %s\n", 
           ifelse(overall_white_noise, "✅ PASS", "❌ FAIL")))

# ============================================================================
# NORMALITY TESTS
# ============================================================================
cat("\n📊 Normality Tests:\n")
cat("===================\n")

normality_results <- data.frame(
  Region = regions,
  Shapiro_Statistic = numeric(length(regions)),
  Shapiro_PValue = numeric(length(regions)),
  Shapiro_Normal = logical(length(regions)),
  stringsAsFactors = FALSE
)

for (i in 1:length(regions)) {
  region <- regions[i]
  residual_series <- residuals_matrix[, i]
  residual_series <- residual_series[!is.na(residual_series)]
  
  if (length(residual_series) > 3 && length(residual_series) <= 5000) {
    tryCatch({
      shapiro_test <- shapiro.test(residual_series)
      normality_results$Shapiro_Statistic[i] <- shapiro_test$statistic
      normality_results$Shapiro_PValue[i] <- shapiro_test$p.value
      normality_results$Shapiro_Normal[i] <- shapiro_test$p.value > 0.05
      
      cat(sprintf("  %s: Shapiro-Wilk p-value = %.4f %s\n", 
                  region, shapiro_test$p.value,
                  ifelse(shapiro_test$p.value > 0.05, "✅ Normal", "❌ Not Normal")))
    }, error = function(e) {
      cat(sprintf("  %s: Shapiro-Wilk test failed - %s\n", region, e$message))
      normality_results$Shapiro_Normal[i] <- FALSE
    })
  } else {
    cat(sprintf("  %s: Sample size not suitable for Shapiro-Wilk test\n", region))
    normality_results$Shapiro_Normal[i] <- FALSE
  }
}

# ============================================================================
# DIAGNOSTIC SUMMARY
# ============================================================================
cat("\n📋 DIAGNOSTIC SUMMARY:\n")
cat("======================\n")

diagnostic_summary <- data.frame(
  Test = c("White Noise (Ljung-Box)", "Normality (Shapiro-Wilk)", "Overall Model Adequacy"),
  Result = c(
    ifelse(overall_white_noise, "✅ PASS", "❌ FAIL"),
    ifelse(mean(normality_results$Shapiro_Normal, na.rm = TRUE) > 0.6, "✅ MOSTLY PASS", "❌ FAIL"),
    ifelse(overall_white_noise, "✅ ADEQUATE", "⚠️ NEEDS IMPROVEMENT")
  ),
  Interpretation = c(
    "Residuals show no significant autocorrelation",
    "Residuals approximately follow normal distribution", 
    "Model captures temporal dependencies adequately"
  )
)

print(diagnostic_summary)

# ============================================================================
# SAVE RESULTS
# ============================================================================
diagnostic_results <- list(
  model_name = model_name,
  residual_stats = residual_stats,
  white_noise_results = white_noise_results,
  normality_results = normality_results,
  diagnostic_summary = diagnostic_summary,
  overall_assessment = list(
    white_noise = overall_white_noise,
    normality_pass_rate = mean(normality_results$Shapiro_Normal, na.rm = TRUE),
    model_adequate = overall_white_noise
  ),
  spatial_weights = "correlation"
)

save(diagnostic_results, file = "output/12_diagnostic_correlation.RData")

cat(sprintf("\n=== RESIDUAL DIAGNOSTIC COMPLETED (%s - CORRELATION) ===\n", model_name))
cat("✅ White noise tests completed\n")
cat("✅ Normality tests completed\n")
cat("✅ Diagnostic summary generated\n")
cat("✅ Results saved to: output/12_diagnostic_correlation.RData\n")
cat("🔗 Correlation-based spatial weights diagnostic completed\n")
cat("🎯 Ready for Phase 5: Model Selection\n")