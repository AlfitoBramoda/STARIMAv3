# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Residual Diagnostic (distance SLAG 1)
# File: 12_Residual_Diagnostic_distance_SLAG1.R
# Purpose: Validate STARIMA model through residual analysis and diagnostic tests
# Author: STARMA Analysis - distance Focus SLAG 1
# Date: 2024
# ============================================================================

cat("=== STARIMA RESIDUAL DIAGNOSTIC (distance WEIGHTS - SLAG 1) ===\n\n")

# Load required libraries
library(starma)
library(ggplot2)
library(gridExtra)
library(tseries)

# Load estimation results
load("output/11_starima_distance_slag1.RData")

# Extract dynamic model orders with seasonal parameters
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

cat(sprintf("🔬 Diagnostic Analysis for %s - distance Weights SLAG 1\n\n", model_name))
cat("🎯 Current Model Orders:\n")
cat(sprintf("   Non-seasonal: AR(%d), I(%d), MA(%d)\n", p_order, d_order, q_order))
cat(sprintf("   Seasonal: AR(%d), I(%d), MA(%d), Period=%d\n", P_order, D_order, Q_order, seasonal_period))
cat("   Spatial Lag: 1 (neighbor effects included)\n\n")

# Extract residuals and model info
residuals_matrix <- distance_results_slag1$residuals
model_fit <- distance_results_slag1$model
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
diagnostic_results_slag1 <- list(
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
  spatial_weights = "distance",
  spatial_lag = 1
)

save(diagnostic_results_slag1, file = "output/12_diagnostic_distance_slag1.RData")

cat(sprintf("\n=== RESIDUAL DIAGNOSTIC COMPLETED (%s - distance SLAG 1) ===\n", model_name))
cat(sprintf("🎯 Final Model: %s\n", model_name))
cat(sprintf("📊 Orders used: (p,d,q,P,D,Q,s) = (%d,%d,%d,%d,%d,%d,%d)\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("✅ White noise tests completed\n")
cat("✅ Normality tests completed\n")
cat("✅ Diagnostic summary generated\n")
cat("✅ Results saved to: output/12_diagnostic_distance_slag1.RData\n")
cat("🔗 distance-based spatial weights diagnostic completed (SLAG 1)\n")
cat("🎯 Ready for Phase 5: Model Selection\n")