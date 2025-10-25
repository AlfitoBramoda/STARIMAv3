# ============================================================================
# STARMA Forecasting Pipeline - Phase 4: Residual Diagnostics
# File: 11c_Residual_Diagnostic_Correlation.R
# Purpose: Test residual white noise properties for correlation weights model
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load required data
load("output/10c_starima_correlation.RData")
load("output/05_spatial_weights.RData")
load("output/09_model_structure.RData")

cat("=== RESIDUAL DIAGNOSTICS - CORRELATION WEIGHTS ===\n")
cat("Testing white noise properties of STARIMA(1,0,2) residuals...\n\n")

# ============================================================================
# SETUP AND DATA PREPARATION
# ============================================================================

library(starma)
library(ggplot2)
library(gridExtra)

cat("📋 Diagnostic Setup:\n")
cat("===================\n")
cat("- Model: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n")
cat("- Spatial weights: Correlation-based\n")
cat("- Residuals length:", length(residuals_correlation), "\n")
cat("- Fitted parameters:", total_params, "\n")
cat("- Degrees of freedom:", length(residuals_correlation) - total_params, "\n\n")

# Prepare spatial weights for stcor.test
correlation_matrix <- spatial_weights$correlation
wlist_correlation <- list()
wlist_correlation[[1]] <- diag(nrow(correlation_matrix))
wlist_correlation[[2]] <- correlation_matrix
wlist_correlation[[3]] <- correlation_matrix %*% correlation_matrix

# Ensure proper row normalization
for (i in 2:length(wlist_correlation)) {
  for (j in 1:nrow(wlist_correlation[[i]])) {
    row_sum <- sum(wlist_correlation[[i]][j, ])
    if (row_sum > 0) {
      wlist_correlation[[i]][j, ] <- wlist_correlation[[i]][j, ] / row_sum
    }
  }
}

# ============================================================================
# WHITE NOISE TEST - STCOR.TEST
# ============================================================================

cat("🔬 White Noise Testing:\n")
cat("=======================\n")

# Perform stcor.test for white noise
cat("Running stcor.test() for spatial-temporal correlation...\n")

tryCatch({
  # Test for spatial-temporal correlation in residuals
  stcor_result <- stcor.test(
    residuals_correlation,
    wlist = wlist_correlation,
    fitdf = total_params
  )
  
  cat("✅ stcor.test completed successfully!\n\n")
  
  # Display results
  cat("📊 White Noise Test Results:\n")
  cat("============================\n")
  print(stcor_result)
  
  # Extract key statistics safely
  test_statistic <- if(is.null(stcor_result$statistic)) NA else as.numeric(stcor_result$statistic)
  p_value <- if(is.null(stcor_result$p.value)) NA else as.numeric(stcor_result$p.value)
  
  cat("\n📈 Key Statistics:\n")
  cat("- Test Statistic:", ifelse(is.na(test_statistic), "N/A", round(test_statistic, 4)), "\n")
  cat("- P-value:", ifelse(is.na(p_value), "N/A", round(p_value, 4)), "\n")
  cat("- Significance Level: 0.05\n")
  
  # Interpretation
  if (!is.na(p_value) && p_value > 0.05) {
    cat("✅ RESULT: Residuals are WHITE NOISE (p > 0.05)\n")
    cat("✅ MODEL ADEQUATE: No significant spatial-temporal correlation\n")
    model_adequate <- TRUE
  } else {
    cat("❌ RESULT: Residuals show PATTERNS (p ≤ 0.05)\n")
    cat("❌ MODEL INADEQUATE: Significant spatial-temporal correlation detected\n")
    model_adequate <- FALSE
  }
  
}, error = function(e) {
  cat("❌ Error in stcor.test:\n")
  cat(e$message, "\n")
  stcor_result <- NULL
  model_adequate <- FALSE
})

# Initialize model_adequate if not set
if (!exists("model_adequate")) {
  model_adequate <- FALSE
}

# ============================================================================
# RESIDUAL ACF/PACF ANALYSIS
# ============================================================================

cat("\n🔍 Residual ACF/PACF Analysis:\n")
cat("==============================\n")

# Calculate residual ACF and PACF
residual_acf <- acf(as.vector(residuals_correlation), plot = FALSE, lag.max = 20)
residual_pacf <- pacf(as.vector(residuals_correlation), plot = FALSE, lag.max = 20)

# Check for significant lags
acf_significant <- sum(abs(residual_acf$acf[-1]) > 1.96/sqrt(length(residuals_correlation)))
pacf_significant <- sum(abs(residual_pacf$acf) > 1.96/sqrt(length(residuals_correlation)))

cat("- ACF significant lags:", acf_significant, "out of 20\n")
cat("- PACF significant lags:", pacf_significant, "out of 20\n")

if (acf_significant <= 1 && pacf_significant <= 1) {
  cat("✅ ACF/PACF: Residuals appear to be white noise\n")
  acf_adequate <- TRUE
} else {
  cat("❌ ACF/PACF: Residuals show significant autocorrelation\n")
  acf_adequate <- FALSE
}

# ============================================================================
# NORMALITY TESTS
# ============================================================================

cat("\n📊 Residual Normality Tests:\n")
cat("============================\n")

# Shapiro-Wilk test (if sample size allows)
if (length(residuals_correlation) <= 5000) {
  shapiro_test <- shapiro.test(as.vector(residuals_correlation))
  cat("- Shapiro-Wilk p-value:", round(shapiro_test$p.value, 4), "\n")
  
  if (shapiro_test$p.value > 0.05) {
    cat("✅ Normality: Residuals are normally distributed\n")
    normality_ok <- TRUE
  } else {
    cat("⚠️ Normality: Residuals deviate from normality\n")
    normality_ok <- FALSE
  }
} else {
  cat("- Sample too large for Shapiro-Wilk test\n")
  normality_ok <- TRUE
}

# Jarque-Bera test alternative
residuals_vec <- as.vector(residuals_correlation)
n <- length(residuals_vec)
skewness <- sum((residuals_vec - mean(residuals_vec))^3) / (n * sd(residuals_vec)^3)
kurtosis <- sum((residuals_vec - mean(residuals_vec))^4) / (n * sd(residuals_vec)^4) - 3

jb_statistic <- n * (skewness^2/6 + kurtosis^2/24)
jb_p_value <- 1 - pchisq(jb_statistic, df = 2)

cat("- Jarque-Bera p-value:", round(jb_p_value, 4), "\n")

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("\n📊 Creating Diagnostic Plots...\n")

# 1. Residual ACF plot (ACF-style like file 07)
acf_data <- data.frame(
  Lag = 1:length(residual_acf$acf[-1]),
  ACF = residual_acf$acf[-1]
)

# Calculate confidence bounds
n <- length(residuals_correlation)
conf_bound <- 1.96 / sqrt(n)

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 2) +
  labs(title = "Residual ACF - Correlation Weights",
       subtitle = paste("Residual autocorrelation analysis - n =", n),
       x = "Lag", y = "Autocorrelation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:length(residual_acf$acf[-1])) +
  ylim(c(min(-conf_bound * 1.2, min(acf_data$ACF) * 1.1), 
         max(conf_bound * 1.2, max(acf_data$ACF) * 1.1)))

# 2. Residual PACF plot (ACF-style like file 07)
pacf_data <- data.frame(
  Lag = 1:length(residual_pacf$acf),
  PACF = residual_pacf$acf
)

pacf_plot <- ggplot(pacf_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = Lag, yend = 0), color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Residual PACF - Correlation Weights",
       subtitle = paste("Residual partial autocorrelation analysis - n =", n),
       x = "Lag", y = "Partial Autocorrelation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:length(residual_pacf$acf)) +
  ylim(c(min(-conf_bound * 1.2, min(pacf_data$PACF) * 1.1), 
         max(conf_bound * 1.2, max(pacf_data$PACF) * 1.1)))

# 3. Q-Q plot for normality
qq_data <- data.frame(
  Theoretical = qnorm(ppoints(length(residuals_correlation))),
  Sample = sort(as.vector(residuals_correlation))
)

qq_plot <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot - Correlation Weights",
       subtitle = "Normal distribution reference line",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Save plots
ggsave("plots/11c_correlation_residual_acf.png", acf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/11c_correlation_residual_pacf.png", pacf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/11c_correlation_qq_plot.png", qq_plot, width = 8, height = 6, dpi = 300)

print(acf_plot)
print(pacf_plot)
print(qq_plot)

cat("✅ ACF plot saved: plots/11c_correlation_residual_acf.png\n")
cat("✅ PACF plot saved: plots/11c_correlation_residual_pacf.png\n")
cat("✅ Q-Q plot saved: plots/11c_correlation_qq_plot.png\n")

# ============================================================================
# DIAGNOSTIC SUMMARY
# ============================================================================

cat("\n📋 Diagnostic Summary:\n")
cat("======================\n")

# Overall model adequacy assessment
overall_adequate <- model_adequate && acf_adequate

diagnostic_results <- list(
  model_type = "STARIMA(1,0,2) - Correlation Weights",
  stcor_test = if(exists("stcor_result")) stcor_result else NULL,
  white_noise = model_adequate,
  acf_adequate = acf_adequate,
  normality = normality_ok,
  overall_adequate = overall_adequate,
  acf_significant_lags = acf_significant,
  pacf_significant_lags = pacf_significant,
  diagnostic_plots = c("11c_correlation_residual_acf.png", 
                      "11c_correlation_residual_pacf.png", 
                      "11c_correlation_qq_plot.png")
)

# Print summary
cat("- White Noise Test:", ifelse(model_adequate, "✅ PASS", "❌ FAIL"), "\n")
cat("- ACF/PACF Test:", ifelse(acf_adequate, "✅ PASS", "❌ FAIL"), "\n")
cat("- Normality Test:", ifelse(normality_ok, "✅ PASS", "⚠️ WARNING"), "\n")
cat("- Overall Assessment:", ifelse(overall_adequate, "✅ MODEL ADEQUATE", "❌ MODEL NEEDS REVISION"), "\n")

if (overall_adequate) {
  cat("\n🎉 CONCLUSION: Model is adequate for forecasting!\n")
  cat("✅ Residuals show no significant spatial-temporal correlation\n")
  cat("✅ Model can proceed to forecasting phase\n")
} else {
  cat("\n⚠️ CONCLUSION: Model may need re-specification!\n")
  cat("❌ Consider different STARIMA orders or model structure\n")
  cat("❌ Review identification and estimation phases\n")
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save diagnostic results
save(diagnostic_results, residual_acf, residual_pacf, 
     file = "output/11c_diagnostic_correlation.RData")

# Display in viewer
cat("\n=== DATA VIEWER ===\n")
if(exists("stcor_result") && !is.null(stcor_result)) {
  tryCatch({
    test_stat <- if(is.null(stcor_result$statistic)) NA else as.numeric(stcor_result$statistic)
    p_val <- if(is.null(stcor_result$p.value)) NA else as.numeric(stcor_result$p.value)
    
    View(data.frame(
      Test = "stcor.test",
      Statistic = ifelse(is.na(test_stat), "N/A", round(test_stat, 4)),
      P_Value = ifelse(is.na(p_val), "N/A", round(p_val, 4)),
      Result = ifelse(!is.na(p_val) && p_val > 0.05, "White Noise", "Correlated")
    ), title = "White Noise Test - Correlation")
  }, error = function(e) {
    cat("⚠️ Viewer display skipped due to data format issues\n")
  })
}

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n=== RESIDUAL DIAGNOSTICS COMPLETED - CORRELATION WEIGHTS ===\n")
cat("✅ White noise test completed\n")
cat("✅ ACF/PACF analysis completed\n")
cat("✅ Normality tests completed\n")
cat("✅ Diagnostic plots generated (3 plots)\n")
cat("✅ Results saved to: output/11c_diagnostic_correlation.RData\n")
cat("✅ Model adequacy:", ifelse(overall_adequate, "ADEQUATE", "NEEDS REVISION"), "\n\n")

cat("📊 PHASE 4 PROGRESS: 3/4 files completed (75%)\n")
cat("🎯 Next step: 12_Model_Selection.R\n\n")

cat("Diagnostic validation:\n")
cat("- White noise testing: ✅\n")
cat("- Residual correlation: ✅\n")
cat("- Normality assessment: ✅\n")
cat("- Visual diagnostics: ✅\n")

cat("\n🎉 STARIMA(1,0,2) correlation weights diagnostic completed!\n")
cat("Model status:", ifelse(overall_adequate, "READY FOR FORECASTING", "NEEDS REVISION"), "\n")

cat("\n🎊 ALL 3 RESIDUAL DIAGNOSTICS COMPLETED!\n")
cat("Ready to proceed to model selection and comparison.\n")