# ============================================================================
# RESIDUAL DIAGNOSTICS - UNIFORM WEIGHTS
# ============================================================================

library(starma)
library(ggplot2)
library(gridExtra)

# Load required data
if (!file.exists("output/11_starima_uniform.RData")) {
  stop("❌ Missing required file: output/11_starima_uniform.RData")
}
load("output/11_starima_uniform.RData")

if (!file.exists("output/07_spatial_weights_uniform.RData")) {
  stop("❌ Missing required file: output/07_spatial_weights_uniform.RData")
}
load("output/07_spatial_weights_uniform.RData")

# Extract data from uniform_results
residuals_matrix <- uniform_results$residuals
residual_stats <- uniform_results$residual_stats
p_order <- uniform_results$orders$p
q_order <- uniform_results$orders$q
d_order <- uniform_results$orders$d
max_spatial_lag <- uniform_results$orders$max_spatial_lag
total_params <- uniform_results$fit_statistics$parameters

cat("=== RESIDUAL DIAGNOSTICS - UNIFORM WEIGHTS ===\n")
cat(sprintf("Model STARIMA(%d,%d,%d)\n", p_order, d_order, q_order))
cat("- Residuals dim:", paste(dim(residuals_matrix), collapse="x"), "\n")
cat("- Parameters:", total_params, "\n\n")

# ============================================================================
# RESIDUAL ANALYSIS
# ============================================================================

# Convert residuals to vector
residuals_vec <- as.vector(residuals_matrix)
residuals_clean <- residuals_vec[!is.na(residuals_vec)]

cat("📊 Residual Statistics:\n")
print(residual_stats)

# ============================================================================
# ACF/PACF ANALYSIS
# ============================================================================

cat("\n🔍 ACF/PACF Analysis:\n")

# Calculate ACF and PACF
residual_acf <- acf(residuals_clean, plot = FALSE, lag.max = 20)
residual_pacf <- pacf(residuals_clean, plot = FALSE, lag.max = 20)

# Confidence bounds
n <- length(residuals_clean)
conf_bound <- 1.96 / sqrt(n)

# Check significant lags
acf_significant <- which(abs(residual_acf$acf[-1]) > conf_bound)
pacf_significant <- which(abs(residual_pacf$acf) > conf_bound)

cat("- Significant ACF lags:", length(acf_significant), "\n")
cat("- Significant PACF lags:", length(pacf_significant), "\n")

# ============================================================================
# WHITE NOISE TEST
# ============================================================================

cat("\n🔬 White Noise Testing:\n")

# Build spatial weights
W <- spatial_weights$uniform
wlist_uniform <- list(diag(nrow(W)), W)

# Normalize weights
for (i in 1:length(wlist_uniform)) {
  for (j in 1:nrow(wlist_uniform[[i]])) {
    rs <- sum(wlist_uniform[[i]][j, ])
    if (rs > 0) {
      wlist_uniform[[i]][j, ] <- wlist_uniform[[i]][j, ] / rs
    } else if (i == 1) {
      wlist_uniform[[i]][j, j] <- 1
    }
  }
}

# Standardize residuals
residuals_std <- scale(residuals_clean, center = TRUE, scale = TRUE)
residuals_std <- as.vector(residuals_std)

# Test with stcor.test
stcor_result <- NULL
model_adequate <- FALSE

tryCatch({
  stcor_result <- stcor.test(
    residuals_std, 
    wlist = wlist_uniform, 
    fitdf = total_params
  )
  
  if (!is.null(stcor_result)) {
    test_stat <- as.numeric(stcor_result$statistic)
    p_value <- as.numeric(stcor_result$p.value)
    
    cat("- Test Statistic:", round(test_stat, 4), "\n")
    cat("- P-value:", round(p_value, 6), "\n")
    
    if (p_value > 0.05) {
      cat("✅ RESULT: Residuals are WHITE NOISE\n")
      model_adequate <- TRUE
    } else {
      cat("❌ RESULT: Residuals show correlation\n")
      model_adequate <- FALSE
    }
  }
}, error = function(e) {
  cat("❌ stcor.test failed, using alternative test\n")
  
  # Alternative: Box-Ljung test
  if (length(residuals_clean) > 10) {
    box_test <- Box.test(residuals_clean, lag = min(10, length(residuals_clean)/4), type = "Ljung-Box")
    cat("- Box-Ljung statistic:", round(box_test$statistic, 4), "\n")
    cat("- P-value:", round(box_test$p.value, 6), "\n")
    
    model_adequate <- box_test$p.value > 0.05
    
    stcor_result <- list(
      statistic = box_test$statistic,
      p.value = box_test$p.value,
      method = "Box-Ljung test"
    )
  }
})

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("\n📊 Creating Diagnostic Plots...\n")

# 1. ACF plot
acf_data <- data.frame(
  Lag = 1:length(residual_acf$acf[-1]),
  ACF = residual_acf$acf[-1]
)

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Residual ACF - Uniform Weights",
       x = "Lag", y = "Autocorrelation") +
  theme_minimal()

# 2. PACF plot
pacf_data <- data.frame(
  Lag = 1:length(residual_pacf$acf),
  PACF = residual_pacf$acf
)

pacf_plot <- ggplot(pacf_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Residual PACF - Uniform Weights",
       x = "Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# 3. Q-Q plot
qq_data <- data.frame(
  Theoretical = qnorm(ppoints(length(residuals_clean))),
  Sample = sort(residuals_clean)
)

qq_plot <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot - Uniform Weights",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Save plots
dir.create("plots", showWarnings = FALSE)
ggsave("plots/12_uniform_residual_acf.png", acf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/12_uniform_residual_pacf.png", pacf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/12_uniform_qq_plot.png", qq_plot, width = 8, height = 6, dpi = 300)

print(acf_plot)
print(pacf_plot)
print(qq_plot)

# ============================================================================
# DIAGNOSTIC SUMMARY
# ============================================================================

acf_adequate <- length(acf_significant) <= 2
overall_adequate <- model_adequate && acf_adequate

diagnostic_results <- list(
  model_type = paste0("STARIMA(", p_order, ",", d_order, ",", q_order, ") - Uniform Weights"),
  stcor_test = stcor_result,
  white_noise = model_adequate,
  acf_adequate = acf_adequate,
  overall_adequate = overall_adequate,
  acf_significant_lags = acf_significant,
  pacf_significant_lags = pacf_significant,
  residual_stats = residual_stats
)

cat("\n📋 Diagnostic Summary:\n")
cat("- White Noise Test:", ifelse(model_adequate, "✅ PASS", "❌ FAIL"), "\n")
cat("- ACF/PACF Test:", ifelse(acf_adequate, "✅ PASS", "❌ FAIL"), "\n")
cat("- Overall Assessment:", ifelse(overall_adequate, "✅ ADEQUATE", "❌ NEEDS REVISION"), "\n")

# Save results
save(diagnostic_results, residual_acf, residual_pacf, 
     file = "output/12_diagnostic_uniform.RData")

cat("\n✅ Diagnostic completed and saved to: output/12_diagnostic_uniform.RData\n")
cat("✅ Plots saved to: plots/12_uniform_*.png\n")