# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Residual Diagnostic (uniform)
# File: 12_Residual_Diagnostic_uniform.R
# Purpose: Validate STARIMA model through residual analysis and diagnostic tests
# Author: STARMA Analysis - uniform Focus
# Date: 2024
# ============================================================================

cat("=== STARIMA RESIDUAL DIAGNOSTIC (uniform WEIGHTS) ===\n\n")

# Load required libraries
library(starma)
library(ggplot2)
library(gridExtra)
library(tseries)

# Load estimation results
load("output/11_starima_uniform.RData")

# Extract dynamic model orders with seasonal parameters
p_order <- uniform_results$orders$p
d_order <- uniform_results$orders$d
q_order <- uniform_results$orders$q
# Extract seasonal parameters if available
P_order <- if (!is.null(uniform_results$orders$P)) uniform_results$orders$P else 0
Q_order <- if (!is.null(uniform_results$orders$Q)) uniform_results$orders$Q else 0
D_order <- if (!is.null(uniform_results$orders$D)) uniform_results$orders$D else 1
seasonal_period <- if (!is.null(uniform_results$orders$s)) uniform_results$orders$s else 12

model_name <- sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)%d", 
                     p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period)

cat(sprintf("🔬 Diagnostic Analysis for %s - uniform Weights\n\n", model_name))
cat("🎯 Current Model Orders:\n")
cat(sprintf("   Non-seasonal: AR(%d), I(%d), MA(%d)\n", p_order, d_order, q_order))
cat(sprintf("   Seasonal: AR(%d), I(%d), MA(%d), Period=%d\n\n", P_order, D_order, Q_order, seasonal_period))

# Extract residuals and model info
residuals_matrix <- uniform_results$residuals
model_fit <- uniform_results$model
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
# RESIDUAL ACF/PACF ANALYSIS (SPATIAL LAG 0 & 1)
# ============================================================================
cat("\n📊 Residual ACF/PACF Analysis:\n")
cat("==============================\n")

# Load spatial weights for residual spatial analysis
load("output/07_spatial_weights_uniform.RData")
C <- spatial_weights$uniform

# Create spatial weights list for residual analysis
wlist_residual <- list()
wlist_residual[[1]] <- diag(nrow(C))  # Spatial lag 0 (Identity)
wlist_residual[[2]] <- C              # Spatial lag 1 (uniform)

# Row normalization
for (i in seq_along(wlist_residual)) {
  rs <- rowSums(wlist_residual[[i]])
  rs[rs == 0] <- 1
  wlist_residual[[i]] <- wlist_residual[[i]] / rs
}

# Compute STACF and STPACF for residuals
max_lag <- min(40, nrow(residuals_matrix) - 1)  # Same as STACF/STPACF in files 8&9

tryCatch({
  # STACF of residuals
  residual_stacf <- stacf(residuals_matrix, wlist = wlist_residual, tlag.max = max_lag, plot = FALSE)
  
  # STPACF of residuals  
  residual_stpacf <- stpacf(residuals_matrix, wlist = wlist_residual, tlag.max = max_lag, plot = FALSE)
  
  cat("✅ Residual STACF/STPACF computed successfully\n")
  
  # Plot setup
  library(gridExtra)
  temporal_lags <- 1:nrow(residual_stacf)
  n_obs <- nrow(residuals_matrix)
  conf_bound <- 1.96 / sqrt(n_obs)
  
  # ============================================================================
  # RESIDUAL STACF PLOTS (Spatial Lag 0 & 1)
  # ============================================================================
  cat("📈 Creating Residual STACF plots...\n")
  
  # STACF - Spatial Lag 0 (Within-region effects)
  stacf_slag0_data <- data.frame(
    Lag = temporal_lags,
    ACF = residual_stacf[, 1]
  )
  
  p_stacf_slag0 <- ggplot(stacf_slag0_data, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
    geom_point(color = "darkgreen", size = 2) +
    labs(title = "Residual STACF: uniform Weights - Spatial Lag 0",
         subtitle = "Within-region residual autouniform (Identity matrix)",
         x = "Temporal Lag", y = "Residual STACF") +
    theme_minimal()
  
  # STACF - Spatial Lag 1 (Neighbor effects)
  stacf_slag1_data <- data.frame(
    Lag = temporal_lags,
    ACF = residual_stacf[, 2]
  )
  
  p_stacf_slag1 <- ggplot(stacf_slag1_data, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
    geom_point(color = "darkred", size = 2) +
    labs(title = "Residual STACF: uniform Weights - Spatial Lag 1",
         subtitle = "Neighbor residual autouniform (uniform matrix)",
         x = "Temporal Lag", y = "Residual STACF") +
    theme_minimal()
  
  # ============================================================================
  # RESIDUAL STPACF PLOTS (Spatial Lag 0 & 1)
  # ============================================================================
  cat("📈 Creating Residual STPACF plots...\n")
  
  # STPACF - Spatial Lag 0 (Within-region effects)
  stpacf_slag0_data <- data.frame(
    Lag = temporal_lags,
    PACF = residual_stpacf[, 1]
  )
  
  p_stpacf_slag0 <- ggplot(stpacf_slag0_data, aes(x = Lag, y = PACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
    geom_point(color = "darkgreen", size = 2) +
    labs(title = "Residual STPACF: uniform Weights - Spatial Lag 0",
         subtitle = "Within-region residual partial autouniform (Identity matrix)",
         x = "Temporal Lag", y = "Residual STPACF") +
    theme_minimal()
  
  # STPACF - Spatial Lag 1 (Neighbor effects)
  stpacf_slag1_data <- data.frame(
    Lag = temporal_lags,
    PACF = residual_stpacf[, 2]
  )
  
  p_stpacf_slag1 <- ggplot(stpacf_slag1_data, aes(x = Lag, y = PACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
    geom_point(color = "darkred", size = 2) +
    labs(title = "Residual STPACF: uniform Weights - Spatial Lag 1",
         subtitle = "Neighbor residual partial autouniform (uniform matrix)",
         x = "Temporal Lag", y = "Residual STPACF") +
    theme_minimal()
  
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
  # Individual plots
  ggsave("plots/12_residual_stacf_uniform_slag0.png", p_stacf_slag0, width = 10, height = 6, dpi = 300)
  ggsave("plots/12_residual_stacf_uniform_slag1.png", p_stacf_slag1, width = 10, height = 6, dpi = 300)
  ggsave("plots/12_residual_stpacf_uniform_slag0.png", p_stpacf_slag0, width = 10, height = 6, dpi = 300)
  ggsave("plots/12_residual_stpacf_uniform_slag1.png", p_stpacf_slag1, width = 10, height = 6, dpi = 300)
  
  # Combined plots
  combined_stacf <- grid.arrange(p_stacf_slag0, p_stacf_slag1, ncol = 2)
  combined_stpacf <- grid.arrange(p_stpacf_slag0, p_stpacf_slag1, ncol = 2)
  
  ggsave("plots/12_residual_stacf_uniform_combined.png", combined_stacf, width = 16, height = 6, dpi = 300)
  ggsave("plots/12_residual_stpacf_uniform_combined.png", combined_stpacf, width = 16, height = 6, dpi = 300)
  
  # Display plots
  print(p_stacf_slag0)
  print(p_stacf_slag1)
  print(p_stpacf_slag0)
  print(p_stpacf_slag1)
  
  cat("✅ Residual STACF/STPACF plots saved\n")
  
  # Check for significant residual autouniform
  significant_stacf_slag0 <- any(abs(residual_stacf[, 1]) > conf_bound, na.rm = TRUE)
  significant_stacf_slag1 <- any(abs(residual_stacf[, 2]) > conf_bound, na.rm = TRUE)
  significant_stpacf_slag0 <- any(abs(residual_stpacf[, 1]) > conf_bound, na.rm = TRUE)
  significant_stpacf_slag1 <- any(abs(residual_stpacf[, 2]) > conf_bound, na.rm = TRUE)
  
  cat("\n🔍 Residual Autouniform Assessment:\n")
  cat(sprintf("- STACF Spatial Lag 0: %s\n", ifelse(significant_stacf_slag0, "❌ Significant autocorr.", "✅ No significant autocorr.")))
  cat(sprintf("- STACF Spatial Lag 1: %s\n", ifelse(significant_stacf_slag1, "❌ Significant autocorr.", "✅ No significant autocorr.")))
  cat(sprintf("- STPACF Spatial Lag 0: %s\n", ifelse(significant_stpacf_slag0, "❌ Significant partial autocorr.", "✅ No significant partial autocorr.")))
  cat(sprintf("- STPACF Spatial Lag 1: %s\n", ifelse(significant_stpacf_slag1, "❌ Significant partial autocorr.", "✅ No significant partial autocorr.")))
  
}, error = function(e) {
  cat("⚠️ Residual STACF/STPACF analysis failed:", e$message, "\n")
  cat("🔄 Continuing with basic diagnostic tests...\n")
})

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
    "Residuals show no significant autouniform",
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
  spatial_weights = "uniform"
)

save(diagnostic_results, file = "output/12_diagnostic_uniform.RData")

cat(sprintf("\n=== RESIDUAL DIAGNOSTIC COMPLETED (%s - uniform) ===\n", model_name))
cat(sprintf("🎯 Final Model: %s\n", model_name))
cat(sprintf("📊 Orders used: (p,d,q,P,D,Q,s) = (%d,%d,%d,%d,%d,%d,%d)\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("✅ White noise tests completed\n")
cat("✅ Normality tests completed\n")
cat("✅ Diagnostic summary generated\n")
cat("✅ Results saved to: output/12_diagnostic_uniform.RData\n")
cat("🔗 uniform-based spatial weights diagnostic completed\n")
cat("🎯 Ready for Phase 5: Model Selection\n")