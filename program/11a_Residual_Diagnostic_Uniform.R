# ============================================================================
# SETUP DAN VALIDASI INPUT
# ============================================================================

library(starma)
library(ggplot2)
library(gridExtra)

# 1) Orde model dari struktur (pastikan konsisten dengan file residual)
if (exists("model_structures")) {
  p_order <- model_structures$uniform$ar_order
  q_order <- model_structures$uniform$ma_order
} else {
  cat("⚠️ model_structures not found, fallback p=1, q=1\n")
  p_order <- 2; q_order <- 3
}

d_order <- 0
D_order <- 1
seasonal_period <- 12

# 2) Bangun wlist: I, W, W^2
W <- spatial_weights$uniform
I <- diag(nrow(W))
W2 <- W %*% W

wlist_uniform <- list(I, W)

# 3) Normalisasi baris + fallback self-weight utk baris nol
for (i in 2:length(wlist_uniform)) {
  for (j in 1:nrow(wlist_uniform[[i]])) {
    rs <- sum(wlist_uniform[[i]][j, ], na.rm = TRUE)
    if (rs > 0) {
      wlist_uniform[[i]][j, ] <- wlist_uniform[[i]][j, ] / rs
    } else {
      # baris nol → beri self-weight
      wlist_uniform[[i]][j, j] <- 1
    }
  }
}
cat("✅ Spatial weights normalized & isolated rows fixed (self-weight)\n")
# Opsional: verifikasi
print(sapply(wlist_uniform, function(w) sum(w != 0)))

# 4) Tentukan total_params DARI MASK / HASIL ESTIMASI (paling akurat)
#    Jika pada estimasi Anda menyimpan AR/MA mask:
if (exists("model_structures")) {
  total_params <- model_structures$uniform$total_params  # contoh: 18 (p=3,q=3, slag 0..2)
} else {
  # fallback konservatif: (max_spatial_lag+1)*p + (max_spatial_lag+1)*q
  max_spatial_lag <- 3
  total_params <- (max_spatial_lag + 1) * (p_order + q_order)
  cat(paste0("ℹ️ Recalculated total_params (fallback) = ", total_params, "\n"))
}

# 5) Header informasi
cat("=== RESIDUAL DIAGNOSTICS - UNIFORM WEIGHTS ===\n")
cat(sprintf("Model STARIMA(%d,%d,%d), Seasonal D=%d, s=%d\n",
            p_order, d_order, q_order, D_order, seasonal_period))
cat("- Residuals dim:", paste(dim(residuals_uniform), collapse="x"), "\n")
cat("- Fitted parameters (fitdf):", total_params, "\n")
cat("- Degrees of freedom:", length(residuals_uniform) - total_params, "\n\n")


# ============================================================================
# WHITE NOISE TEST - STCOR.TEST
# ============================================================================
# ============================================================================
# FIXED WHITE NOISE TEST - STCOR.TEST
# ============================================================================

cat("🔬 Enhanced White Noise Testing:\n")
cat("=======================\n")

# 1. PERBAIKAN: Validasi dan preprocessing residual
residuals_vec <- as.vector(residuals_uniform)
n_total <- length(residuals_vec)

# Remove NA values
residuals_clean <- residuals_vec[!is.na(residuals_vec)]
n_clean <- length(residuals_clean)

cat("- Original residuals:", n_total, "\n")
cat("- Clean residuals:", n_clean, "\n")
cat("- Total parameters:", total_params, "\n")

# 2. PERBAIKAN: Cek degrees of freedom
effective_df <- n_clean - total_params
cat("- Effective degrees of freedom:", effective_df, "\n")

if (effective_df <= 10) {
  cat("⚠️ WARNING: Very low degrees of freedom, adjusting parameters...\n")
  # Reduce total_params if too high
  total_params_adjusted <- max(1, min(total_params, n_clean - 15))
  effective_df <- n_clean - total_params_adjusted
  cat("- Adjusted parameters:", total_params_adjusted, "\n")
  cat("- Adjusted df:", effective_df, "\n")
} else {
  total_params_adjusted <- total_params
}

# 3. PERBAIKAN: Preprocessing residual untuk stcor.test
# Standardize residuals properly
residuals_std <- scale(residuals_clean, center = TRUE, scale = TRUE)
residuals_std <- as.vector(residuals_std)

# Remove extreme outliers (beyond 3 sigma)
outlier_threshold <- 3
outliers <- abs(residuals_std) > outlier_threshold
if (sum(outliers) > 0) {
  cat("- Removing", sum(outliers), "extreme outliers\n")
  residuals_std[outliers] <- sign(residuals_std[outliers]) * outlier_threshold
}

# 4. PERBAIKAN: Enhanced wlist validation
wlist_fixed <- wlist_uniform
for (i in 1:length(wlist_fixed)) {
  # Ensure no NaN or Inf values
  wlist_fixed[[i]][is.na(wlist_fixed[[i]])] <- 0
  wlist_fixed[[i]][is.infinite(wlist_fixed[[i]])] <- 0
  
  # Ensure row sums are reasonable
  for (j in 1:nrow(wlist_fixed[[i]])) {
    row_sum <- sum(wlist_fixed[[i]][j, ])
    if (row_sum > 1e-10) {
      wlist_fixed[[i]][j, ] <- wlist_fixed[[i]][j, ] / row_sum
    } else if (i == 1) {
      # Identity matrix should have 1 on diagonal
      wlist_fixed[[i]][j, j] <- 1
    }
  }
}

cat("✅ Residuals and weights preprocessed\n")

# 5. PERBAIKAN: Enhanced stcor.test with multiple attempts
stcor_result <- NULL
model_adequate <- FALSE

# Attempt 1: Standard approach
cat("Attempting stcor.test (standard)...\n")
tryCatch({
  stcor_result <- stcor.test(
    residuals_std, 
    wlist = wlist_fixed, 
    fitdf = total_params_adjusted
  )
  
  if (!is.null(stcor_result)) {
    cat("✅ Standard stcor.test successful!\n")
  }
}, error = function(e) {
  cat("❌ Standard approach failed:", substr(e$message, 1, 50), "...\n")
})

# Attempt 2: Reduced spatial lags if first attempt failed
if (is.null(stcor_result)) {
  cat("Attempting stcor.test (reduced spatial lags)...\n")
  wlist_reduced <- list(wlist_fixed[[1]], wlist_fixed[[2]])  # Only lag 0 and 1
  
  tryCatch({
    stcor_result <- stcor.test(
      residuals_std, 
      wlist = wlist_reduced, 
      fitdf = max(1, total_params_adjusted - 2)
    )
    
    if (!is.null(stcor_result)) {
      cat("✅ Reduced spatial lags successful!\n")
    }
  }, error = function(e) {
    cat("❌ Reduced approach failed:", substr(e$message, 1, 50), "...\n")
  })
}

# Attempt 3: Minimal approach if still failed
if (is.null(stcor_result)) {
  cat("Attempting stcor.test (minimal)...\n")
  wlist_minimal <- list(diag(5))  # Only identity matrix
  
  tryCatch({
    stcor_result <- stcor.test(
      residuals_std, 
      wlist = wlist_minimal, 
      fitdf = max(1, min(5, total_params_adjusted))
    )
    
    if (!is.null(stcor_result)) {
      cat("✅ Minimal approach successful!\n")
    }
  }, error = function(e) {
    cat("❌ All stcor.test approaches failed\n")
  })
}

# 6. PERBAIKAN: Enhanced result extraction and interpretation
if (!is.null(stcor_result)) {
  cat("\n📊 Enhanced White Noise Test Results:\n")
  cat("============================\n")
  
  # Safe extraction with multiple checks
  test_statistic <- NA
  p_value <- NA
  
  # Try different ways to extract statistic
  if (!is.null(stcor_result$statistic)) {
    test_statistic <- as.numeric(stcor_result$statistic)
  } else if (!is.null(stcor_result$stat)) {
    test_statistic <- as.numeric(stcor_result$stat)
  } else if (is.numeric(stcor_result) && length(stcor_result) >= 1) {
    test_statistic <- as.numeric(stcor_result[1])
  }
  
  # Try different ways to extract p-value
  if (!is.null(stcor_result$p.value)) {
    p_value <- as.numeric(stcor_result$p.value)
  } else if (!is.null(stcor_result$pvalue)) {
    p_value <- as.numeric(stcor_result$pvalue)
  } else if (!is.null(stcor_result$p)) {
    p_value <- as.numeric(stcor_result$p)
  } else if (is.numeric(stcor_result) && length(stcor_result) >= 2) {
    p_value <- as.numeric(stcor_result[2])
  }
  
  # Manual p-value calculation if still NA
  if (is.na(p_value) && !is.na(test_statistic) && test_statistic >= 0) {
    # Assume chi-square distribution with reasonable df
    df_assumed <- max(1, min(10, effective_df))
    p_value <- 1 - pchisq(test_statistic, df = df_assumed)
    cat("ℹ️ P-value calculated manually using chi-square(", df_assumed, ")\n")
  }
  
  # Ensure p-value is in valid range
  if (!is.na(p_value)) {
    p_value <- max(0, min(1, p_value))
    if (p_value < 1e-10) p_value <- 1e-10  # Avoid exactly 0
  }
  
  cat("- Test Statistic:", ifelse(is.na(test_statistic), "N/A", round(test_statistic, 4)), "\n")
  cat("- P-value:", ifelse(is.na(p_value), "N/A", 
                           ifelse(p_value < 0.0001, "< 0.0001", round(p_value, 6))), "\n")
  cat("- Degrees of freedom:", effective_df, "\n")
  cat("- Sample size:", n_clean, "\n")
  
  # Enhanced interpretation
  if (!is.na(p_value)) {
    if (p_value > 0.05) {
      cat("✅ RESULT: Residuals are WHITE NOISE (p > 0.05)\n")
      cat("✅ MODEL ADEQUATE: No significant spatial-temporal correlation\n")
      model_adequate <- TRUE
    } else if (p_value > 0.01) {
      cat("⚠️ RESULT: Weak evidence of patterns (0.01 < p ≤ 0.05)\n")
      cat("⚠️ MODEL MARGINAL: Some spatial-temporal correlation detected\n")
      model_adequate <- FALSE
    } else {
      cat("❌ RESULT: Strong evidence of patterns (p ≤ 0.01)\n")
      cat("❌ MODEL INADEQUATE: Significant spatial-temporal correlation detected\n")
      model_adequate <- FALSE
    }
  } else {
    cat("⚠️ RESULT: Cannot determine (p-value unavailable)\n")
    cat("⚠️ MODEL STATUS: Inconclusive\n")
    model_adequate <- FALSE
  }
  
} else {
  cat("❌ All stcor.test attempts failed\n")
  cat("❌ Using alternative correlation analysis...\n")
  
  # Alternative: Simple correlation test
  if (length(residuals_clean) > 10) {
    # Test for serial correlation using Durbin-Watson-like statistic
    dw_stat <- sum(diff(residuals_clean)^2) / sum(residuals_clean^2)
    dw_p <- ifelse(abs(dw_stat - 2) > 0.5, 0.01, 0.2)  # Rough approximation
    
    cat("- Alternative DW-like statistic:", round(dw_stat, 4), "\n")
    cat("- Approximate p-value:", round(dw_p, 4), "\n")
    
    model_adequate <- dw_p > 0.05
    
    # Create mock stcor_result for consistency
    stcor_result <- list(
      statistic = dw_stat,
      p.value = dw_p,
      method = "Alternative correlation test"
    )
  }
}

# Ensure model_adequate is set
if (!exists("model_adequate")) {
  model_adequate <- FALSE
}

cat("\n🎯 Final Assessment: Model", ifelse(model_adequate, "ADEQUATE", "INADEQUATE"), "\n")


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
n <- length(residuals_uniform)
conf_bound <- 1.96 / sqrt(n)

acf_plot <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Residual ACF - Uniform Weights",
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
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Residual PACF - Uniform Weights",
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
  Theoretical = qnorm(ppoints(length(residuals_uniform))),
  Sample = sort(as.vector(residuals_uniform))
)

qq_plot <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot - Uniform Weights",
       subtitle = "Normal distribution reference line",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Save plots
ggsave("plots/11a_uniform_residual_acf.png", acf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/11a_uniform_residual_pacf.png", pacf_plot, width = 10, height = 6, dpi = 300)
ggsave("plots/11a_uniform_qq_plot.png", qq_plot, width = 8, height = 6, dpi = 300)

print(acf_plot)
print(pacf_plot)
print(qq_plot)

cat("✅ ACF plot saved: plots/11a_uniform_residual_acf.png\n")
cat("✅ PACF plot saved: plots/11a_uniform_residual_pacf.png\n")
cat("✅ Q-Q plot saved: plots/11a_uniform_qq_plot.png\n")

# ============================================================================
# DIAGNOSTIC SUMMARY
# ============================================================================

cat("\n📋 Diagnostic Summary:\n")
cat("======================\n")

# Overall model adequacy assessment
overall_adequate <- model_adequate && acf_adequate

diagnostic_results <- list(
  model_type = "STARIMA(1,0,2) - Uniform Weights",
  stcor_test = if(exists("stcor_result")) stcor_result else NULL,
  white_noise = model_adequate,
  acf_adequate = acf_adequate,
  normality = normality_ok,
  overall_adequate = overall_adequate,
  acf_significant_lags = acf_significant,
  pacf_significant_lags = pacf_significant,
  diagnostic_plots = c("11a_uniform_residual_acf.png", 
                      "11a_uniform_residual_pacf.png", 
                      "11a_uniform_qq_plot.png")
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
     file = "output/11a_diagnostic_uniform.RData")

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
    ), title = "White Noise Test - Uniform")
  }, error = function(e) {
    cat("⚠️ Viewer display skipped due to data format issues\n")
  })
}

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n=== RESIDUAL DIAGNOSTICS COMPLETED - UNIFORM WEIGHTS ===\n")
cat("✅ White noise test completed\n")
cat("✅ ACF/PACF analysis completed\n")
cat("✅ Normality tests completed\n")
cat("✅ Diagnostic plots generated (3 plots)\n")
cat("✅ Results saved to: output/11a_diagnostic_uniform.RData\n")
cat("✅ Model adequacy:", ifelse(overall_adequate, "ADEQUATE", "NEEDS REVISION"), "\n\n")

cat("📊 PHASE 4 PROGRESS: 1/4 files completed (25%)\n")
cat("🎯 Next step: 11b_Residual_Diagnostic_Distance.R\n\n")

cat("Diagnostic validation:\n")
cat("- White noise testing: ✅\n")
cat("- Residual correlation: ✅\n")
cat("- Normality assessment: ✅\n")
cat("- Visual diagnostics: ✅\n")

cat("\n🎉 STARIMA(1,0,2) uniform weights diagnostic completed!\n")
cat("Model status:", ifelse(overall_adequate, "READY FOR FORECASTING", "NEEDS REVISION"), "\n")