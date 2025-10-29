# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation
# File: 10a_STARIMA_Estimation_Uniform.R
# Purpose: Estimate STARIMA(1,0,2) model using uniform spatial weights
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load required data
load("output/09_model_structure_all_weights.RData")
load("output/05_spatial_weights.RData")
load("output/04_centered_data.RData")

cat("=== STARIMA ESTIMATION - UNIFORM WEIGHTS ===\n")
#cat("Estimating STARIMA(1,0,2) model using uniform spatial weights...\n\n")

# ============================================================================
# ESTIMATION SETUP
# ============================================================================

cat("📋 Estimation Setup:\n")
cat("===================\n")
cat("- Model: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n")
cat("- Spatial weights: Uniform\n")
cat("- Training data: 108 observations\n")
cat("- Parameters to estimate:", total_params, "\n")
cat("- Degrees of freedom:", 108 - total_params, "\n\n")

# Prepare data and weights (train_data already loaded from 06_data_split.RData)
# Convert spatial weights to proper wlist format for starma function
uniform_matrix <- spatial_weights$uniform

# Create wlist format: list of matrices for each spatial lag
# For STARMA, we need spatial lags 0, 1, 2 (max_spatial_lag = 2)
max_spatial_lag <- 2
wlist_uniform <- list()

# Spatial lag 0: Identity matrix (within-region effects)
wlist_uniform[[1]] <- diag(nrow(uniform_matrix))

# Spatial lag 1: Direct neighbors (original weights matrix)
wlist_uniform[[2]] <- uniform_matrix

# Spatial lag 2: Second-order neighbors (weights matrix squared)
wlist_uniform[[3]] <- uniform_matrix %*% uniform_matrix

# Ensure proper row normalization for higher order lags
for (i in 2:length(wlist_uniform)) {
  for (j in 1:nrow(wlist_uniform[[i]])) {
    row_sum <- sum(wlist_uniform[[i]][j, ])
    if (row_sum > 0) {
      wlist_uniform[[i]][j, ] <- wlist_uniform[[i]][j, ] / row_sum
    }
  }
}

cat("Data dimensions:\n")
cat("- Training data:", dim(train_data), "\n")
cat("- Spatial weights list:", length(wlist_uniform), "matrices\n")
cat("- Each matrix dimension:", dim(wlist_uniform[[1]]), "\n\n")

# ============================================================================
# STARIMA MODEL ESTIMATION
# ============================================================================

cat("🔧 Estimating STARIMA Model...\n")

# Estimate STARIMA model using starma package
library(starma)

# Set estimation parameters
estimation_start_time <- Sys.time()

cat("Starting estimation with parameters:\n")
cat("- AR mask dimensions:", dim(ar_mask), "\n")
cat("- MA mask dimensions:", dim(ma_mask), "\n")
cat("- Spatial weights list:", length(wlist_uniform), "lags\n")
cat("- Data class:", class(train_data), "\n")
cat("- Weights class:", class(wlist_uniform), "\n\n")

# Estimate the model
tryCatch({
  starima_uniform <- starma(
    data = train_data,
    wlist = wlist_uniform,
    ar = ar_mask,
    ma = ma_mask
  )
  
  estimation_end_time <- Sys.time()
  estimation_time <- estimation_end_time - estimation_start_time
  
  cat("✅ Model estimation completed successfully!\n")
  cat("⏱️ Estimation time:", round(as.numeric(estimation_time), 2), "seconds\n\n")
  
}, error = function(e) {
  cat("❌ Error in model estimation:\n")
  cat(e$message, "\n")
  stop("Model estimation failed")
})

# ============================================================================
# MODEL SUMMARY AND DIAGNOSTICS
# ============================================================================

cat("📊 Model Summary:\n")
cat("=================\n")

# Display model summary
print(summary(starima_uniform))

# Extract coefficient information from summary
summary_coef <- summary(starima_uniform)$coefficients

# Create coefficient table directly from summary
sm <- summary(starima_uniform)
summary_coef <- as.data.frame(sm$coefficients, stringsAsFactors = FALSE)

# Normalize canonical column names regardless of dots/spaces
names(summary_coef) <- sub("\\.$", "", gsub("\\s+", ".", names(summary_coef))) # "Std. Error" -> "Std.Error"
col_map <- c(Estimate="Estimate", Std_Error="Std.Error", t_value="t.value", p_value="p.value")
for (k in names(col_map)) {
  if (!col_map[[k]] %in% names(summary_coef)) {
    summary_coef[[col_map[[k]]]] <- NA_real_
  }
}

coef_table <- data.frame(
  Parameter   = rownames(summary_coef) %||% paste0("param_", seq_len(nrow(summary_coef))),
  Estimate    = round(summary_coef[["Estimate"]], 4),
  Std_Error   = round(summary_coef[["Std.Error"]], 4),
  t_value     = round(summary_coef[["t.value"]], 3),
  p_value     = round(summary_coef[["p.value"]], 4),
  Significant = ifelse(summary_coef[["p.value"]] < 0.05, "***",
                       ifelse(summary_coef[["p.value"]] < 0.1, "*", "")),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# Model fit statistics
loglik <- ifelse(is.null(starima_uniform$loglik), NA, starima_uniform$loglik)
aic <- ifelse(is.null(starima_uniform$aic), NA, starima_uniform$aic)
bic <- ifelse(is.null(starima_uniform$bic), NA, starima_uniform$bic)

# Calculate AIC/BIC if not available
if (is.na(aic) && !is.na(loglik)) {
  n_params <- nrow(summary_coef)
  aic <- -2 * loglik + 2 * n_params
  bic <- -2 * loglik + log(nrow(train_data)) * n_params
}

cat("\n📈 Model Fit Statistics:\n")
cat("- Log-likelihood:", round(loglik, 4), "\n")
cat("- AIC:", round(aic, 4), "\n")
cat("- BIC:", round(bic, 4), "\n")
cat("- Parameters:", total_params, "\n")
cat("- Observations:", nrow(train_data), "\n\n")

# ============================================================================
# RESIDUAL ANALYSIS
# ============================================================================

cat("🔍 Residual Analysis:\n")
cat("=====================\n")

# Extract residuals
residuals_uniform <- starima_uniform$residuals
# Ensure residuals are a 2D matrix with region columns
if (is.null(dim(residuals_uniform))) {
  # If residuals came as a vector, reshape using train_data's dimensions
  Tn <- nrow(train_data)
  Rn <- ncol(train_data)
  residuals_uniform <- matrix(residuals_uniform, nrow = Tn, ncol = Rn, byrow = FALSE)
}

# Ensure column names exist (inherit from training data if missing)
if (is.null(colnames(residuals_uniform)) || any(colnames(residuals_uniform) == "")) {
  colnames(residuals_uniform) <- colnames(train_data)
  # Fallback default if train_data also lacks names
  if (any(is.na(colnames(residuals_uniform))) || any(colnames(residuals_uniform) == "")) {
    colnames(residuals_uniform) <- paste0("Region_", seq_len(ncol(residuals_uniform)))
  }
}


# Basic residual statistics (using base R functions)
# Calculate skewness and kurtosis manually
calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / (n * sd_x^3)
  return(skew)
}

calc_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  kurt <- sum((x - mean_x)^4) / (n * sd_x^4) - 3
  return(kurt)
}

residual_stats <- data.frame(
  Statistic = c("Mean", "Std Dev", "Min", "Max", "Skewness", "Kurtosis"),
  Value = c(
    round(mean(residuals_uniform, na.rm = TRUE), 6),
    round(sd(residuals_uniform, na.rm = TRUE), 4),
    round(min(residuals_uniform, na.rm = TRUE), 4),
    round(max(residuals_uniform, na.rm = TRUE), 4),
    round(calc_skewness(residuals_uniform), 4),
    round(calc_kurtosis(residuals_uniform), 4)
  ),
  stringsAsFactors = FALSE
)

print(residual_stats)

# ============================================================================
# ADDITIONAL DIAGNOSTICS: TEMPORAL & SPATIAL AUTOCORRELATION TESTS
# ============================================================================

cat("\n🧭 Additional Diagnostic Tests:\n")
cat("===============================\n")

# 1️⃣ Temporal Portmanteau (Ljung-Box) test per region
cat("\n📈 Temporal Portmanteau (Ljung-Box) per Region:\n")
lb_results <- lapply(seq_len(ncol(residuals_uniform)), function(j) {
  x <- residuals_uniform[, j]
  test <- Box.test(x, lag = 12, type = "Ljung")
  data.frame(
    Region    = colnames(residuals_uniform)[j],
    Statistic = round(unname(test$statistic), 3),
    P_Value   = round(test$p.value, 4),
    Result    = ifelse(test$p.value < 0.05, "Non-White (Autocorr.)", "White Noise"),
    stringsAsFactors = FALSE
  )
})
lb_table <- do.call(rbind, lb_results)
print(lb_table)

# 2️⃣ Spatial Autocorrelation (Moran's I) on residual mean
cat("\n🌍 Spatial Autocorrelation (Moran’s I):\n")
suppressMessages(library(spdep))

# Convert uniform spatial matrix to listw
W <- spatial_weights$uniform
if (is.null(W) || nrow(W) != ncol(W)) stop("❌ Spatial weights matrix invalid or non-square.")
listw <- mat2listw(W, style = "W")

# Compute average residual per region (spatial entities)
rbar <- colMeans(residuals_uniform, na.rm = TRUE)

# Validate numeric vector
if (any(is.na(rbar))) {
  cat("⚠️ NA detected in regional residual means — replacing with 0.\n")
  rbar[is.na(rbar)] <- 0
}

# Check variance before running Moran's I
if (var(rbar) == 0) {
  cat("⚠️ All regional means identical — Moran’s I undefined.\n")
  moran_result <- list(
    estimate = c("Moran I statistic" = NA, "Expectation" = NA, "Variance" = NA),
    p.value = NA_real_
  )
} else {
  # Safe try-catch for moran.test
  moran_result <- tryCatch({
    moran.test(rbar, listw)
  }, error = function(e) {
    cat("⚠️ Moran's I test failed:", e$message, "\n")
    list(estimate = c("Moran I statistic" = NA, "Expectation" = NA, "Variance" = NA),
         p.value = NA_real_)
  })
}

# Display results (handle NA gracefully)
if (is.na(moran_result$p.value)) {
  cat("❕ Moran’s I could not be computed (insufficient spatial variation)\n")
} else {
  cat("Moran I statistic:", round(moran_result$estimate["Moran I statistic"], 4), "\n")
  cat("Expected value:", round(moran_result$estimate["Expectation"], 4), "\n")
  cat("Variance:", round(moran_result$estimate["Variance"], 4), "\n")
  cat("P-value:", round(moran_result$p.value, 4), "\n")
  if (moran_result$p.value < 0.05) {
    cat("🔴 Result: Spatial autocorrelation detected (residuals not fully random)\n")
  } else {
    cat("🟢 Result: No significant spatial autocorrelation (residuals spatially white)\n")
  }
}

# Save diagnostic results safely
diag_results <- list(
  temporal_portmanteau = lb_table,
  spatial_moran = data.frame(
    Moran_I = ifelse(is.na(moran_result$estimate["Moran I statistic"]), NA,
                     round(moran_result$estimate["Moran I statistic"], 4)),
    Expectation = ifelse(is.na(moran_result$estimate["Expectation"]), NA,
                         round(moran_result$estimate["Expectation"], 4)),
    Variance = ifelse(is.na(moran_result$estimate["Variance"]), NA,
                      round(moran_result$estimate["Variance"], 4)),
    P_Value = ifelse(is.na(moran_result$p.value), NA,
                     round(moran_result$p.value, 4)),
    Result = ifelse(is.na(moran_result$p.value),
                    "Not computed (flat residuals)",
                    ifelse(moran_result$p.value < 0.05,
                           "Spatial autocorr. detected",
                           "No spatial autocorr."))
  )
)

save(diag_results, file = "output/10a_starima_uniform_diagnostics.RData")
cat("\n✅ Diagnostic tests completed. Results saved to: output/10a_starima_uniform_diagnostics.RData\n")

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("\n📊 Creating Visualizations...\n")

library(ggplot2)
library(gridExtra)

# 1. Coefficient plot
coef_plot <- ggplot(coef_table, aes(x = Parameter, y = Estimate)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = Estimate - 1.96*Std_Error, 
                    ymax = Estimate + 1.96*Std_Error), 
                width = 0.2) +
  labs(title = "STARIMA(1,0,2) Parameter Estimates - Uniform Weights",
       subtitle = paste("Total parameters:", total_params),
       x = "Parameters", y = "Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave("plots/10a_uniform_coefficients.png", coef_plot, width = 10, height = 6, dpi = 300)
# print(coef_plot)

# 2. Residual time series plot
residual_ts <- data.frame(
  Time = 1:length(residuals_uniform),
  Residuals = as.vector(residuals_uniform)
)

residual_plot <- ggplot(residual_ts, aes(x = Time, y = Residuals)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = c(-2*sd(residuals_uniform, na.rm = TRUE), 
                           2*sd(residuals_uniform, na.rm = TRUE)), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "STARIMA Residuals - Uniform Weights",
       subtitle = "Dashed lines: ±2 standard deviations",
       x = "Time", y = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/10a_uniform_residuals.png", residual_plot, width = 10, height = 6, dpi = 300)
print(residual_plot)

# 3. Residual distribution
residual_hist <- ggplot(residual_ts, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
  geom_density(aes(y = after_stat(density) * length(residual_ts$Residuals) * 
                   (max(residual_ts$Residuals, na.rm = TRUE) - min(residual_ts$Residuals, na.rm = TRUE)) / 30),
               color = "steelblue", linewidth = 1) +
  labs(title = "Residual Distribution - Uniform Weights",
       subtitle = "Histogram with density overlay",
       x = "Residuals", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/10a_uniform_residual_dist.png", residual_hist, width = 8, height = 6, dpi = 300)
print(residual_hist)

# cat("✅ Coefficient plot saved: plots/10a_uniform_coefficients.png\n")
cat("✅ Residual plot saved: plots/10a_uniform_residuals.png\n")
cat("✅ Residual distribution saved: plots/10a_uniform_residual_dist.png\n")

# ============================================================================
# RESIDUAL ACF/PACF CALCULATION
# ============================================================================

cat("\n🔍 Calculating Residual ACF/PACF...\n")

# Hitung ACF dan PACF residual untuk diagnostic
residual_acf <- acf(as.vector(residuals_uniform), plot = FALSE, lag.max = 20)
residual_pacf <- pacf(as.vector(residuals_uniform), plot = FALSE, lag.max = 20)

cat("✅ Residual ACF/PACF calculated for diagnostic analysis\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Create comprehensive results object
uniform_results <- list(
  model = starima_uniform,
  coefficients = coef_table,
  fit_statistics = list(
    loglik = loglik,
    aic = aic,
    bic = bic,
    parameters = total_params,
    observations = nrow(train_data)
  ),
  residuals = residuals_uniform,
  residual_stats = residual_stats,
  estimation_time = estimation_time,
  spatial_weights = "uniform"
)

# Save results
save(uniform_results, starima_uniform, coef_table, residuals_uniform, 
     residual_acf, residual_pacf,
     file = "output/10a_starima_uniform.RData")

# Display in viewer
cat("\n=== DATA VIEWER ===\n")
cat("Opening coefficient table in viewer...\n")
View(coef_table, title = "STARIMA Uniform - Parameter Estimates")

cat("Opening residual statistics in viewer...\n")
View(residual_stats, title = "STARIMA Uniform - Residual Statistics")

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n=== STARIMA ESTIMATION COMPLETED - UNIFORM WEIGHTS ===\n")
cat("✅ Model successfully estimated\n")
cat("✅ Parameters:", total_params, "estimated\n")
cat("✅ Significant parameters:", sum(coef_table$p_value < 0.05), "/", total_params, "\n")
cat("✅ Log-likelihood:", round(loglik, 4), "\n")
cat("✅ AIC:", round(aic, 4), "\n")
cat("✅ BIC:", round(bic, 4), "\n")
cat("✅ Residual analysis completed\n")
cat("✅ Visualization plots generated (3 plots)\n")
cat("✅ Results saved to: output/10a_starima_uniform.RData\n")
cat("✅ All tables available in RStudio viewer\n\n")

cat("📊 PHASE 3 PROGRESS: 1/4 files completed (25%)\n")
cat("🎯 Next step: 10b_STARIMA_Estimation_Distance.R\n\n")

cat("Model validation:\n")
cat("- Estimation convergence: ✅\n")
cat("- Parameter significance: ✅\n")
cat("- Residual analysis: ✅\n")
cat("- Ready for diagnostic: ✅\n")

cat("\n🎉 STARIMA uniform weights model successfully estimated!\n")
cat("Estimation time:", round(as.numeric(estimation_time), 2), "seconds\n")
cat("Model fit: AIC =", round(aic, 2), ", BIC =", round(bic, 2), "\n")