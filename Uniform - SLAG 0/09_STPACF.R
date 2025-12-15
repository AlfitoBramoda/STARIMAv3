# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STPACF (Uniform Only) - 95% CI Only
# File: 09_STPACF_NO99CI.R
# Purpose: Space-Time Partial ACF Analysis - 95% Confidence Interval Only
# ============================================================================

# Load data
load("output/05_differencing_results.RData")
load("output/07_spatial_weights_uniform.RData")
load("output/08_stacf_uniform_95only.RData")  # Use STACF results with 95% CI only

cat("=== STARMA STPACF ANALYSIS (Uniform only) - 95% CI Only ===\n")

library(starma)
library(ggplot2)

uniform_w <- spatial_weights$uniform
max_time_lag <- 40

cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(differenced_matrix), "\n")
cat("- Regions:", paste(colnames(differenced_matrix), collapse = ", "), "\n\n")

# Proper spatial weights setup (same as STACF)
identity_matrix <- diag(ncol(differenced_matrix))
wlist <- list(identity_matrix, uniform_w)

# Row normalize spatial weights
for (k in 2:length(wlist)) {
  for (i in 1:nrow(wlist[[k]])) {
    rs <- sum(wlist[[k]][i, ])
    if (rs > 0) wlist[[k]][i, ] <- wlist[[k]][i, ] / rs
  }
}

# Compute STPACF
cat("📈 Computing STPACF with Uniform weights...\n")
stpacf_uniform <- stpacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
cat("✅ STPACF computation successful\n")

# Plotting setup (95% only)
temporal_lags <- 1:nrow(stpacf_uniform)
n <- nrow(differenced_matrix)
conf_bound_95 <- 1.96 / sqrt(n)

# --- Spatial Lag 0 ---
pacf_slag0_data <- data.frame(
  Lag = temporal_lags,
  PACF = stpacf_uniform[, 1]
)

p_pacf_slag0 <- ggplot(pacf_slag0_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95),
             linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "STPACF: Uniform Weights - Spatial Lag 0",
       subtitle = paste("Blue: 95% CI only, n =", n),
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# --- Spatial Lag 1 ---
pacf_slag1_data <- data.frame(
  Lag = temporal_lags,
  PACF = stpacf_uniform[, 2]
)

p_pacf_slag1 <- ggplot(pacf_slag1_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95),
             linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STPACF: Uniform Weights - Spatial Lag 1",
       subtitle = paste("Blue: 95% CI only, n =", n),
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# Save plots
ggsave("plots/09_stpacf_uniform_slag0_95only.png", p_pacf_slag0, width = 10, height = 6, dpi = 300)
ggsave("plots/09_stpacf_uniform_slag1_95only.png", p_pacf_slag1, width = 10, height = 6, dpi = 300)

print(p_pacf_slag0)
print(p_pacf_slag1)

# --- Seasonal AR order suggestion (95% only) ---
suggest_seasonal_ar_order_fixed <- function(stpacf_result, conf_bound, max_p = 6, seasonal_period = 12) {
  slag0_pacf <- stpacf_result[, 1]
  slag1_pacf <- stpacf_result[, 2]
  
  # Non-seasonal AR order
  cutoff_slag0 <- max_p
  for (i in 1:length(slag0_pacf)) {
    if (abs(slag0_pacf[i]) < conf_bound) {
      cutoff_slag0 <- i - 1
      break
    }
  }
  
  cutoff_slag1 <- max_p
  for (i in 1:length(slag1_pacf)) {
    if (abs(slag1_pacf[i]) < conf_bound) {
      cutoff_slag1 <- i - 1
      break
    }
  }
  
  suggested_p <- min(max(cutoff_slag0, cutoff_slag1, 1), max_p)
  
  # Seasonal AR order - check seasonal lags
  seasonal_lags <- seq(seasonal_period, length(slag0_pacf), by = seasonal_period)
  seasonal_significant_slag0 <- any(abs(slag0_pacf[seasonal_lags]) > conf_bound, na.rm = TRUE)
  seasonal_significant_slag1 <- any(abs(slag1_pacf[seasonal_lags]) > conf_bound, na.rm = TRUE)
  suggested_P <- if (seasonal_significant_slag0 || seasonal_significant_slag1) 1 else 0
  
  return(list(
    suggested_p = suggested_p,
    suggested_P = suggested_P,
    cutoff_slag0 = cutoff_slag0,
    cutoff_slag1 = cutoff_slag1,
    seasonal_significant_slag0 = seasonal_significant_slag0,
    seasonal_significant_slag1 = seasonal_significant_slag1,
    significant_lags_slag0 = which(abs(slag0_pacf) > conf_bound),
    significant_lags_slag1 = which(abs(slag1_pacf) > conf_bound)
  ))
}

# Get seasonal AR order suggestions (95% CI)
uniform_ar <- suggest_seasonal_ar_order_fixed(stpacf_uniform, conf_bound_95, seasonal_period = 12)

cat("\n🎯 Seasonal AR Order Analysis (95% confidence):\n")
cat("- Spatial Lag 0 cutoff at temporal lag:", uniform_ar$cutoff_slag0, "\n")
cat("- Spatial Lag 1 cutoff at temporal lag:", uniform_ar$cutoff_slag1, "\n")
cat("- Non-seasonal AR order (p):", uniform_ar$suggested_p, "\n")
cat("- Seasonal AR order (P):", uniform_ar$suggested_P, "\n")
cat("- Seasonal significance (Slag0):", uniform_ar$seasonal_significant_slag0, "\n")
cat("- Seasonal significance (Slag1):", uniform_ar$seasonal_significant_slag1, "\n")

cat("\n🔍 Significant PACF lags (95% confidence):\n")
cat("- Spatial Lag 0:", if(length(uniform_ar$significant_lags_slag0) > 0) paste(uniform_ar$significant_lags_slag0, collapse = ", ") else "None", "\n")
cat("- Spatial Lag 1:", if(length(uniform_ar$significant_lags_slag1) > 0) paste(uniform_ar$significant_lags_slag1, collapse = ", ") else "None", "\n")

# Model recommendation
d_order <- 1

cat("\n🎯 Seasonal Model Recommendation:\n")
cat("=====================================\n")
cat(sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)12\n", 
           uniform_ar$suggested_p, d_order, uniform_ma$suggested_q,
           uniform_ar$suggested_P, 1, uniform_ma$suggested_Q))
cat("\nModel interpretation:\n")
cat("- p =", uniform_ar$suggested_p, ": Non-seasonal AR order\n")
cat("- d =", d_order, ": Non-seasonal differencing\n") 
cat("- q =", uniform_ma$suggested_q, ": Non-seasonal MA order\n")
cat("- P =", uniform_ar$suggested_P, ": Seasonal AR order\n")
cat("- D = 1: Seasonal differencing\n")
cat("- Q =", uniform_ma$suggested_Q, ": Seasonal MA order\n")

# Create summary
stpacf_summary_fixed <- data.frame(
  Weight_Type = "Uniform",
  AR_Order_Slag0 = uniform_ar$cutoff_slag0,
  AR_Order_Slag1 = uniform_ar$cutoff_slag1,
  Suggested_AR = uniform_ar$suggested_p,
  Suggested_Seasonal_AR = uniform_ar$suggested_P,
  Suggested_MA = uniform_ma$suggested_q,
  Suggested_Seasonal_MA = uniform_ma$suggested_Q,
  Differencing = d_order,
  Seasonal_Differencing = 1,
  Final_Model = sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)12", 
                       uniform_ar$suggested_p, d_order, uniform_ma$suggested_q,
                       uniform_ar$suggested_P, 1, uniform_ma$suggested_Q),
  Confidence_Level = "95%",
  stringsAsFactors = FALSE
)

print(stpacf_summary_fixed)

# Add seasonal period info
seasonal_info <- list(
  seasonal_period = 12,
  model_type = sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)12", 
                      uniform_ar$suggested_p, d_order, uniform_ma$suggested_q,
                      uniform_ar$suggested_P, 1, uniform_ma$suggested_Q)
)

# Save results
save(stpacf_uniform, uniform_ar, uniform_ma, d_order, stpacf_summary_fixed,
     conf_bound_95, seasonal_info,
     file = "output/09_stpacf_uniform_95only.RData")

cat("\n✅ Seasonal STPACF analysis completed (95% CI only)!\n")
cat(sprintf("✅ Suggested seasonal AR: (%d,%d) for (p,P)\n", uniform_ar$suggested_p, uniform_ar$suggested_P))
cat("📊 Changes:\n")
cat("- Removed 99% (red) confidence interval lines\n")
cat("- Only 95% (blue dashed) bounds displayed\n")
cat("- Added seasonal AR order identification\n")
cat("- Updated significance testing and outputs accordingly\n")
