# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STPACF (distance Only) - FIXED
# File: 09_STPACF_FIXED.R
# Purpose: Space-Time Partial ACF Analysis - CORRECTED
# ============================================================================

# Load data
load("output/05_differencing_results.RData")
load("output/07_spatial_weights_idw.RData")
load("output/08_stacf_distance_only.RData")  # Use fixed STACF results

cat("=== STARMA STPACF ANALYSIS (distance only) - FIXED ===\n")

library(starma)
library(ggplot2)

distance_w <- W
max_time_lag <- 40

cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(differenced_matrix), "\n")
cat("- Regions:", paste(colnames(differenced_matrix), collapse = ", "), "\n\n")

# FIXED: Proper spatial weights setup (same as STACF)
identity_matrix <- diag(ncol(differenced_matrix))
wlist <- list(identity_matrix, distance_w)

# Row normalize spatial weights properly
for (k in 2:length(wlist)) {
  for (i in 1:nrow(wlist[[k]])) {
    rs <- sum(wlist[[k]][i, ])
    if (rs > 0) wlist[[k]][i, ] <- wlist[[k]][i, ] / rs
  }
}

# Compute STPACF
cat("📈 Computing STPACF with distance weights...\n")
stpacf_distance <- stpacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
cat("✅ STPACF computation successful\n")

# FIXED: Enhanced plotting with both spatial lags
temporal_lags <- 1:nrow(stpacf_distance)
n <- nrow(differenced_matrix)
conf_bound_95 <- 1.96 / sqrt(n)
conf_bound_99 <- 2.58 / sqrt(n)

# Plot for Spatial Lag 0 (Identity) - Within-region PACF
pacf_slag0_data <- data.frame(
  Lag = temporal_lags,
  PACF = stpacf_distance[, 1]
)

p_pacf_slag0 <- ggplot(pacf_slag0_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95), linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_hline(yintercept = c(conf_bound_99, -conf_bound_99), linetype = "solid", color = "red", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "STPACF: distance Weights - Spatial Lag 0 (Within-Region)",
       subtitle = paste("Blue: 95% CI, Red: 99% CI, n =", n),
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# Plot for Spatial Lag 1 (distance weights) - Between-region PACF
pacf_slag1_data <- data.frame(
  Lag = temporal_lags,
  PACF = stpacf_distance[, 2]
)

p_pacf_slag1 <- ggplot(pacf_slag1_data, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95), linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_hline(yintercept = c(conf_bound_99, -conf_bound_99), linetype = "solid", color = "red", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STPACF: distance Weights - Spatial Lag 1 (Between-Region)",
       subtitle = paste("Blue: 95% CI, Red: 99% CI, n =", n),
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# Save plots
ggsave("plots/09_stpacf_distance_slag0_fixed.png", p_pacf_slag0, width = 10, height = 6, dpi = 300)
ggsave("plots/09_stpacf_distance_slag1_fixed.png", p_pacf_slag1, width = 10, height = 6, dpi = 300)

print(p_pacf_slag0)
print(p_pacf_slag1)

# FIXED: Better AR order suggestion
suggest_ar_order_fixed <- function(stpacf_result, conf_bound, max_p = 6) {
  # Check both spatial lags for AR order
  slag0_pacf <- stpacf_result[, 1]  # Within-region
  slag1_pacf <- stpacf_result[, 2]  # Between-region
  
  # Find cutoff for spatial lag 0 (primary)
  cutoff_slag0 <- max_p
  for (i in 1:length(slag0_pacf)) {
    if (abs(slag0_pacf[i]) < conf_bound) {
      cutoff_slag0 <- i - 1
      break
    }
  }
  
  # Find cutoff for spatial lag 1 (secondary)
  cutoff_slag1 <- max_p
  for (i in 1:length(slag1_pacf)) {
    if (abs(slag1_pacf[i]) < conf_bound) {
      cutoff_slag1 <- i - 1
      break
    }
  }
  
  # Take the maximum of both (conservative approach)
  suggested_p <- min(max(cutoff_slag0, cutoff_slag1, 1), max_p)
  
  return(list(
    suggested_p = suggested_p,
    cutoff_slag0 = cutoff_slag0,
    cutoff_slag1 = cutoff_slag1,
    significant_lags_slag0 = which(abs(slag0_pacf) > conf_bound),
    significant_lags_slag1 = which(abs(slag1_pacf) > conf_bound)
  ))
}

# Get AR order suggestions using 99% confidence
distance_ar <- suggest_ar_order_fixed(stpacf_distance, conf_bound_99)

cat("\n🎯 FIXED AR Order Analysis:\n")
cat("- Spatial Lag 0 cutoff at temporal lag:", distance_ar$cutoff_slag0, "\n")
cat("- Spatial Lag 1 cutoff at temporal lag:", distance_ar$cutoff_slag1, "\n")
cat("- Suggested AR order:", distance_ar$suggested_p, "\n")

cat("\n🔍 Significant PACF lags (99% confidence):\n")
cat("- Spatial Lag 0:", if(length(distance_ar$significant_lags_slag0) > 0) paste(distance_ar$significant_lags_slag0, collapse = ", ") else "None", "\n")
cat("- Spatial Lag 1:", if(length(distance_ar$significant_lags_slag1) > 0) paste(distance_ar$significant_lags_slag1, collapse = ", ") else "None", "\n")

# Differencing order (from previous step)
d_order <- 1

# FIXED: Model recommendation
cat("\n🎯 FIXED Model Recommendation:\n")
cat("=====================================\n")
cat("STARIMA(", distance_ar$suggested_p, ", ", d_order, ", ", distance_ma$suggested_q, ")\n", sep = "")
cat("\nModel interpretation:\n")
cat("- p =", distance_ar$suggested_p, ": AR order (temporal dependence)\n")
cat("- d =", d_order, ": Differencing order (stationarity)\n") 
cat("- q =", distance_ma$suggested_q, ": MA order (error correction)\n")

# Create summary
stpacf_summary_fixed <- data.frame(
  Weight_Type = "distance",
  AR_Order_Slag0 = distance_ar$cutoff_slag0,
  AR_Order_Slag1 = distance_ar$cutoff_slag1,
  Suggested_AR = distance_ar$suggested_p,
  Suggested_MA = distance_ma$suggested_q,
  Differencing = d_order,
  Final_Model = paste0("STARIMA(", distance_ar$suggested_p, ",", d_order, ",", distance_ma$suggested_q, ")"),
  Confidence_Level = "99%",
  stringsAsFactors = FALSE
)

print(stpacf_summary_fixed)

# Save results
save(stpacf_distance, distance_ar, distance_ma, d_order, stpacf_summary_fixed,
     conf_bound_95, conf_bound_99,
     file = "output/09_stpacf_distance_only.RData")

cat("\n✅ FIXED STPACF analysis completed!\n")
cat("📊 Key improvements:\n")
cat("- Both spatial lags (0 and 1) analyzed and displayed\n")
cat("- Proper confidence bounds (95% and 99%)\n")
cat("- Conservative AR order selection\n")
cat("- Clear model interpretation\n")
cat("- Enhanced significance testing\n")