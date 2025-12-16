# ============================================================================
# STARMA STACF Analysis with Seasonal MA Support
# File: STACF_Full_Seasonal.R
# Purpose: Compute STACF and suggest MA & Seasonal MA orders
# ============================================================================

library(starma)
library(ggplot2)

cat("=== STARMA STACF Analysis (support seasonal MA) ===\n\n")

# Load data
load("output/05_differencing_results.RData")  # differenced_matrix
load("output/07_spatial_weights_distance.RData")  # spatial_weights

distance_w <- spatial_weights$distance
n_regions <- ncol(differenced_matrix)
max_time_lag <- 40
seasonal_period <- 12  # monthly data

# Spatial weights setup
identity_matrix <- diag(n_regions)
wlist <- list(identity_matrix, distance_w)
for (k in 2:length(wlist)) {
  for (i in 1:nrow(wlist[[k]])) {
    rs <- sum(wlist[[k]][i, ])
    if (rs > 0) wlist[[k]][i, ] <- wlist[[k]][i, ] / rs
  }
}

# Compute STACF
stacf_distance <- stacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
cat("✅ STACF computation successful for distance weights\n")

# Confidence bounds
n <- nrow(differenced_matrix)
conf_bound_95 <- 1.96 / sqrt(n)

# ----------------------------
# Function to suggest MA & seasonal MA
# ----------------------------
suggest_ma_order_seasonal <- function(stacf_result, s, conf_bound, max_q = 6, max_Q = 2) {
  slag0_acf <- stacf_result[-1, 1]  # remove lag 0
  slag1_acf <- stacf_result[-1, 2]
  
  # Non-seasonal MA order (q)
  cutoff_slag0 <- max_q
  for (i in 1:length(slag0_acf)) {
    if (abs(slag0_acf[i]) < conf_bound) {
      cutoff_slag0 <- i - 1
      break
    }
  }
  cutoff_slag1 <- max_q
  for (i in 1:length(slag1_acf)) {
    if (abs(slag1_acf[i]) < conf_bound) {
      cutoff_slag1 <- i - 1
      break
    }
  }
  suggested_q <- min(max(cutoff_slag0, cutoff_slag1, 1), max_q)
  
  # Seasonal MA order (Q)
  seasonal_lags <- s * (1:max_Q)
  seasonal_slag0 <- sapply(seasonal_lags, function(lag) if(lag <= length(slag0_acf)) slag0_acf[lag] else 0)
  seasonal_slag1 <- sapply(seasonal_lags, function(lag) if(lag <= length(slag1_acf)) slag1_acf[lag] else 0)
  suggested_Q <- max(sum(abs(seasonal_slag0) > conf_bound),
                     sum(abs(seasonal_slag1) > conf_bound))
  
  return(list(
    suggested_q = suggested_q,
    suggested_Q = suggested_Q,
    cutoff_slag0 = cutoff_slag0,
    cutoff_slag1 = cutoff_slag1,
    significant_nonseasonal_slag0 = which(abs(slag0_acf) > conf_bound),
    significant_nonseasonal_slag1 = which(abs(slag1_acf) > conf_bound),
    significant_seasonal_slag0 = which(abs(seasonal_slag0) > conf_bound),
    significant_seasonal_slag1 = which(abs(seasonal_slag1) > conf_bound)
  ))
}

# Compute suggested MA & seasonal MA
ma_suggestion <- suggest_ma_order_seasonal(stacf_distance, seasonal_period, conf_bound_95)

# Create AR suggestion for file 09 compatibility
ar_suggestion <- list(
  suggested_p = 1,  # Default non-seasonal AR
  suggested_P = 1   # Default seasonal AR
)

cat("🎯 Suggested MA Orders:\n")
cat("- Non-seasonal MA (q):", ma_suggestion$suggested_q, "\n")
cat("- Seasonal MA (Q):", ma_suggestion$suggested_Q, "\n")
cat("- Cutoff lag SL0:", ma_suggestion$cutoff_slag0, "\n")
cat("- Cutoff lag SL1:", ma_suggestion$cutoff_slag1, "\n")

cat("\n🎯 Default AR Orders (for compatibility):\n")
cat("- Non-seasonal AR (p):", ar_suggestion$suggested_p, "\n")
cat("- Seasonal AR (P):", ar_suggestion$suggested_P, "\n")

cat("\n🔍 Significant lags (95% CI):\n")
cat("- Non-seasonal SL0:", ma_suggestion$significant_nonseasonal_slag0, "\n")
cat("- Non-seasonal SL1:", ma_suggestion$significant_nonseasonal_slag1, "\n")
cat("- Seasonal SL0:", ma_suggestion$significant_seasonal_slag0, "\n")
cat("- Seasonal SL1:", ma_suggestion$significant_seasonal_slag1, "\n")

# Save results
save(stacf_distance, ma_suggestion, ar_suggestion, differenced_matrix, conf_bound_95,
     file = "output/08_stacf_distance_95only_seasonal.RData")

cat("\n✅ STACF analysis with seasonal MA completed!\n")
