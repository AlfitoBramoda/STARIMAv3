# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation (Correlation Final)
# File: 10_Model_Structure_Correlation_Final.R
# Purpose: Create AR/MA mask matrices for correlation weight type + plotting ACF-style
# Author: STARMA Analysis - Correlation Focus
# Date: 2024
# ============================================================================

# Load required data
load("output/08_stacf_correlation_only.RData")
load("output/09_stpacf_correlation_only.RData")
load("output/07_spatial_weights_correlation.RData")
load("output/05_differencing_results.RData")

library(ggplot2)

cat("=== STARMA MODEL STRUCTURE DEFINITION (CORRELATION WEIGHTS) ===\n\n")

# Configuration
n_regions <- 5           # Number of regions
max_spatial_lag <- 2     # Maximum spatial lag (from spatial weights)
d_order <- 1             # Non-seasonal differencing
D_order <- 1             # Seasonal differencing applied
seasonal_period <- 12    # Seasonal period (e.g., 12 months)
n_observations <- 96     # Number of training observations

# Helper functions
create_ar_mask <- function(p_order, max_spatial_lag) {
  if (is.null(p_order) || is.na(p_order) || p_order <= 0) p_order <- 3
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 0
  ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = p_order)
  if (p_order >= 1) ar_mask[, 1:p_order] <- 1
  if (nrow(ar_mask) == 0 || ncol(ar_mask) == 0) ar_mask <- matrix(1, nrow = 1, ncol = 1)
  return(ar_mask)
}

create_ma_mask <- function(q_order, max_spatial_lag) {
  if (is.null(q_order) || is.na(q_order) || q_order <= 0) q_order <- 3
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 0
  ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = q_order)
  if (q_order >= 1) ma_mask[, 1:q_order] <- 1
  if (nrow(ma_mask) == 0 || ncol(ma_mask) == 0) ma_mask <- matrix(1, nrow = 1, ncol = 1)
  return(ma_mask)
}

# Process correlation weights
weight_type <- "correlation"
model_structures <- list()
plots <- list()

cat("\n📊 Processing weight type:", weight_type, "\n")

# Load identification results for correct orders
if (file.exists("output/09_stpacf_correlation_only.RData")) {
  load("output/09_stpacf_correlation_only.RData")
  p_order <- correlation_ar$suggested_p      # from STPACF
  q_order <- correlation_ma$suggested_q      # from STACF
} else {
  p_order <- 3; q_order <- 3  # Use STARIMA(3,1,3) as default
}

# Calculate parameters with correct orders
max_spatial_lag <- 2
ar_mask <- matrix(1, nrow = max_spatial_lag + 1, ncol = p_order)
ma_mask <- matrix(1, nrow = max_spatial_lag + 1, ncol = q_order)

cat("- AR order (p):", p_order, "\n")
cat("- MA order (q):", q_order, "\n")

ar_mask <- create_ar_mask(p_order, max_spatial_lag)
ma_mask <- create_ma_mask(q_order, max_spatial_lag)

total_ar_params <- sum(ar_mask)
total_ma_params <- sum(ma_mask)
total_params <- total_ar_params + total_ma_params
complexity_ratio <- total_params / n_observations
parsimony_score <- n_observations / total_params
df <- n_observations - total_params
complexity_level <- if (complexity_ratio < 0.1) "LOW" else if (complexity_ratio < 0.2) "MODERATE" else "HIGH"
df_assessment <- if (df > 50) "SUFFICIENT" else if (df > 20) "ADEQUATE" else "LIMITED"

# Integration info
integration_info <- list(
  d = d_order,
  D = D_order,
  seasonal_period = seasonal_period,
  differencing_type = if (D_order > 0) "SEASONAL" else if (d_order > 0) "NON-SEASONAL" else "NONE"
)

# Save model structure
model_structures[[weight_type]] <- list(
  ar_mask = ar_mask,
  ma_mask = ma_mask,
  ar_order = p_order,
  ma_order = q_order,
  total_ar_params = total_ar_params,
  total_ma_params = total_ma_params,
  total_params = total_params,
  complexity_ratio = complexity_ratio,
  parsimony_score = parsimony_score,
  df = df,
  complexity_level = complexity_level,
  df_assessment = df_assessment,
  integration_order = integration_info
)

cat("- Total parameters:", total_params, "\n")
cat("- Complexity level:", complexity_level, "\n")
cat("- Degrees of freedom:", df, "(", df_assessment, ")\n")

# Save results
save(model_structures, plots, file = "output/10_model_structure_correlation_weights.RData")

cat("\n✅ STARIMA model structure for correlation weights saved to 'output/10_model_structure_correlation_weights.RData'\n")
cat("✅ Integration orders (d, D) added to model\n")
cat("🎯 Ready for STARIMA estimation for correlation weight type\n")
cat("🔗 Using correlation-based spatial relationships\n")