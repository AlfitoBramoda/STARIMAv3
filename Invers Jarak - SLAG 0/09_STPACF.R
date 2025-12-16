# ============================================================================
# STARMA Full Pipeline - STARIMA with Seasonal AR/MA + Spatial Lags
# File: STARIMA_Full_Pipeline_With_STPACF.R
# Purpose: Create mask matrices for STARIMA using STPACF (p, P) suggestions
# Author: STARMA Analysis
# Date: 2025
# ============================================================================

library(ggplot2)

cat("=== STARMA FULL STARIMA PIPELINE (with Seasonal AR/MA & Spatial Lag) ===\n")

# ----------------------------
# CONFIGURATION
# ----------------------------
n_regions <- 5
max_spatial_lag <- 2
d_order <- 0
D_order <- 1
seasonal_period <- 12
n_observations <- 96

# Load STPACF results with seasonal AR suggestion
load("output/08_stacf_distance_95only_seasonal.RData")  # ar_suggestion

# Example: MA order default (bisa dari STACF)
q <- 0
Q <- 0
s <- seasonal_period

# ----------------------------
# HELPER FUNCTIONS: Create full AR/MA masks
# ----------------------------
create_full_ar_mask <- function(p_order, P_order, max_spatial_lag) {
  # Handle zero orders
  if (p_order == 0 && P_order == 0) {
    ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    return(ar_mask)
  }
  
  n_col <- max(p_order + P_order, 1)  # Ensure at least 1 column
  ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = n_col)
  
  # Non-seasonal AR
  if(p_order >= 1) ar_mask[1, 1:p_order] <- 1
  # Seasonal AR
  if(P_order >= 1 && n_col >= (p_order + P_order)) {
    ar_mask[1, (p_order + 1):(p_order + P_order)] <- 1
  }
  # Spatial lag 1 - only if we have columns
  if(max_spatial_lag >= 1 && ncol(ar_mask) >= 1) {
    if(p_order >= 1) ar_mask[2, 1] <- 1
    if(P_order >= 1 && ncol(ar_mask) >= (p_order + 1)) {
      ar_mask[2, (p_order + 1)] <- 1
    }
  }
  return(ar_mask)
}

create_full_ma_mask <- function(q_order, Q_order, max_spatial_lag) {
  # Handle zero orders
  if (q_order == 0 && Q_order == 0) {
    ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    return(ma_mask)
  }
  
  n_col <- max(q_order + Q_order, 1)  # Ensure at least 1 column
  ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = n_col)
  
  # Non-seasonal MA
  if(q_order >= 1) ma_mask[1, 1:q_order] <- 1
  # Seasonal MA
  if(Q_order >= 1 && n_col >= (q_order + Q_order)) {
    ma_mask[1, (q_order + 1):(q_order + Q_order)] <- 1
  }
  # Spatial lag 1 - only if we have columns
  if(max_spatial_lag >= 1 && ncol(ma_mask) >= 1) {
    if(q_order >= 1) ma_mask[2, 1] <- 1
    if(Q_order >= 1 && ncol(ma_mask) >= (q_order + 1)) {
      ma_mask[2, (q_order + 1)] <- 1
    }
  }
  return(ma_mask)
}

create_mask_plot <- function(mask_matrix, title) {
  mask_df <- expand.grid(
    Spatial_Lag = 0:(nrow(mask_matrix)-1),
    Temporal_Lag = 1:ncol(mask_matrix)
  )
  mask_df$Parameter <- as.vector(t(mask_matrix))
  mask_df$Estimated <- ifelse(mask_df$Parameter == 1, "Yes", "No")
  
  p <- ggplot(mask_df, aes(x = Temporal_Lag, y = Spatial_Lag, fill = Estimated)) +
    geom_tile(color = "white", size = 1) +
    scale_fill_manual(values = c("No" = "lightgray", "Yes" = "darkblue")) +
    labs(title = title, x = "Temporal Lag", y = "Spatial Lag",
         subtitle = paste("Total parameters:", sum(mask_matrix))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 1:ncol(mask_matrix)) +
    scale_y_continuous(breaks = 0:(nrow(mask_matrix)-1))
  
  return(list(plot = p, df = mask_df))
}

# ----------------------------
# CREATE MASKS
# ----------------------------
p <- ar_suggestion$suggested_p
P <- ar_suggestion$suggested_P

ar_mask_full <- create_full_ar_mask(p, P, max_spatial_lag)
ma_mask_full <- create_full_ma_mask(q, Q, max_spatial_lag)

# ----------------------------
# PLOT MASKS
# ----------------------------
ar_plot <- create_mask_plot(ar_mask_full, "STARIMA AR Mask (with Seasonal AR)")
ma_plot <- create_mask_plot(ma_mask_full, "STARIMA MA Mask (with Seasonal MA)")

# ----------------------------
# MODEL STRUCTURE INFO
# ----------------------------
total_ar_params <- sum(ar_mask_full)
total_ma_params <- sum(ma_mask_full)
total_params <- total_ar_params + total_ma_params
complexity_ratio <- total_params / n_observations
parsimony_score <- n_observations / total_params
df <- n_observations - total_params
complexity_level <- if(complexity_ratio < 0.1) "LOW" else if(complexity_ratio < 0.2) "MODERATE" else "HIGH"
df_assessment <- if(df > 50) "SUFFICIENT" else if(df > 20) "ADEQUATE" else "LIMITED"

integration_info <- list(
  d = d_order, D = D_order, seasonal_period = s,
  differencing_type = if(D_order > 0) "SEASONAL" else if(d_order > 0) "NON-SEASONAL" else "NONE"
)

model_structures <- list(
  ar_mask = ar_mask_full,
  ma_mask = ma_mask_full,
  ar_order = p, ma_order = q,
  seasonal_ar_order = P, seasonal_ma_order = Q,
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

# ----------------------------
# SAVE RESULTS
# ----------------------------
plots <- list(AR = ar_plot, MA = ma_plot)
save(model_structures, plots, file = "output/STARIMA_Full_Model_Structure_With_STPACF.RData")

cat("\n✅ Full STARIMA model structure saved!\n")
cat("🎯 Includes AR/MA musiman, differencing, and spatial lags\n")
