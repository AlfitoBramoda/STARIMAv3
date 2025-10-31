# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation (Uniform Final)
# File: 10_Model_Structure_Uniform_Final.R
# Purpose: Create AR/MA mask matrices for uniform weight type + plotting ACF-style
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load required data
load("output/08_stacf_uniform_only.RData")
load("output/09_stpacf_uniform_only.RData")
load("output/07_spatial_weights_uniform.RData")
load("output/05_differencing_results.RData")

library(ggplot2)

cat("=== STARMA MODEL STRUCTURE DEFINITION (UNIFORM WEIGHTS) ===\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================
n_regions <- 5           # Number of regions
max_spatial_lag <- 2     # Maximum spatial lag (from spatial weights)
d_order <- 1             # Non-seasonal differencing
D_order <- 1             # Seasonal differencing applied
seasonal_period <- 12    # Seasonal period (e.g., 12 months)
n_observations <- 96     # Jumlah observasi training

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
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

create_mask_plot <- function(mask_matrix, title) {
  if (is.null(mask_matrix) || length(mask_matrix) == 0 ||
      nrow(mask_matrix) == 0 || ncol(mask_matrix) == 0) {
    cat("⚠️ Warning: Empty mask matrix for", title, "\n")
    return(list(plot = NULL, df = NULL))
  }
  mask_df <- expand.grid(
    Spatial_Lag = 0:(nrow(mask_matrix)-1),
    Temporal_Lag = 1:ncol(mask_matrix)
  )
  param_vector <- as.vector(t(mask_matrix))
  if (length(param_vector) != nrow(mask_df)) {
    cat("⚠️ Warning: Parameter vector length mismatch in", title, "\n")
    param_vector <- rep(0, nrow(mask_df))
  }
  mask_df$Parameter <- param_vector
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

# ============================================================================
# PROCESS UNIFORM WEIGHTS
# ============================================================================
weight_type <- "uniform"
model_structures <- list()
plots <- list()

cat("\n📊 Processing weight type:", weight_type, "\n")

ar_obj <- NULL
ma_obj <- NULL

tryCatch({
  ar_obj <- get(paste0(weight_type, "_ar"), envir = .GlobalEnv)
}, error = function(e) {
  cat("⚠️ Warning:", paste0(weight_type, "_ar"), "not found, using default\n")
})

tryCatch({
  ma_obj <- get(paste0(weight_type, "_ma"), envir = .GlobalEnv)
}, error = function(e) {
  cat("⚠️ Warning:", paste0(weight_type, "_ma"), "not found, using default\n")
})

# Load identification results for correct orders
if (file.exists("output/09_stpacf_uniform_only.RData")) {
  load("output/09_stpacf_uniform_only.RData")
  p_order <- uniform_ar$suggested_p      # from STPACF
  q_order <- uniform_ma$suggested_q      # from STACF
} else {
  p_order <- 3; q_order <- 3  # Use STARIMA(3,1,3) as default
}

# Calculate parameters with correct orders
max_spatial_lag <- 2
ar_mask <- matrix(1, nrow = max_spatial_lag + 1, ncol = p_order)  # 3x3
ma_mask <- matrix(1, nrow = max_spatial_lag + 1, ncol = q_order)  # 3x3

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

# ===== Integration Info =====
integration_info <- list(
  d = d_order,
  D = D_order,
  seasonal_period = seasonal_period,
  differencing_type = if (D_order > 0) "SEASONAL" else if (d_order > 0) "NON-SEASONAL" else "NONE"
)

# Simpan struktur model
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

# Buat plot mask
ar_plot_result <- create_mask_plot(ar_mask, paste0(weight_type, " AR Mask"))
ma_plot_result <- create_mask_plot(ma_mask, paste0(weight_type, " MA Mask"))
plots[[paste0(weight_type, "_AR")]] <- ar_plot_result
plots[[paste0(weight_type, "_MA")]] <- ma_plot_result

cat("- Total parameters:", total_params, "\n")
cat("- Complexity level:", complexity_level, "\n")
cat("- Degrees of freedom:", df, "(", df_assessment, ")\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(model_structures, plots, file = "output/10_model_structure_uniform_weights.RData")

cat("\n✅ STARIMA model structure for uniform weights saved to 'output/10_model_structure_uniform_weights.RData'\n")
cat("✅ Integration orders (d, D) added to model\n")
cat("🎯 Ready for STARIMA estimation for uniform weight type\n")