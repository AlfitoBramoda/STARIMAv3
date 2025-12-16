# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation (Uniform Final)
# File: 10_Model_Structure_Uniform_Final.R
# Purpose: Create AR/MA mask matrices for uniform weight type + plotting ACF-style
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load required data
load("output/08_stacf_uniform_95only.RData")
load("output/09_stpacf_uniform_95only.RData")
load("output/07_spatial_weights_uniform.RData")
load("output/05_differencing_results.RData")

library(ggplot2)

cat("=== STARMA MODEL STRUCTURE DEFINITION (UNIFORM WEIGHTS) ===\n\n")

# ============================================================================
# 🎯 CUSTOM SEASONAL STARIMA ORDERS - EDIT HERE ONLY!
# ============================================================================
n_regions <- 5           # Number of regions
max_spatial_lag <- 2     # Maximum spatial lag (from spatial weights)

# 🧪 CUSTOM NON-SEASONAL PARAMETERS (EDIT THESE!):
p_order <- 0             # Non-seasonal AR order (try: 1, 2, 3, 4)
d_order <- 0             # Non-seasonal differencing (usually 0 or 1)
q_order <- 1             # Non-seasonal MA order (try: 1, 2, 3)

# 🧪 CUSTOM SEASONAL PARAMETERS (EDIT THESE!):
P_order <- 1             # Seasonal AR order (try: 0, 1, 2)
D_order <- 1             # Seasonal differencing (keep at 1 for monthly data)
Q_order <- 0             # Seasonal MA order (try: 0, 1, 2)
seasonal_period <- 12    # Seasonal period (keep at 12 for monthly)

n_observations <- 96     # Jumlah observasi training

# 📊 Popular combinations to try:
# STARIMA(1,0,1)×(0,1,0)12 - Non-seasonal only
# STARIMA(1,0,1)×(1,1,1)12 - Simple seasonal  
# STARIMA(2,0,2)×(1,1,1)12 - Balanced with seasonal
# STARIMA(1,0,2)×(2,1,1)12 - Complex seasonal AR
# STARIMA(3,0,1)×(1,1,2)12 - Complex seasonal MA

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
create_ar_mask <- function(p_order, max_spatial_lag) {
  # 🎯 RESEARCH MODE: Handle ALL orders including 0 for real comparison
  if (is.null(p_order) || is.na(p_order)) p_order <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 0
  
  # For zero AR order - create minimal mask
  if (p_order == 0) {
    ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: AR order = 0, mask = %dx%d, params = %d\n", 
               nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
    return(ar_mask)
  }
  
  # For non-zero AR order - create proper mask
  ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = p_order)
  
  # Activate temporal lags only (no spatial for simplicity)
  for (p in 1:p_order) {
    ar_mask[1, p] <- 1  # Temporal lag p, spatial lag 0
  }
  
  cat(sprintf("🔬 RESEARCH: AR order = %d, mask = %dx%d, params = %d\n", 
             p_order, nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
  return(ar_mask)
}

create_ma_mask <- function(q_order, max_spatial_lag) {
  # 🎯 RESEARCH MODE: Handle ALL orders including 0 for real comparison
  if (is.null(q_order) || is.na(q_order)) q_order <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 0
  
  # For zero MA order - create minimal mask
  if (q_order == 0) {
    ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: MA order = 0, mask = %dx%d, params = %d\n", 
               nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
    return(ma_mask)
  }
  
  # For non-zero MA order - create proper mask
  ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = q_order)
  
  # Activate temporal lags only (no spatial for simplicity)
  for (q in 1:q_order) {
    ma_mask[1, q] <- 1  # Temporal lag q, spatial lag 0
  }
  
  cat(sprintf("🔬 RESEARCH: MA order = %d, mask = %dx%d, params = %d\n", 
             q_order, nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
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

cat("🎯 CUSTOM Seasonal STARIMA Configuration:\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("- Source: CUSTOM orders (auto-identification disabled)\n")

# 🎯 RESEARCH MODE: Respect exact orders for real comparison
max_spatial_lag <- 0  # Disable spatial for cleaner comparison

# Calculate exact temporal lags needed
total_ar_lags <- p_order  # Use exact non-seasonal order
total_ma_lags <- q_order  # Use exact non-seasonal order

# Add seasonal lags if specified
if (P_order > 0) total_ar_lags <- max(total_ar_lags, P_order * seasonal_period)
if (Q_order > 0) total_ma_lags <- max(total_ma_lags, Q_order * seasonal_period)

cat("🔬 RESEARCH MODE: Exact order handling\n")
cat(sprintf("- Input orders: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order))
cat(sprintf("- Calculated temporal lags: AR=%d, MA=%d\n", total_ar_lags, total_ma_lags))

# Use the fixed mask creation functions
ar_mask <- create_ar_mask(total_ar_lags, max_spatial_lag)
ma_mask <- create_ma_mask(total_ma_lags, max_spatial_lag)

cat("- Non-seasonal AR order (p):", p_order, "\n")
cat("- Non-seasonal MA order (q):", q_order, "\n")
cat("- Seasonal AR order (P):", P_order, "\n")
cat("- Seasonal MA order (Q):", Q_order, "\n")

total_ar_params <- sum(ar_mask)
total_ma_params <- sum(ma_mask)
total_params <- total_ar_params + total_ma_params
complexity_ratio <- total_params / n_observations
parsimony_score <- if(total_params > 0) n_observations / total_params else Inf
df <- n_observations - total_params
complexity_level <- if (complexity_ratio < 0.1) "LOW" else if (complexity_ratio < 0.2) "MODERATE" else "HIGH"
df_assessment <- if (df > 50) "SUFFICIENT" else if (df > 20) "ADEQUATE" else "LIMITED"

# ===== Seasonal Integration Info =====
integration_info <- list(
  p = p_order,
  d = d_order,
  q = q_order,
  P = P_order,
  D = D_order,
  Q = Q_order,
  s = seasonal_period,
  differencing_type = if (D_order > 0) "SEASONAL" else if (d_order > 0) "NON-SEASONAL" else "NONE",
  model_type = sprintf("STARIMA(%d,%d,%d) × (%d,%d,%d)%d", 
                      p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period)
)

# Simpan struktur model dengan seasonal components
model_structures[[weight_type]] <- list(
  ar_mask = ar_mask,
  ma_mask = ma_mask,
  # Non-seasonal orders
  ar_order = p_order,
  d_order = d_order,  # FIXED: Add missing d_order
  ma_order = q_order,
  # Seasonal orders
  seasonal_ar_order = P_order,
  seasonal_d_order = D_order,  # FIXED: Add missing D_order
  seasonal_ma_order = Q_order,
  seasonal_period = seasonal_period,
  # Parameter counts
  total_ar_params = total_ar_params,
  total_ma_params = total_ma_params,
  total_params = total_params,
  # Model assessment
  complexity_ratio = complexity_ratio,
  parsimony_score = parsimony_score,
  df = df,
  complexity_level = complexity_level,
  df_assessment = df_assessment,
  # Integration info
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

# 🔍 DEBUG: Print detailed mask information
cat("\n🔍 DETAILED MASK DEBUG:\n")
cat("======================\n")
cat(sprintf("Input orders: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order))
cat(sprintf("Calculated total lags: AR=%d, MA=%d\n", total_ar_lags, total_ma_lags))
cat(sprintf("AR mask dimensions: %dx%d\n", nrow(ar_mask), ncol(ar_mask)))
cat(sprintf("MA mask dimensions: %dx%d\n", nrow(ma_mask), ncol(ma_mask)))
cat("AR mask content:\n")
print(ar_mask)
cat("MA mask content:\n")
print(ma_mask)
cat(sprintf("Active AR parameters: %d\n", sum(ar_mask)))
cat(sprintf("Active MA parameters: %d\n", sum(ma_mask)))
cat(sprintf("Total active parameters: %d\n", sum(ar_mask) + sum(ma_mask)))
cat("======================\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(model_structures, plots, file = "output/10_model_structure_uniform_weights.RData")

cat("\n💾 RESULTS SAVED:\n")
cat("================\n")
cat("✅ Model structures saved to: output/10_model_structure_uniform_weights.RData\n")
cat("✅ All orders properly saved:\n")
cat(sprintf("   • p_order = %d (saved as ar_order)\n", p_order))
cat(sprintf("   • d_order = %d (saved as d_order)\n", d_order))
cat(sprintf("   • q_order = %d (saved as ma_order)\n", q_order))
cat(sprintf("   • P_order = %d (saved as seasonal_ar_order)\n", P_order))
cat(sprintf("   • D_order = %d (saved as seasonal_d_order)\n", D_order))
cat(sprintf("   • Q_order = %d (saved as seasonal_ma_order)\n", Q_order))
cat(sprintf("   • seasonal_period = %d\n", seasonal_period))
cat("✅ Masks and integration info included\n")

cat("\n=== MODEL STRUCTURE DEFINITION COMPLETED ===\n")
cat(sprintf("🎯 Final Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("📊 Next step: 11_STARIMA_Estimation.R\n")