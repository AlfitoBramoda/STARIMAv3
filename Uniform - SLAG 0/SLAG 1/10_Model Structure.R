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

cat("=== STARMA MODEL STRUCTURE DEFINITION (UNIFORM WEIGHTS - SLAG 1) ===\n\n")

# ============================================================================
# 🎯 CUSTOM SEASONAL STARIMA ORDERS - EDIT HERE ONLY!
# ============================================================================
n_regions <- 5           # Number of regions
max_spatial_lag <- 1     # Maximum spatial lag (SLAG 1 only)

# 🧪 CUSTOM NON-SEASONAL PARAMETERS (EDIT THESE!):
p_order <- 0             # Non-seasonal AR order (try: 1, 2, 3, 4)
d_order <- 0             # Non-seasonal differencing (usually 0 or 1)
q_order <- 0             # Non-seasonal MA order (try: 1, 2, 3)

# 🧪 CUSTOM SEASONAL PARAMETERS (EDIT THESE!):
P_order <- 0             # Seasonal AR order (try: 0, 1, 2)
D_order <- 1             # Seasonal differencing (keep at 1 for monthly data)
Q_order <- 1             # Seasonal MA order (try: 0, 1, 2)
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
create_ar_mask <- function(total_ar_lags, max_spatial_lag) {
  # 🎯 RESEARCH MODE: Handle ALL orders including 0 for real comparison
  if (is.null(total_ar_lags) || is.na(total_ar_lags)) total_ar_lags <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 1
  
  # For zero AR order - create minimal mask
  if (total_ar_lags == 0) {
    ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: AR total_lags = 0, mask = %dx%d, params = %d\n", 
               nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
    return(ar_mask)
  }
  
  # 🔥 SLAG 1 ONLY: Create mask with ONLY spatial lag 1
  ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = total_ar_lags)
  
  # 🔧 CRITICAL FIX: Activate specific lags based on orders - SLAG 1 ONLY
  # Non-seasonal AR lags (1, 2, 3, ...) - SLAG 1
  if (p_order > 0) {
    for (p in 1:p_order) {
      if (p <= total_ar_lags) {
        ar_mask[2, p] <- 1  # Non-seasonal AR lag p - SLAG 1
        cat(sprintf("✅ Activated non-seasonal AR lag %d (SLAG 1)\n", p))
      }
    }
  }
  
  # Seasonal AR lags (12, 24, 36, ...) - SLAG 1
  if (P_order > 0) {
    for (P in 1:P_order) {
      seasonal_lag <- P * seasonal_period
      if (seasonal_lag <= total_ar_lags) {
        ar_mask[2, seasonal_lag] <- 1  # Seasonal AR lag - SLAG 1
        cat(sprintf("✅ Activated seasonal AR lag %d (P=%d, SLAG 1)\n", seasonal_lag, P))
      }
    }
  }
  
  cat(sprintf("🔥 SLAG 1 ONLY AR mask: %dx%d, params = %d\n", 
             nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
  return(ar_mask)
}

create_ma_mask <- function(total_ma_lags, max_spatial_lag) {
  # 🎯 RESEARCH MODE: Handle ALL orders including 0 for real comparison
  if (is.null(total_ma_lags) || is.na(total_ma_lags)) total_ma_lags <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 1
  
  # For zero MA order - create minimal mask
  if (total_ma_lags == 0) {
    ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: MA total_lags = 0, mask = %dx%d, params = %d\n", 
               nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
    return(ma_mask)
  }
  
  # 🔥 SLAG 1 ONLY: Create mask with ONLY spatial lag 1
  ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = total_ma_lags)
  
  # 🔧 CRITICAL FIX: Activate specific lags based on orders - SLAG 1 ONLY
  # Non-seasonal MA lags (1, 2, 3, ...) - SLAG 1
  if (q_order > 0) {
    for (q in 1:q_order) {
      if (q <= total_ma_lags) {
        ma_mask[2, q] <- 1  # Non-seasonal MA lag q - SLAG 1
        cat(sprintf("✅ Activated non-seasonal MA lag %d (SLAG 1)\n", q))
      }
    }
  }
  
  # Seasonal MA lags (12, 24, 36, ...) - SLAG 1
  if (Q_order > 0) {
    for (Q in 1:Q_order) {
      seasonal_lag <- Q * seasonal_period
      if (seasonal_lag <= total_ma_lags) {
        ma_mask[2, seasonal_lag] <- 1  # Seasonal MA lag - SLAG 1
        cat(sprintf("✅ Activated seasonal MA lag %d (Q=%d, SLAG 1)\n", seasonal_lag, Q))
      }
    }
  }
  
  cat(sprintf("🔥 SLAG 1 ONLY MA mask: %dx%d, params = %d\n", 
             nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
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

# 🚫 AUTO-IDENTIFICATION DISABLED FOR CUSTOM ORDERS
# Uncomment below to use auto-identified orders instead of custom:
# if (file.exists("output/09_stpacf_uniform_only.RData")) {
#   load("output/09_stpacf_uniform_only.RData")
#   if (exists("uniform_ar") && !is.null(uniform_ar$suggested_p)) {
#     p_order <- uniform_ar$suggested_p
#   }
#   if (exists("uniform_ma") && !is.null(uniform_ma$suggested_q)) {
#     q_order <- uniform_ma$suggested_q
#   }
# }

cat("🎯 CUSTOM Seasonal STARIMA Configuration:\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("- Source: CUSTOM orders (auto-identification disabled)\n")

# Calculate seasonal STARIMA parameters with parsimonious approach
max_spatial_lag <- 1  # SLAG 1 only

# Create seasonal masks - combine non-seasonal and seasonal components - FIXED
total_ar_lags <- p_order  # Use exact non-seasonal order
total_ma_lags <- q_order  # Use exact non-seasonal order

# Add seasonal lags if specified - CRITICAL FIX
if (P_order > 0) total_ar_lags <- max(total_ar_lags, P_order * seasonal_period)
if (Q_order > 0) total_ma_lags <- max(total_ma_lags, Q_order * seasonal_period)

# 🔧 CRITICAL FIX: Ensure minimum lags for seasonal models
if (Q_order > 0 && total_ma_lags < seasonal_period) {
  total_ma_lags <- seasonal_period  # Need at least seasonal_period lags for seasonal MA
  cat(sprintf("🔧 FIXED: MA lags increased to %d for seasonal MA(%d)\n", total_ma_lags, Q_order))
}
if (P_order > 0 && total_ar_lags < seasonal_period) {
  total_ar_lags <- seasonal_period  # Need at least seasonal_period lags for seasonal AR
  cat(sprintf("🔧 FIXED: AR lags increased to %d for seasonal AR(%d)\n", total_ar_lags, P_order))
}

# Ensure minimum dimensions
if (total_ar_lags == 0) total_ar_lags <- 1
if (total_ma_lags == 0) total_ma_lags <- 1

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
parsimony_score <- n_observations / total_params
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

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(model_structures, plots, file = "output/10_model_structure_uniform_weights.RData")

cat("\n✅ Seasonal STARIMA model structure for uniform weights saved to 'output/10_model_structure_uniform_weights.RData'\n")
cat(sprintf("✅ Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("✅ Seasonal integration orders (p,d,q,P,D,Q,s) added to model\n")
cat("🎯 Ready for seasonal STARIMA estimation for uniform weight type\n")