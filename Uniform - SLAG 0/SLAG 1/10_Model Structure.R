# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation (Uniform SLAG 1)
# File: 10_Model_Structure_Uniform_SLAG1.R
# Purpose: Create AR/MA mask matrices for uniform weight type + SLAG 1
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load required data with error handling
if (file.exists("output/08_stacf_uniform_only.RData")) {
  load("output/08_stacf_uniform_only.RData")
  cat("✅ STACF data loaded\n")
} else {
  cat("⚠️ STACF data not found - proceeding without it\n")
}

if (file.exists("output/07_spatial_weights_uniform.RData")) {
  load("output/07_spatial_weights_uniform.RData")
  cat("✅ Spatial weights loaded\n")
} else {
  cat("⚠️ Spatial weights not found - creating default\n")
  n_regions <- 5
  spatial_weights <- list(
    uniform = matrix(0.25, nrow = n_regions, ncol = n_regions)
  )
  diag(spatial_weights$uniform) <- 0
}

if (file.exists("output/05_differencing_results.RData")) {
  load("output/05_differencing_results.RData")
  cat("✅ Differencing results loaded\n")
} else {
  cat("⚠️ Differencing results not found - proceeding without it\n")
}

library(ggplot2)

cat("=== STARMA MODEL STRUCTURE DEFINITION (UNIFORM WEIGHTS - SLAG 1) ===\n\n")

# ============================================================================
# 🎯 CUSTOM SEASONAL STARIMA ORDERS - EDIT HERE ONLY!
# ============================================================================
n_regions <- 5           # Number of regions
max_spatial_lag <- 1     # Maximum spatial lag (SLAG 1)

# 🧪 CUSTOM NON-SEASONAL PARAMETERS (EDIT THESE!):
p_order <- 0             # Non-seasonal AR order (try: 1, 2, 3, 4)
d_order <- 0             # Non-seasonal differencing (usually 0 or 1)
q_order <- 1             # Non-seasonal MA order (try: 1, 2, 3)

# 🧪 CUSTOM SEASONAL PARAMETERS (EDIT THESE!):
P_order <- 0             # Seasonal AR order (try: 0, 1, 2)
D_order <- 1             # Seasonal differencing (keep at 1 for monthly data)
Q_order <- 1             # Seasonal MA order (try: 0, 1, 2)
seasonal_period <- 12    # Seasonal period (keep at 12 for monthly)

n_observations <- 96     # Jumlah observasi training

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
create_ar_mask <- function(total_ar_lags, max_spatial_lag) {
  if (is.null(total_ar_lags) || is.na(total_ar_lags)) total_ar_lags <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 1
  
  if (total_ar_lags == 0) {
    ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: AR total_lags = 0, mask = %dx%d, params = %d\n", 
               nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
    return(ar_mask)
  }
  
  ar_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = total_ar_lags)
  
  # Non-seasonal AR lags
  if (p_order > 0) {
    for (p in 1:p_order) {
      if (p <= total_ar_lags) {
        ar_mask[1, p] <- 1
        cat(sprintf("✅ Activated non-seasonal AR lag %d\n", p))
      }
    }
  }
  
  # Seasonal AR lags
  if (P_order > 0) {
    for (P in 1:P_order) {
      seasonal_lag <- P * seasonal_period
      if (seasonal_lag <= total_ar_lags) {
        ar_mask[1, seasonal_lag] <- 1
        cat(sprintf("✅ Activated seasonal AR lag %d (P=%d)\n", seasonal_lag, P))
      }
    }
  }
  
  # SLAG 1: Add spatial lag 1 components
  if (max_spatial_lag >= 1) {
    if (p_order > 0) {
      ar_mask[2, 1] <- 1
      cat("✅ Activated SLAG 1 AR component\n")
    }
    if (P_order > 0 && seasonal_period <= total_ar_lags) {
      ar_mask[2, seasonal_period] <- 1
      cat("✅ Activated SLAG 1 seasonal AR component\n")
    }
  }
  
  cat(sprintf("🔬 RESEARCH: AR total_lags = %d, mask = %dx%d, params = %d\n", 
             total_ar_lags, nrow(ar_mask), ncol(ar_mask), sum(ar_mask)))
  return(ar_mask)
}

create_ma_mask <- function(total_ma_lags, max_spatial_lag) {
  if (is.null(total_ma_lags) || is.na(total_ma_lags)) total_ma_lags <- 0
  if (is.null(max_spatial_lag) || is.na(max_spatial_lag) || max_spatial_lag < 0) max_spatial_lag <- 1
  
  if (total_ma_lags == 0) {
    ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = 1)
    cat(sprintf("🔬 RESEARCH: MA total_lags = 0, mask = %dx%d, params = %d\n", 
               nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
    return(ma_mask)
  }
  
  ma_mask <- matrix(0, nrow = max_spatial_lag + 1, ncol = total_ma_lags)
  
  # Non-seasonal MA lags
  if (q_order > 0) {
    for (q in 1:q_order) {
      if (q <= total_ma_lags) {
        ma_mask[1, q] <- 1
        cat(sprintf("✅ Activated non-seasonal MA lag %d\n", q))
      }
    }
  }
  
  # Seasonal MA lags
  if (Q_order > 0) {
    for (Q in 1:Q_order) {
      seasonal_lag <- Q * seasonal_period
      if (seasonal_lag <= total_ma_lags) {
        ma_mask[1, seasonal_lag] <- 1
        cat(sprintf("✅ Activated seasonal MA lag %d (Q=%d)\n", seasonal_lag, Q))
      }
    }
  }
  
  # SLAG 1: Add spatial lag 1 components
  if (max_spatial_lag >= 1) {
    if (q_order > 0) {
      ma_mask[2, 1] <- 1
      cat("✅ Activated SLAG 1 MA component\n")
    }
    if (Q_order > 0 && seasonal_period <= total_ma_lags) {
      ma_mask[2, seasonal_period] <- 1
      cat("✅ Activated SLAG 1 seasonal MA component\n")
    }
  }
  
  cat(sprintf("🔬 RESEARCH: MA total_lags = %d, mask = %dx%d, params = %d\n", 
             total_ma_lags, nrow(ma_mask), ncol(ma_mask), sum(ma_mask)))
  return(ma_mask)
}

# ============================================================================
# PROCESS UNIFORM WEIGHTS - SLAG 1
# ============================================================================
weight_type <- "uniform"
model_structures <- list()
plots <- list()

cat("\n📊 Processing weight type:", weight_type, "- SLAG 1\n")

cat("🎯 CUSTOM Seasonal STARIMA Configuration:\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("- Source: CUSTOM orders (auto-identification disabled)\n")
cat("- Spatial Lag: 1 (includes neighbor effects)\n")

# Calculate exact temporal lags needed
total_ar_lags <- p_order
total_ma_lags <- q_order

# Add seasonal lags if specified
if (P_order > 0) total_ar_lags <- max(total_ar_lags, P_order * seasonal_period)
if (Q_order > 0) total_ma_lags <- max(total_ma_lags, Q_order * seasonal_period)

# Ensure minimum lags for seasonal models
if (Q_order > 0 && total_ma_lags < seasonal_period) {
  total_ma_lags <- seasonal_period
  cat(sprintf("🔧 FIXED: MA lags increased to %d for seasonal MA(%d)\n", total_ma_lags, Q_order))
}
if (P_order > 0 && total_ar_lags < seasonal_period) {
  total_ar_lags <- seasonal_period
  cat(sprintf("🔧 FIXED: AR lags increased to %d for seasonal AR(%d)\n", total_ar_lags, P_order))
}

cat("🔬 RESEARCH MODE: Exact order handling\n")
cat(sprintf("- Input orders: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order))
cat(sprintf("- Calculated temporal lags: AR=%d, MA=%d\n", total_ar_lags, total_ma_lags))

# Create masks
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

# Seasonal Integration Info
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

# Save model structure
model_structures[[weight_type]] <- list(
  ar_mask = ar_mask,
  ma_mask = ma_mask,
  ar_order = p_order,
  d_order = d_order,
  ma_order = q_order,
  seasonal_ar_order = P_order,
  seasonal_d_order = D_order,
  seasonal_ma_order = Q_order,
  seasonal_period = seasonal_period,
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

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(model_structures, file = "output/10_model_structure_uniform_weights.RData")

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

cat("\n=== MODEL STRUCTURE DEFINITION COMPLETED (SLAG 1) ===\n")
cat(sprintf("🎯 Final Model: STARIMA(%d,%d,%d) × (%d,%d,%d)%d\n", 
           p_order, d_order, q_order, P_order, D_order, Q_order, seasonal_period))
cat("📊 Next step: 11_STARIMA_Estimation.R\n")