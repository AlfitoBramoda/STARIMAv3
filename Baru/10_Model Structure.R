# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation
# File   : 09_Model_Structure.R
# Purpose: Create AR and MA mask matrices based on STACF/STPACF identification
# Author : STARMA Analysis
# Date   : 2024
# ============================================================================

cat("🚀 PHASE 3: STARIMA MODEL STRUCTURE DEFINITION STARTED...\n\n")

# ============================================================================
# GLOBAL PARAMETERS
# ============================================================================
MAX_SPATIAL_LAG <- 2
MAX_TEMPORAL_LAG <- 40

cat("📋 Global Parameters:\n")
cat("- MAX_SPATIAL_LAG:", MAX_SPATIAL_LAG, "\n")
cat("- MAX_TEMPORAL_LAG:", MAX_TEMPORAL_LAG, "\n\n")

# ============================================================================
# 1️⃣ LOAD REQUIRED DATA
# ============================================================================

load("output/08_stacf_analysis.RData")
load("output/09_stpacf_analysis.RData")
load("output/07_spatial_weights.RData")
load("output/05_differencing_results.RData")

# Detect differencing order
if (file.exists("output/05_differencing_results.RData")) {
  load("output/05_differencing_results.RData")
  if (exists("integration_order") && length(integration_order) == 1 && integration_order > 0) {
    d_order_from_diff <- integration_order
    cat("✅ Differencing detected: d_order =", d_order_from_diff, "\n")
  } else {
    cat("✅ integration_order =", integration_order, "\n")
    d_order_from_diff <- 1
    cat("ℹ️ No differencing detected. Default d_order =", d_order_from_diff, "\n")
  }
} else {
  d_order_from_diff <- 1
  cat("⚠️ Differencing file not found, assuming d_order =", d_order_from_diff, "\n")
}

# Prefer d_order from STPACF if available
if (exists("d_order") && !is.null(d_order)) {
  cat("✅ Using d_order from STPACF analysis:", d_order, "\n")
} else {
  d_order <- d_order_from_diff
  cat("✅ Using d_order from differencing results:", d_order, "\n")
}

cat("\n=== STARIMA MODEL STRUCTURE DEFINITION ===\n")

# ============================================================================
# 2️⃣ IDENTIFICATION SUMMARY
# ============================================================================

cat("\n📊 STACF/STPACF Identification Summary:\n")
cat("=====================================\n")

# --- Display identified AR/MA orders ---
cat("STACF Results (MA Order):\n")
if (exists("uniform_ma")) cat("- Uniform weights: MA(", uniform_ma$suggested_q, ")\n")
if (exists("distance_ma")) cat("- Distance weights: MA(", distance_ma$suggested_q, ")\n")
if (exists("correlation_ma")) cat("- Correlation weights: MA(", correlation_ma$suggested_q, ")\n")

cat("\nSTPACF Results (AR Order):\n")
if (exists("uniform_ar")) cat("- Uniform weights: AR(", uniform_ar$suggested_p, ")\n")
if (exists("distance_ar")) cat("- Distance weights: AR(", distance_ar$suggested_p, ")\n")
if (exists("correlation_ar")) cat("- Correlation weights: AR(", correlation_ar$suggested_p, ")\n")

cat("\n🎯 Identified STARIMA Models:\n")
if (exists("uniform_ar") && exists("uniform_ma"))
  cat("- Uniform weights: STARIMA(", uniform_ar$suggested_p, d_order, uniform_ma$suggested_q, ")\n")
if (exists("distance_ar") && exists("distance_ma"))
  cat("- Distance weights: STARIMA(", distance_ar$suggested_p, d_order, distance_ma$suggested_q, ")\n")
if (exists("correlation_ar") && exists("correlation_ma"))
  cat("- Correlation weights: STARIMA(", correlation_ar$suggested_p, d_order, correlation_ma$suggested_q, ")\n")

# ============================================================================
# 3️⃣ MODEL STRUCTURE PARAMETERS
# ============================================================================

n_regions <- 5
max_spatial_lag <- MAX_SPATIAL_LAG

# --- Select reference (uniform) ---
p_order <- if (exists("uniform_ar")) uniform_ar$suggested_p else 1
q_order <- if (exists("uniform_ma")) uniform_ma$suggested_q else 2

# Normalize and protect from negative / NA
p_order <- suppressWarnings(as.integer(p_order[1]))
q_order <- suppressWarnings(as.integer(q_order[1]))
if (is.na(p_order) || p_order < 0) p_order <- 0L
if (is.na(q_order) || q_order < 0) q_order <- 0L

D_order <- 1
s_period <- 12

cat("\n📋 Model Structure Parameters:\n")
cat("- Regions (N):", n_regions, "\n")
cat("- Max spatial lag (S):", max_spatial_lag, "\n")
cat("- AR order (p):", p_order, "\n")
cat("- Differencing (d):", d_order, "\n")
cat("- MA order (q):", q_order, "\n")
cat("- Seasonal (D):", D_order, " period =", s_period, "\n")
cat("- Spec: STARIMA(", p_order, ",", d_order, ",", q_order, ") × ( ,", D_order, ", )_", s_period, "\n\n")

# ============================================================================
# 4️⃣ MASK MATRIX CREATION
# ============================================================================

# ---- AR Mask ----
create_ar_mask <- function(p_order, max_spatial_lag) {
  if (p_order == 0) {
    m <- matrix(numeric(0), nrow = max_spatial_lag + 1L, ncol = 0L)
    rownames(m) <- paste("Spatial_Lag", 0:max_spatial_lag)
    return(m)
  }
  m <- matrix(1L, nrow = max_spatial_lag + 1L, ncol = p_order)
  rownames(m) <- paste("Spatial_Lag", 0:max_spatial_lag)
  colnames(m) <- paste("Temporal_Lag", seq_len(p_order))
  m
}

# ---- MA Mask ----
create_ma_mask <- function(q_order, max_spatial_lag) {
  if (q_order == 0) {
    m <- matrix(numeric(0), nrow = max_spatial_lag + 1L, ncol = 0L)
    rownames(m) <- paste("Spatial_Lag", 0:max_spatial_lag)
    return(m)
  }
  m <- matrix(1L, nrow = max_spatial_lag + 1L, ncol = q_order)
  rownames(m) <- paste("Spatial_Lag", 0:max_spatial_lag)
  colnames(m) <- paste("Temporal_Lag", seq_len(q_order))
  m
}

# --- Create both masks ---
ar_mask <- create_ar_mask(p_order, max_spatial_lag)
ma_mask <- create_ma_mask(q_order, max_spatial_lag)

cat("AR Mask Matrix:", paste(dim(ar_mask), collapse = " x "),
    "| Total parameters:", sum(ar_mask), "\n")
cat("MA Mask Matrix:", paste(dim(ma_mask), collapse = " x "),
    "| Total parameters:", sum(ma_mask), "\n")

# ============================================================================
# 5️⃣ PARAMETER STRUCTURE ANALYSIS
# ============================================================================

total_ar_params <- sum(ar_mask)
total_ma_params <- sum(ma_mask)
total_params <- total_ar_params + total_ma_params

cat("\n📊 Parameter Summary:\n")
cat("- AR params:", total_ar_params, "\n")
cat("- MA params:", total_ma_params, "\n")
cat("- Total:", total_params, "\n")

# ============================================================================
# 6️⃣ MODEL COMPLEXITY ASSESSMENT
# ============================================================================

n_observations <- 108
complexity_ratio <- if (total_params > 0) total_params / n_observations else 0
parsimony_score <- if (total_params > 0) n_observations / total_params else Inf

complexity_level <- if (complexity_ratio < 0.1) "LOW (Good)" else
  if (complexity_ratio < 0.2) "MODERATE (Acceptable)" else "HIGH (May overfit)"

df <- n_observations - total_params
df_assessment <- if (df > 50) "SUFFICIENT (Good)" else
  if (df > 20) "ADEQUATE (Acceptable)" else "LIMITED (Concerning)"

cat("\n🎯 Model Complexity:\n")
cat("- Ratio:", round(complexity_ratio, 4),
    "| Parsimony:", round(parsimony_score, 2),
    "| DF:", df, "→", df_assessment, "\n")
cat("- Level:", complexity_level, "\n")

# ============================================================================
# 7️⃣ VISUALIZATION
# ============================================================================

library(ggplot2)

create_mask_plot <- function(mask_matrix, title, filename) {
  if (ncol(mask_matrix) == 0) {
    p <- ggplot() +
      annotate("text", x = 1, y = 1, label = "No parameters (order = 0)", size = 5) +
      theme_void() +
      labs(title = paste("STARIMA Model Structure:", title),
           subtitle = "No temporal lags to estimate")
    return(p)
  }
  
  mask_df <- expand.grid(
    Spatial_Lag  = 0:(nrow(mask_matrix) - 1L),
    Temporal_Lag = seq_len(ncol(mask_matrix))
  )
  mask_df$Parameter <- as.vector(mask_matrix)
  mask_df$Estimated <- ifelse(mask_df$Parameter == 1, "Yes", "No")
  
  ggplot(mask_df, aes(x = Temporal_Lag, y = Spatial_Lag, fill = Estimated)) +
    geom_tile(color = "white", size = 1) +
    scale_fill_manual(values = c("No" = "lightgray", "Yes" = "darkblue")) +
    labs(title = paste("STARIMA Model Structure:", title),
         subtitle = paste("Total parameters:", sum(mask_matrix)),
         x = "Temporal Lag", y = "Spatial Lag") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq_len(ncol(mask_matrix))) +
    scale_y_continuous(breaks = 0:(nrow(mask_matrix) - 1L))
}

ar_plot <- create_mask_plot(ar_mask, "AR Mask Matrix", "plots/09_ar_mask_structure.png")
ma_plot <- create_mask_plot(ma_mask, "MA Mask Matrix", "plots/09_ma_mask_structure.png")

# ============================================================================
# 8️⃣ MODEL SPECIFICATION SUMMARY
# ============================================================================

model_specification <- list(
  model_type = paste0("STARIMA(", p_order, ",", d_order, ",", q_order, ")"),
  spatial_dimensions = list(n_regions = n_regions, max_spatial_lag = max_spatial_lag),
  temporal_dimensions = list(ar_order = p_order, integration_order = d_order, ma_order = q_order),
  parameter_structure = list(
    ar_mask = ar_mask, ma_mask = ma_mask,
    total_ar_params = total_ar_params, total_ma_params = total_ma_params,
    total_params = total_params
  ),
  complexity_metrics = list(
    parameter_ratio = complexity_ratio,
    parsimony_score = parsimony_score,
    degrees_of_freedom = df,
    complexity_level = complexity_level,
    df_assessment = df_assessment
  ),
  estimation_ready = TRUE
)

summary_table <- data.frame(
  Aspect = c("Model Type", "Regions", "Max Spatial Lag",
             "AR Order", "Integration Order", "MA Order",
             "AR Params", "MA Params", "Total Params",
             "Complexity Level", "DF Assessment", "Ready for Estimation"),
  Value = c(model_specification$model_type,
            n_regions, max_spatial_lag, p_order, d_order, q_order,
            total_ar_params, total_ma_params, total_params,
            complexity_level, df_assessment, "✅ YES"),
  stringsAsFactors = FALSE
)

# ============================================================================
# 9️⃣ SAVE RESULTS
# ============================================================================

save_path <- "output/10_model_structure.RData"
save(ar_mask, ma_mask, model_specification, summary_table,
     p_order, q_order, d_order, n_regions, max_spatial_lag,
     total_ar_params, total_ma_params, total_params,
     file = save_path)

cat("\n💾 Model structure saved to:", save_path, "\n")

if (interactive()) {
  View(summary_table, title = "STARIMA Model Specification Summary")
  View(ar_mask, title = "AR Mask Matrix")
  View(ma_mask, title = "MA Mask Matrix")
} else {
  cat("ℹ️ Non-interactive mode: skipping View() calls\n")
}

# ============================================================================
# 🔚 COMPLETION SUMMARY
# ============================================================================

cat("\n=== MODEL STRUCTURE DEFINITION COMPLETED ===\n")
cat("✅ AR mask created (", total_ar_params, ")\n")
cat("✅ MA mask created (", total_ma_params, ")\n")
cat("✅ Total parameters:", total_params, "\n")
cat("✅ Complexity level:", complexity_level, "\n")
cat("✅ Degrees of freedom:", df, "(", df_assessment, ")\n")
cat("✅ Ready for STARIMA estimation (Phase 4)\n")
cat("🎯 Next file → 11a_STARIMA_Estimation_Uniform.R\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
