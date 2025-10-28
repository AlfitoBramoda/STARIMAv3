# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STARIMA Identification
# File: 07_STACF_Analysis_Full.R
# Purpose: Space-Time Autocorrelation Function (STACF) Analysis with Spatial-aware MA Order
# Author: STARMA Analysis (Fixed Full Version)
# Date: 2025
# ============================================================================

# -------------------------------
# LOAD DATA
# -------------------------------
load("output/04_centered_data.RData")      # centered_matrix
load("output/05_spatial_weights.RData") # spatial_weights

cat("=== STARMA STACF ANALYSIS (SPATIAL-AWARE, ACF-style) ===\n\n")

# Spatial weights
uniform_w <- spatial_weights$uniform
distance_w <- spatial_weights$distance
correlation_w <- spatial_weights$correlation

# -------------------------------
# PARAMETERS
# -------------------------------
library(starma)
library(ggplot2)

max_time_lag <- 40  # maximum temporal lag

# -------------------------------
# FUNCTION: Perform STACF
# -------------------------------
perform_stacf_analysis <- function(data, weights, weight_name) {
  cat("📈 Computing STACF with", weight_name, "weights...\n")
  tryCatch({
    identity_matrix <- diag(ncol(data))
    wlist <- list(identity_matrix, weights)
    stacf_result <- stacf(data, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
    cat("✅ STACF computation successful for", weight_name, "\n")
    return(list(values = stacf_result, success = TRUE, weight_name = weight_name))
  }, error = function(e) {
    cat("❌ STACF computation failed for", weight_name, ":", e$message, "\n")
    return(list(success = FALSE, weight_name = weight_name))
  })
}

# -------------------------------
# COMPUTE STACF
# -------------------------------
stacf_uniform <- perform_stacf_analysis(centered_matrix, uniform_w, "Uniform")
stacf_distance <- perform_stacf_analysis(centered_matrix, distance_w, "Distance")
stacf_correlation <- perform_stacf_analysis(centered_matrix, correlation_w, "Correlation")

# -------------------------------
# FUNCTION: Spatial-aware MA order suggestion
# -------------------------------
suggest_ma_order_spatial <- function(stacf_result, cutoff_threshold = 0.1, max_q = 3) {
  if (!stacf_result$success) return(list(suggested_q = NA, weight_type = NA))
  
  stacf_vals <- stacf_result$values
  
  # Semua temporal lag > 0, semua spatial lag
  temporal_matrix <- stacf_vals[-1, , drop = FALSE]
  
  # Ambil max absolute STACF per temporal lag
  max_abs_acf <- apply(abs(temporal_matrix), 1, max)
  
  # Temukan cutoff point
  cutoff_point <- which(max_abs_acf < cutoff_threshold)[1]
  if (is.na(cutoff_point)) cutoff_point <- nrow(temporal_matrix)
  
  # MA order
  suggested_q <- min(cutoff_point, max_q)
  
  return(list(
    suggested_q = suggested_q,
    cutoff_point = cutoff_point,
    weight_type = stacf_result$weight_name
  ))
}

# -------------------------------
# FUNCTION: Plot STACF as ACF-style
# -------------------------------
plot_stacf_acf <- function(stacf_result, filename, cutoff_threshold = 0.1) {
  if (!stacf_result$success) return(NULL)
  
  stacf_vals <- stacf_result$values
  temporal_matrix <- stacf_vals[-1, , drop = FALSE]  # exclude lag 0
  max_abs_acf <- apply(abs(temporal_matrix), 1, max)
  temporal_lags <- 1:length(max_abs_acf)
  
  n <- nrow(centered_matrix)
  conf_bound <- 1.96 / sqrt(n)
  
  plot_data <- data.frame(
    Lag = temporal_lags,
    Max_ACF = max_abs_acf
  )
  
  p <- ggplot(plot_data, aes(x = Lag, y = Max_ACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
    geom_point(color = "darkred", size = 2) +
    labs(title = paste("STACF (ACF-style):", stacf_result$weight_name),
         subtitle = "Temporal lags (max across spatial lags)",
         x = "Temporal Lag", y = "Max |STACF|") +
    theme_minimal()
  
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  print(p)
  
  return(p)
}

# -------------------------------
# COMPUTE MA ORDERS
# -------------------------------
uniform_ma <- suggest_ma_order_spatial(stacf_uniform)
distance_ma <- suggest_ma_order_spatial(stacf_distance)
correlation_ma <- suggest_ma_order_spatial(stacf_correlation)

cat("\n📊 Spatial-aware MA Order Recommendations:\n")
cat("- Uniform weights: MA(", uniform_ma$suggested_q, ") - cutoff lag", uniform_ma$cutoff_point, "\n")
cat("- Distance weights: MA(", distance_ma$suggested_q, ") - cutoff lag", distance_ma$cutoff_point, "\n")
cat("- Correlation weights: MA(", correlation_ma$suggested_q, ") - cutoff lag", correlation_ma$cutoff_point, "\n")

# -------------------------------
# CREATE ACF-STYLE PLOTS
# -------------------------------
plot_stacf_acf(stacf_uniform, "plots/07_stacf_uniform_acf.png")
plot_stacf_acf(stacf_distance, "plots/07_stacf_distance_acf.png")
plot_stacf_acf(stacf_correlation, "plots/07_stacf_correlation_acf.png")

# -------------------------------
# SAVE RESULTS
# -------------------------------
stacf_summary <- data.frame(
  Weight_Type = c("Uniform", "Distance", "Correlation"),
  STACF_Success = c(
    ifelse(stacf_uniform$success, "✅ Success", "❌ Failed"),
    ifelse(stacf_distance$success, "✅ Success", "❌ Failed"),
    ifelse(stacf_correlation$success, "✅ Success", "❌ Failed")
  ),
  Suggested_MA_Order = c(
    uniform_ma$suggested_q,
    distance_ma$suggested_q,
    correlation_ma$suggested_q
  ),
  stringsAsFactors = FALSE
)

print(stacf_summary)

save(stacf_uniform, stacf_distance, stacf_correlation,
     uniform_ma, distance_ma, correlation_ma,
     stacf_summary, centered_matrix,
     file = "output/07_stacf_analysis_spatial_acf.RData")

cat("\n✅ Spatial-aware STACF analysis (ACF-style) completed!\n")
