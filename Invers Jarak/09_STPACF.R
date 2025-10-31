# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STARIMA Identification (Distance Only)
# File: 09_STPACF_Analysis_Distance.R
# Purpose: Space-Time Partial Autocorrelation Function (STPACF) Analysis
#          for AR order identification using Distance weights only
# Author: STARMA Analysis (Simplified)
# Date: 2025
# ============================================================================

# -------------------------------
# LOAD REQUIRED DATA
# -------------------------------
load("output/05_differencing_results.RData")       # differenced_matrix
load("output/07_spatial_weights_distance.RData")   # spatial_weights
load("output/08_stacf_distance_only.RData")        # distance_ma (from previous step)

cat("=== STARMA STPACF ANALYSIS (Distance only) ===\n")
cat("Space-Time Partial Autocorrelation Function Analysis for AR order identification...\n\n")

# -------------------------------
# DATA INFO
# -------------------------------
cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(differenced_matrix), "\n")
cat("- Number of regions:", ncol(differenced_matrix), "\n")
cat("- Time periods:", nrow(differenced_matrix), "\n")
cat("- Regions:", paste(colnames(differenced_matrix), collapse = ", "), "\n\n")

# Get distance weights
distance_w <- spatial_weights$distance

# -------------------------------
# PARAMETERS
# -------------------------------
library(starma)
library(ggplot2)

max_time_lag <- 40
max_space_lag <- 2

cat("🔍 STPACF Parameters:\n")
cat("- Maximum temporal lag:", max_time_lag, "\n")
cat("- Maximum spatial lag:", max_space_lag, "\n\n")

# -------------------------------
# STPACF COMPUTATION
# -------------------------------
cat("📈 Computing STPACF with Distance weights...\n")

tryCatch({
  identity_matrix <- diag(ncol(differenced_matrix))
  wlist <- list(identity_matrix, distance_w)
  
  stpacf_distance <- stpacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
  
  cat("✅ STPACF computation successful (Distance weights)\n")
}, error = function(e) {
  cat("❌ Error computing STPACF:", e$message, "\n")
})

# -------------------------------
# PACF-STYLE PLOT (like ARIMA)
# -------------------------------
cat("\n📊 Creating PACF-style STPACF plot (Distance weights)...\n")

temporal_pacf <- stpacf_distance[, 1]  # Spatial lag 0
n <- nrow(differenced_matrix)
conf_bound <- 0.1

pacf_df <- data.frame(
  Lag = 1:length(temporal_pacf),
  PACF = temporal_pacf
)

p <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), color = "blue", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STPACF (PACF-style): Distance Weights",
       subtitle = paste("Temporal lags (spatial lag 0) - n =", n),
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

ggsave("plots/09_stpacf_distance_pacf.png", p, width = 10, height = 6, dpi = 300)
print(p)
cat("✅ PACF-style plot saved: plots/09_stpacf_distance_pacf.png\n")

# ============================================================================
# AR ORDER RECOMMENDATION (Distance Only)
# ============================================================================

cat("\n=== AR ORDER IDENTIFICATION (Distance Only) ===\n")

# Function to suggest AR order based on STPACF cutoff
suggest_ar_order <- function(analysis) {
  if (is.null(analysis)) return(NULL)
  
  temporal_pacf <- analysis$temporal_pacf
  cutoff_threshold <- 0.1
  cutoff_point <- 0
  
  for (i in 1:length(temporal_pacf)) {
    if (abs(temporal_pacf[i]) < cutoff_threshold) {
      cutoff_point <- i - 1
      break
    }
  }
  
  suggested_p <- min(cutoff_point, 3)
  
  return(list(
    suggested_p = suggested_p,
    cutoff_point = cutoff_point,
    weight_type = analysis$weight_type
  ))
}

# Get AR order suggestions (only distance)
distance_analysis <- list(
  temporal_pacf = stpacf_distance[, 1],   # spatial lag 0
  weight_type = "Distance"
)
distance_ar <- suggest_ar_order(distance_analysis)

# Because monthly differencing has been applied earlier
d_order <- 1

# ============================================================================
# DISPLAY PROPOSED MODEL
# ============================================================================
cat("\n🎯 Proposed STARIMA Model (Distance Weights Only):\n")
cat("STARIMA(", distance_ar$suggested_p, ", ", d_order, ", ", distance_ma$suggested_q, ")\n", sep = "")

# ============================================================================
# SAVE RESULTS (Distance Only)
# ============================================================================
stpacf_summary <- data.frame(
  Weight_Type = "Distance",
  STPACF_Success = "✅ Success",
  Suggested_AR_Order = paste0("AR(", distance_ar$suggested_p, ")"),
  Suggested_MA_Order = paste0("MA(", distance_ma$suggested_q, ")"),
  Differencing_d = d_order,
  Proposed_Model = paste0("STARIMA(", distance_ar$suggested_p, ",", d_order, ",", distance_ma$suggested_q, ")"),
  stringsAsFactors = FALSE
)

print(stpacf_summary)

save(stpacf_distance,
     distance_analysis,
     distance_ar,
     distance_ma,
     d_order,
     stpacf_summary,
     file = "output/09_stpacf_distance_analysis.RData")

cat("\n✅ STPACF analysis completed for Distance weights only.\n")
cat("✅ Differencing order (d) set to 1 based on monthly differencing step.\n")
cat("✅ Proposed model: STARIMA(", distance_ar$suggested_p, ", ", d_order, ", ", distance_ma$suggested_q, ")\n", sep = "")
cat("✅ Results saved to output/09_stpacf_distance_analysis.RData\n\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n=== SUMMARY (Distance only) ===\n")
cat("- Suggested AR order:", distance_ar$suggested_p, "\n")
cat("- Suggested MA order (from STACF):", distance_ma$suggested_q, "\n")
cat("\n🎯 Proposed STARIMA model: STARIMA(", distance_ar$suggested_p, ", ", d_order, ", ", distance_ma$suggested_q, ")\n", sep = "")

save(stpacf_distance, distance_ar, distance_ma, differenced_matrix,
     file = "output/09_stpacf_distance_only.RData")

cat("\n✅ STPACF analysis (Distance only) completed successfully!\n")
cat("✅ Results saved: output/09_stpacf_distance_only.RData\n")
cat("📁 Ready for Phase 3: STARIMA Estimation\n")