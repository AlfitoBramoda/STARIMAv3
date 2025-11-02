# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STARIMA Identification (Correlation Only)
# File: 09_STPACF_Analysis_Correlation.R
# Purpose: Space-Time Partial Autocorrelation Function (STPACF) Analysis
#          for AR order identification using Correlation weights only
# Author: STARMA Analysis - Correlation Focus
# Date: 2024
# ============================================================================

# Load required data
load("output/05_differencing_results.RData")       # differenced_matrix
load("output/07_spatial_weights_correlation.RData")  # spatial_weights
load("output/08_stacf_correlation_only.RData")       # correlation_ma (from previous step)

cat("=== STARMA STPACF ANALYSIS (Correlation only) ===\n")
cat("Space-Time Partial Autocorrelation Function Analysis for AR order identification...\n\n")

# Data info
cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(differenced_matrix), "\n")
cat("- Number of regions:", ncol(differenced_matrix), "\n")
cat("- Time periods:", nrow(differenced_matrix), "\n")
cat("- Regions:", paste(colnames(differenced_matrix), collapse = ", "), "\n\n")

# Get correlation weights
correlation_w <- spatial_weights$correlation

# Parameters
library(starma)
library(ggplot2)

max_time_lag <- 40
max_space_lag <- 2

cat("🔍 STPACF Parameters:\n")
cat("- Maximum temporal lag:", max_time_lag, "\n")
cat("- Maximum spatial lag:", max_space_lag, "\n\n")

# STPACF computation
cat("📈 Computing STPACF with Correlation weights...\n")

tryCatch({
  identity_matrix <- diag(ncol(differenced_matrix))
  wlist <- list(identity_matrix, correlation_w)
  
  stpacf_correlation <- stpacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
  
  cat("✅ STPACF computation successful (Correlation weights)\n")
}, error = function(e) {
  cat("❌ Error computing STPACF:", e$message, "\n")
})

# PACF-style plot (SEPARATE PLOTS FOR EACH SPATIAL LAG)
cat("\n📊 Creating PACF-style STPACF plots (Correlation weights)...\n")
library(gridExtra)

n <- nrow(differenced_matrix)
conf_bound <- 1.96 / sqrt(n)

# Plot 1: Spatial Lag 0 (Within-region effects)
temporal_pacf_slag0 <- stpacf_correlation[, 1]
pacf_df_slag0 <- data.frame(
  Lag = 1:length(temporal_pacf_slag0),
  PACF = temporal_pacf_slag0
)

p1 <- ggplot(pacf_df_slag0, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), color = "blue", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "STPACF: Correlation Weights - Spatial Lag 0",
       subtitle = "Within-region effects (Identity matrix)",
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# Plot 2: Spatial Lag 1 (Neighbor effects)
temporal_pacf_slag1 <- stpacf_correlation[, 2]
pacf_df_slag1 <- data.frame(
  Lag = 1:length(temporal_pacf_slag1),
  PACF = temporal_pacf_slag1
)

p2 <- ggplot(pacf_df_slag1, aes(x = Lag, y = PACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), color = "blue", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STPACF: Correlation Weights - Spatial Lag 1",
       subtitle = "Neighbor effects (Correlation matrix)",
       x = "Temporal Lag", y = "Partial Autocorrelation") +
  theme_minimal()

# Combine plots
combined_pacf_plot <- grid.arrange(p1, p2, ncol = 2)

# Save individual plots
ggsave("plots/09_stpacf_correlation_slag0.png", p1, width = 10, height = 6, dpi = 300)
ggsave("plots/09_stpacf_correlation_slag1.png", p2, width = 10, height = 6, dpi = 300)

# Save combined plot
ggsave("plots/09_stpacf_correlation_combined.png", combined_pacf_plot, width = 16, height = 6, dpi = 300)

print(p1)
print(p2)
cat("✅ PACF-style plots saved: slag0, slag1, and combined versions\n")

# AR order recommendation
cat("\n=== AR ORDER IDENTIFICATION (Correlation Only) ===\n")

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

# Get AR order suggestions (only correlation)
correlation_analysis <- list(
  temporal_pacf = stpacf_correlation[, 1],   # spatial lag 0
  weight_type = "Correlation"
)
correlation_ar <- suggest_ar_order(correlation_analysis)

# Differencing order
d_order <- 1

# Display proposed model
cat("\n🎯 Proposed STARIMA Model (Correlation Weights Only):\n")
cat("STARIMA(", correlation_ar$suggested_p, ", ", d_order, ", ", correlation_ma$suggested_q, ")\n", sep = "")

# Save results
stpacf_summary <- data.frame(
  Weight_Type = "Correlation",
  STPACF_Success = "✅ Success",
  Suggested_AR_Order = paste0("AR(", correlation_ar$suggested_p, ")"),
  Suggested_MA_Order = paste0("MA(", correlation_ma$suggested_q, ")"),
  Differencing_d = d_order,
  Proposed_Model = paste0("STARIMA(", correlation_ar$suggested_p, ",", d_order, ",", correlation_ma$suggested_q, ")"),
  stringsAsFactors = FALSE
)

print(stpacf_summary)

save(stpacf_correlation,
     correlation_analysis,
     correlation_ar,
     correlation_ma,
     d_order,
     stpacf_summary,
     file = "output/09_stpacf_correlation_analysis.RData")

cat("\n✅ STPACF analysis completed for Correlation weights only.\n")
cat("✅ Proposed model: STARIMA(", correlation_ar$suggested_p, ", ", d_order, ", ", correlation_ma$suggested_q, ")\n", sep = "")
cat("✅ Results saved to output/09_stpacf_correlation_analysis.RData\n\n")

# Summary
cat("\n=== SUMMARY (Correlation only) ===\n")
cat("- Suggested AR order:", correlation_ar$suggested_p, "\n")
cat("- Suggested MA order (from STACF):", correlation_ma$suggested_q, "\n")
cat("\n🎯 Proposed STARIMA model: STARIMA(", correlation_ar$suggested_p, ", ", d_order, ", ", correlation_ma$suggested_q, ")\n", sep = "")

save(stpacf_correlation, correlation_ar, correlation_ma, differenced_matrix,
     file = "output/09_stpacf_correlation_only.RData")

cat("\n✅ STPACF analysis (Correlation only) completed successfully!\n")
cat("✅ Results saved: output/09_stpacf_correlation_only.RData\n")
cat("📊 Plots saved: slag0, slag1, and combined versions\n")
cat("📁 Ready for Phase 3: STARIMA Estimation\n")