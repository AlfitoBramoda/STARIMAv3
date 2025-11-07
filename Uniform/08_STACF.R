# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STACF (Uniform Only)
# File: 07_STACF_Analysis_Uniform.R
# Purpose: Compute Space-Time ACF for Uniform Weights Only
# Author: STARMA Analysis (Simplified)
# Date: 2025
# ============================================================================

# -------------------------------
# LOAD DATA
# -------------------------------
load("output/05_differencing_results.RData")      # differenced_matrix
load("output/07_spatial_weights_uniform.RData")    # spatial_weights

cat("=== STARMA STACF ANALYSIS (Uniform weights only) ===\n\n")

# Ambil bobot uniform
uniform_w <- spatial_weights$uniform

# -------------------------------
# PARAMETERS
# -------------------------------
library(starma)
library(ggplot2)

max_time_lag <- 40  # maksimum lag waktu

# -------------------------------
# HITUNG STACF (Uniform)
# -------------------------------
cat("📈 Computing STACF with Uniform weights...\n")

identity_matrix <- diag(ncol(differenced_matrix))
wlist <- list(identity_matrix, uniform_w)

# Coba hitung STACF
stacf_uniform <- stacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)

cat("✅ STACF computation successful for Uniform weights\n")

# -------------------------------
# SPATIAL-AWARE MA ORDER
# -------------------------------
suggest_ma_order_spatial <- function(stacf_result, cutoff_threshold = 0.1, max_q = 10) {
  temporal_matrix <- stacf_result[-1, , drop = FALSE]  # hapus lag 0
  max_abs_acf <- apply(abs(temporal_matrix), 1, max)
  cutoff_point <- which(max_abs_acf < cutoff_threshold)[1]
  if (is.na(cutoff_point)) cutoff_point <- nrow(temporal_matrix)
  suggested_q <- min(cutoff_point, max_q)
  return(list(suggested_q = suggested_q, cutoff_point = cutoff_point))
}

uniform_ma <- suggest_ma_order_spatial(stacf_uniform)

cat("\n📊 Suggested MA order (Uniform weights): MA(", uniform_ma$suggested_q, ") - cutoff lag", uniform_ma$cutoff_point, "\n")
cat("📊 Analysis includes SLAG 0 (within-region) and SLAG 1 (cross-region) effects\n")

# -------------------------------
# PLOT STACF FOR SLAG 0 AND SLAG 1
# -------------------------------
cat("\n📊 Creating separate STACF plots for SLAG 0 and SLAG 1...\n")

n <- nrow(differenced_matrix)
conf_bound <- 1.96 / sqrt(n)

# SLAG 0 (Spatial Lag 0) - Within region effects
slag0_acf <- stacf_uniform[-1, 1]  # Remove lag 0, take spatial lag 0
slag0_df <- data.frame(
  Lag = 1:length(slag0_acf),
  ACF = slag0_acf,
  Spatial_Lag = "SLAG 0 (Within-region)"
)

# SLAG 1 (Spatial Lag 1) - Cross-region effects
if (ncol(stacf_uniform) > 1) {
  slag1_acf <- stacf_uniform[-1, 2]  # Remove lag 0, take spatial lag 1
  slag1_df <- data.frame(
    Lag = 1:length(slag1_acf),
    ACF = slag1_acf,
    Spatial_Lag = "SLAG 1 (Cross-region)"
  )
  
  # Combine both spatial lags
  combined_df <- rbind(slag0_df, slag1_df)
} else {
  combined_df <- slag0_df
  cat("⚠️ Warning: Only SLAG 0 available in STACF results\n")
}

# Combined plot for comparison
p_combined <- ggplot(combined_df, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
  geom_segment(aes(xend = Lag, yend = 0, color = Spatial_Lag), size = 1) +
  geom_point(aes(color = Spatial_Lag), size = 2) +
  facet_wrap(~Spatial_Lag, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("SLAG 0 (Within-region)" = "darkred", 
                                "SLAG 1 (Cross-region)" = "darkblue")) +
  labs(title = "STACF Analysis: SLAG 0 and SLAG 1 Comparison",
       subtitle = "Uniform Weights - Temporal Autocorrelations",
       x = "Temporal Lag", y = "Autocorrelation") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots/07_stacf_uniform_combined.png", p_combined, width = 10, height = 8, dpi = 300)
print(p_combined)  # Display in RStudio plots tab

# Individual plot for SLAG 0
p_slag0 <- ggplot(slag0_df, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STACF - SLAG 0 (Within-region Effects)",
       subtitle = "Uniform Weights - Temporal Autocorrelations",
       x = "Temporal Lag", y = "Autocorrelation") +
  theme_minimal()

ggsave("plots/07_stacf_uniform_slag0.png", p_slag0, width = 10, height = 6, dpi = 300)
print(p_slag0)  # Display in RStudio plots tab

# Individual plot for SLAG 1 (if available)
if (ncol(stacf_uniform) > 1) {
  p_slag1 <- ggplot(slag1_df, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = Lag, yend = 0), color = "darkblue", size = 1) +
    geom_point(color = "darkblue", size = 2) +
    labs(title = "STACF - SLAG 1 (Cross-region Effects)",
         subtitle = "Uniform Weights - Temporal Autocorrelations",
         x = "Temporal Lag", y = "Autocorrelation") +
    theme_minimal()
  
  ggsave("plots/07_stacf_uniform_slag1.png", p_slag1, width = 10, height = 6, dpi = 300)
  print(p_slag1)  # Display in RStudio plots tab
  
  cat("✅ STACF plots saved:\n")
  cat("  - Combined: plots/07_stacf_uniform_acf.png\n")
  cat("  - SLAG 0: plots/07_stacf_uniform_slag0.png\n")
  cat("  - SLAG 1: plots/07_stacf_uniform_slag1.png\n")
} else {
  cat("✅ STACF plots saved:\n")
  cat("  - Combined: plots/07_stacf_uniform_acf.png\n")
  cat("  - SLAG 0: plots/07_stacf_uniform_slag0.png\n")
}

# -------------------------------
# SAVE RESULTS
# -------------------------------
save(stacf_uniform, uniform_ma, differenced_matrix,
     file = "output/08_stacf_uniform_only.RData")

cat("\n✅ STACF analysis (Uniform only) completed!\n")
