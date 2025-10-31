# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STACF (correlation Only)
# File: 07_STACF_Analysis_correlation.R
# Purpose: Compute Space-Time ACF for correlation Weights Only
# Author: STARMA Analysis (Simplified)
# Date: 2025
# ============================================================================

# -------------------------------
# LOAD DATA
# -------------------------------
load("output/05_differencing_results.RData")      # differenced_matrix
load("output/07_spatial_weights_correlation_only.RData")    # spatial_weights

cat("=== STARMA STACF ANALYSIS (correlation weights only) ===\n\n")

# Ambil bobot correlation
correlation_w <- spatial_weights$correlation

# -------------------------------
# PARAMETERS
# -------------------------------
library(starma)
library(ggplot2)

max_time_lag <- 40  # maksimum lag waktu

# -------------------------------
# HITUNG STACF (correlation)
# -------------------------------
cat("📈 Computing STACF with correlation weights...\n")

identity_matrix <- diag(ncol(differenced_matrix))
wlist <- list(identity_matrix, correlation_w)

# Coba hitung STACF
stacf_correlation <- stacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)

cat("✅ STACF computation successful for correlation weights\n")

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

correlation_ma <- suggest_ma_order_spatial(stacf_correlation)

cat("\n📊 Suggested MA order (correlation weights): MA(", correlation_ma$suggested_q, ") - cutoff lag", correlation_ma$cutoff_point, "\n")

# -------------------------------
# PLOT STACF ACF-STYLE
# -------------------------------
temporal_matrix <- stacf_correlation[-1, , drop = FALSE]
max_abs_acf <- apply(abs(temporal_matrix), 1, max)
temporal_lags <- 1:length(max_abs_acf)
n <- nrow(differenced_matrix)
conf_bound <- 0.1

plot_data <- data.frame(
  Lag = temporal_lags,
  Max_ACF = max_abs_acf
)

p <- ggplot(plot_data, aes(x = Lag, y = Max_ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), linetype = "dashed", color = "blue") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STACF (ACF-style): correlation Weights",
       subtitle = "Temporal lags (max across spatial lags)",
       x = "Temporal Lag", y = "Max |STACF|") +
  theme_minimal()

ggsave("plots/07_stacf_correlation_acf.png", p, width = 10, height = 6, dpi = 300)
print(p)

# -------------------------------
# SAVE RESULTS
# -------------------------------
save(stacf_correlation, correlation_ma, differenced_matrix,
     file = "output/08_stacf_correlation_only.RData")

cat("\n✅ STACF analysis (correlation only) completed!\n")
