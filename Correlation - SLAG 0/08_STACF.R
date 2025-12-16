# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STACF (correlation Only) - NO 99% CI
# File: 08_STACF_NO99CI.R
# Purpose: Compute Space-Time ACF for correlation Weights Only - 95% CI Only
# ============================================================================

# Load data
load("output/05_differencing_results.RData")
load("output/07_spatial_weights_correlation.RData")

cat("=== STARMA STACF ANALYSIS (correlation weights only) - 95% CI Only ===\n\n")

library(starma)
library(ggplot2)

correlation_w <- spatial_weights$correlation
max_time_lag <- 40

# Proper spatial weights setup
identity_matrix <- diag(ncol(differenced_matrix))
wlist <- list(identity_matrix, correlation_w)

# Row normalize spatial weights properly
for (k in 2:length(wlist)) {
  for (i in 1:nrow(wlist[[k]])) {
    rs <- sum(wlist[[k]][i, ])
    if (rs > 0) wlist[[k]][i, ] <- wlist[[k]][i, ] / rs
  }
}

# Compute STACF
stacf_correlation <- stacf(differenced_matrix, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
cat("✅ STACF computation successful for correlation weights\n")

# Suggest MA order (95% CI only)
suggest_ma_order_spatial <- function(stacf_result, cutoff_threshold = 0.05, max_q = 6) {
  temporal_matrix <- stacf_result[-1, , drop = FALSE]  # remove lag 0
  n <- nrow(differenced_matrix)
  conf_bound <- 1.96 / sqrt(n)  # 95% confidence
  cutoff_point <- max_q
  for (t_lag in 1:nrow(temporal_matrix)) {
    if (all(abs(temporal_matrix[t_lag, ]) < conf_bound)) {
      cutoff_point <- t_lag - 1
      break
    }
  }
  suggested_q <- min(max(cutoff_point, 1), max_q)
  return(list(suggested_q = suggested_q, cutoff_point = cutoff_point, conf_bound = conf_bound))
}

correlation_ma <- suggest_ma_order_spatial(stacf_correlation)

cat("\n📊 MA order suggestion (correlation weights):\n")
cat("- Suggested MA order:", correlation_ma$suggested_q, "\n")
cat("- Cutoff at temporal lag:", correlation_ma$cutoff_point, "\n")
cat("- Confidence bound (95%):", round(correlation_ma$conf_bound, 4), "\n")

# Plot setup
temporal_lags <- 1:(nrow(stacf_correlation)-1)
n <- nrow(differenced_matrix)
conf_bound_95 <- 1.96 / sqrt(n)

# Plot for Spatial Lag 0 
plot_data_slag0 <- data.frame(
  Lag = temporal_lags,
  ACF = stacf_correlation[-1, 1]
)

p_slag0 <- ggplot(plot_data_slag0, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95), linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "STACF: correlation Weights - Spatial Lag 0",
       subtitle = paste("Blue: 95% CI only, n =", n),
       x = "Temporal Lag", y = "STACF") +
  theme_minimal()

# Plot for Spatial Lag 1 
plot_data_slag1 <- data.frame(
  Lag = temporal_lags,
  ACF = stacf_correlation[-1, 2]
)

p_slag1 <- ggplot(plot_data_slag1, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound_95, -conf_bound_95), linetype = "dashed", color = "blue", alpha = 0.7) +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "STACF: correlation Weights - Spatial Lag 1",
       subtitle = paste("Blue: 95% CI only, n =", n),
       x = "Temporal Lag", y = "STACF") +
  theme_minimal()

# Save plots
ggsave("plots/08_stacf_correlation_slag0_95only.png", p_slag0, width = 10, height = 6, dpi = 300)
ggsave("plots/08_stacf_correlation_slag1_95only.png", p_slag1, width = 10, height = 6, dpi = 300)

print(p_slag0)
print(p_slag1)

# Significance assessment (95% only)
significant_lags_slag0 <- which(abs(stacf_correlation[-1, 1]) > conf_bound_95)
significant_lags_slag1 <- which(abs(stacf_correlation[-1, 2]) > conf_bound_95)

cat("\n🔍 Significance Assessment (95% confidence):\n")
cat("- Spatial Lag 0 significant at temporal lags:", if(length(significant_lags_slag0) > 0) paste(significant_lags_slag0, collapse = ", ") else "None", "\n")
cat("- Spatial Lag 1 significant at temporal lags:", if(length(significant_lags_slag1) > 0) paste(significant_lags_slag1, collapse = ", ") else "None", "\n")

# Save results
save(stacf_correlation, correlation_ma, differenced_matrix, conf_bound_95,
     file = "output/08_stacf_correlation_95only.RData")

cat("\n✅ STACF analysis completed (95% CI only)!\n")
cat("📊 Changes:\n")
cat("- Removed 99% (red) confidence interval lines\n")
cat("- Plots show only 95% (blue dashed) bounds\n")