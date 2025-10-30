# ============================================================================
# 05_Differencing.R - Seasonal Differencing after Box-Cox (Correlation)
# ============================================================================
# Purpose : Use Box-Cox transformed data and apply seasonal differencing
# Author  : STARMA Project - Correlation Analysis
# Date    : 2024
# ============================================================================

cat("🔄 Seasonal Differencing (D=1, s=12) using Box-Cox data (Correlation)...\n")

# Libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

# Load Box-Cox Data
load("output/04_boxcox_data.RData")  
cat("📦 Loaded output/04_boxcox_data.RData\n")

# Choose source data for differencing
if (exists("final_data") && is.matrix(final_data)) {
  source_matrix <- final_data
  cat("✅ Using Box-Cox transformed TRAIN data (final_data)\n")
} else if (exists("train_data") && is.matrix(train_data)) {
  source_matrix <- train_data
  cat("ℹ️ final_data not found — using raw TRAIN data (train_data)\n")
} else {
  stop("❌ Neither 'final_data' nor 'train_data' is available")
}

regions    <- colnames(source_matrix)
n_regions  <- length(regions)
n_obs      <- nrow(source_matrix)

cat("📊 Data dims (TRAIN after Box-Cox):", n_obs, "×", n_regions, "\n\n")

# Seasonal Differencing Parameters
D_order  <- 1   # seasonal difference order
s_period <- 12  # monthly seasonality
cat("📋 Seasonal Differencing Parameters: D =", D_order, "| s =", s_period, "\n\n")

# Apply Seasonal Differencing
cat("🔁 Applying seasonal differencing to each region...\n")

differenced_list <- vector("list", n_regions)
names(differenced_list) <- regions

for (r in regions) {
  differenced_list[[r]] <- diff(source_matrix[, r], lag = s_period, differences = D_order)
}

# Build Differenced Matrix
min_length <- min(sapply(differenced_list, length))
differenced_matrix <- matrix(NA_real_, nrow = min_length, ncol = n_regions)
colnames(differenced_matrix) <- regions

for (i in seq_along(regions)) {
  x <- differenced_list[[ regions[i] ]]
  if (length(x) >= min_length) {
    differenced_matrix[, i] <- tail(x, min_length)
  }
}

# Integration order for compatibility
integration_order <- 1

cat("\n✅ Seasonal differencing completed (Correlation Analysis).\n")
cat("- Differenced matrix dims:", nrow(differenced_matrix), "×", ncol(differenced_matrix), "\n")
cat("- integration_order (scalar):", integration_order, "\n\n")

# Visualization
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

plot_df <- data.frame(
  Index = seq_len(nrow(differenced_matrix)),
  differenced_matrix
)

plot_long <- plot_df |>
  pivot_longer(cols = all_of(regions), names_to = "Region", values_to = "Value")

p <- ggplot(plot_long, aes(x = Index, y = Value, color = Region)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  labs(
    title = "Seasonally Differenced Rainfall (Correlation Analysis)",
    subtitle = paste0("Source: Box-Cox transformed data | Obs: ", nrow(differenced_matrix)),
    x = "Index",
    y = "Differenced Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

ggsave("plots/05_differenced_correlation.png", p, width = 12, height = 8, dpi = 300)
print(p)
cat("📈 Saved plot: plots/05_differenced_correlation.png\n")

# Save Results
save(
  differenced_matrix,
  integration_order,
  file = "output/05_differencing_results.RData"
)

cat("\n💾 Saved to: output/05_differencing_results.RData\n")
cat("🎯 Done: Seasonal differencing for Correlation Analysis.\n")
cat(paste(rep("=", 60), collapse = ""), "\n")