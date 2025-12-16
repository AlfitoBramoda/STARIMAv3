# ============================================================================
# 05_Spatial_Weights.R - Create Spatial Weight Matrix (Correlation Only)
# ============================================================================
# Purpose: Create spatial weight matrix using inter-regional correlation
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🧩 Spatial Weights Creation (Correlation-Based Only) Started...\n")

# Note: Required libraries loaded by 00_Setup.R
# spdep, ggplot2, tidyr, gridExtra

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

cat("\n🗺️ Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# ============================================================================
# CORRELATION MATRIX CALCULATION
# ============================================================================
cat("\n📈 Calculating Correlation Matrix...\n")

correlation_matrix <- cor(rainfall_matrix, use = "pairwise.complete.obs", method = "pearson")

cat("✅ Correlation matrix calculated\n")
print(round(correlation_matrix, 3))

# ============================================================================
# TRANSFORM TO POSITIVE WEIGHTS (ABSOLUTE VALUE) AND NORMALIZE
# ============================================================================
cat("\n🔧 Creating Correlation-Based Weights Matrix...\n")

correlation_weights <- abs(correlation_matrix)

# Remove self-correlation
diag(correlation_weights) <- 0

# Normalize each row to sum = 1
for (i in 1:n_regions) {
  row_sum <- sum(correlation_weights[i, ])
  if (row_sum > 0) {
    correlation_weights[i, ] <- correlation_weights[i, ] / row_sum
  }
}

cat("✅ Correlation-based weights matrix created\n")
cat("Matrix dimensions:", dim(correlation_weights), "\n")
cat("Row sums:", round(rowSums(correlation_weights), 3), "\n")
cat("Weight range: [", round(min(correlation_weights), 3), ",", round(max(correlation_weights), 3), "]\n")

# ============================================================================
# VALIDATION
# ============================================================================
cat("\n🔍 Validating Correlation-Based Weights...\n")

validate_weights <- function(weights) {
  diag_ok <- all(diag(weights) == 0)
  row_ok <- all(abs(rowSums(weights) - 1) < 1e-10)
  nonneg_ok <- all(weights >= 0)
  
  cat("  Diagonal = 0:", ifelse(diag_ok, "✅ PASS", "❌ FAIL"), "\n")
  cat("  Row sums = 1:", ifelse(row_ok, "✅ PASS", "❌ FAIL"), "\n")
  cat("  Non-negative:", ifelse(nonneg_ok, "✅ PASS", "❌ FAIL"), "\n")
  cat("  Min weight:", round(min(weights[weights > 0]), 4), "\n")
  cat("  Max weight:", round(max(weights), 4), "\n")
  cat("  Mean weight:", round(mean(weights[weights > 0]), 4), "\n")
  
  return(diag_ok && row_ok && nonneg_ok)
}

valid <- validate_weights(correlation_weights)
cat("\n🎯 Validation result:", ifelse(valid, "✅ PASS", "❌ FAIL"), "\n")

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📊 Creating Correlation Weights Heatmap...\n")

library(ggplot2)
library(tidyr)

weights_df <- as.data.frame(correlation_weights)
weights_df$From <- rownames(correlation_weights)
melted_weights <- weights_df %>%
  pivot_longer(cols = -From, names_to = "To", values_to = "Weight")

p <- ggplot(melted_weights, aes(x = To, y = From, fill = Weight)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "white", mid = "lightgreen", high = "darkgreen",
                       midpoint = max(correlation_weights) / 2, name = "Weight") +
  labs(title = "Correlation-Based Weight Matrix",
       subtitle = paste("Range: [", round(min(correlation_weights), 3), ",", round(max(correlation_weights), 3), "]"),
       x = "To Region", y = "From Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_fixed()

ggsave("plots/07_correlation_weights.png", p, width = 8, height = 6, dpi = 300)
cat("✅ Heatmap saved: plots/057orrelation_weights.png\n")

# ============================================================================
# SAVE OUTPUT
# ============================================================================
cat("\n💾 Saving results...\n")

spatial_weights <- list(correlation = correlation_weights)

save(spatial_weights, correlation_matrix, rainfall_matrix, integration_order,
     file = "output/07_spatial_weights_correlation.RData")

cat("\n✅ Correlation-Based Weights successfully created and saved!\n")
cat("📁 File: output/07_spatial_weights_correlation.RData\n")
cat("🔄 Next step: Model structure identification (STACF/STPACF)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
