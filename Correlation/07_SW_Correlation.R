# ============================================================================
# 07_SW_Correlation.R - Create Correlation-based Spatial Weight Matrix
# ============================================================================
# Purpose: Create correlation-based spatial weight matrix for STARMA modeling
# Author: STARMA Project - Correlation Analysis
# Date: 2024
# ============================================================================

cat("🗺️ Spatial Weights Creation (Correlation-based) Started...\n")

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions and coordinates
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

cat("\n🗺️ Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# Display coordinates
cat("\n📍 Coordinates:\n")
print(coordinates)

# ============================================================================
# CREATE CORRELATION-BASED WEIGHTS MATRIX
# ============================================================================
cat("\n1️⃣ Creating Correlation-based Weights Matrix...\n")

# Calculate correlation matrix
correlation_matrix <- cor(rainfall_matrix)
cat("📊 Correlation matrix calculated\n")
print(round(correlation_matrix, 3))

# Convert correlation to weights
# Method: Use absolute correlation values, set diagonal to 0, row-normalize
correlation_weights <- abs(correlation_matrix)
diag(correlation_weights) <- 0  # Remove self-correlation

# Row-normalize to ensure row sums = 1
for (i in 1:n_regions) {
  row_sum <- sum(correlation_weights[i, ])
  if (row_sum > 0) {
    correlation_weights[i, ] <- correlation_weights[i, ] / row_sum
  }
}

rownames(correlation_weights) <- colnames(correlation_weights) <- regions

cat("✅ Correlation weights matrix created\n")
cat("Matrix dimensions:", dim(correlation_weights), "\n")
cat("Row sums:", round(rowSums(correlation_weights), 3), "\n")
cat("Weight range: [", round(min(correlation_weights[correlation_weights > 0]), 3), 
    ",", round(max(correlation_weights[correlation_weights > 0]), 3), "]\n")

# ============================================================================
# VALIDATION
# ============================================================================
cat("\n🔍 VALIDATING CORRELATION WEIGHTS...\n")

diag_check <- all(diag(correlation_weights) == 0)
sum_check <- all(abs(rowSums(correlation_weights) - 1) < 1e-10)
non_neg_check <- all(correlation_weights >= 0)

cat("  Diagonal = 0:", ifelse(diag_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Row sums = 1:", ifelse(sum_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Non-negative:", ifelse(non_neg_check, "✅ PASS", "❌ FAIL"), "\n")

cat("  Min weight:", round(min(correlation_weights[correlation_weights > 0]), 4), "\n")
cat("  Max weight:", round(max(correlation_weights), 4), "\n")
cat("  Mean weight:", round(mean(correlation_weights[correlation_weights > 0]), 4), "\n")

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📊 Creating Heatmap Visualization...\n")

library(ggplot2)
library(tidyr)

# Prepare data frame
correlation_df <- as.data.frame(correlation_weights)
correlation_df$From <- rownames(correlation_df)
melted_correlation <- correlation_df %>%
  pivot_longer(cols = -From, names_to = "To", values_to = "Weight")

# Plot
p_correlation <- ggplot(melted_correlation, aes(x = To, y = From, fill = Weight)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = max(correlation_weights) / 2, name = "Weight") +
  labs(title = "Correlation-Based Spatial Weight Matrix",
       subtitle = paste("Range: [", round(min(correlation_weights[correlation_weights > 0]), 4), 
                        ",", round(max(correlation_weights), 4), "]"),
       x = "To Region", y = "From Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_fixed()

# Save heatmap
ggsave("plots/07_correlation_weights.png", p_correlation, width = 8, height = 6, dpi = 300)
cat("✅ Heatmap saved: plots/07_correlation_weights.png\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
spatial_weights <- list(correlation = correlation_weights)

weights_summary <- data.frame(
  Weight_Type = "Correlation",
  Min_Weight = round(min(correlation_weights[correlation_weights > 0]), 4),
  Max_Weight = round(max(correlation_weights), 4),
  Mean_Weight = round(mean(correlation_weights[correlation_weights > 0]), 4),
  Validation = ifelse(diag_check && sum_check && non_neg_check, "✅ PASS", "❌ FAIL"),
  stringsAsFactors = FALSE
)

print(weights_summary)

save(spatial_weights, coordinates, correlation_weights, weights_summary, rainfall_matrix, correlation_matrix,
     file = "output/07_spatial_weights_correlation.RData")

cat("\n✅ Correlation-based Spatial Weights successfully created and saved!\n")
cat("💾 Saved to: output/07_spatial_weights_correlation.RData\n")
cat("📈 Visualization saved to: plots/07_correlation_weights.png\n")
cat("🔗 Based on cross-correlation between regions\n")
cat("🔄 Next step: STACF Analysis\n")
cat(paste(rep("=", 60), collapse = ""), "\n")