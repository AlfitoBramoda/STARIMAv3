# ============================================================================
# 05_Spatial_Weights_Uniform.R - Create Uniform Spatial Weight Matrix
# ============================================================================
# Purpose: Create uniform spatial weight matrix for STARMA modeling
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🗺️ Spatial Weights Creation (Uniform) Started...\n")

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
# CREATE UNIFORM WEIGHTS MATRIX
# ============================================================================
cat("\n1️⃣ Creating Uniform Weights Matrix...\n")

uniform_weights <- matrix(0, nrow = n_regions, ncol = n_regions)
rownames(uniform_weights) <- colnames(uniform_weights) <- regions

# Assign equal weights to all other regions (excluding self)
for (i in 1:n_regions) {
  for (j in 1:n_regions) {
    if (i != j) {
      uniform_weights[i, j] <- 1 / (n_regions - 1)
    }
  }
}

cat("✅ Uniform weights matrix created\n")
cat("Matrix dimensions:", dim(uniform_weights), "\n")
cat("Row sums:", round(rowSums(uniform_weights), 3), "\n")
cat("Weight range: [", round(min(uniform_weights[uniform_weights > 0]), 3), 
    ",", round(max(uniform_weights[uniform_weights > 0]), 3), "]\n")

# ============================================================================
# VALIDATION
# ============================================================================
cat("\n🔍 VALIDATING UNIFORM WEIGHTS...\n")

diag_check <- all(diag(uniform_weights) == 0)
sum_check <- all(abs(rowSums(uniform_weights) - 1) < 1e-10)
non_neg_check <- all(uniform_weights >= 0)

cat("  Diagonal = 0:", ifelse(diag_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Row sums = 1:", ifelse(sum_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Non-negative:", ifelse(non_neg_check, "✅ PASS", "❌ FAIL"), "\n")

cat("  Min weight:", round(min(uniform_weights[uniform_weights > 0]), 4), "\n")
cat("  Max weight:", round(max(uniform_weights), 4), "\n")
cat("  Mean weight:", round(mean(uniform_weights[uniform_weights > 0]), 4), "\n")

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📊 Creating Heatmap Visualization...\n")

library(ggplot2)
library(tidyr)

# Prepare data frame
uniform_df <- as.data.frame(uniform_weights)
uniform_df$From <- rownames(uniform_df)
melted_uniform <- uniform_df %>%
  pivot_longer(cols = -From, names_to = "To", values_to = "Weight")

# Plot
p_uniform <- ggplot(melted_uniform, aes(x = To, y = From, fill = Weight)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = max(uniform_weights) / 2, name = "Weight") +
  labs(title = "Uniform Spatial Weight Matrix",
       subtitle = paste("Range: [", round(min(uniform_weights), 3), ",", round(max(uniform_weights), 3), "]"),
       x = "To Region", y = "From Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_fixed()

# Save heatmap
ggsave("plots/05_uniform_weights.png", p_uniform, width = 8, height = 6, dpi = 300)
cat("✅ Heatmap saved: plots/05_uniform_weights.png\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
spatial_weights <- list(uniform = uniform_weights)

weights_summary <- data.frame(
  Weight_Type = "Uniform",
  Min_Weight = round(min(uniform_weights[uniform_weights > 0]), 4),
  Max_Weight = round(max(uniform_weights), 4),
  Mean_Weight = round(mean(uniform_weights[uniform_weights > 0]), 4),
  Validation = ifelse(diag_check && sum_check && non_neg_check, "✅ PASS", "❌ FAIL"),
  stringsAsFactors = FALSE
)

print(weights_summary)

save(spatial_weights, coordinates, uniform_weights, weights_summary, rainfall_matrix,
     file = "output/07_spatial_weights_uniform.RData")

cat("\n✅ Uniform Spatial Weights successfully created and saved!\n")
cat("💾 Saved to: output/07_spatial_weights_uniform.RData\n")
cat("📈 Visualization saved to: plots/05_uniform_weights.png\n")
cat("🔄 Next step: Data splitting for train/test\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
