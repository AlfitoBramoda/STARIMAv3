# ============================================================================
# 05_Spatial_Weights.R - Create Spatial Weight Matrices
# ============================================================================
# Purpose: Create 3 types of spatial weight matrices for STARMA modeling
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🗺️ Spatial Weights Creation Started...\n")

# Note: Required libraries loaded by 00_Setup.R
# spdep, ggplot2, corrplot, tidyr, gridExtra

# Load centered data
load("output/04_centered_data.RData")
cat("📊 Data loaded: centered_matrix (", nrow(centered_matrix), "x", ncol(centered_matrix), ")\n")

# Define regions and coordinates
regions <- colnames(centered_matrix)
n_regions <- length(regions)

cat("\n🗺️ Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# Display coordinates
cat("\n📍 Coordinates:\n")
print(coordinates)

# Calculate distances between regions
cat("\n📏 Calculating Spatial Distances...\n")
coords_matrix <- as.matrix(coordinates[, c("Longitude", "Latitude")])
rownames(coords_matrix) <- coordinates$Region

# Distance matrix (Euclidean distance)
dist_matrix <- as.matrix(dist(coords_matrix))
rownames(dist_matrix) <- colnames(dist_matrix) <- regions

cat("Distance matrix calculated\n")
print(round(dist_matrix, 2))

# Initialize spatial weights list
spatial_weights <- list()

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🔧 CREATING SPATIAL WEIGHT MATRICES\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# ============================================================================
# 1. UNIFORM WEIGHTS (Equal weights for all neighbors)
# ============================================================================
cat("\n1️⃣ Creating Uniform Weights Matrix...\n")

uniform_weights <- matrix(0, nrow = n_regions, ncol = n_regions)
rownames(uniform_weights) <- colnames(uniform_weights) <- regions

# Set equal weights for all neighbors (excluding self)
for (i in 1:n_regions) {
  for (j in 1:n_regions) {
    if (i != j) {
      uniform_weights[i, j] <- 1 / (n_regions - 1)  # Equal weight for all neighbors
    }
  }
}

# Validate uniform weights
cat("✅ Uniform weights matrix created\n")
cat("Matrix dimensions:", dim(uniform_weights), "\n")
cat("Row sums:", round(rowSums(uniform_weights), 3), "\n")
cat("Weight range: [", round(min(uniform_weights), 3), ",", round(max(uniform_weights[uniform_weights > 0]), 3), "]\n")

spatial_weights$uniform <- uniform_weights

# ============================================================================
# 2. DISTANCE-BASED WEIGHTS (Inverse distance weighting)
# ============================================================================
cat("\n2️⃣ Creating Distance-based Weights Matrix...\n")

distance_weights <- matrix(0, nrow = n_regions, ncol = n_regions)
rownames(distance_weights) <- colnames(distance_weights) <- regions

# Calculate inverse distance weights
for (i in 1:n_regions) {
  distances <- dist_matrix[i, ]
  # Inverse distance (excluding self)
  inv_distances <- ifelse(distances == 0, 0, 1/distances)
  inv_distances[i] <- 0  # Set diagonal to 0
  
  # Normalize to sum to 1
  if (sum(inv_distances) > 0) {
    distance_weights[i, ] <- inv_distances / sum(inv_distances)
  }
}

# Validate distance weights
cat("✅ Distance-based weights matrix created\n")
cat("Matrix dimensions:", dim(distance_weights), "\n")
cat("Row sums:", round(rowSums(distance_weights), 3), "\n")
cat("Weight range: [", round(min(distance_weights), 3), ",", round(max(distance_weights), 3), "]\n")

spatial_weights$distance <- distance_weights

# ============================================================================
# 3. CORRELATION-BASED WEIGHTS (Cross-correlation weighting)
# ============================================================================
cat("\n3️⃣ Creating Correlation-based Weights Matrix...\n")

# Calculate correlation matrix from centered data
correlation_matrix <- cor(centered_matrix)
cat("Correlation matrix calculated\n")

# Convert correlation to weights (absolute correlation, set diagonal to 0)
correlation_weights <- abs(correlation_matrix)
diag(correlation_weights) <- 0

# Normalize rows to sum to 1
for (i in 1:n_regions) {
  if (sum(correlation_weights[i, ]) > 0) {
    correlation_weights[i, ] <- correlation_weights[i, ] / sum(correlation_weights[i, ])
  }
}

# Validate correlation weights
cat("✅ Correlation-based weights matrix created\n")
cat("Matrix dimensions:", dim(correlation_weights), "\n")
cat("Row sums:", round(rowSums(correlation_weights), 3), "\n")
cat("Weight range: [", round(min(correlation_weights), 3), ",", round(max(correlation_weights), 3), "]\n")

spatial_weights$correlation <- correlation_weights

# ============================================================================
# VALIDATION AND SUMMARY
# ============================================================================
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🔍 SPATIAL WEIGHTS VALIDATION\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Validation function
validate_weights <- function(weights, name) {
  cat("\n📊", name, "Weights Validation:\n")
  
  # Check dimensions
  cat("  Dimensions:", paste(dim(weights), collapse = "x"), "\n")
  
  # Check diagonal (should be 0)
  diag_check <- all(diag(weights) == 0)
  cat("  Diagonal = 0:", ifelse(diag_check, "✅ PASS", "❌ FAIL"), "\n")
  
  # Check row sums (should be 1 or close to 1)
  row_sums <- rowSums(weights)
  sum_check <- all(abs(row_sums - 1) < 1e-10)
  cat("  Row sums = 1:", ifelse(sum_check, "✅ PASS", "❌ FAIL"), "\n")
  
  # Check non-negative
  non_neg_check <- all(weights >= 0)
  cat("  Non-negative:", ifelse(non_neg_check, "✅ PASS", "❌ FAIL"), "\n")
  
  # Summary statistics
  cat("  Min weight:", round(min(weights), 4), "\n")
  cat("  Max weight:", round(max(weights), 4), "\n")
  cat("  Mean weight:", round(mean(weights[weights > 0]), 4), "\n")
  
  return(diag_check && sum_check && non_neg_check)
}

# Validate all weight matrices
uniform_valid <- validate_weights(uniform_weights, "Uniform")
distance_valid <- validate_weights(distance_weights, "Distance-based")
correlation_valid <- validate_weights(correlation_weights, "Correlation-based")

all_valid <- uniform_valid && distance_valid && correlation_valid
cat("\n🎯 Overall Validation:", ifelse(all_valid, "✅ ALL PASS", "❌ SOME FAILED"), "\n")

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📊 Creating Spatial Weights Visualizations...\n")

# Function to create heatmap
create_weight_heatmap <- function(weights, title, filename) {
  # Convert matrix to data frame for ggplot
  weights_df <- as.data.frame(weights)
  weights_df$From <- rownames(weights)
  
  # Reshape to long format
  melted_weights <- weights_df %>%
    pivot_longer(cols = -From, names_to = "To", values_to = "Weight")
  
  p <- ggplot(melted_weights, aes(x = To, y = From, fill = Weight)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                         midpoint = max(weights)/2, name = "Weight") +
    labs(title = paste("Spatial Weight Matrix:", title),
         subtitle = paste("Range: [", round(min(weights), 3), ",", round(max(weights), 3), "]"),
         x = "To Region", y = "From Region") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed()
  
  print(p)
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}

# Create heatmaps for all weight matrices
p1 <- create_weight_heatmap(uniform_weights, "Uniform Weights", "plots/05_uniform_weights.png")
cat("✅ Uniform weights heatmap saved: plots/05_uniform_weights.png\n")

p2 <- create_weight_heatmap(distance_weights, "Distance-based Weights", "plots/05_distance_weights.png")
cat("✅ Distance weights heatmap saved: plots/05_distance_weights.png\n")

p3 <- create_weight_heatmap(correlation_weights, "Correlation-based Weights", "plots/05_correlation_weights.png")
cat("✅ Correlation weights heatmap saved: plots/05_correlation_weights.png\n")

# Combined comparison plot
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, p3, ncol = 3, 
                             top = "Spatial Weight Matrices Comparison")
ggsave("plots/05_all_weights_comparison.png", combined_plot, width = 18, height = 6, dpi = 300)
cat("✅ Combined weights comparison saved: plots/05_all_weights_comparison.png\n")

# ============================================================================
# VIEW MATRICES IN RSTUDIO VIEWER
# ============================================================================
cat("\n👁️ Opening matrices in RStudio viewer...\n")

# View uniform weights
View(round(uniform_weights, 4))
cat("✅ Uniform weights matrix opened in viewer\n")

# View distance weights  
View(round(distance_weights, 4))
cat("✅ Distance weights matrix opened in viewer\n")

# View correlation weights
View(round(correlation_weights, 4))
cat("✅ Correlation weights matrix opened in viewer\n")

# View distance matrix for reference
View(round(dist_matrix, 2))
cat("✅ Distance matrix opened in viewer\n")

# ============================================================================
# SUMMARY AND SAVE
# ============================================================================
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("📋 SPATIAL WEIGHTS SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Create summary table
weights_summary <- data.frame(
  Weight_Type = c("Uniform", "Distance-based", "Correlation-based"),
  Min_Weight = c(round(min(uniform_weights[uniform_weights > 0]), 4),
                 round(min(distance_weights[distance_weights > 0]), 4),
                 round(min(correlation_weights[correlation_weights > 0]), 4)),
  Max_Weight = c(round(max(uniform_weights), 4),
                 round(max(distance_weights), 4),
                 round(max(correlation_weights), 4)),
  Mean_Weight = c(round(mean(uniform_weights[uniform_weights > 0]), 4),
                  round(mean(distance_weights[distance_weights > 0]), 4),
                  round(mean(correlation_weights[correlation_weights > 0]), 4)),
  Validation = c(ifelse(uniform_valid, "✅ PASS", "❌ FAIL"),
                 ifelse(distance_valid, "✅ PASS", "❌ FAIL"),
                 ifelse(correlation_valid, "✅ PASS", "❌ FAIL")),
  stringsAsFactors = FALSE
)

print(weights_summary)

# Save all results
save(spatial_weights, coordinates, dist_matrix, correlation_matrix,
     weights_summary, centered_matrix, integration_order,
     file = "output/05_spatial_weights.RData")

cat("\n✅ Spatial weights creation completed successfully!\n")
cat("📊 Created 3 weight matrices: Uniform, Distance-based, Correlation-based\n")
cat("🔍 All matrices validated and ready for STARMA modeling\n")
cat("📈 4 visualization plots created and saved\n")
cat("👁️ All matrices available in RStudio viewer\n")
cat("💾 Results saved to: output/05_spatial_weights.RData\n")
cat("🔄 Next step: Data splitting for train/test\n")
cat(paste(rep("=", 60), collapse = ""), "\n")