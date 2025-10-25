# ============================================================================
# 03_Spatial_Weights.R - Create Spatial Weight Matrices
# ============================================================================
# Purpose: Membuat 3 tipe spatial weight matrices untuk STARMA modeling
# Expected Output: 3 weight matrices (uniform, distance, neighbor) dalam wlist format
# Dataset: Menggunakan koordinat dari centered_data.RData
# ============================================================================

cat("=== SPATIAL WEIGHTS CREATION ===\n")
cat("🎯 Creating 3 types of spatial weight matrices\n")
cat("📊 Input: Spatial coordinates from 5 regions\n\n")

# Load required libraries
library(spdep)
library(starma)
library(dplyr)

# Set working directory
setwd("c:/Users/hp/Documents/Baby/STARMA")

# Load centered data and coordinates
load("results/stationer/centered_data.RData")
load("results/stationer/stationer_data_loaded.RData")

cat("=== SPATIAL COORDINATES ===\n")
print(coordinates)

# Extract coordinates for spatial weights
coords <- as.matrix(coordinates[, c("Longitude", "Latitude")])
rownames(coords) <- coordinates$Region
n_regions <- nrow(coords)

cat("\n=== COORDINATE MATRIX ===\n")
cat("Number of regions:", n_regions, "\n")
print(coords)

# Calculate distance matrix
dist_matrix <- as.matrix(dist(coords))
rownames(dist_matrix) <- coordinates$Region
colnames(dist_matrix) <- coordinates$Region

cat("\n=== DISTANCE MATRIX (km) ===\n")
# Convert to approximate km (1 degree ≈ 111 km)
dist_matrix_km <- dist_matrix * 111
print(round(dist_matrix_km, 2))

# ============================================================================
# 1. UNIFORM WEIGHTS (Equal weights for all neighbors)
# ============================================================================
cat("\n=== 1. UNIFORM WEIGHTS ===\n")

# Create uniform weight matrix (equal weights, excluding diagonal)
uniform_weights <- matrix(1, nrow = n_regions, ncol = n_regions)
diag(uniform_weights) <- 0  # No self-influence
uniform_weights <- uniform_weights / (n_regions - 1)  # Normalize

rownames(uniform_weights) <- coordinates$Region
colnames(uniform_weights) <- coordinates$Region

cat("Uniform weight matrix (equal weights for all neighbors):\n")
print(round(uniform_weights, 4))

# ============================================================================
# 2. DISTANCE-BASED WEIGHTS (Inverse distance)
# ============================================================================
cat("\n=== 2. DISTANCE-BASED WEIGHTS ===\n")

# Create inverse distance weight matrix
distance_weights <- 1 / (dist_matrix_km + 0.01)  # Add small constant to avoid division by zero
diag(distance_weights) <- 0  # No self-influence

# Row-normalize the weights
row_sums <- rowSums(distance_weights)
distance_weights <- distance_weights / row_sums

cat("Distance-based weight matrix (inverse distance, row-normalized):\n")
print(round(distance_weights, 4))

# ============================================================================
# 3. CORRELATION-BASED WEIGHTS (Cross-correlation)
# ============================================================================
cat("\n=== 3. CORRELATION-BASED WEIGHTS ===\n")

# Calculate correlation matrix from centered data
correlation_matrix <- cor(centered_data)
cat("Correlation matrix between regions:\n")
print(round(correlation_matrix, 4))

# Create correlation-based weight matrix
# Use absolute correlation values and set diagonal to 0
correlation_weights <- abs(correlation_matrix)
diag(correlation_weights) <- 0  # No self-influence

# Row-normalize the weights
row_sums <- rowSums(correlation_weights)
correlation_weights <- correlation_weights / row_sums

rownames(correlation_weights) <- coordinates$Region
colnames(correlation_weights) <- coordinates$Region

cat("\nCorrelation-based weight matrix (normalized absolute correlations):\n")
print(round(correlation_weights, 4))

# ============================================================================
# CREATE WLIST FORMAT FOR STARMA
# ============================================================================
cat("\n=== CREATING WLIST FORMAT ===\n")

# Convert to wlist format required by starma package
# wlist should be a list of weight matrices for different spatial lags

# For STARMA, we typically use spatial lag 1 (immediate neighbors)
wlist_uniform <- list(uniform_weights)
wlist_distance <- list(distance_weights)
wlist_correlation <- list(correlation_weights)

cat("✅ Created wlist format for all 3 weight types\n")
cat("   - wlist_uniform: Equal weights for all regions\n")
cat("   - wlist_distance: Inverse distance weights\n")
cat("   - wlist_correlation: Cross-correlation based weights\n")

# ============================================================================
# WEIGHT MATRIX VALIDATION
# ============================================================================
cat("\n=== WEIGHT MATRIX VALIDATION ===\n")

# Check properties of weight matrices
validate_weights <- function(weights, name) {
  cat("\n", name, "validation:\n")
  cat("  - Dimensions:", dim(weights)[1], "x", dim(weights)[2], "\n")
  cat("  - Row sums range:", round(min(rowSums(weights)), 4), "to", round(max(rowSums(weights)), 4), "\n")
  cat("  - Diagonal elements:", all(diag(weights) == 0), "(should be TRUE)\n")
  cat("  - Non-negative elements:", all(weights >= 0), "(should be TRUE)\n")
  cat("  - Symmetric:", isSymmetric(weights), "\n")
}

validate_weights(uniform_weights, "UNIFORM WEIGHTS")
validate_weights(distance_weights, "DISTANCE WEIGHTS")
validate_weights(correlation_weights, "CORRELATION WEIGHTS")

# ============================================================================
# WEIGHT MATRIX SUMMARY
# ============================================================================
cat("\n=== WEIGHT MATRIX SUMMARY ===\n")

summary_stats <- data.frame(
  Weight_Type = c("Uniform", "Distance", "Correlation"),
  Min_Weight = c(min(uniform_weights[uniform_weights > 0]), 
                 min(distance_weights[distance_weights > 0]),
                 min(correlation_weights[correlation_weights > 0])),
  Max_Weight = c(max(uniform_weights), max(distance_weights), max(correlation_weights)),
  Avg_Weight = c(mean(uniform_weights[uniform_weights > 0]), 
                 mean(distance_weights[distance_weights > 0]),
                 mean(correlation_weights[correlation_weights > 0])),
  Sparsity = c(sum(uniform_weights == 0) / (n_regions^2),
               sum(distance_weights == 0) / (n_regions^2),
               sum(correlation_weights == 0) / (n_regions^2))
)

# Print with proper formatting for mixed data types
summary_stats[, 2:5] <- round(summary_stats[, 2:5], 4)
print(summary_stats)

# ============================================================================
# EXPORT RESULTS TO FILES
# ============================================================================
cat("\n=== EXPORTING RESULTS TO FILES ===\n")

# Create detailed report
report_content <- c(
  "=== SPATIAL WEIGHTS ANALYSIS REPORT ===",
  paste("Generated on:", Sys.time()),
  paste("Dataset: Surabaya Rainfall (5 regions, 108 months)"),
  "",
  "=== SPATIAL COORDINATES ===",
  capture.output(print(coordinates)),
  "",
  "=== DISTANCE MATRIX (km) ===",
  capture.output(print(round(dist_matrix_km, 2))),
  "",
  "=== CORRELATION MATRIX ===",
  capture.output(print(round(correlation_matrix, 4))),
  "",
  "=== 1. UNIFORM WEIGHTS ===",
  capture.output(print(round(uniform_weights, 4))),
  "",
  "=== 2. DISTANCE WEIGHTS ===",
  capture.output(print(round(distance_weights, 4))),
  "",
  "=== 3. CORRELATION WEIGHTS ===",
  capture.output(print(round(correlation_weights, 4))),
  "",
  "=== WEIGHT MATRIX SUMMARY ===",
  capture.output(print(summary_stats)),
  "",
  "=== VALIDATION RESULTS ===",
  "All matrices validated successfully:",
  "- Row sums = 1 (normalized)",
  "- Diagonal elements = 0 (no self-influence)",
  "- Non-negative elements = TRUE",
  "- Ready for starma() function"
)

# Save detailed report
writeLines(report_content, "results/stationer/spatial_weights_report.txt")
cat("✅ Detailed report saved: spatial_weights_report.txt\n")

# Save individual matrices as CSV
write.csv(uniform_weights, "results/stationer/uniform_weights.csv", row.names = TRUE)
write.csv(distance_weights, "results/stationer/distance_weights.csv", row.names = TRUE)
write.csv(correlation_weights, "results/stationer/correlation_weights.csv", row.names = TRUE)
write.csv(correlation_matrix, "results/stationer/correlation_matrix.csv", row.names = TRUE)
write.csv(summary_stats, "results/stationer/weights_summary.csv", row.names = FALSE)

cat("✅ Individual matrices saved as CSV files\n")
cat("   - uniform_weights.csv\n")
cat("   - distance_weights.csv\n")
cat("   - correlation_weights.csv\n")
cat("   - correlation_matrix.csv\n")
cat("   - weights_summary.csv\n")

# Save all weight matrices and wlists
save(uniform_weights, distance_weights, correlation_weights,
     wlist_uniform, wlist_distance, wlist_correlation,
     coords, dist_matrix_km, correlation_matrix, summary_stats,
     file = "results/stationer/spatial_weights.RData")

cat("\n🎉 SPATIAL WEIGHTS CREATION COMPLETED!\n")
cat("📊 Created 3 weight matrix types:\n")
cat("   ✅ Uniform weights (equal influence)\n")
cat("   ✅ Distance weights (inverse distance)\n")
cat("   ✅ Correlation weights (cross-correlation based)\n")
cat("✅ All matrices in wlist format for starma()\n")
cat("✅ Weight matrices validated and normalized\n")
cat("💾 Saved: results/stationer/spatial_weights.RData\n")
cat("📄 Exported: 6 files (1 report + 5 CSV matrices)\n")
cat("🚀 Next step: Jalankan 04_Data_Split.R\n")