# ============================================================================
# 06_Uniform_Weights.R
# ============================================================================
# Purpose : Create uniform spatial weights matrix for 5 regions
# Author  : STARMA Project
# Date    : 2024
# ============================================================================

cat("🎯 Uniform Spatial Weights Matrix Creation...\n")

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions and coordinates
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

cat("\n🗺 Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# Display coordinates
cat("\n📍 Coordinates:\n")
print(coordinates)

# ============================================================================
# FUNCTION: create_uniform_weights
# ============================================================================
create_uniform_weights <- function(n_regions, region_names) {
  # Create uniform weights matrix
  W <- matrix(0, nrow = n_regions, ncol = n_regions)
  rownames(W) <- colnames(W) <- region_names
  
  # Fill off-diagonal elements with equal weights
  for (i in 1:n_regions) {
    for (j in 1:n_regions) {
      if (i != j) {
        W[i, j] <- 1 / (n_regions - 1)  # Equal weight for all neighbors
      }
    }
  }
  
  # Diagonal remains 0 (no self-influence)
  diag(W) <- 0
  
  cat("\n✅ Uniform weights matrix created\n")
  cat("Each region has equal influence:", round(1/(n_regions-1), 4), "\n")
  
  return(W)
}

# ============================================================================
# CREATE UNIFORM WEIGHTS MATRIX
# ============================================================================
uniform_weights <- create_uniform_weights(n_regions, regions)

cat("\n--- 🎯 UNIFORM WEIGHTS MATRIX ---\n")
print(round(uniform_weights, 4))

# Verify properties
cat("\n--- ✅ MATRIX PROPERTIES VERIFICATION ---\n")
row_sums <- rowSums(uniform_weights)
for(i in 1:length(regions)) {
  cat(sprintf("%s: %.6f\n", regions[i], row_sums[i]))
}

# Check properties
diag_ok <- all(diag(uniform_weights) == 0)
row_ok <- all(abs(row_sums - 1) < 1e-10)
symmetric_ok <- isTRUE(all.equal(uniform_weights, t(uniform_weights), tolerance = 1e-10))
nonneg_ok <- all(uniform_weights >= 0)

cat(sprintf("All rows sum to 1: %s\n", ifelse(row_ok, "✅ YES", "❌ NO")))
cat(sprintf("Diagonal = 0: %s\n", ifelse(diag_ok, "✅ YES", "❌ NO")))
cat(sprintf("Symmetric matrix: %s\n", ifelse(symmetric_ok, "✅ YES", "❌ NO")))
cat(sprintf("Non-negative weights: %s\n", ifelse(nonneg_ok, "✅ YES", "❌ NO")))

# Weight statistics
non_zero_weights <- uniform_weights[uniform_weights > 0]
cat(sprintf("Weight value: %.6f (all off-diagonal elements)\n", unique(non_zero_weights)))
cat(sprintf("Number of connections per region: %d\n", n_regions - 1))

# ============================================================================
# ANALYSIS
# ============================================================================
cat("\n--- 📊 UNIFORM WEIGHTS ANALYSIS ---\n")

# Create analysis summary
uniform_analysis <- data.frame(
  Region = regions,
  Connections = rep(n_regions - 1, n_regions),
  Weight_Per_Neighbor = rep(round(1/(n_regions-1), 6), n_regions),
  Total_Outgoing_Weight = rep(1.0, n_regions),
  Influence_Type = rep("Equal to all neighbors", n_regions),
  stringsAsFactors = FALSE
)

print(uniform_analysis)

# ============================================================================
# SAVE RESULTS
# ============================================================================
spatial_weights <- list(uniform = uniform_weights)

weights_summary <- data.frame(
  Weight_Type = "Uniform (Equal Weights)",
  Min_Weight = min(non_zero_weights),
  Max_Weight = max(non_zero_weights),
  Mean_Weight = mean(non_zero_weights),
  Std_Weight = sd(non_zero_weights),  # Should be 0 for uniform
  Validation = ifelse(diag_ok && row_ok && nonneg_ok, "✅ PASS", "❌ CHECK"),
  stringsAsFactors = FALSE
)

save(spatial_weights, uniform_weights, uniform_analysis, weights_summary,
     file = "output/06_uniform_weights.RData")

cat("\n💾 Saved -> output/06_uniform_weights.RData\n")

# ============================================================================
# CONCEPTUAL EXPLANATION
# ============================================================================
cat("\n--- 🎓 UNIFORM WEIGHTS CONCEPT ---\n")
cat("Uniform spatial weights assume:\n")
cat("• All regions have EQUAL influence on each other\n")
cat("• No distance or correlation considerations\n")
cat("• Simplest form of spatial dependence\n")
cat("• Each neighbor contributes equally: 1/(n-1) = 1/4 = 0.25\n")
cat("• Symmetric matrix: w_ij = w_ji\n")
cat("• Baseline for comparison with distance/correlation weights\n")

cat(paste(rep("=", 60), collapse = ""), "\n")