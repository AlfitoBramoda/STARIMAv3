# ============================================================================
# 07_SW_Uniform.R
# Purpose : Create uniform spatial weights matrix for STARIMA modeling
# Input   : output/06_distance_matrix_km.RData (not used, but for consistency)
# Output  : output/07_spatial_weights_uniform.RData
# ============================================================================

cat("🚀 Creating Uniform Spatial Weights for STARIMA...\n")

dir.create("artifacts", showWarnings = FALSE, recursive = TRUE)

# ------------------------------- Load ---------------------------------------
# Load distance matrix for consistency (though not used for uniform weights)
infile <- "output/06_distance_matrix_km.RData"
if (file.exists(infile)) {
  load(infile)
  cat("📊 Distance matrix loaded (for reference only)\n")
}

# Load basic data for region names
load("output/01_rainfall_data.RData")
regions <- colnames(rainfall_matrix)
n <- length(regions)

# ----------------------------- Create Uniform Weights ----------------------
# Create uniform weights matrix: equal weights for all neighbors
W <- matrix(0, nrow = n, ncol = n)
rownames(W) <- colnames(W) <- regions

# Fill off-diagonal elements with equal weights
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      W[i, j] <- 1 / (n - 1)  # Equal weight for all neighbors
    }
  }
}

# Diagonal remains 0 (no self-influence)
diag(W) <- 0

cat("✅ Uniform weights matrix created\n")
cat("Each region has equal influence:", round(1/(n-1), 4), "\n")

# ------------------------------- Validation ---------------------------------
diag_ok   <- all(diag(W) == 0)
row_ok    <- all(abs(rowSums(W) - 1) < 1e-10)
nonneg_ok <- all(W >= 0)
symmetric_ok <- isTRUE(all.equal(W, t(W), tolerance = 1e-8))

met <- W[W > 0]
cat("✅ Uniform weights validated successfully.\n",
    "  Diagonal=0  : ", ifelse(diag_ok, "PASS", "FAIL"), "\n",
    "  Row sums=1  : ", ifelse(row_ok,  "PASS", "FAIL"), "\n",
    "  Non-negative: ", ifelse(nonneg_ok,"PASS", "FAIL"), "\n",
    "  Symmetric   : ", ifelse(symmetric_ok,"PASS", "FAIL"), "\n",
    sprintf("  Weight range: [%.6f, %.6f]\n",
            min(met, na.rm = TRUE), max(met, na.rm = TRUE)), sep = "")

# Display Spatial Weights Matrix in console
cat("\n--- 🎯 SPATIAL WEIGHTS MATRIX (UNIFORM) ---\n")
print(round(W, 4))

# Verify row sums = 1
cat("\n--- ✅ ROW SUMS VERIFICATION ---\n")
row_sums_check <- rowSums(W)
for(i in 1:length(regions)) {
  cat(sprintf("%s: %.6f\n", regions[i], row_sums_check[i]))
}
cat(sprintf("All rows sum to 1: %s\n", ifelse(all(abs(row_sums_check - 1) < 1e-10), "✅ YES", "❌ NO")))

# ------------------------------ Analytics -----------------------------------
uniform_analysis <- data.frame(
  Region = regions,
  Equal_Neighbors = rep(n-1, n),
  Weight_Per_Neighbor = rep(round(1/(n-1), 6), n),
  Influence_Pattern = rep("Equal to all", n),
  Spatial_Structure = rep("Uniform connectivity", n),
  row.names = NULL
)

cat("\n--- 📊 UNIFORM WEIGHTS ANALYSIS ---\n")
print(uniform_analysis)

# ------------------------------- Persist ------------------------------------
spatial_weights <- list(uniform = W)
weights_summary <- data.frame(
  Weight_Type = "Uniform (Equal Weights)",
  Min_Weight  = min(met, na.rm = TRUE),
  Max_Weight  = max(met, na.rm = TRUE),
  Mean_Weight = mean(met, na.rm = TRUE),
  Std_Weight  = sd(met, na.rm = TRUE),  # Should be 0 for uniform
  Validation  = ifelse(diag_ok && row_ok && nonneg_ok && symmetric_ok, "✅ PASS", "❌ CHECK"),
  stringsAsFactors = FALSE
)

save(spatial_weights, W, uniform_analysis, weights_summary,
     file = "output/07_spatial_weights_uniform.RData")
write.csv(W, file = "artifacts/07_uniform_weights.csv", row.names = TRUE)

cat("\n💾 Saved -> output/07_spatial_weights_uniform.RData\n")
cat("🧾 Exported -> artifacts/07_uniform_weights.csv\n")

# ============================================================================
# UNIFORM WEIGHTS CHARACTERISTICS
# ============================================================================
cat("\n--- 🔍 UNIFORM WEIGHTS CHARACTERISTICS ---\n")
cat("✅ Symmetric matrix (w_ij = w_ji)\n")
cat("✅ Equal influence assumption\n")
cat("✅ No geographic distance consideration\n")
cat("✅ Simplest spatial dependence structure\n")
cat("✅ Baseline for comparison with IDW and correlation weights\n")
cat(sprintf("✅ Each region influences others equally: %.4f\n", 1/(n-1)))

cat(paste(rep("=", 60), collapse = ""), "\n")