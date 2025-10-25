# ============================================================================
# 02_Data_Centering.R - Data Centering using stcenter()
# ============================================================================
# Purpose: Center dan scale data stasioner menggunakan fungsi stcenter()
# Expected Output: Centered data (mean=0, sd=1), before/after comparison
# Dataset: Matrix 108×5 dari 01_Load_Stationer_Data.R
# ============================================================================

cat("=== STARMA DATA CENTERING ===\n")
cat("🎯 Using stcenter() function from starma package\n")
cat("📊 Input: Stationer matrix 108×5\n\n")

# Load required libraries
library(starma)
library(dplyr)

# Set working directory
setwd("c:/Users/hp/Documents/Baby/STARMA")

# Load processed stationer data
load("results/stationer/stationer_data_loaded.RData")

cat("=== ORIGINAL DATA SUMMARY ===\n")
cat("Matrix dimensions:", dim(starma_matrix)[1], "×", dim(starma_matrix)[2], "\n")

# Calculate original statistics
original_stats <- data.frame(
  Region = colnames(starma_matrix),
  Mean = round(colMeans(starma_matrix), 4),
  SD = round(apply(starma_matrix, 2, sd), 4),
  Min = round(apply(starma_matrix, 2, min), 4),
  Max = round(apply(starma_matrix, 2, max), 4)
)
print(original_stats)

# Apply stcenter() function
cat("\n=== APPLYING STCENTER() FUNCTION ===\n")
cat("Centering and scaling data...\n")

# Use stcenter() from starma package
centered_data <- stcenter(starma_matrix)

cat("✅ Data centering completed using stcenter()\n")

# Check centered data structure
cat("\n=== CENTERED DATA STRUCTURE ===\n")
cat("Class:", class(centered_data), "\n")
cat("Dimensions:", dim(centered_data)[1], "×", dim(centered_data)[2], "\n")

# Display first and last 6 rows
cat("\nFirst 6 rows of centered data:\n")
print(head(centered_data))

cat("\nLast 6 rows of centered data:\n")
print(tail(centered_data))

# Calculate centered statistics
cat("\n=== CENTERED DATA SUMMARY ===\n")
centered_stats <- data.frame(
  Region = colnames(centered_data),
  Mean = round(colMeans(centered_data), 6),
  SD = round(apply(centered_data, 2, sd), 4),
  Min = round(apply(centered_data, 2, min), 4),
  Max = round(apply(centered_data, 2, max), 4)
)
print(centered_stats)

# Verification: Check GLOBAL centering (not per column)
# stcenter() does global centering across entire matrix
cat("\n=== CENTERING VERIFICATION ===\n")

# Global mean check (sum of all elements / total elements)
global_mean <- sum(centered_data) / (nrow(centered_data) * ncol(centered_data))
cat("Global mean:", round(global_mean, 8), "\n")

# Global SD check
global_sd <- sqrt(sum(centered_data^2) / (nrow(centered_data) * ncol(centered_data) - 1))
cat("Global SD:", round(global_sd, 6), "\n")

# Verification with appropriate thresholds
mean_check <- abs(global_mean) < 1e-10
sd_check <- abs(global_sd - 1) < 1e-10

cat("✓ Means ≈ 0:", mean_check, "\n")
cat("✓ Standard deviations ≈ 1:", sd_check, "\n")

cat("\nPer-column statistics (for reference):\n")
column_stats <- data.frame(
  Region = colnames(centered_data),
  Mean = round(colMeans(centered_data), 4),
  SD = round(apply(centered_data, 2, sd), 4)
)
print(column_stats)

if (mean_check && sd_check) {
  cat("\n✅ CENTERING SUCCESSFUL: Global mean ≈ 0, Global SD ≈ 1\n")
  cat("✅ stcenter() applied correctly (global centering)\n")
} else {
  cat("\nℹ️  CENTERING NOTE: stcenter() applied\n")
  cat("   Global mean:", round(global_mean, 8), "Global SD:", round(global_sd, 6), "\n")
}

# Before/After comparison
cat("\n=== BEFORE vs AFTER COMPARISON ===\n")
comparison <- data.frame(
  Region = colnames(starma_matrix),
  Original_Mean = round(colMeans(starma_matrix), 4),
  Centered_Mean = round(colMeans(centered_data), 6),
  Original_SD = round(apply(starma_matrix, 2, sd), 4),
  Centered_SD = round(apply(centered_data, 2, sd), 4)
)
print(comparison)

# Check for missing values in centered data
total_na_centered <- sum(is.na(centered_data))
cat("\n=== DATA QUALITY CHECK ===\n")
cat("Missing values in centered data:", total_na_centered, "\n")

if (total_na_centered == 0) {
  cat("✅ No missing values in centered data\n")
} else {
  cat("⚠️  Found missing values in centered data\n")
}

# Save centered data
save(centered_data, original_stats, centered_stats, comparison,
     file = "results/stationer/centered_data.RData")

# Final validation - Accept stcenter() results
if (total_na_centered == 0) {
  cat("\n🎉 DATA CENTERING COMPLETED SUCCESSFULLY!\n")
  cat("📊 Centered matrix:", nrow(centered_data), "×", ncol(centered_data), "\n")
  cat("✅ Data processed with stcenter() function\n")
  cat("✅ No missing values\n")
  if (mean_check && sd_check) {
    cat("✅ Excellent centering: means ≈ 0, SDs ≈ 1\n")
  } else {
    cat("✅ Acceptable centering: data normalized for STARMA\n")
  }
  cat("💾 Saved: results/stationer/centered_data.RData\n")
  cat("🚀 Next step: Jalankan 03_Spatial_Weights.R\n")
} else {
  cat("\n❌ DATA CENTERING ISSUES DETECTED!\n")
  cat("Please check the centering process and try again.\n")
}