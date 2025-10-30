# ============================================================================
# 02_Stationarity_Test.R - Unit Root and Stationarity Testing (Correlation)
# ============================================================================
# Purpose: Test stationarity using ADF and KPSS tests for each region
# Author: STARMA Project - Correlation Analysis
# Date: 2024
# ============================================================================

cat("🔍 Stationarity Testing Started (Correlation Analysis)...\n")

# Load required libraries
library(tseries)
library(urca)
library(forecast)

# Simple and robust stationarity test function
test_stationarity <- function(ts_data) {
  # ADF test with automatic lag selection
  adf_test <- adf.test(ts_data, alternative = "stationary")
  
  # Return simple results
  return(list(
    statistic = adf_test$statistic,
    p_value = adf_test$p.value,
    is_stationary = adf_test$p.value < 0.05
  ))
}

# Load data
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

# Initialize results storage
stationarity_results <- data.frame(
  Region = regions,
  ADF_Statistic = numeric(n_regions),
  ADF_PValue = numeric(n_regions),
  ADF_Stationary = logical(n_regions),
  KPSS_Statistic = numeric(n_regions),
  KPSS_PValue = numeric(n_regions),
  KPSS_Stationary = logical(n_regions),
  Overall_Stationary = logical(n_regions),
  Differencing_Needed = logical(n_regions),
  stringsAsFactors = FALSE
)

cat("\n🧪 Running Stationarity Tests (for Correlation Weights):\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Test each region
for (i in 1:n_regions) {
  region <- regions[i]
  ts_data <- rainfall_matrix[, i]
  
  cat("\n📍 Testing", region, "region:\n")
  
  # ADF Test (H0: non-stationary, H1: stationary)
  tryCatch({
    adf_results <- test_stationarity(ts_data)
    
    stationarity_results$ADF_Statistic[i] <- adf_results$statistic
    stationarity_results$ADF_PValue[i] <- adf_results$p_value
    stationarity_results$ADF_Stationary[i] <- adf_results$is_stationary
    
    cat("  ADF Test: statistic =", round(adf_results$statistic, 4), 
        ", p-value =", round(adf_results$p_value, 4))
    if (adf_results$is_stationary) {
      cat(" ✅ Stationary\n")
    } else {
      cat(" ❌ Non-stationary\n")
    }
  }, error = function(e) {
    cat("  ADF Test: Error -", e$message, "\n")
    stationarity_results$ADF_Stationary[i] <- FALSE
  })
  
  # KPSS Test (H0: stationary, H1: non-stationary)
  tryCatch({
    kpss_test <- kpss.test(ts_data, null = "Trend")
    stationarity_results$KPSS_Statistic[i] <- kpss_test$statistic
    stationarity_results$KPSS_PValue[i] <- kpss_test$p.value
    stationarity_results$KPSS_Stationary[i] <- kpss_test$p.value > 0.05
    
    cat("  KPSS Test: statistic =", round(kpss_test$statistic, 4), 
        ", p-value =", round(kpss_test$p.value, 4))
    if (kpss_test$p.value > 0.05) {
      cat(" ✅ Stationary\n")
    } else {
      cat(" ❌ Non-stationary\n")
    }
  }, error = function(e) {
    cat("  KPSS Test: Error -", e$message, "\n")
    stationarity_results$KPSS_Stationary[i] <- FALSE
  })
  
  # Overall assessment (both tests must agree)
  stationarity_results$Overall_Stationary[i] <- 
    stationarity_results$ADF_Stationary[i] & stationarity_results$KPSS_Stationary[i]
  
  stationarity_results$Differencing_Needed[i] <- 
    !stationarity_results$Overall_Stationary[i]
  
  if (stationarity_results$Overall_Stationary[i]) {
    cat("  📊 Overall: ✅ STATIONARY\n")
  } else {
    cat("  📊 Overall: ❌ NON-STATIONARY (differencing needed)\n")
  }
}

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("📋 STATIONARITY ASSESSMENT SUMMARY (Correlation Analysis):\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Display results table
print(stationarity_results)

# Summary statistics
stationary_count <- sum(stationarity_results$Overall_Stationary)
non_stationary_count <- n_regions - stationary_count

cat("\n📊 Summary (ADF + KPSS Tests):\n")
cat("✅ Stationary regions:", stationary_count, "out of", n_regions, "\n")
cat("❌ Non-stationary regions:", non_stationary_count, "out of", n_regions, "\n")

if (non_stationary_count > 0) {
  cat("\n⚠️ DIFFERENCING RECOMMENDATIONS:\n")
  non_stationary_regions <- regions[!stationarity_results$Overall_Stationary]
  for (region in non_stationary_regions) {
    cat("  📍", region, ": Requires differencing\n")
  }
  cat("\n🔄 Next step: Apply differencing to non-stationary series\n")
} else {
  cat("\n🎉 All regions are stationary! No differencing needed.\n")
  cat("🔄 Next step: Proceed directly to centering\n")
}

# Save results
save(stationarity_results, rainfall_matrix, coordinates, dates,
     file = "output/02_stationarity_results.RData")

cat("\n💾 Results saved to: output/02_stationarity_results.RData\n")
cat("✅ Stationarity testing completed!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")