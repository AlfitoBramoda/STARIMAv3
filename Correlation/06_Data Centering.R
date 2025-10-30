# ============================================================================
# 06_Data_Centering.R - Data Centering and Scaling (Correlation Analysis)
# Purpose: Center and scale stationary data using starma::stcenter()
# Author : STARMA Project - Correlation Analysis
# Date   : 2024
# ============================================================================

cat("🎯 Data Centering Started (Correlation Analysis)...\n")

# Libraries
suppressPackageStartupMessages({
  library(starma)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

# Load differencing results
load("output/05_differencing_results.RData")
if (!exists("differenced_matrix"))
  stop("❌ 'differenced_matrix' not found")
cat("📊 Data loaded: differenced_matrix (", nrow(differenced_matrix), "x", ncol(differenced_matrix), ")\n")

regions <- colnames(differenced_matrix)
n_regions <- length(regions)

# BEFORE CENTERING - Stats
cat("\n📊 BEFORE CENTERING - Original Statistics:\n")
original_stats <- data.frame(
  Region = regions,
  Mean = round(apply(differenced_matrix, 2, mean, na.rm = TRUE), 4),
  SD   = round(apply(differenced_matrix, 2, sd,   na.rm = TRUE), 4),
  Min  = round(apply(differenced_matrix, 2, min,  na.rm = TRUE), 4),
  Max  = round(apply(differenced_matrix, 2, max,  na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(original_stats)

# Centering
cat("\n🔄 Applying stcenter()...\n")
centered_matrix <- NULL
centering_params <- NULL

tryCatch({
  centered_result <- stcenter(differenced_matrix)
  
  if (is.list(centered_result)) {
    centered_matrix <- centered_result$center
    centering_params <- list(
      means = centered_result$mean,
      sds   = centered_result$sd
    )
  } else {
    centered_matrix <- centered_result
    centering_params <- list(
      means = apply(differenced_matrix, 2, mean, na.rm = TRUE),
      sds   = apply(differenced_matrix, 2, sd,   na.rm = TRUE)
    )
  }
  cat("✅ Centering completed (stcenter)\n")
}, error = function(e) {
  cat("⚠️ stcenter() error -> using manual centering. Message:", e$message, "\n")
  centered_matrix <- scale(differenced_matrix, center = TRUE, scale = TRUE)
  centering_params <- list(
    means = attr(centered_matrix, "scaled:center"),
    sds   = attr(centered_matrix, "scaled:scale")
  )
  attributes(centered_matrix) <- NULL
  dim(centered_matrix) <- c(nrow(differenced_matrix), ncol(differenced_matrix))
  colnames(centered_matrix) <- regions
  cat("✅ Manual centering completed\n")
})

# AFTER CENTERING - Stats & Validation
cat("\n📊 AFTER CENTERING - Centered Statistics:\n")
centered_stats <- data.frame(
  Region = regions,
  Mean = round(apply(centered_matrix, 2, mean, na.rm = TRUE), 6),
  SD   = round(apply(centered_matrix, 2, sd,   na.rm = TRUE), 6),
  Min  = round(apply(centered_matrix, 2, min,  na.rm = TRUE), 4),
  Max  = round(apply(centered_matrix, 2, max,  na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(centered_stats)

# Validation
cat("\n🔍 Centering Validation:\n")
mean_check <- all(abs(apply(centered_matrix, 2, mean)) < 0.15)
sd_check   <- all(abs(apply(centered_matrix, 2, sd)   - 1) < 0.15)
cat("Mean ≈ 0 check:", ifelse(mean_check, "✅ PASS", "❌ FAIL"), "\n")
cat("SD   ≈ 1 check:", ifelse(sd_check,   "✅ PASS", "❌ FAIL"), "\n")

# Save outputs
save(centered_matrix, centering_params, integration_order,
     coordinates, original_stats, centered_stats,
     file = "output/06_centered_data.RData")

cat("\n✅ Data centering completed successfully (Correlation Analysis)\n")
cat("💾 Results saved to: output/06_centered_data.RData\n")
cat("🔄 Next step: Create correlation-based spatial weight matrices\n")
cat(paste(rep("=", 50), collapse = ""), "\n")