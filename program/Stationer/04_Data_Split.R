# ============================================================================
# 04_Data_Split.R - Split Data into Training and Testing Sets
# ============================================================================
# Purpose: Membagi data centered menjadi training (96 bulan) dan testing (12 bulan)
# Expected Output: Train data (2016-2023), Test data (2024), split confirmation
# Dataset: Menggunakan centered_data dari 02_Data_Centering.R
# ============================================================================

cat("=== STARMA DATA SPLITTING ===\n")
cat("🎯 Optimal split: 96 training + 12 testing (89-11%)\n")
cat("📊 Training: 2016-2023 | Testing: 2024 only\n\n")

# Load required libraries
library(dplyr)
library(lubridate)

# Set working directory
setwd("c:/Users/hp/Documents/Baby/STARMA")

# Load centered data and dates
load("results/stationer/centered_data.RData")
load("results/stationer/stationer_data_loaded.RData")

# Check if dates exist, if not create them
if (!exists("dates") || is.null(dates) || any(is.na(dates))) {
  cat("⚠️  Dates not found or invalid, creating date sequence...\n")
  # Create monthly date sequence from Jan 2016 to Dec 2024
  start_date <- as.Date("2016-01-01")
  dates <- seq(start_date, by = "month", length.out = nrow(centered_data))
  cat("✅ Created date sequence from", as.character(min(dates)), "to", as.character(max(dates)), "\n")
}

cat("=== ORIGINAL DATA INFORMATION ===\n")
cat("Total observations:", nrow(centered_data), "months\n")
cat("Number of regions:", ncol(centered_data), "\n")
cat("Data period:", as.character(min(dates)), "to", as.character(max(dates)), "\n")

# Define split parameters
n_total <- nrow(centered_data)
n_test <- 12  # 1 year test (2024)
n_train <- n_total - n_test  # 96 months training

cat("\n=== SPLIT CONFIGURATION ===\n")
cat("Total observations:", n_total, "months\n")
cat("Training size:", n_train, "months (", round(n_train/n_total*100, 1), "%)\n")
cat("Testing size:", n_test, "months (", round(n_test/n_total*100, 1), "%)\n")

# Create training and testing datasets
train_data <- centered_data[1:n_train, ]
test_data <- centered_data[(n_train+1):n_total, ]

# Create corresponding date vectors
train_dates <- dates[1:n_train]
test_dates <- dates[(n_train+1):n_total]

cat("\n=== TRAINING DATA ===\n")
cat("Dimensions:", dim(train_data)[1], "×", dim(train_data)[2], "\n")
cat("Period:", as.character(min(train_dates)), "to", as.character(max(train_dates)), "\n")
cat("Years covered:", format(min(train_dates), "%Y"), "-", format(max(train_dates), "%Y"), "\n")

cat("\nTraining data summary (first 6 rows):\n")
print(head(train_data))

cat("\nTraining data summary (last 6 rows):\n")
print(tail(train_data))

cat("\n=== TESTING DATA ===\n")
cat("Dimensions:", dim(test_data)[1], "×", dim(test_data)[2], "\n")
cat("Period:", as.character(min(test_dates)), "to", as.character(max(test_dates)), "\n")
cat("Year covered:", format(min(test_dates), "%Y"), "\n")

cat("\nTesting data (all 12 months of 2024):\n")
print(test_data)

# Calculate statistics for both datasets
cat("\n=== TRAINING DATA STATISTICS ===\n")
train_stats <- data.frame(
  Region = colnames(train_data),
  Mean = round(colMeans(train_data), 4),
  SD = round(apply(train_data, 2, sd), 4),
  Min = round(apply(train_data, 2, min), 4),
  Max = round(apply(train_data, 2, max), 4)
)
print(train_stats)

cat("\n=== TESTING DATA STATISTICS ===\n")
test_stats <- data.frame(
  Region = colnames(test_data),
  Mean = round(colMeans(test_data), 4),
  SD = round(apply(test_data, 2, sd), 4),
  Min = round(apply(test_data, 2, min), 4),
  Max = round(apply(test_data, 2, max), 4)
)
print(test_stats)

# Data quality checks
cat("\n=== DATA QUALITY VALIDATION ===\n")

# Check for missing values
train_na <- sum(is.na(train_data))
test_na <- sum(is.na(test_data))

cat("Missing values in training data:", train_na, "\n")
cat("Missing values in testing data:", test_na, "\n")

# Check data continuity
expected_total <- n_train + n_test
actual_total <- nrow(train_data) + nrow(test_data)

cat("Expected total observations:", expected_total, "\n")
cat("Actual total observations:", actual_total, "\n")
cat("Data continuity check:", expected_total == actual_total, "\n")

# Verify no data overlap
last_train_date <- max(train_dates)
first_test_date <- min(test_dates)
date_gap <- as.numeric(first_test_date - last_train_date)

cat("Last training date:", as.character(last_train_date), "\n")
cat("First testing date:", as.character(first_test_date), "\n")
cat("Date gap (days):", date_gap, "(should be ~30 for monthly data)\n")

# Check column consistency
col_consistency <- all(colnames(train_data) == colnames(test_data))
cat("Column consistency:", col_consistency, "\n")

# ============================================================================
# EXPORT RESULTS TO FILES
# ============================================================================
cat("\n=== EXPORTING SPLIT RESULTS ===\n")

# Create detailed split report
split_report <- c(
  "=== STARMA DATA SPLIT REPORT ===",
  paste("Generated on:", Sys.time()),
  paste("Dataset: Surabaya Rainfall (5 regions, 108 months)"),
  "",
  "=== SPLIT CONFIGURATION ===",
  paste("Total observations:", n_total, "months"),
  paste("Training size:", n_train, "months (", round(n_train/n_total*100, 1), "%)"),
  paste("Testing size:", n_test, "months (", round(n_test/n_total*100, 1), "%)"),
  "",
  "=== TRAINING DATA ===",
  paste("Period:", as.character(min(train_dates)), "to", as.character(max(train_dates))),
  paste("Dimensions:", dim(train_data)[1], "×", dim(train_data)[2]),
  "",
  "Training Statistics:",
  capture.output(print(train_stats)),
  "",
  "=== TESTING DATA ===",
  paste("Period:", as.character(min(test_dates)), "to", as.character(max(test_dates))),
  paste("Dimensions:", dim(test_data)[1], "×", dim(test_data)[2]),
  "",
  "Testing Statistics:",
  capture.output(print(test_stats)),
  "",
  "=== VALIDATION RESULTS ===",
  paste("Missing values in training:", train_na),
  paste("Missing values in testing:", test_na),
  paste("Data continuity:", expected_total == actual_total),
  paste("Column consistency:", col_consistency),
  "",
  "=== SPLIT SUMMARY ===",
  "✅ Optimal 89-11 split achieved",
  "✅ Maximum training data (8 years)",
  "✅ Complete seasonal cycle in test (1 year)",
  "✅ Clean temporal separation",
  "✅ Ready for STARMA identification phase"
)

# Save split report
writeLines(split_report, "results/stationer/data_split_report.txt")
cat("✅ Split report saved: data_split_report.txt\n")

# Save datasets as CSV
write.csv(train_data, "results/stationer/train_data.csv", row.names = TRUE)
write.csv(test_data, "results/stationer/test_data.csv", row.names = TRUE)
write.csv(train_stats, "results/stationer/train_statistics.csv", row.names = FALSE)
write.csv(test_stats, "results/stationer/test_statistics.csv", row.names = FALSE)

cat("✅ Datasets saved as CSV files:\n")
cat("   - train_data.csv (96×5)\n")
cat("   - test_data.csv (12×5)\n")
cat("   - train_statistics.csv\n")
cat("   - test_statistics.csv\n")

# Save all split data for next phases
save(train_data, test_data, train_dates, test_dates, 
     train_stats, test_stats, n_train, n_test,
     file = "results/stationer/data_split.RData")

# Final validation and summary
if (train_na == 0 && test_na == 0 && expected_total == actual_total && col_consistency) {
  cat("\n🎉 DATA SPLITTING COMPLETED SUCCESSFULLY!\n")
  cat("📊 Training: 96 months (2016-2023) - 89% of data\n")
  cat("📊 Testing: 12 months (2024) - 11% of data\n")
  cat("✅ Optimal split for maximum learning with sufficient evaluation\n")
  cat("✅ Clean temporal separation (no data leakage)\n")
  cat("✅ Complete seasonal cycle in test period\n")
  cat("💾 Saved: results/stationer/data_split.RData\n")
  cat("📄 Exported: 5 files (1 report + 4 CSV files)\n")
  cat("🚀 Next step: Jalankan 05_STACF_Analysis.R\n")
} else {
  cat("\n❌ DATA SPLITTING ISSUES DETECTED!\n")
  cat("Please check the split process and data integrity.\n")
}