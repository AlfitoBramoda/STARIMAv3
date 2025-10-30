# ============================================================================
# STARIMA Forecasting Pipeline - Phase 0: Setup Data (Correlation)
# File: 00_Setup_Data_Correlation.R
# Purpose: Initial data setup and validation for correlation-based analysis
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

cat("=== STARIMA PIPELINE SETUP - CORRELATION WEIGHTS ===\n\n")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
  cat("✅ Created output directory\n")
}

# Load required libraries
required_packages <- c("starma", "forecast", "ggplot2", "dplyr", "tidyr", "corrplot")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    cat("✅ Installed and loaded:", pkg, "\n")
  } else {
    cat("✅ Loaded:", pkg, "\n")
  }
}

# Set global options
options(digits = 6)
set.seed(12345)

cat("\n📊 Setup Information:\n")
cat("- Pipeline: STARIMA Forecasting\n")
cat("- Focus: Correlation-based spatial weights\n")
cat("- Output directory: output/\n")
cat("- Random seed: 12345\n")

cat("\n✅ Setup completed successfully!\n")
cat("🎯 Next step: 01_Load_Data.R\n")