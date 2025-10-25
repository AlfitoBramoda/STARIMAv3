# ============================================================================
# 00_Setup.R - STARMA Stationer Version Setup
# ============================================================================
# Purpose: Setup environment untuk STARMA forecasting dengan dataset stasioner
# Expected Output: Library loading messages, "Setup selesai" message
# Dataset: Menggunakan data bulanan yang sudah stasioner (Box-Cox + differencing)
# ============================================================================

cat("=== STARMA STATIONER VERSION SETUP ===\n")
cat("🎯 Dataset: Stasioner (Box-Cox + Differencing)\n")
cat("📊 Resolution: Monthly data (108 observations)\n")
cat("Memulai instalasi dan loading library...\n\n")

# Daftar library yang diperlukan untuk STARMA
required_packages <- c(
  # Core STARMA packages
  "starma",      # Space-Time ARMA modeling (UTAMA)
  "spdep",       # Spatial dependence analysis untuk weight matrices
  
  # Data manipulation
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "lubridate",   # Date handling
  
  # Visualization
  "ggplot2",     # Visualisasi
  "corrplot",    # Correlation plots
  "gridExtra",   # Multiple plots arrangement
  
  # Time series & Matrix
  "forecast",    # Time series forecasting utilities
  "tseries",     # Time series analysis
  "Matrix",      # Matrix operations
  
  # File I/O
  "readr"        # CSV reading
)

# Function untuk install dan load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    cat(paste("Installing", package, "...\n"))
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
    cat(paste("✓", package, "loaded successfully\n"))
  } else {
    cat(paste("✓", package, "already loaded\n"))
  }
}

# Install dan load semua packages
for (pkg in required_packages) {
  install_and_load(pkg)
}

# Set working directory ke root project
setwd("c:/Users/hp/Documents/Baby/STARMA")

# Create output directories untuk stationer version
output_dirs <- c("results/stationer", "plots/stationer", "models/stationer")
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("📁 Created directory:", dir, "\n")
  }
}

cat("\n=== STATIONER SETUP INFORMATION ===\n")
cat("Working Directory:", getwd(), "\n")
cat("R Version:", R.version.string, "\n")
cat("Stationer Datasets:\n")
stationer_files <- list.files("dataset/Stationer", pattern = "*.csv", full.names = FALSE)
for (file in stationer_files) {
  cat("  -", file, "\n")
}

# Verify starma package
if ("starma" %in% loadedNamespaces()) {
  cat("\n🎯 STARMA Package Ready!\n")
  cat("Available functions: stcenter(), stacf(), stpacf(), starma(), stcor.test()\n")
} else {
  cat("\n⚠️  Warning: starma package not loaded properly\n")
}

# Verify spdep package
if ("spdep" %in% loadedNamespaces()) {
  cat("🗺️  SPDEP Package Ready!\n")
  cat("Available functions: dnearneigh(), nb2mat(), nblag()\n")
} else {
  cat("⚠️  Warning: spdep package not loaded properly\n")
}

cat("\n🎉 STATIONER SETUP SELESAI!\n")
cat("📊 Dataset: 108 monthly observations (2016-2024)\n")
cat("✅ Data sudah stasioner (Box-Cox + differencing)\n")
cat("🚀 Next step: Jalankan 01_Load_Stationer_Data.R\n")