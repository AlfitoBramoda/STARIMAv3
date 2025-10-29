# ============================================================================
# 00_Setup.R - STARIMA Environment Setup
# ============================================================================
# Purpose: Install and load all required libraries for STARIMA forecasting
# Author: STARIMA Project
# Date: 2024
# ============================================================================

cat("🚀 STARIMA Environment Setup Started...\n")

# Required packages for STARIMA analysis
required_packages <- c(
  "starma",      # Main STARMA package
  "spdep",       # Spatial dependence analysis
  "tseries",     # Time series analysis
  "urca",        # Unit root and cointegration tests
  "forecast",    # Forecasting functions
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "ggplot2",     # Visualization
  "gridExtra",   # Multiple plots
  "corrplot",    # Correlation plots
  "readr",       # Data reading
  "sf"           # Spatial features (for spdep)
)

# Enhanced function to install and load packages with error handling
install_and_load <- function(package) {
  tryCatch({
    # Try to load the package
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      cat(paste("📦 Installing", package, "...\n"))
      
      # Install with error handling
      install.packages(package, dependencies = TRUE, repos = "https://cran.r-project.org")
      
      # Try to load again
      if (require(package, character.only = TRUE, quietly = TRUE)) {
        cat(paste("✅", package, "installed and loaded successfully\n"))
        return(TRUE)
      } else {
        cat(paste("❌ Failed to load", package, "after installation\n"))
        return(FALSE)
      }
    } else {
      cat(paste("✅", package, "already available\n"))
      return(TRUE)
    }
  }, error = function(e) {
    cat(paste("❌ Error with", package, ":", e$message, "\n"))
    return(FALSE)
  })
}

# Install and load all packages
cat("\n📚 Installing and Loading Required Libraries:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

failed_packages <- c()
success_count <- 0

for (pkg in required_packages) {
  if (install_and_load(pkg)) {
    success_count <- success_count + 1
  } else {
    failed_packages <- c(failed_packages, pkg)
  }
}

cat("\n📊 Package Installation Summary:\n")
cat(paste("✅ Successfully loaded:", success_count, "out of", length(required_packages), "packages\n"))

if (length(failed_packages) > 0) {
  cat(paste("❌ Failed packages:", paste(failed_packages, collapse = ", "), "\n"))
  cat("⚠️ You may need to install these manually\n")
} else {
  cat("🎉 All packages loaded successfully!\n")
}

# Set global options
options(digits = 4)
options(scipen = 999)

# Create output directory if not exists
if (!dir.exists("../output")) {
  dir.create("../output")
  cat("📁 Created output directory\n")
}

if (!dir.exists("../plots")) {
  dir.create("../plots")
  cat("📁 Created plots directory\n")
}

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("🎉 Setup selesai! Environment siap untuk STARIMA analysis\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Display session info
cat("\n📋 Session Information:\n")
print(sessionInfo())
