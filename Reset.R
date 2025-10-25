# ============================================================================
# Reset.R - STARMA Project Environment Reset
# ============================================================================
# Purpose: Reset R environment dan hapus semua file temporary/hasil
# Usage: source("Reset.R") atau Rscript Reset.R
# ============================================================================

cat("=== RESETTING STARMA PROJECT ENVIRONMENT ===\n")

# 1. Clear workspace
rm(list = ls())
cat("✅ Workspace cleared\n")

# 2. Clear plots
if(!is.null(dev.list())) {
  dev.off()
  cat("✅ Plots cleared\n")
}

# 3. Set working directory
setwd("C:\\Users\\hp\\Documents\\Baby\\STARIMAv3")
cat("✅ Working directory:", getwd(), "\n")

# 4. Remove output directories dan isinya
output_dirs <- c("results", "plots", "models")
for(dir in output_dirs) {
  if(dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
    cat("🗑️  Removed directory:", dir, "\n")
  }
}

# 5. Remove temporary RData files
temp_files <- list.files(pattern = "*.RData", full.names = TRUE)
if(length(temp_files) > 0) {
  file.remove(temp_files)
  cat("🗑️  Removed", length(temp_files), "RData files\n")
}

# 6. Remove plot files
plot_files <- list.files(pattern = "*.png|*.pdf|*.jpg", full.names = TRUE)
if(length(plot_files) > 0) {
  file.remove(plot_files)
  cat("🗑️  Removed", length(plot_files), "plot files\n")
}

# 7. Reset graphics parameters
par(mfrow = c(1, 1))
cat("✅ Graphics parameters reset\n")

# 8. Detach loaded packages (kecuali base packages)
loaded_packages <- search()[grepl("package:", search())]
base_packages <- c("package:stats", "package:graphics", "package:grDevices", 
                   "package:utils", "package:datasets", "package:methods", "package:base")
user_packages <- setdiff(loaded_packages, base_packages)

for(pkg in user_packages) {
  try(detach(pkg, character.only = TRUE, unload = TRUE), silent = TRUE)
}
if(length(user_packages) > 0) {
  cat("📦 Detached", length(user_packages), "user packages\n")
}

cat("\n=== ENVIRONMENT RESET COMPLETED ===\n")
cat("🎯 Next step: Jalankan program/00_Setup.R untuk setup ulang\n")
cat("📁 Available datasets:", length(list.files("dataset", pattern = "*.csv")), "CSV files\n")