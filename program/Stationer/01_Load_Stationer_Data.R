# ============================================================================
# 01_Load_Stationer_Data.R - Load Stationary Dataset
# ============================================================================
# Purpose: Load dataset stasioner dan konversi ke format spatio-temporal matrix
# Expected Output: Stationer dataset structure, 108×5 matrix format
# Dataset: Menggunakan kolom diff1 (sudah stasioner)
# ============================================================================

cat("=== LOADING STATIONER DATASET ===\n")

# Load required libraries
library(readr)
library(dplyr)
library(lubridate)

# Set working directory
setwd("c:/Users/hp/Documents/Baby/STARMA")

# List stationer files
stationer_files <- list.files("dataset/Stationer", pattern = "*.csv", full.names = TRUE)
cat("Found", length(stationer_files), "stationer files:\n")
for (file in basename(stationer_files)) {
  cat("  -", file, "\n")
}

# Load all stationer datasets
stationer_data <- list()
regions <- c("Barat", "Selatan", "Tengah", "Timur", "Utara")

for (i in 1:length(regions)) {
  region <- regions[i]
  file_path <- paste0("dataset/Stationer/", region, "_Stasioner.csv")
  
  if (file.exists(file_path)) {
    data <- read_delim(file_path, delim = ",", locale = locale(decimal_mark = "."))
    stationer_data[[region]] <- data
    cat("✓ Loaded", region, ":", nrow(data), "observations\n")
  } else {
    cat("⚠️  File not found:", file_path, "\n")
  }
}

# Extract diff1 column (stationer data) dan create spatio-temporal matrix
cat("\n=== CREATING SPATIO-TEMPORAL MATRIX ===\n")

# Initialize matrix (108 rows = months, 5 cols = regions)
n_months <- nrow(stationer_data[[1]])
n_regions <- length(regions)
starma_matrix <- matrix(NA, nrow = n_months, ncol = n_regions)
colnames(starma_matrix) <- regions

# Fill matrix with diff1 values (stationer data)
for (i in 1:length(regions)) {
  region <- regions[i]
  # Convert to numeric to handle any formatting issues
  diff1_values <- as.numeric(stationer_data[[region]]$diff1)
  starma_matrix[, i] <- diff1_values
  cat("✓ Converted", region, "diff1 to numeric:", length(diff1_values), "values\n")
}

# Create date vector
dates <- dmy(stationer_data[[1]]$Date)
rownames(starma_matrix) <- format(dates, "%Y-%m")

cat("✅ Spatio-temporal matrix created:\n")
cat("   Dimensions:", dim(starma_matrix)[1], "months ×", dim(starma_matrix)[2], "regions\n")
cat("   Period:", min(dates), "to", max(dates), "\n")

# Check data types and convert matrix to numeric
cat("\n=== DATA TYPE CONVERSION ===\n")
cat("Matrix class:", class(starma_matrix), "\n")
cat("Matrix mode:", mode(starma_matrix), "\n")

# Ensure matrix is numeric
starma_matrix <- apply(starma_matrix, 2, as.numeric)
cat("✅ Matrix converted to numeric\n")

# Display matrix structure
cat("\n=== MATRIX STRUCTURE ===\n")
cat("First 6 rows:\n")
print(head(starma_matrix))

cat("\nLast 6 rows:\n")
print(tail(starma_matrix))

# Summary statistics per region
cat("\n=== STATIONER DATA SUMMARY ===\n")

# Check if matrix is numeric before calculating statistics
if (is.numeric(starma_matrix)) {
  summary_stats <- data.frame(
    Region = regions,
    Mean = round(colMeans(starma_matrix, na.rm = TRUE), 4),
    SD = round(apply(starma_matrix, 2, sd, na.rm = TRUE), 4),
    Min = round(apply(starma_matrix, 2, min, na.rm = TRUE), 4),
    Max = round(apply(starma_matrix, 2, max, na.rm = TRUE), 4),
    NA_Count = colSums(is.na(starma_matrix))
  )
  print(summary_stats)
} else {
  cat("⚠️  Matrix is not numeric. Checking data types...\n")
  str(starma_matrix)
}

# Check for missing values
if (is.numeric(starma_matrix)) {
  total_na <- sum(is.na(starma_matrix))
  cat("\nMissing values:", total_na, "out of", prod(dim(starma_matrix)), "observations\n")
  
  # Data quality check
  if (total_na == 0) {
    cat("✅ No missing values - data ready for STARMA\n")
  } else {
    cat("⚠️  Found missing values - need handling\n")
  }
} else {
  cat("\n⚠️  Cannot check missing values - matrix not numeric\n")
}

# Coordinates extraction (for spatial weights later)
coordinates <- data.frame(
  Region = regions,
  Longitude = sapply(stationer_data, function(x) x$Longitude[1]),
  Latitude = sapply(stationer_data, function(x) x$Latitude[1])
)
cat("\n=== SPATIAL COORDINATES ===\n")
print(coordinates)

# Save processed data
save(starma_matrix, dates, coordinates, summary_stats, 
     file = "results/stationer/stationer_data_loaded.RData")

# Final validation
if (is.numeric(starma_matrix) && sum(is.na(starma_matrix)) == 0) {
  cat("\n🎉 STATIONER DATA LOADING COMPLETED!\n")
  cat("📊 Matrix format:", nrow(starma_matrix), "months ×", ncol(starma_matrix), "regions\n")
  cat("✅ Data sudah stasioner (diff1 values)\n")
  cat("✅ Matrix is numeric and complete\n")
  cat("💾 Saved: results/stationer/stationer_data_loaded.RData\n")
  cat("🚀 Next step: Jalankan 02_Data_Centering.R\n")
} else {
  cat("\n❌ DATA LOADING ISSUES DETECTED!\n")
  cat("Please check data format and try again.\n")
}