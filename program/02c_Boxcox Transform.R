# ============================================================================
# 03_BoxCox_Transform.R - Box-Cox Transformation (Optional)
# ============================================================================


cat("📦 Box-Cox Transformation Started...\n")

library(forecast)
library(car)
library(ggplot2)
library(gridExtra)

# Load data
load("output/02_data_split.RData")

regions <- colnames(train_data)
n_regions <- length(regions)

# Check if transformation is needed
cat("📊 Checking need for Box-Cox transformation...\n")

# Add small constant to handle zeros
rainfall_positive <- train_data + 0.001

# Calculate lambda for each region
lambda_values <- numeric(n_regions)
names(lambda_values) <- regions

for (i in 1:n_regions) {
  lambda_values[i] <- BoxCox.lambda(rainfall_positive[, i])
}

cat("Optimal lambda values:\n")
print(round(lambda_values, 4))

# Use overall lambda (median of individual lambdas)
lambda_overall <- median(lambda_values)
cat("Overall lambda:", round(lambda_overall, 4), "\n")

# Apply Box-Cox transformation
if (abs(lambda_overall - 1) > 0.1) {
  cat("Applying Box-Cox transformation...\n")
  
  boxcox_matrix <- apply(rainfall_positive, 2, BoxCox, lambda = lambda_overall)
  colnames(boxcox_matrix) <- regions
  
  # Compare before and after
  cat("\nTransformation Results:\n")
  cat("Original range:", range(train_data), "\n")
  cat("Transformed range:", range(boxcox_matrix), "\n")
  
  # Variance comparison
  original_vars <- apply(train_data, 2, var, na.rm = TRUE)
  transformed_vars <- apply(boxcox_matrix, 2, var, na.rm = TRUE)
  
  variance_comparison <- data.frame(
    Region = regions,
    Original_Var = round(original_vars, 4),
    Transformed_Var = round(transformed_vars, 4),
    Ratio = round(transformed_vars / original_vars, 4)
  )
  
  print(variance_comparison)
  
  # Use transformed data
  final_data <- boxcox_matrix
  transformation_applied <- TRUE
  
} else {
  cat("Box-Cox transformation not needed (lambda ≈ 1)\n")
  final_data <- train_data
  transformation_applied <- FALSE
  boxcox_matrix <- NULL
  variance_comparison <- NULL
}

# ============================================================================
# 📊 Visualization: Histogram & Q-Q Plot
# ============================================================================

cat("\n📈 Generating Box-Cox diagnostic plots...\n")

if (!dir.exists("output/plots_boxcox")) dir.create("output/plots_boxcox", recursive = TRUE)

for (region in regions) {
  original_data <- rainfall_positive[, region]
  
  if (transformation_applied) {
    transformed_data <- boxcox_matrix[, region]
  } else {
    transformed_data <- original_data
  }
  
  # 1️⃣ Histogram Before & After
  p1 <- ggplot(data.frame(x = original_data), aes(x)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.8) +
    labs(title = paste("📊 Original Distribution -", region),
         x = "Curah Hujan (mm)", y = "Frekuensi") +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(data.frame(x = transformed_data), aes(x)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.8) +
    labs(title = paste("🔄 Box-Cox Transformed -", region),
         x = "Nilai Transformed", y = "Frekuensi") +
    theme_minimal(base_size = 12)
  
  # 2️⃣ Q-Q Plot Before & After
  p3 <- ggplot(data.frame(sample = original_data), aes(sample = sample)) +
    stat_qq(color = "steelblue") + stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot (Original) -", region),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal(base_size = 12)
  
  p4 <- ggplot(data.frame(sample = transformed_data), aes(sample = sample)) +
    stat_qq(color = "forestgreen") + stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot (Transformed) -", region),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal(base_size = 12)
  
  # Simpan semua plot ke satu gambar
  png(filename = paste0("output/plots_boxcox/BoxCox_", region, ".png"),
      width = 1200, height = 900)
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  dev.off()
}

cat("✅ Diagnostic plots saved to folder: output/plots_boxcox/\n")

# ============================================================================
# 💾 Save Results
# ============================================================================

save(final_data, train_data, boxcox_matrix, lambda_overall, lambda_values,
     transformation_applied, variance_comparison, coordinates, dates,
     file = "output/02c_boxcox_data.RData")

cat("\n💾 Results saved to: output/02c_boxcox_data.RData\n")
cat("✅ Box-Cox transformation and visualization completed!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
