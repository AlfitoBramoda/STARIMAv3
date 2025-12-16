# ============================================================================
# 03_BoxCox_Transform.R - Box-Cox Transformation (Enhanced with Lambda Tracking)
# ============================================================================

cat("📦 Box-Cox Transformation Started...\n")

library(forecast)
library(car)
library(ggplot2)
library(gridExtra)

# ----------------------------------------------------------------------------
# 1️⃣ Load Data
# ----------------------------------------------------------------------------
load("output/03_data_split.RData")

regions <- colnames(train_data)
n_regions <- length(regions)

cat("📊 Checking need for Box-Cox transformation...\n")

# Add small constant to handle zeros
rainfall_positive <- train_data + 0.001

# ----------------------------------------------------------------------------
# 2️⃣ Initial Lambda Calculation (Pre-Transformation)
# ----------------------------------------------------------------------------
cat("🔹 Calculating initial lambda for each region...\n")

lambda_values_original <- numeric(n_regions)
names(lambda_values_original) <- regions

for (i in 1:n_regions) {
  lambda_values_original[i] <- BoxCox.lambda(rainfall_positive[, i])
}

cat("Initial lambda values (before transformation):\n")
print(round(lambda_values_original, 4))

lambda_overall <- median(lambda_values_original)
cat("Overall lambda (median):", round(lambda_overall, 4), "\n")

# ----------------------------------------------------------------------------
# 3️⃣ Apply Box-Cox Transformation
# ----------------------------------------------------------------------------
if (abs(lambda_overall - 1) > 0.1) {
  cat("✅ Applying Box-Cox transformation...\n")
  
  boxcox_matrix <- apply(rainfall_positive, 2, BoxCox, lambda = lambda_overall)
  colnames(boxcox_matrix) <- regions
  
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
  
  transformation_applied <- TRUE
  final_data <- boxcox_matrix
  
} else {
  cat("⚠️ Box-Cox transformation not needed (lambda ≈ 1)\n")
  final_data <- train_data
  boxcox_matrix <- train_data
  transformation_applied <- FALSE
}

# ----------------------------------------------------------------------------
# 4️⃣ Post-Transformation Lambda Calculation
# ----------------------------------------------------------------------------
cat("\n🔹 Calculating lambda after transformation...\n")

lambda_values_transformed <- numeric(n_regions)
names(lambda_values_transformed) <- regions

for (i in 1:n_regions) {
  lambda_values_transformed[i] <- BoxCox.lambda(boxcox_matrix[, i])
}

lambda_comparison <- data.frame(
  Region = regions,
  Lambda_Before = round(lambda_values_original, 4),
  Lambda_After = round(lambda_values_transformed, 4),
  Difference = round(lambda_values_transformed - lambda_values_original, 4)
)

cat("\n📈 Lambda Comparison (Before vs After):\n")
print(lambda_comparison)

# ----------------------------------------------------------------------------
# 5️⃣ Visualization (Histogram & Q-Q Plot)
# ----------------------------------------------------------------------------
cat("\n📊 Generating diagnostic plots...\n")

if (!dir.exists("output/plots_boxcox")) dir.create("output/plots_boxcox", recursive = TRUE)

for (region in regions) {
  original_data <- rainfall_positive[, region]
  transformed_data <- boxcox_matrix[, region]
  
  # Histograms
  p1 <- ggplot(data.frame(x = original_data), aes(x)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Original -", region), x = "Rainfall (mm)", y = "Freq") +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(data.frame(x = transformed_data), aes(x)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
    labs(title = paste("Box-Cox Transformed -", region), x = "Transformed Value", y = "Freq") +
    theme_minimal(base_size = 12)
  
  # Q-Q Plots
  p3 <- ggplot(data.frame(sample = original_data), aes(sample = sample)) +
    stat_qq(color = "steelblue") + stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Original -", region)) +
    theme_minimal(base_size = 12)
  
  p4 <- ggplot(data.frame(sample = transformed_data), aes(sample = sample)) +
    stat_qq(color = "forestgreen") + stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Transformed -", region)) +
    theme_minimal(base_size = 12)
  
  png(filename = paste0("output/plots_boxcox/BoxCox_", region, ".png"),
      width = 1200, height = 900)
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  dev.off()
}

cat("✅ Diagnostic plots saved to: output/plots_boxcox/\n")

# ----------------------------------------------------------------------------
# 6️⃣ Save Results
# ----------------------------------------------------------------------------
save(
  final_data, train_data, boxcox_matrix, lambda_overall,
  lambda_values_original, lambda_values_transformed, lambda_comparison,
  transformation_applied, variance_comparison, coordinates, dates,
  file = "output/04_boxcox_data.RData"
)

cat("\n💾 Results saved to: output/04_boxcox_data.RData\n")
cat("✅ Box-Cox transformation completed with lambda tracking!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
