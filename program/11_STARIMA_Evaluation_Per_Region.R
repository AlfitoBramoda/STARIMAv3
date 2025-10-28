# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Evaluation per Region (Fixed Version)
# File: 11_STARIMA_Evaluation_Per_Region_FIXED.R
# Purpose: Evaluate STARIMA model accuracy (MAE, MSE, RMSE) for each region
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# ============================================================================
# LIBRARY SETUP
# ============================================================================
cat("🚀 Starting STARIMA Evaluation (Fixed Version)...\n\n")

required_packages <- c("Metrics", "dplyr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/10a_starima_uniform.RData")  # Model & residuals
load("output/04_centered_data.RData")        # centered_matrix, test_data

cat("📦 Loaded model and data successfully.\n")
cat("- Training data dimensions:", dim(centered_matrix), "\n\n")

# ============================================================================
# FITTED VALUES HANDLING (AUTO-DETECTION)
# ============================================================================
cat("🔧 Checking fitted values from STARIMA model...\n")

fitted_values <- tryCatch({
  fv <- fitted(uniform_results$model)
  if (is.null(fv)) stop("No fitted values found.")
  fv
}, error = function(e) {
  cat("⚠️ Fitted values not available. Reconstructing manually using residuals...\n")
  
  resid_uniform <- uniform_results$model$residuals
  if (is.null(resid_uniform)) stop("Residuals not found in model object.")
  
  # Ensure same dimension with training data
  if (nrow(resid_uniform) != nrow(centered_matrix)) {
    min_len <- min(nrow(resid_uniform), nrow(centered_matrix))
    resid_uniform <- resid_uniform[1:min_len, , drop = FALSE]
    centered_matrix <- centered_matrix[1:min_len, , drop = FALSE]
  }
  
  fitted_manual <- centered_matrix - resid_uniform
  cat("✅ Fitted values reconstructed manually.\n")
  fitted_manual
})

# Ensure numeric matrix
if (is.list(fitted_values)) fitted_values <- do.call(cbind, fitted_values)
if (inherits(fitted_values, "ts")) fitted_values <- as.matrix(fitted_values)
fitted_values <- apply(fitted_values, 2, as.numeric)

# Sync columns
colnames(fitted_values) <- colnames(centered_matrix)
regions <- colnames(centered_matrix)

cat("📊 Fitted values ready. Dimensions:", dim(fitted_values), "\n\n")

# ============================================================================
# ACCURACY METRICS PER REGION
# ============================================================================
region_metrics <- data.frame(
  Region = character(),
  MAE = numeric(),
  MSE = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

cat("⚙️ Calculating accuracy metrics per region...\n")

for (r in regions) {
  y_true <- centered_matrix[, r]
  y_pred <- fitted_values[, r]
  
  # Skip region if all NA
  if (all(is.na(y_pred)) || all(is.na(y_true))) next
  
  mae_val  <- mean(abs(y_true - y_pred), na.rm = TRUE)
  mse_val  <- mean((y_true - y_pred)^2, na.rm = TRUE)
  rmse_val <- sqrt(mse_val)
  
  region_metrics <- rbind(region_metrics,
                          data.frame(Region = r,
                                     MAE = round(mae_val, 4),
                                     MSE = round(mse_val, 4),
                                     RMSE = round(rmse_val, 4)))
  
  cat(sprintf("Region %-10s | MAE = %.4f | RMSE = %.4f\n", r, mae_val, rmse_val))
}

cat("\n✅ Accuracy metrics calculated successfully!\n\n")

# ============================================================================
# SUMMARY TABLE
# ============================================================================
cat("=== MODEL ACCURACY SUMMARY PER REGION ===\n")
print(region_metrics)

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📈 Generating RMSE visualization...\n")

p_rmse <- ggplot(region_metrics, aes(x = Region, y = RMSE, fill = Region)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_text(aes(label = RMSE), vjust = -0.5, size = 3.5) +
  labs(title = "STARIMA Model Accuracy per Region",
       subtitle = "RMSE per region (Uniform Weights)",
       x = "Region", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/11_starima_rmse_per_region_fixed.png", p_rmse, width = 8, height = 5, dpi = 300)
print(p_rmse)

cat("✅ Visualization saved: plots/11_starima_rmse_per_region_fixed.png\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
if (!dir.exists("output")) dir.create("output")

save(region_metrics, p_rmse, file = "output/11_starima_evaluation_per_region_fixed.RData")

cat("\n💾 Results saved to: output/11_starima_evaluation_per_region_fixed.RData\n")
cat("🎯 Evaluation completed successfully!\n")
cat("You can now compare MAE/MSE/RMSE per region just like academic Table 5.\n")
