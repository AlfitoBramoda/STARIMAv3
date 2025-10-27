# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4: Evaluation Per Region (All Weights)
# File   : 11_STARIMA_Evaluation_Per_Region_All_Weights.R
# Purpose: Evaluate STARIMA model residuals (MAE, MSE, RMSE) per region for all weights
# Author : STARMA Analysis
# Date   : 2025
# ============================================================================

cat("🚀 Starting STARIMA Evaluation Per Region for All Weights...\n\n")

# ----------------------------------------------------------------------------
# 1️⃣ Libraries
# ----------------------------------------------------------------------------
required_packages <- c("Metrics", "dplyr", "ggplot2", "tidyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------------------------------------------------------
# 2️⃣ Load Data
# ----------------------------------------------------------------------------
load("output/02b_data_split.RData")   # train_data, test_data
regions <- colnames(train_data)
cat("📦 Loaded training data (", nrow(train_data), "rows ×", ncol(train_data), "regions)\n\n")

# ----------------------------------------------------------------------------
# 3️⃣ Define model files
# ----------------------------------------------------------------------------
weight_types <- c("uniform", "distance", "correlation")
model_files <- paste0("output/10", c("a","b","c"), "_starima_", weight_types, ".RData")

all_metrics <- data.frame()

# ----------------------------------------------------------------------------
# 4️⃣ Loop over all models
# ----------------------------------------------------------------------------
for (i in seq_along(weight_types)) {
  w <- weight_types[i]
  model_file <- model_files[i]
  
  if (!file.exists(model_file)) {
    cat("⚠️ File", model_file, "not found — skipped.\n")
    next
  }
  
  cat("📘 Evaluating:", toupper(w), "weights...\n")
  load(model_file)
  
  results_obj <- get(paste0(w, "_results"))
  residuals <- results_obj$model$residuals
  
  if (is.null(residuals)) {
    cat("❌ No residuals found for", w, "— skipped.\n")
    next
  }
  
  region_metrics <- data.frame(
    Region = regions,
    MAE = NA, MSE = NA, RMSE = NA,
    Weight_Type = toupper(w)
  )
  
  for (r in seq_along(regions)) {
    resid <- residuals[, r]
    mae_val <- mean(abs(resid), na.rm = TRUE)
    mse_val <- mean(resid^2, na.rm = TRUE)
    rmse_val <- sqrt(mse_val)
    
    region_metrics[r, "MAE"]  <- round(mae_val, 4)
    region_metrics[r, "MSE"]  <- round(mse_val, 4)
    region_metrics[r, "RMSE"] <- round(rmse_val, 4)
  }
  
  all_metrics <- rbind(all_metrics, region_metrics)
  cat("✅", toupper(w), "evaluation done.\n\n")
}

# ----------------------------------------------------------------------------
# 5️⃣ Summary Table
# ----------------------------------------------------------------------------
cat("📊 STARIMA Residual Evaluation Summary (All Weights):\n\n")
print(all_metrics)

# ----------------------------------------------------------------------------
# 6️⃣ Visualization
# ----------------------------------------------------------------------------
if (!dir.exists("plots")) dir.create("plots")

p_rmse <- ggplot(all_metrics, aes(x = Region, y = RMSE, fill = Weight_Type)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(aes(label = RMSE), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(title = "STARIMA RMSE per Region Across Spatial Weights",
       subtitle = "Comparison of Uniform, Distance, and Correlation weights",
       y = "RMSE", x = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank())

ggsave("plots/11_starima_rmse_per_region_all_weights.png", p_rmse, width = 9, height = 5, dpi = 300)
print(p_rmse)

# ----------------------------------------------------------------------------
# 7️⃣ Save Output
# ----------------------------------------------------------------------------
if (!dir.exists("output")) dir.create("output")
save(all_metrics, p_rmse, file = "output/11_starima_evaluation_per_region_all_weights.RData")

cat("\n💾 Results saved to: output/11_starima_evaluation_per_region_all_weights.RData\n")
cat("✅ Visualization saved: plots/11_starima_rmse_per_region_all_weights.png\n")
cat("🎯 Evaluation per region (all weights) completed successfully.\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
