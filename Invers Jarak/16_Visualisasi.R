# ============================================================================
# STARIMA Model Evaluation - Distance Weights Only
# File   : 16_Compare_Distance_Only.R
# Purpose: Evaluasi hasil forecasting STARIMA dengan bobot distance
# Author  : STARMA Analysis
# Date    : 2025
# ============================================================================

cat("=== STARIMA FORECAST EVALUATION (DISTANCE WEIGHTS ONLY) ===\n\n")

# ----------------------------------------------------------------------------
# Dependencies
# ----------------------------------------------------------------------------
req <- c("ggplot2", "dplyr", "tidyr")
for (p in req) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# ----------------------------------------------------------------------------
# Load Forecast Results
# ----------------------------------------------------------------------------
if (!file.exists("output/15_forecast_distance.RData")) {
  stop("❌ Distance forecast results not found. Run output/15_forecast_distance.RData")
}

load("output/15_forecast_distance.RData")  # expected: results_distance
load("output/03_data_split.RData")         # expected: test_data, test_time

cat("✅ Distance forecast results loaded successfully\n\n")

# ----------------------------------------------------------------------------
# Display Evaluation Metrics
# ----------------------------------------------------------------------------
cat("📊 MODEL PERFORMANCE METRICS (Distance Weights)\n")
cat("==============================================\n")

distance_metrics <- results_distance$metrics
print(distance_metrics)

summary_distance <- distance_metrics %>%
  summarise(
    Avg_MAE  = round(mean(MAE, na.rm = TRUE), 4),
    Avg_MSE  = round(mean(MSE, na.rm = TRUE), 4),
    Avg_RMSE = round(mean(RMSE, na.rm = TRUE), 4),
    Min_RMSE = round(min(RMSE, na.rm = TRUE), 4),
    Max_RMSE = round(max(RMSE, na.rm = TRUE), 4)
  )

cat("\n📈 SUMMARY STATISTICS:\n")
print(summary_distance)

# ----------------------------------------------------------------------------
# Visualization 1: RMSE per Region
# ----------------------------------------------------------------------------
if (!dir.exists("plots")) dir.create("plots")

p1 <- ggplot(distance_metrics, aes(x = Region, y = RMSE)) +
  geom_col(fill = "#E74C3C", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.4, size = 3) +
  labs(title = "STARIMA Forecast Performance (Distance Weights)",
       subtitle = "RMSE per Region (Lower is Better)",
       x = "Region", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("plots/16_distance_rmse_per_region.png", p1, width = 10, height = 6, dpi = 300)
print(p1)

# ----------------------------------------------------------------------------
# Visualization 2: Forecast vs Actual (Per Region)
# ----------------------------------------------------------------------------
cat("\n📈 Generating forecast vs actual plots per region...\n")

# Use the correct forecast data (original scale after inverse transformation)
forecast_data <- results_distance$forecast_original_scale

# Debug: Check dimensions
cat("📊 Debug info:\n")
cat("- test_time length:", length(test_time), "\n")
cat("- test_data dimensions:", dim(test_data), "\n")
cat("- forecast_data dimensions:", dim(forecast_data), "\n")
cat("- forecast_data columns:", colnames(forecast_data), "\n")

# Ensure forecast_data has the same column names as test_data
if (!is.null(forecast_data) && !is.null(colnames(test_data))) {
  colnames(forecast_data) <- colnames(test_data)
}

for (region in colnames(test_data)) {
  cat(" - Plotting region:", region, "\n")
  
  # Safety check for data availability
  if (region %in% colnames(forecast_data)) {
    df_plot <- data.frame(
      Time = test_time,
      Actual = as.numeric(test_data[, region]),
      Forecast = as.numeric(forecast_data[, region])
    )
  } else {
    cat("   ⚠️ Forecast data not found for region:", region, "\n")
    next
  }
  
  p_region <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1.2, alpha = 0.8) +
    geom_point(aes(y = Actual), color = "black", size = 1.5) +
    geom_line(aes(y = Forecast), color = "#E74C3C", linetype = "dashed", size = 1) +
    geom_point(aes(y = Forecast), color = "#E74C3C", size = 1.5) +
    labs(title = paste("Forecast vs Actual -", region),
         subtitle = paste0("RMSE: ", round(distance_metrics$RMSE[distance_metrics$Region == region], 3)),
         x = "Time", y = "Rainfall (standardized)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(paste0("plots/16_distance_forecast_", region, ".png"), p_region, width = 10, height = 6, dpi = 300)
  print(p_region)
}

cat("✅ All regional forecast plots generated.\n\n")

# ----------------------------------------------------------------------------
# Combined Faceted Visualization
# ----------------------------------------------------------------------------
all_long <- data.frame()

for (region in colnames(test_data)) {
  # Safety check for data availability
  if (region %in% colnames(forecast_data)) {
    df <- data.frame(
      Time = test_time,
      Region = region,
      Actual = as.numeric(test_data[, region]),
      Forecast = as.numeric(forecast_data[, region])
    )
  } else {
    cat("   ⚠️ Skipping region:", region, "(no forecast data)\n")
    next
  }
  
  df_long <- df %>%
    pivot_longer(cols = c(Actual, Forecast), names_to = "Type", values_to = "Value")
  
  all_long <- rbind(all_long, df_long)
}

p_combined <- ggplot(all_long, aes(x = Time, y = Value, color = Type)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~Region, scales = "free_y", ncol = 2) +
  labs(title = "STARIMA Forecast (Distance Weights) - All Regions",
       subtitle = "Black = Actual, Red Dashed = Forecast",
       x = "Time", y = "Rainfall (standardized)") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "#E74C3C")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/16_distance_forecast_all_regions.png", p_combined, width = 14, height = 10, dpi = 300)
print(p_combined)

# ----------------------------------------------------------------------------
# Save Evaluation Summary
# ----------------------------------------------------------------------------
evaluation_distance <- list(
  metrics = distance_metrics,
  summary = summary_distance,
  forecast_original_scale = forecast_data,
  forecast_all_scales = results_distance,  # Keep all forecast scales
  test_data = test_data
)

save(evaluation_distance, file = "output/16_distance_evaluation.RData")

cat("\n💾 Evaluation results saved → output/16_distance_evaluation.RData\n")
cat("📊 Plots saved in folder → plots/\n")
cat("✅ STARIMA (Distance) evaluation completed successfully!\n")