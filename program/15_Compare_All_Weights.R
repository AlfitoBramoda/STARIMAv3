# ============================================================================
# STARIMA Weights Comparison
# File   : 15_Compare_All_Weights.R
# Purpose: Membandingkan hasil forecasting ketiga pembobotan
# ============================================================================

cat("=== COMPARING ALL STARIMA WEIGHTS ===\n\n")

# Set seed for reproducible results
set.seed(12345)

# Dependencies
req <- c("ggplot2","dplyr","tidyr","gridExtra")
for (p in req) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Run all forecasting scripts with error handling
cat("Running forecasting for all weights...\n")

tryCatch({
  source("program/14a_STARIMA_Forecasting_Uniform.R")
  cat("✅ Uniform forecasting completed\n")
}, error = function(e) {
  cat("❌ Error in Uniform forecasting:", e$message, "\n")
})

tryCatch({
  source("program/14b_STARIMA_Forecasting_Distance.R")
  cat("✅ Distance forecasting completed\n")
}, error = function(e) {
  cat("❌ Error in Distance forecasting:", e$message, "\n")
})

tryCatch({
  source("program/14c_STARIMA_Forecasting_Correlation.R")
  cat("✅ Correlation forecasting completed\n")
}, error = function(e) {
  cat("❌ Error in Correlation forecasting:", e$message, "\n")
  cat("Continuing with available results...\n")
})

# Load results with error handling
results_list <- list()

if (file.exists("output/14a_forecast_uniform.RData")) {
  load("output/14a_forecast_uniform.RData")
  results_list$uniform <- results_uniform
  cat("✅ Uniform results loaded\n")
} else {
  cat("⚠️ Uniform results not found\n")
}

if (file.exists("output/14b_forecast_distance.RData")) {
  load("output/14b_forecast_distance.RData")
  results_list$distance <- results_distance
  cat("✅ Distance results loaded\n")
} else {
  cat("⚠️ Distance results not found\n")
}

if (file.exists("output/14c_forecast_correlation.RData")) {
  load("output/14c_forecast_correlation.RData")
  results_list$correlation <- results_correlation
  cat("✅ Correlation results loaded\n")
} else {
  cat("⚠️ Correlation results not found\n")
}

load("output/02_data_split.RData")             # test_data, test_time

cat("All forecasting completed. Comparing results...\n")
cat("✅ UNIFORM SAFETY MEASURES applied to ALL weight types:\n")
cat("  - Bounds checking (0.5x to 1.5x training range)\n")
cat("  - Extreme value detection (threshold: 20)\n")
cat("  - Explosive growth prevention (3x previous value)\n")
cat("  - Large jump smoothing (5x standard deviation)\n")
cat("  - Spatial effect constraints (max 50% of forecast)\n\n")

# Combine metrics from available results
all_metrics <- data.frame()

if ("uniform" %in% names(results_list)) {
  all_metrics <- rbind(all_metrics, results_list$uniform$metrics)
}
if ("distance" %in% names(results_list)) {
  all_metrics <- rbind(all_metrics, results_list$distance$metrics)
}
if ("correlation" %in% names(results_list)) {
  all_metrics <- rbind(all_metrics, results_list$correlation$metrics)
}

if (nrow(all_metrics) == 0) {
  stop("❌ No forecast results available for comparison")
}

# Summary statistics by weight type
summary_stats <- all_metrics %>%
  group_by(Weight_Type) %>%
  summarise(
    Avg_MAE = round(mean(MAE, na.rm = TRUE), 4),
    Avg_MSE = round(mean(MSE, na.rm = TRUE), 4),
    Avg_RMSE = round(mean(RMSE, na.rm = TRUE), 4),
    Min_RMSE = round(min(RMSE, na.rm = TRUE), 4),
    Max_RMSE = round(max(RMSE, na.rm = TRUE), 4),
    .groups = 'drop'
  )

cat("📊 SUMMARY COMPARISON:\n")
print(summary_stats)

# Detailed comparison by region
cat("\n📋 DETAILED COMPARISON BY REGION:\n")
comparison_wide <- all_metrics %>%
  select(Region, RMSE, Weight_Type) %>%
  pivot_wider(names_from = Weight_Type, values_from = RMSE, names_prefix = "RMSE_")

print(comparison_wide)

# Find best weight for each region
best_weights <- comparison_wide %>%
  rowwise() %>%
  mutate(
    Best_Weight = names(.)[which.min(c(RMSE_Uniform, RMSE_Distance, RMSE_Correlation)) + 1],
    Best_RMSE = min(c(RMSE_Uniform, RMSE_Distance, RMSE_Correlation), na.rm = TRUE)
  ) %>%
  select(Region, Best_Weight, Best_RMSE)

cat("\n🏆 BEST WEIGHT FOR EACH REGION:\n")
print(best_weights)

# Overall winner
overall_winner <- summary_stats[which.min(summary_stats$Avg_RMSE), ]
cat("\n🥇 OVERALL BEST WEIGHT:", overall_winner$Weight_Type, 
    "with Avg RMSE:", overall_winner$Avg_RMSE, "\n")

# Visualization 1: RMSE Comparison Bar Chart
if (!dir.exists("plots")) dir.create("plots")

p1 <- ggplot(all_metrics, aes(x = Region, y = RMSE, fill = Weight_Type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = RMSE), position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  labs(title = "RMSE Comparison Across All Weights",
       subtitle = "Lower is Better",
       x = "Region", y = "RMSE", fill = "Weight Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_brewer(type = "qual", palette = "Set2")

ggsave("plots/15_rmse_comparison_all_weights.png", p1, width = 12, height = 8, dpi = 300)
print(p1)

# Visualization 2: Average Performance
p2 <- ggplot(summary_stats, aes(x = Weight_Type, y = Avg_RMSE, fill = Weight_Type)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = Avg_RMSE), vjust = -0.5, size = 4) +
  labs(title = "Average RMSE Performance by Weight Type",
       x = "Weight Type", y = "Average RMSE") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(type = "qual", palette = "Set1")

ggsave("plots/15_average_performance.png", p2, width = 8, height = 6, dpi = 300)
print(p2)

# Visualization 2b: Combined plot for all regions
cat("\n📊 Creating combined forecast plot for all regions...\n")

# Prepare data for combined plot
all_forecasts_long <- data.frame()

for (region in colnames(test_data)) {
  region_data <- data.frame(
    Time = test_time,
    Region = region,
    Actual = as.numeric(test_data[, region]),
    Uniform = if("uniform" %in% names(results_list)) as.numeric(results_list$uniform$forecast[, region]) else NA,
    Distance = if("distance" %in% names(results_list)) as.numeric(results_list$distance$forecast[, region]) else NA,
    Correlation = if("correlation" %in% names(results_list)) as.numeric(results_list$correlation$forecast[, region]) else NA
  )
  
  region_long <- region_data %>%
    pivot_longer(cols = c(Uniform, Distance, Correlation), 
                 names_to = "Weight_Type", values_to = "Forecast") %>%
    filter(!is.na(Forecast))
  
  all_forecasts_long <- rbind(all_forecasts_long, region_long)
}

# Create faceted plot
p_combined <- ggplot(all_forecasts_long, aes(x = Time)) +
  geom_line(aes(y = Actual), color = "black", size = 1, alpha = 0.8) +
  geom_point(aes(y = Actual), color = "black", size = 1.5) +
  geom_line(aes(y = Forecast, color = Weight_Type), size = 0.8, linetype = "dashed") +
  geom_point(aes(y = Forecast, color = Weight_Type), size = 1) +
  facet_wrap(~Region, scales = "free_y", ncol = 2) +
  labs(title = "STARIMA Forecast Comparison - All Regions",
       subtitle = "Black: Actual, Colored: Forecasts by Weight Type",
       x = "Time", y = "Rainfall (standardized)",
       color = "Weight Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.text = element_text(face = "bold")) +
  scale_color_brewer(type = "qual", palette = "Dark2")

ggsave("plots/15_forecast_comparison_all_regions.png", p_combined, 
       width = 16, height = 12, dpi = 300)
print(p_combined)

# Visualization 3: Forecast Comparison for ALL Regions
cat("\n📈 Creating forecast comparison plots for ALL regions...\n")

# Create plots for each region
for (region in colnames(test_data)) {
  cat("Creating plot for region:", region, "\n")
  
  # Extract forecasts for current region
  forecast_comparison <- data.frame(
    Time = test_time,
    Actual = as.numeric(test_data[, region]),
    Uniform = if("uniform" %in% names(results_list)) as.numeric(results_list$uniform$forecast[, region]) else NA,
    Distance = if("distance" %in% names(results_list)) as.numeric(results_list$distance$forecast[, region]) else NA,
    Correlation = if("correlation" %in% names(results_list)) as.numeric(results_list$correlation$forecast[, region]) else NA
  )
  
  # Remove NA columns
  forecast_comparison <- forecast_comparison[, !apply(is.na(forecast_comparison), 2, all)]
  
  # Reshape for plotting
  forecast_long <- forecast_comparison %>%
    pivot_longer(cols = -c(Time, Actual), 
                 names_to = "Weight_Type", values_to = "Forecast")
  
  # Get RMSE values for this region
  region_rmse <- comparison_wide[comparison_wide$Region == region, ]
  rmse_text <- paste0(
    "RMSE - Uniform: ", ifelse("RMSE_Uniform" %in% names(region_rmse), round(region_rmse$RMSE_Uniform, 3), "N/A"),
    ", Distance: ", ifelse("RMSE_Distance" %in% names(region_rmse), round(region_rmse$RMSE_Distance, 3), "N/A"),
    ", Correlation: ", ifelse("RMSE_Correlation" %in% names(region_rmse), round(region_rmse$RMSE_Correlation, 3), "N/A")
  )
  
  # Create plot
  p_region <- ggplot(forecast_long, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1.2, alpha = 0.8) +
    geom_point(aes(y = Actual), color = "black", size = 2) +
    geom_line(aes(y = Forecast, color = Weight_Type), size = 1, linetype = "dashed") +
    geom_point(aes(y = Forecast, color = Weight_Type), size = 1.5) +
    labs(title = paste("Forecast Comparison -", region),
         subtitle = rmse_text,
         x = "Time", y = "Rainfall (standardized)",
         color = "Weight Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom") +
    scale_color_brewer(type = "qual", palette = "Dark2")
  
  # Save plot
  ggsave(paste0("plots/15_forecast_comparison_", region, ".png"), p_region, 
         width = 12, height = 8, dpi = 300)
  
  # Print plot
  print(p_region)
}

cat("✅ All regional forecast plots created\n")

# Statistical significance test
cat("\n📊 STATISTICAL ANALYSIS:\n")

# Calculate improvement percentages
uniform_rmse <- summary_stats$Avg_RMSE[summary_stats$Weight_Type == "Uniform"]
distance_rmse <- summary_stats$Avg_RMSE[summary_stats$Weight_Type == "Distance"]
correlation_rmse <- summary_stats$Avg_RMSE[summary_stats$Weight_Type == "Correlation"]

if (distance_rmse < uniform_rmse) {
  distance_improvement <- round(((uniform_rmse - distance_rmse) / uniform_rmse) * 100, 2)
  cat("Distance weights improve over Uniform by:", distance_improvement, "%\n")
} else {
  distance_degradation <- round(((distance_rmse - uniform_rmse) / uniform_rmse) * 100, 2)
  cat("Distance weights perform worse than Uniform by:", distance_degradation, "%\n")
}

if (correlation_rmse < uniform_rmse) {
  correlation_improvement <- round(((uniform_rmse - correlation_rmse) / uniform_rmse) * 100, 2)
  cat("Correlation weights improve over Uniform by:", correlation_improvement, "%\n")
} else {
  correlation_degradation <- round(((correlation_rmse - uniform_rmse) / uniform_rmse) * 100, 2)
  cat("Correlation weights perform worse than Uniform by:", correlation_degradation, "%\n")
}

# Count wins per weight type
weight_wins <- best_weights %>%
  count(Best_Weight, name = "Regions_Won") %>%
  arrange(desc(Regions_Won))

cat("\n🏆 REGIONS WON BY EACH WEIGHT TYPE:\n")
print(weight_wins)

# Save comprehensive results
comparison_results <- list(
  summary_stats = summary_stats,
  detailed_comparison = comparison_wide,
  best_weights_per_region = best_weights,
  overall_winner = overall_winner,
  weight_wins = weight_wins,
  all_forecasts = list(
    uniform = if("uniform" %in% names(results_list)) results_list$uniform$forecast else NULL,
    distance = if("distance" %in% names(results_list)) results_list$distance$forecast else NULL,
    correlation = if("correlation" %in% names(results_list)) results_list$correlation$forecast else NULL
  ),
  actual_data = test_data
)

save(comparison_results, file = "output/15_weights_comparison.RData")

cat("\n💾 Comparison results saved to: output/15_weights_comparison.RData\n")
cat("📊 Plots saved in folder 'plots/'\n")
cat("✅ Weight comparison analysis completed!\n\n")

# Final summary
cat("=== FINAL SUMMARY ===\n")
cat("Best overall weight type:", overall_winner$Weight_Type, "\n")
cat("Average RMSE:", overall_winner$Avg_RMSE, "\n")
cat("Total regions analyzed:", nrow(best_weights), "\n")
cat("Most successful weight type:", weight_wins$Best_Weight[1], 
    "winning", weight_wins$Regions_Won[1], "regions\n")