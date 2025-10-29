# ============================================================================
# STARMA Forecasting Pipeline - Phase 7: Best Model Selection
# File: 16_Best_Model_Selection.R
# Purpose: Select best STARIMA model based on forecasting performance (RMSE)
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

cat("=== BEST MODEL SELECTION BASED ON FORECASTING PERFORMANCE ===\n")
cat("Selecting best spatial weighting scheme based on average RMSE...\n\n")

# ============================================================================
# LOAD REQUIRED DATA
# ============================================================================

# Load forecasting results from all three models
load("output/14a_forecast_uniform.RData")     # results_uniform
load("output/14b_forecast_distance.RData")    # results_distance  
load("output/14c_forecast_correlation.RData") # results_correlation
load("output/15_weights_comparison.RData")    # comparison results

library(ggplot2)
library(dplyr)

cat("📊 Loaded forecasting results from all three spatial weighting schemes\n\n")

# ============================================================================
# EXTRACT PERFORMANCE METRICS
# ============================================================================

cat("📈 Extracting performance metrics...\n")

# Extract metrics from each model
uniform_metrics <- results_uniform$metrics
distance_metrics <- results_distance$metrics
correlation_metrics <- results_correlation$metrics

# Create comprehensive performance table
performance_summary <- data.frame(
  Spatial_Weight = c("Uniform", "Distance", "Correlation"),
  Average_MAE = c(
    mean(uniform_metrics$MAE, na.rm = TRUE),
    mean(distance_metrics$MAE, na.rm = TRUE),
    mean(correlation_metrics$MAE, na.rm = TRUE)
  ),
  Average_MSE = c(
    mean(uniform_metrics$MSE, na.rm = TRUE),
    mean(distance_metrics$MSE, na.rm = TRUE),
    mean(correlation_metrics$MSE, na.rm = TRUE)
  ),
  Average_RMSE = c(
    mean(uniform_metrics$RMSE, na.rm = TRUE),
    mean(distance_metrics$RMSE, na.rm = TRUE),
    mean(correlation_metrics$RMSE, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

# Round values for better display
performance_summary[, 2:4] <- round(performance_summary[, 2:4], 4)

print(performance_summary)

# ============================================================================
# RANKING AND SELECTION
# ============================================================================

cat("\n🏆 Ranking models based on forecasting performance...\n")

# Rank models (1 = best, 3 = worst)
performance_summary$MAE_Rank <- rank(performance_summary$Average_MAE)
performance_summary$MSE_Rank <- rank(performance_summary$Average_MSE)
performance_summary$RMSE_Rank <- rank(performance_summary$Average_RMSE)

# Calculate overall rank (sum of ranks - lower is better)
performance_summary$Overall_Rank <- performance_summary$MAE_Rank + 
                                   performance_summary$MSE_Rank + 
                                   performance_summary$RMSE_Rank

# Sort by overall rank
performance_summary <- performance_summary[order(performance_summary$Overall_Rank), ]

cat("\n📊 Final Ranking:\n")
print(performance_summary[, c("Spatial_Weight", "Average_RMSE", "RMSE_Rank", "Overall_Rank")])

# ============================================================================
# BEST MODEL SELECTION
# ============================================================================

best_model <- performance_summary[1, ]
best_weight_type <- best_model$Spatial_Weight

cat("\n🎯 BEST MODEL SELECTION RESULTS:\n")
cat("================================\n")
cat("🥇 Best Spatial Weight:", best_weight_type, "\n")
cat("📊 Average RMSE:", best_model$Average_RMSE, "\n")
cat("📊 Average MAE:", best_model$Average_MAE, "\n")
cat("📊 Overall Rank:", best_model$Overall_Rank, "/9 (lower is better)\n")

# Performance improvement over worst model
worst_rmse <- max(performance_summary$Average_RMSE)
improvement <- ((worst_rmse - best_model$Average_RMSE) / worst_rmse) * 100

cat("📈 Performance improvement over worst model:", round(improvement, 2), "%\n")

# ============================================================================
# DETAILED COMPARISON
# ============================================================================

cat("\n🔍 Detailed Performance Comparison:\n")
cat("===================================\n")

# Create detailed comparison table
detailed_comparison <- rbind(
  data.frame(uniform_metrics, Model = "Uniform"),
  data.frame(distance_metrics, Model = "Distance"), 
  data.frame(correlation_metrics, Model = "Correlation")
)

# Regional performance analysis
regional_performance <- detailed_comparison %>%
  group_by(Region) %>%
  summarise(
    Best_Model = Model[which.min(RMSE)],
    Best_RMSE = min(RMSE, na.rm = TRUE),
    Worst_RMSE = max(RMSE, na.rm = TRUE),
    RMSE_Range = Worst_RMSE - Best_RMSE,
    .groups = 'drop'
  )

cat("\n🌍 Regional Performance Analysis:\n")
print(regional_performance)

# Count wins per model
model_wins <- table(regional_performance$Best_Model)
cat("\n🏆 Regional Wins Count:\n")
print(model_wins)

# ============================================================================
# STATISTICAL SIGNIFICANCE TEST
# ============================================================================

cat("\n📊 Statistical Significance Testing...\n")

# Perform pairwise t-tests on RMSE values
uniform_rmse <- uniform_metrics$RMSE
distance_rmse <- distance_metrics$RMSE
correlation_rmse <- correlation_metrics$RMSE

# T-tests
t_test_results <- data.frame(
  Comparison = c("Uniform vs Distance", "Uniform vs Correlation", "Distance vs Correlation"),
  P_Value = c(
    t.test(uniform_rmse, distance_rmse)$p.value,
    t.test(uniform_rmse, correlation_rmse)$p.value,
    t.test(distance_rmse, correlation_rmse)$p.value
  ),
  Significant = c(
    t.test(uniform_rmse, distance_rmse)$p.value < 0.05,
    t.test(uniform_rmse, correlation_rmse)$p.value < 0.05,
    t.test(distance_rmse, correlation_rmse)$p.value < 0.05
  )
)

t_test_results$P_Value <- round(t_test_results$P_Value, 4)
print(t_test_results)

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("\n📊 Creating performance visualization...\n")

# Performance comparison plot
perf_plot <- ggplot(performance_summary, aes(x = reorder(Spatial_Weight, -Average_RMSE), 
                                             y = Average_RMSE, fill = Spatial_Weight)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Average_RMSE, 3)), vjust = -0.5) +
  labs(title = "STARIMA Forecasting Performance Comparison",
       subtitle = paste("Best Model:", best_weight_type, "with RMSE =", round(best_model$Average_RMSE, 4)),
       x = "Spatial Weight Type", y = "Average RMSE",
       fill = "Spatial Weight") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Uniform" = "steelblue", "Distance" = "forestgreen", "Correlation" = "darkorange"))

ggsave("plots/16_best_model_selection.png", perf_plot, width = 10, height = 6, dpi = 300)
print(perf_plot)

# ============================================================================
# SAVE RESULTS
# ============================================================================

best_model_results <- list(
  performance_summary = performance_summary,
  best_model = list(
    spatial_weight = best_weight_type,
    average_rmse = best_model$Average_RMSE,
    average_mae = best_model$Average_MAE,
    overall_rank = best_model$Overall_Rank,
    improvement_percent = improvement
  ),
  regional_performance = regional_performance,
  model_wins = model_wins,
  statistical_tests = t_test_results,
  detailed_comparison = detailed_comparison
)

save(best_model_results, file = "output/16_best_model_selection.RData")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n=== BEST MODEL SELECTION COMPLETED ===\n")
cat("✅ Performance-based model selection completed\n")
cat("🏆 Best spatial weighting scheme:", best_weight_type, "\n")
cat("📊 Best average RMSE:", round(best_model$Average_RMSE, 4), "\n")
cat("📈 Performance improvement:", round(improvement, 2), "%\n")
cat("🌍 Regional wins:", paste(names(model_wins), "=", model_wins, collapse = ", "), "\n")
cat("💾 Results saved to: output/16_best_model_selection.RData\n")
cat("📊 Visualization saved to: plots/16_best_model_selection.png\n\n")

cat("🎯 CONCLUSION:\n")
cat("The", best_weight_type, "spatial weighting scheme provides the best\n")
cat("forecasting performance with an average RMSE of", round(best_model$Average_RMSE, 4), "\n")
cat("representing a", round(improvement, 2), "% improvement over the worst performing model.\n")