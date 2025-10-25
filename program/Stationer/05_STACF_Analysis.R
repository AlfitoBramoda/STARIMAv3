# ============================================================================
# STARMA FORECASTING - STACF ANALYSIS
# ============================================================================
# Purpose: Space-Time AutoCorrelation Function Analysis for MA Order Identification
# Input: Training data from previous step
# Output: STACF plots and MA order recommendations
# ============================================================================

# Clear environment and load libraries
rm(list = ls())
library(starma)
library(ggplot2)
library(gridExtra)

# Set working directory and create output folder
setwd("c:/Users/hp/Documents/Baby/STARMA")
output_dir <- "results/stationer/05_stacf_analysis"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("=== STARMA STACF ANALYSIS ===\n")
cat("Loading training data...\n")

# Load training data and spatial weights
load("results/stationer/data_split.RData")
load("results/stationer/spatial_weights.RData")

# Verify data structure
cat("Training data dimensions:", dim(train_data), "\n")
cat("Time periods:", nrow(train_data), "\n")
cat("Regions:", ncol(train_data), "\n")

# STACF Analysis with different spatial weights
cat("\n=== COMPUTING STACF ===\n")

# Parameters for STACF
max_lag_time <- 12  # Maximum time lags
max_lag_space <- 2  # Maximum spatial lags

# Data format: rows=time, columns=sites (already correct format)
cat("Data format:", dim(train_data), "(time × regions)\n")

# Create proper wlist format (identity matrix first, then spatial weights)
I_matrix <- diag(5)  # Identity matrix for 5 regions

# 1. STACF with Uniform Weights
cat("Computing STACF with uniform weights...\n")
wlist_uniform_stacf <- list(I_matrix, uniform_weights)
stacf_uniform <- stacf(train_data, wlist = wlist_uniform_stacf, 
                      tlag.max = max_lag_time, plot = FALSE)

# 2. STACF with Distance Weights  
cat("Computing STACF with distance weights...\n")
wlist_distance_stacf <- list(I_matrix, distance_weights)
stacf_distance <- stacf(train_data, wlist = wlist_distance_stacf,
                       tlag.max = max_lag_time, plot = FALSE)

# 3. STACF with Correlation Weights
cat("Computing STACF with correlation weights...\n")
wlist_correlation_stacf <- list(I_matrix, correlation_weights)
stacf_correlation <- stacf(train_data, wlist = wlist_correlation_stacf,
                          tlag.max = max_lag_time, plot = FALSE)

# Extract STACF values and create summary
cat("\n=== STACF RESULTS SUMMARY ===\n")

# Function to extract significant lags
extract_significant_lags <- function(stacf_result, weight_name) {
  stacf_matrix <- stacf_result
  significant_lags <- which(abs(stacf_matrix) > 0.1, arr.ind = TRUE)
  
  if (nrow(significant_lags) > 0) {
    sig_df <- data.frame(
      time_lag = significant_lags[,1],
      space_lag = significant_lags[,2] - 1,
      stacf_value = stacf_matrix[significant_lags],
      weight_type = weight_name
    )
    return(sig_df)
  } else {
    return(data.frame())
  }
}

# Extract significant lags for each weight type
sig_uniform <- extract_significant_lags(stacf_uniform, "Uniform")
sig_distance <- extract_significant_lags(stacf_distance, "Distance")  
sig_correlation <- extract_significant_lags(stacf_correlation, "Correlation")

# Combine results
all_significant <- rbind(sig_uniform, sig_distance, sig_correlation)

# Display significant lags
if (nrow(all_significant) > 0) {
  cat("Significant STACF lags (|STACF| > 0.1):\n")
  print(all_significant)
} else {
  cat("No significant STACF lags found.\n")
}

# MA Order Recommendation
cat("\n=== MA ORDER RECOMMENDATION ===\n")
if (nrow(all_significant) > 0) {
  max_time_lag <- max(all_significant$time_lag)
  max_space_lag <- max(all_significant$space_lag)
  cat("Recommended MA order: (", max_time_lag, ",", max_space_lag, ")\n")
} else {
  cat("Recommended MA order: (1,1) - default\n")
  max_time_lag <- 1
  max_space_lag <- 1
}

# Create visualization data
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Function to create STACF plot data
create_stacf_plot_data <- function(stacf_result, weight_name) {
  stacf_matrix <- stacf_result
  plot_data <- expand.grid(
    time_lag = 1:nrow(stacf_matrix),
    space_lag = 0:(ncol(stacf_matrix)-1)
  )
  plot_data$stacf_value <- as.vector(stacf_matrix)
  plot_data$weight_type <- weight_name
  plot_data$significant <- abs(plot_data$stacf_value) > 0.1
  return(plot_data)
}

# Create plot data for all weight types
plot_uniform <- create_stacf_plot_data(stacf_uniform, "Uniform")
plot_distance <- create_stacf_plot_data(stacf_distance, "Distance")
plot_correlation <- create_stacf_plot_data(stacf_correlation, "Correlation")

# Combine plot data
stacf_plot_data <- rbind(plot_uniform, plot_distance, plot_correlation)

# Create STACF plots (both traditional and heatmap)
cat("Creating STACF plots...\n")

# 1. TRADITIONAL BAR PLOTS (like ACF/PACF)
cat("Creating traditional STACF bar plots...\n")

# Function to create traditional STACF bar plot
create_traditional_stacf_plot <- function(stacf_matrix, weight_name, output_dir) {
  # Extract space lag 0 (same location) for traditional plot
  stacf_values <- stacf_matrix[, 1]  # Space lag 0
  time_lags <- 1:length(stacf_values)
  
  # Create significance threshold (rule of thumb: ±0.1 or ±1.96/sqrt(n))
  n_obs <- 96  # training observations
  threshold <- 1.96 / sqrt(n_obs)  # ≈ 0.2
  
  # Create data frame
  plot_data <- data.frame(
    Time_Lag = time_lags,
    STACF_Value = stacf_values,
    Significant = abs(stacf_values) > threshold
  )
  
  # Create bar plot
  p <- ggplot(plot_data, aes(x = Time_Lag, y = STACF_Value)) +
    geom_col(aes(fill = Significant), alpha = 0.7, width = 0.6) +
    geom_hline(yintercept = c(-threshold, threshold), 
               linetype = "dashed", color = "red", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "darkblue"),
                     name = "Significant") +
    scale_x_continuous(breaks = 1:12) +
    labs(title = paste("STACF Plot -", weight_name, "Weights (Space Lag 0)"),
         subtitle = paste("Red dashed lines: significance threshold (±", round(threshold, 3), ")"),
         x = "Time Lag", y = "STACF Value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
  
  # Save plot
  filename <- file.path(output_dir, paste0("stacf_traditional_", tolower(weight_name), ".png"))
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("✅ Saved:", filename, "\n")
  
  return(p)
}

# Create traditional bar plots
traditional_uniform <- create_traditional_stacf_plot(stacf_uniform, "Uniform", output_dir)
traditional_distance <- create_traditional_stacf_plot(stacf_distance, "Distance", output_dir)
traditional_correlation <- create_traditional_stacf_plot(stacf_correlation, "Correlation", output_dir)

# 2. HEATMAP PLOTS (for spatio-temporal visualization)
cat("Creating STACF heatmap plots...\n")

# Function to create heatmap for each weight type
create_stacf_heatmap <- function(stacf_matrix, weight_name, output_dir) {
  # Convert matrix to long format for ggplot
  plot_data <- expand.grid(
    Time_Lag = 1:nrow(stacf_matrix),
    Space_Lag = 0:(ncol(stacf_matrix)-1)
  )
  plot_data$STACF_Value <- as.vector(stacf_matrix)
  plot_data$Significant <- abs(plot_data$STACF_Value) > 0.1
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = Space_Lag, y = Time_Lag, fill = STACF_Value)) +
    geom_tile(color = "white", size = 0.1) +
    geom_text(aes(label = ifelse(Significant, "*", "")), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, name = "STACF\nValue") +
    scale_x_continuous(breaks = 0:1, labels = c("0", "1")) +
    scale_y_continuous(breaks = seq(1, 12, 2)) +
    labs(title = paste("STACF Heatmap -", weight_name, "Weights"),
         subtitle = "* indicates |STACF| > 0.1 (significant)",
         x = "Space Lag", y = "Time Lag") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 10))
  
  # Save plot
  filename <- file.path(output_dir, paste0("stacf_heatmap_", tolower(weight_name), ".png"))
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  cat("✅ Saved:", filename, "\n")
  
  return(p)
}

# Create individual heatmaps
heatmap_uniform <- create_stacf_heatmap(stacf_uniform, "Uniform", output_dir)
heatmap_distance <- create_stacf_heatmap(stacf_distance, "Distance", output_dir)
heatmap_correlation <- create_stacf_heatmap(stacf_correlation, "Correlation", output_dir)

# Create comparison plot
cat("Creating STACF comparison plot...\n")

# Line plot for time lags (space lag 0 vs 1)
comparison_data <- data.frame(
  Time_Lag = rep(1:12, 6),
  STACF_Value = c(stacf_uniform[,1], stacf_uniform[,2],
                  stacf_distance[,1], stacf_distance[,2],
                  stacf_correlation[,1], stacf_correlation[,2]),
  Weight_Type = rep(c("Uniform", "Distance", "Correlation"), each = 24),
  Space_Lag = rep(rep(c("Space Lag 0", "Space Lag 1"), each = 12), 3)
)

comparison_plot <- ggplot(comparison_data, aes(x = Time_Lag, y = STACF_Value, 
                                              color = Weight_Type, linetype = Space_Lag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c("Uniform" = "blue", "Distance" = "red", "Correlation" = "green")) +
  labs(title = "STACF Comparison Across Weight Types",
       subtitle = "Dashed lines indicate significance threshold (±0.1)",
       x = "Time Lag", y = "STACF Value",
       color = "Weight Type", linetype = "Space Lag") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "bottom")

# Save comparison plot
comparison_filename <- file.path(output_dir, "stacf_comparison.png")
ggsave(comparison_filename, comparison_plot, width = 12, height = 8, dpi = 300)
cat("✅ Saved:", comparison_filename, "\n")

# Create seasonal focus plot (highlight lag 12)
seasonal_data <- comparison_data[comparison_data$Time_Lag == 12, ]
seasonal_plot <- ggplot(seasonal_data, aes(x = Weight_Type, y = STACF_Value, 
                                          fill = Space_Lag)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(STACF_Value, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Space Lag 0" = "lightblue", "Space Lag 1" = "darkblue")) +
  labs(title = "Seasonal Pattern (Time Lag 12) - STACF Values",
       subtitle = "Strong negative correlation indicates seasonal dependency",
       x = "Weight Type", y = "STACF Value", fill = "Space Lag") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

# Save seasonal plot
seasonal_filename <- file.path(output_dir, "stacf_seasonal_lag12.png")
ggsave(seasonal_filename, seasonal_plot, width = 10, height = 6, dpi = 300)
cat("✅ Saved:", seasonal_filename, "\n")

# Create combined traditional comparison (all 3 weights in one plot)
cat("Creating combined traditional STACF comparison...\n")

combined_traditional_data <- data.frame(
  Time_Lag = rep(1:12, 3),
  STACF_Value = c(stacf_uniform[,1], stacf_distance[,1], stacf_correlation[,1]),
  Weight_Type = rep(c("Uniform", "Distance", "Correlation"), each = 12)
)

# Calculate significance threshold
n_obs <- 96
threshold <- 1.96 / sqrt(n_obs)

combined_traditional_plot <- ggplot(combined_traditional_data, aes(x = Time_Lag, y = STACF_Value, fill = Weight_Type)) +
  geom_col(position = "dodge", alpha = 0.7, width = 0.8) +
  geom_hline(yintercept = c(-threshold, threshold), 
             linetype = "dashed", color = "red", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("Uniform" = "lightblue", "Distance" = "lightcoral", "Correlation" = "lightgreen")) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "STACF Comparison - Traditional Bar Plot Style",
       subtitle = paste("Red dashed lines: significance threshold (±", round(threshold, 3), ") | Space Lag 0"),
       x = "Time Lag", y = "STACF Value", fill = "Weight Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "bottom")

# Save combined traditional plot
combined_traditional_filename <- file.path(output_dir, "stacf_traditional_comparison.png")
ggsave(combined_traditional_filename, combined_traditional_plot, width = 12, height = 8, dpi = 300)
cat("✅ Saved:", combined_traditional_filename, "\n")

# Save results
cat("\n=== SAVING RESULTS ===\n")

# Save STACF objects
save(stacf_uniform, stacf_distance, stacf_correlation,
     file = file.path(output_dir, "stacf_results.RData"))

# Save significant lags
write.csv(all_significant, file.path(output_dir, "significant_lags.csv"), row.names = FALSE)

# Save plot data
write.csv(stacf_plot_data, file.path(output_dir, "stacf_plot_data.csv"), row.names = FALSE)

# Save MA order recommendation
ma_recommendation <- data.frame(
  time_order = max_time_lag,
  space_order = max_space_lag,
  basis = ifelse(nrow(all_significant) > 0, "Significant lags", "Default")
)
write.csv(ma_recommendation, file.path(output_dir, "ma_order_recommendation.csv"), row.names = FALSE)

# Create summary report
cat("\n=== GENERATING REPORT ===\n")

report_content <- paste0(
  "STARMA STACF ANALYSIS REPORT\n",
  "============================\n\n",
  "Analysis Date: ", Sys.Date(), "\n",
  "Training Data: ", nrow(train_data), " time periods, ", ncol(train_data), " regions\n\n",
  "STACF Parameters:\n",
  "- Maximum time lags: ", max_lag_time, "\n",
  "- Maximum spatial lags: ", max_lag_space, "\n",
  "- Significance threshold: |STACF| > 0.1\n\n",
  "RESULTS:\n",
  "- Total significant lags found: ", nrow(all_significant), "\n",
  "- Recommended MA order: (", max_time_lag, ",", max_space_lag, ")\n\n",
  "SIGNIFICANT LAGS BY WEIGHT TYPE:\n",
  "- Uniform weights: ", nrow(sig_uniform), " significant lags\n",
  "- Distance weights: ", nrow(sig_distance), " significant lags\n",
  "- Correlation weights: ", nrow(sig_correlation), " significant lags\n\n",
  "FILES GENERATED:\n",
  "- stacf_results.RData: STACF computation results\n",
  "- significant_lags.csv: All significant STACF lags\n",
  "- stacf_plot_data.csv: Data for visualization\n",
  "- ma_order_recommendation.csv: Recommended MA order\n",
  "- stacf_analysis_report.txt: This report\n"
)

writeLines(report_content, file.path(output_dir, "stacf_analysis_report.txt"))

cat("\n🎉 STACF ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("📊 Training data:", nrow(train_data), "months ×", ncol(train_data), "regions\n")
cat("📈 STACF computed with 3 weight types\n")
cat("📊 Visualizations created: 9 plots generated\n")
cat("   - 3 traditional bar plots (ACF-style) - MAIN INTERPRETATION\n")
cat("   - 3 heatmap plots (spatio-temporal) - ADVANCED ANALYSIS\n")
cat("   - 1 comparison line plot\n")
cat("   - 1 seasonal pattern plot (lag 12)\n")
cat("   - 1 combined traditional comparison - BEST FOR MA ORDER\n")
cat("💾 Results saved in:", output_dir, "\n")
cat("🎯 Recommended MA order: (", max_time_lag, ",", max_space_lag, ")\n")
cat("🚀 Next step: Jalankan 06_STPACF_Analysis.R\n")