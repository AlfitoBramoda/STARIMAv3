# ============================================================================
# STARMA FORECASTING - STPACF ANALYSIS
# ============================================================================
# Purpose: Space-Time Partial AutoCorrelation Function Analysis for AR Order Identification
# Input: Training data from previous step
# Output: STPACF plots and AR order recommendations
# ============================================================================

# Clear environment and load libraries
rm(list = ls())
library(starma)
library(ggplot2)
library(gridExtra)

# Set working directory and create output folder
setwd("c:/Users/hp/Documents/Baby/STARMA")
output_dir <- "results/stationer/06_stpacf_analysis"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("=== STARMA STPACF ANALYSIS ===\n")
cat("Loading training data...\n")

# Load training data and spatial weights
load("results/stationer/data_split.RData")
load("results/stationer/spatial_weights.RData")

# Verify data structure
cat("Training data dimensions:", dim(train_data), "\n")
cat("Time periods:", nrow(train_data), "\n")
cat("Regions:", ncol(train_data), "\n")

# STPACF Analysis with different spatial weights
cat("\n=== COMPUTING STPACF ===\n")

# Parameters for STPACF
max_lag_time <- 12  # Maximum time lags
max_lag_space <- 2  # Maximum spatial lags

# Data format: rows=time, columns=sites (already correct format)
cat("Data format:", dim(train_data), "(time × regions)\n")

# Create proper wlist format (identity matrix first, then spatial weights)
I_matrix <- diag(5)  # Identity matrix for 5 regions

# 1. STPACF with Uniform Weights
cat("Computing STPACF with uniform weights...\n")
wlist_uniform_stpacf <- list(I_matrix, uniform_weights)
stpacf_uniform <- stpacf(train_data, wlist = wlist_uniform_stpacf, 
                        tlag.max = max_lag_time, plot = FALSE)

# 2. STPACF with Distance Weights  
cat("Computing STPACF with distance weights...\n")
wlist_distance_stpacf <- list(I_matrix, distance_weights)
stpacf_distance <- stpacf(train_data, wlist = wlist_distance_stpacf,
                         tlag.max = max_lag_time, plot = FALSE)

# 3. STPACF with Correlation Weights
cat("Computing STPACF with correlation weights...\n")
wlist_correlation_stpacf <- list(I_matrix, correlation_weights)
stpacf_correlation <- stpacf(train_data, wlist = wlist_correlation_stpacf,
                            tlag.max = max_lag_time, plot = FALSE)

# Extract STPACF values and create summary
cat("\n=== STPACF RESULTS SUMMARY ===\n")

# Function to extract significant lags
extract_significant_lags <- function(stpacf_result, weight_name) {
  stpacf_matrix <- stpacf_result
  significant_lags <- which(abs(stpacf_matrix) > 0.1, arr.ind = TRUE)
  
  if (nrow(significant_lags) > 0) {
    sig_df <- data.frame(
      time_lag = significant_lags[,1],
      space_lag = significant_lags[,2] - 1,
      stpacf_value = stpacf_matrix[significant_lags],
      weight_type = weight_name
    )
    return(sig_df)
  } else {
    return(data.frame())
  }
}

# Extract significant lags for each weight type
sig_uniform <- extract_significant_lags(stpacf_uniform, "Uniform")
sig_distance <- extract_significant_lags(stpacf_distance, "Distance")  
sig_correlation <- extract_significant_lags(stpacf_correlation, "Correlation")

# Combine results
all_significant <- rbind(sig_uniform, sig_distance, sig_correlation)

# Display significant lags
if (nrow(all_significant) > 0) {
  cat("Significant STPACF lags (|STPACF| > 0.1):\n")
  print(all_significant)
} else {
  cat("No significant STPACF lags found.\n")
}

# AR Order Recommendation
cat("\n=== AR ORDER RECOMMENDATION ===\n")
if (nrow(all_significant) > 0) {
  max_time_lag <- max(all_significant$time_lag)
  max_space_lag <- max(all_significant$space_lag)
  cat("Recommended AR order: (", max_time_lag, ",", max_space_lag, ")\n")
} else {
  cat("Recommended AR order: (1,1) - default\n")
  max_time_lag <- 1
  max_space_lag <- 1
}

# Create visualization data
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Function to create STPACF plot data
create_stpacf_plot_data <- function(stpacf_result, weight_name) {
  stpacf_matrix <- stpacf_result
  plot_data <- expand.grid(
    time_lag = 1:nrow(stpacf_matrix),
    space_lag = 0:(ncol(stpacf_matrix)-1)
  )
  plot_data$stpacf_value <- as.vector(stpacf_matrix)
  plot_data$weight_type <- weight_name
  plot_data$significant <- abs(plot_data$stpacf_value) > 0.1
  return(plot_data)
}

# Create plot data for all weight types
plot_uniform <- create_stpacf_plot_data(stpacf_uniform, "Uniform")
plot_distance <- create_stpacf_plot_data(stpacf_distance, "Distance")
plot_correlation <- create_stpacf_plot_data(stpacf_correlation, "Correlation")

# Combine plot data
stpacf_plot_data <- rbind(plot_uniform, plot_distance, plot_correlation)

# Create STPACF plots (both traditional and heatmap)
cat("Creating STPACF plots...\n")

# 1. TRADITIONAL BAR PLOTS (like ACF/PACF)
cat("Creating traditional STPACF bar plots...\n")

# Function to create traditional STPACF bar plot
create_traditional_stpacf_plot <- function(stpacf_matrix, weight_name, output_dir) {
  # Extract space lag 0 (same location) for traditional plot
  stpacf_values <- stpacf_matrix[, 1]  # Space lag 0
  time_lags <- 1:length(stpacf_values)
  
  # Create significance threshold (rule of thumb: ±1.96/sqrt(n))
  n_obs <- 96  # training observations
  threshold <- 1.96 / sqrt(n_obs)  # ≈ 0.2
  
  # Create data frame
  plot_data <- data.frame(
    Time_Lag = time_lags,
    STPACF_Value = stpacf_values,
    Significant = abs(stpacf_values) > threshold
  )
  
  # Create bar plot
  p <- ggplot(plot_data, aes(x = Time_Lag, y = STPACF_Value)) +
    geom_col(aes(fill = Significant), alpha = 0.7, width = 0.6) +
    geom_hline(yintercept = c(-threshold, threshold), 
               linetype = "dashed", color = "red", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    scale_fill_manual(values = c("FALSE" = "lightcoral", "TRUE" = "darkred"),
                     name = "Significant") +
    scale_x_continuous(breaks = 1:12) +
    labs(title = paste("STPACF Plot -", weight_name, "Weights (Space Lag 0)"),
         subtitle = paste("Red dashed lines: significance threshold (±", round(threshold, 3), ")"),
         x = "Time Lag", y = "STPACF Value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
  
  # Save plot
  filename <- file.path(output_dir, paste0("stpacf_traditional_", tolower(weight_name), ".png"))
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("✅ Saved:", filename, "\n")
  
  return(p)
}

# Create traditional bar plots
traditional_uniform <- create_traditional_stpacf_plot(stpacf_uniform, "Uniform", output_dir)
traditional_distance <- create_traditional_stpacf_plot(stpacf_distance, "Distance", output_dir)
traditional_correlation <- create_traditional_stpacf_plot(stpacf_correlation, "Correlation", output_dir)

# 2. HEATMAP PLOTS (for spatio-temporal visualization)
cat("Creating STPACF heatmap plots...\n")

# Function to create heatmap for each weight type
create_stpacf_heatmap <- function(stpacf_matrix, weight_name, output_dir) {
  # Convert matrix to long format for ggplot
  plot_data <- expand.grid(
    Time_Lag = 1:nrow(stpacf_matrix),
    Space_Lag = 0:(ncol(stpacf_matrix)-1)
  )
  plot_data$STPACF_Value <- as.vector(stpacf_matrix)
  plot_data$Significant <- abs(plot_data$STPACF_Value) > 0.1
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = Space_Lag, y = Time_Lag, fill = STPACF_Value)) +
    geom_tile(color = "white", size = 0.1) +
    geom_text(aes(label = ifelse(Significant, "*", "")), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, name = "STPACF\nValue") +
    scale_x_continuous(breaks = 0:1, labels = c("0", "1")) +
    scale_y_continuous(breaks = seq(1, 12, 2)) +
    labs(title = paste("STPACF Heatmap -", weight_name, "Weights"),
         subtitle = "* indicates |STPACF| > 0.1 (significant)",
         x = "Space Lag", y = "Time Lag") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 10))
  
  # Save plot
  filename <- file.path(output_dir, paste0("stpacf_heatmap_", tolower(weight_name), ".png"))
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  cat("✅ Saved:", filename, "\n")
  
  return(p)
}

# Create individual heatmaps
heatmap_uniform <- create_stpacf_heatmap(stpacf_uniform, "Uniform", output_dir)
heatmap_distance <- create_stpacf_heatmap(stpacf_distance, "Distance", output_dir)
heatmap_correlation <- create_stpacf_heatmap(stpacf_correlation, "Correlation", output_dir)

# Create comparison plot
cat("Creating STPACF comparison plot...\n")

# Line plot for time lags (space lag 0 vs 1)
comparison_data <- data.frame(
  Time_Lag = rep(1:12, 6),
  STPACF_Value = c(stpacf_uniform[,1], stpacf_uniform[,2],
                   stpacf_distance[,1], stpacf_distance[,2],
                   stpacf_correlation[,1], stpacf_correlation[,2]),
  Weight_Type = rep(c("Uniform", "Distance", "Correlation"), each = 24),
  Space_Lag = rep(rep(c("Space Lag 0", "Space Lag 1"), each = 12), 3)
)

comparison_plot <- ggplot(comparison_data, aes(x = Time_Lag, y = STPACF_Value, 
                                              color = Weight_Type, linetype = Space_Lag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c("Uniform" = "blue", "Distance" = "red", "Correlation" = "green")) +
  labs(title = "STPACF Comparison Across Weight Types",
       subtitle = "Dashed lines indicate significance threshold (±0.1)",
       x = "Time Lag", y = "STPACF Value",
       color = "Weight Type", linetype = "Space Lag") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "bottom")

# Save comparison plot
comparison_filename <- file.path(output_dir, "stpacf_comparison.png")
ggsave(comparison_filename, comparison_plot, width = 12, height = 8, dpi = 300)
cat("✅ Saved:", comparison_filename, "\n")

# Create STACF vs STPACF comparison (if STACF results exist)
if (file.exists("results/stationer/05_stacf_analysis/stacf_results.RData")) {
  cat("Creating STACF vs STPACF comparison...\n")
  load("results/stationer/05_stacf_analysis/stacf_results.RData")
  
  # Compare uniform weights only for clarity
  comparison_acf_pacf <- data.frame(
    Time_Lag = rep(1:12, 4),
    Value = c(stacf_uniform[,1], stacf_uniform[,2],
              stpacf_uniform[,1], stpacf_uniform[,2]),
    Function_Type = rep(c("STACF", "STPACF"), each = 24),
    Space_Lag = rep(rep(c("Space Lag 0", "Space Lag 1"), each = 12), 2)
  )
  
  acf_pacf_plot <- ggplot(comparison_acf_pacf, aes(x = Time_Lag, y = Value, 
                                                   color = Function_Type, linetype = Space_Lag)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
    scale_x_continuous(breaks = 1:12) +
    scale_color_manual(values = c("STACF" = "blue", "STPACF" = "red")) +
    labs(title = "STACF vs STPACF Comparison (Uniform Weights)",
         subtitle = "STACF identifies MA order, STPACF identifies AR order",
         x = "Time Lag", y = "Correlation Value",
         color = "Function Type", linetype = "Space Lag") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "bottom")
  
  # Save ACF vs PACF plot
  acf_pacf_filename <- file.path(output_dir, "stacf_vs_stpacf_comparison.png")
  ggsave(acf_pacf_filename, acf_pacf_plot, width = 12, height = 8, dpi = 300)
  cat("✅ Saved:", acf_pacf_filename, "\n")
}

# Create combined traditional comparison (all 3 weights in one plot)
cat("Creating combined traditional STPACF comparison...\n")

combined_traditional_data <- data.frame(
  Time_Lag = rep(1:12, 3),
  STPACF_Value = c(stpacf_uniform[,1], stpacf_distance[,1], stpacf_correlation[,1]),
  Weight_Type = rep(c("Uniform", "Distance", "Correlation"), each = 12)
)

# Calculate significance threshold
n_obs <- 96
threshold <- 1.96 / sqrt(n_obs)

combined_traditional_plot <- ggplot(combined_traditional_data, aes(x = Time_Lag, y = STPACF_Value, fill = Weight_Type)) +
  geom_col(position = "dodge", alpha = 0.7, width = 0.8) +
  geom_hline(yintercept = c(-threshold, threshold), 
             linetype = "dashed", color = "red", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("Uniform" = "lightcoral", "Distance" = "darkred", "Correlation" = "pink")) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "STPACF Comparison - Traditional Bar Plot Style",
       subtitle = paste("Red dashed lines: significance threshold (±", round(threshold, 3), ") | Space Lag 0"),
       x = "Time Lag", y = "STPACF Value", fill = "Weight Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "bottom")

# Save combined traditional plot
combined_traditional_filename <- file.path(output_dir, "stpacf_traditional_comparison.png")
ggsave(combined_traditional_filename, combined_traditional_plot, width = 12, height = 8, dpi = 300)
cat("✅ Saved:", combined_traditional_filename, "\n")

# Save results
cat("\n=== SAVING RESULTS ===\n")

# Save STPACF objects
save(stpacf_uniform, stpacf_distance, stpacf_correlation,
     file = file.path(output_dir, "stpacf_results.RData"))

# Save significant lags
write.csv(all_significant, file.path(output_dir, "significant_lags.csv"), row.names = FALSE)

# Save plot data
write.csv(stpacf_plot_data, file.path(output_dir, "stpacf_plot_data.csv"), row.names = FALSE)

# Save AR order recommendation
ar_recommendation <- data.frame(
  time_order = max_time_lag,
  space_order = max_space_lag,
  basis = ifelse(nrow(all_significant) > 0, "Significant lags", "Default")
)
write.csv(ar_recommendation, file.path(output_dir, "ar_order_recommendation.csv"), row.names = FALSE)

# Create summary report
cat("\n=== GENERATING REPORT ===\n")

report_content <- paste0(
  "STARMA STPACF ANALYSIS REPORT\n",
  "=============================\n\n",
  "Analysis Date: ", Sys.Date(), "\n",
  "Training Data: ", nrow(train_data), " time periods, ", ncol(train_data), " regions\n\n",
  "STPACF Parameters:\n",
  "- Maximum time lags: ", max_lag_time, "\n",
  "- Maximum spatial lags: ", max_lag_space, "\n",
  "- Significance threshold: |STPACF| > 0.1\n\n",
  "RESULTS:\n",
  "- Total significant lags found: ", nrow(all_significant), "\n",
  "- Recommended AR order: (", max_time_lag, ",", max_space_lag, ")\n\n",
  "SIGNIFICANT LAGS BY WEIGHT TYPE:\n",
  "- Uniform weights: ", nrow(sig_uniform), " significant lags\n",
  "- Distance weights: ", nrow(sig_distance), " significant lags\n",
  "- Correlation weights: ", nrow(sig_correlation), " significant lags\n\n",
  "VISUALIZATIONS CREATED:\n",
  "- 3 traditional STPACF bar plots (PACF-style)\n",
  "- 3 STPACF heatmap plots (spatio-temporal)\n",
  "- 1 STPACF comparison line plot\n",
  "- 1 STACF vs STPACF comparison plot\n",
  "- 1 combined traditional comparison plot\n\n",
  "FILES GENERATED:\n",
  "- stpacf_results.RData: STPACF computation results\n",
  "- significant_lags.csv: All significant STPACF lags\n",
  "- stpacf_plot_data.csv: Data for visualization\n",
  "- ar_order_recommendation.csv: Recommended AR order\n",
  "- stpacf_analysis_report.txt: This report\n",
  "- Multiple PNG plot files (9+ visualizations)\n"
)

writeLines(report_content, file.path(output_dir, "stpacf_analysis_report.txt"))

cat("\n🎉 STPACF ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("📊 Training data:", nrow(train_data), "months ×", ncol(train_data), "regions\n")
cat("📈 STPACF computed with 3 weight types\n")
cat("📊 Visualizations created: 9+ plots generated\n")
cat("   - 3 traditional bar plots (PACF-style) - MAIN INTERPRETATION\n")
cat("   - 3 heatmap plots (spatio-temporal) - ADVANCED ANALYSIS\n")
cat("   - 1 comparison line plot\n")
cat("   - 1 STACF vs STPACF comparison plot\n")
cat("   - 1 combined traditional comparison - BEST FOR AR ORDER\n")
cat("💾 Results saved in:", output_dir, "\n")
cat("🎯 Recommended AR order: (", max_time_lag, ",", max_space_lag, ")\n")
cat("🚀 Next step: Jalankan 07_Model_Structure.R\n")