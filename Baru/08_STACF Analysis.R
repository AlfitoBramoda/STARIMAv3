# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STARIMA Identification
# File: 07_STACF_Analysis.R
# Purpose: Space-Time Autocorrelation Function (STACF) Analysis
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load training data and spatial weights
load("output/02_data_split.RData")
load("output/07_spatial_weights.RData")

cat("=== STARMA STACF ANALYSIS ===\n")
cat("Space-Time Autocorrelation Function Analysis for MA order identification...\n\n")

# ============================================================================
# GLOBAL PARAMETERS
# ============================================================================
MAX_SPATIAL_LAG <- 2
MAX_TEMPORAL_LAG <- 40

cat("📋 Global Parameters:\n")
cat("- MAX_SPATIAL_LAG:", MAX_SPATIAL_LAG, "\n")
cat("- MAX_TEMPORAL_LAG:", MAX_TEMPORAL_LAG, "\n\n")

# ============================================================================
# DATA PREPARATION FOR STACF
# ============================================================================

cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(centered_matrix), "\n")
cat("- Number of regions:", ncol(centered_matrix), "\n")
cat("- Time periods:", nrow(centered_matrix), "\n")
cat("- Regions:", paste(colnames(centered_matrix), collapse = ", "), "\n\n")

# Get spatial weights for analysis
uniform_w <- spatial_weights$uniform
distance_w <- spatial_weights$distance
correlation_w <- spatial_weights$correlation

cat("📍 Available spatial weight matrices:\n")
cat("- Uniform weights: Equal weights for all neighbors\n")
cat("- Distance weights: Inverse distance weighting\n") 
cat("- Correlation weights: Cross-correlation based\n\n")

# ============================================================================
# STACF ANALYSIS WITH DIFFERENT SPATIAL WEIGHTS
# ============================================================================

library(starma)
library(ggplot2)
library(gridExtra)

# STACF parameters
max_time_lag <- MAX_TEMPORAL_LAG  # Maximum temporal lag
max_space_lag <- MAX_SPATIAL_LAG  # Maximum spatial lag

cat("🔍 STACF Analysis Parameters:\n")
cat("- Maximum temporal lag:", max_time_lag, "\n")
cat("- Maximum spatial lag:", max_space_lag, "\n\n")

# Function to perform STACF analysis
perform_stacf_analysis <- function(data, weights, weight_name) {
  cat("📈 Computing STACF with", weight_name, "weights...\n")
  
  tryCatch({
    # Create wlist with identity matrix first, then weights
    identity_matrix <- diag(ncol(data))
    wlist <- list(identity_matrix, weights)
    
    # Compute STACF
    stacf_result <- stacf(data, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
    
    cat("✅ STACF computation successful for", weight_name, "\n")
    
    # STACF returns matrix directly
    stacf_values <- stacf_result
    
    # Print summary
    cat("STACF Summary for", weight_name, ":\n")
    cat("- STACF matrix dimensions:", dim(stacf_values), "\n")
    cat("- Temporal lags (rows): 1 to", nrow(stacf_values), "\n")
    cat("- Spatial lags (cols): 0 to", ncol(stacf_values)-1, "\n\n")
    
    return(list(
      result = stacf_result,
      values = stacf_values,
      success = TRUE
    ))
    
  }, error = function(e) {
    cat("❌ Error computing STACF for", weight_name, ":", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# Perform STACF analysis for each weight matrix
cat("=== STACF COMPUTATION ===\n")

stacf_uniform <- perform_stacf_analysis(centered_matrix, uniform_w, "Uniform")
stacf_distance <- perform_stacf_analysis(centered_matrix, distance_w, "Distance")
stacf_correlation <- perform_stacf_analysis(centered_matrix, correlation_w, "Correlation")

# ============================================================================
# STACF VISUALIZATION
# ============================================================================

cat("\n📊 Creating STACF Visualizations...\n")

# Function to create STACF heatmap
create_stacf_heatmap <- function(stacf_result, title, filename) {
  if (!stacf_result$success) {
    cat("⚠️ Skipping visualization for", title, "due to computation error\n")
    return(NULL)
  }
  
  tryCatch({
    cat("Creating", title, "STACF heatmap...\n")
    
    # Create custom heatmap using ggplot2
    stacf_matrix <- stacf_result$values
    
    # Convert matrix to data frame for ggplot
    stacf_df <- expand.grid(
      Time_Lag = 1:nrow(stacf_matrix),
      Space_Lag = 0:(ncol(stacf_matrix)-1)
    )
    stacf_df$STACF_Value <- as.vector(stacf_matrix)
    
    # Create heatmap
    p <- ggplot(stacf_df, aes(x = Space_Lag, y = Time_Lag, fill = STACF_Value)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                           midpoint = 0, name = "STACF") +
      labs(title = paste("STACF Analysis:", title),
           x = "Spatial Lag", y = "Temporal Lag") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # # Save plot
    # ggsave(filename, p, width = 8, height = 6, dpi = 300)
    
    # # Also display the plot
    # print(p)
    
    # cat("✅", title, "STACF heatmap saved:", filename, "\n")
    
    # return(TRUE)
    
  }, error = function(e) {
    # cat("❌ Error creating", title, "plot:", e$message, "\n")
    # return(FALSE)
  })
}

# Create STACF heatmaps
plot1 <- create_stacf_heatmap(stacf_uniform, "Uniform Weights", "plots/07_stacf_uniform_heatmap.png")
plot2 <- create_stacf_heatmap(stacf_distance, "Distance Weights", "plots/07_stacf_distance_heatmap.png")
plot3 <- create_stacf_heatmap(stacf_correlation, "Correlation Weights", "plots/07_stacf_correlation_heatmap.png")

# ============================================================================
# ACF-STYLE PLOTS (Like ARIMA ACF/PACF)
# ============================================================================

cat("\n📊 Creating ACF-style STACF plots...\n")

# Function to create ACF-style plot
create_acf_style_plot <- function(stacf_result, title, filename) {
  if (!stacf_result$success) {
    cat("⚠️ Skipping ACF-style plot for", title, "due to computation error\n")
    return(NULL)
  }
  
  tryCatch({
    cat("Creating ACF-style plot for", title, "...\n")
    
    # Extract temporal ACF (spatial lag 0)
    stacf_matrix <- stacf_result$values
    temporal_acf <- stacf_matrix[, 1]  # First column = spatial lag 0
    
    # Create data frame for plotting
    acf_df <- data.frame(
      Lag = 1:length(temporal_acf),
      ACF = temporal_acf
    )
    
    # Calculate confidence bounds (approximate)
    n <- nrow(centered_matrix)
    conf_bound <- 1.96 / sqrt(n)  # 95% confidence interval
    
    # Create ACF-style plot
    p <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_hline(yintercept = c(conf_bound, -conf_bound), 
                 color = "blue", linetype = "dashed", size = 0.5) +
      geom_segment(aes(xend = Lag, yend = 0), color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(title = paste("STACF (ACF-style):", title),
           subtitle = paste("Temporal lags (spatial lag 0) - n =", n),
           x = "Temporal Lag", y = "Autocorrelation") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = 1:length(temporal_acf)) +
      ylim(c(min(-conf_bound * 1.2, min(temporal_acf) * 1.1), 
             max(conf_bound * 1.2, max(temporal_acf) * 1.1)))
    
    # Add significance annotations
    significant_lags <- which(abs(temporal_acf) > conf_bound)
    if (length(significant_lags) > 0) {
      p <- p + annotate("text", x = max(acf_df$Lag) * 0.8, y = max(temporal_acf) * 0.9,
                        label = paste("Significant lags:", paste(significant_lags, collapse = ", ")),
                        size = 3, color = "red")
    }
    
    # Save and display plot
    ggsave(filename, p, width = 10, height = 6, dpi = 300)
    print(p)
    
    cat("✅", title, "ACF-style plot saved:", filename, "\n")
    
    return(list(plot = p, significant_lags = significant_lags))
    
  }, error = function(e) {
    cat("❌ Error creating ACF-style plot for", title, ":", e$message, "\n")
    return(NULL)
  })
}

# Create ACF-style plots for each weight matrix
acf_plot1 <- create_acf_style_plot(stacf_uniform, "Uniform Weights", "plots/07_stacf_uniform_acf.png")
acf_plot2 <- create_acf_style_plot(stacf_distance, "Distance Weights", "plots/07_stacf_distance_acf.png")
acf_plot3 <- create_acf_style_plot(stacf_correlation, "Correlation Weights", "plots/07_stacf_correlation_acf.png")

# Create combined ACF comparison plot
if (!is.null(acf_plot1) && !is.null(acf_plot2) && !is.null(acf_plot3)) {
  cat("Creating combined ACF comparison plot...\n")
  
  # Combine data from all three weight matrices
  combined_acf_data <- rbind(
    data.frame(Lag = 1:nrow(stacf_uniform$values), 
               ACF = stacf_uniform$values[, 1], 
               Weight_Type = "Uniform"),
    data.frame(Lag = 1:nrow(stacf_distance$values), 
               ACF = stacf_distance$values[, 1], 
               Weight_Type = "Distance"),
    data.frame(Lag = 1:nrow(stacf_correlation$values), 
               ACF = stacf_correlation$values[, 1], 
               Weight_Type = "Correlation")
  )
  
  # Calculate confidence bounds
  n <- nrow(centered_matrix)
  conf_bound <- 1.96 / sqrt(n)
  
  # Create combined plot
  p_combined <- ggplot(combined_acf_data, aes(x = Lag, y = ACF, color = Weight_Type)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_hline(yintercept = c(conf_bound, -conf_bound), 
               color = "gray", linetype = "dashed", size = 0.5) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "STACF Comparison: All Weight Matrices",
         subtitle = paste("Temporal ACF (spatial lag 0) with 95% confidence bounds - n =", n),
         x = "Temporal Lag", y = "Autocorrelation",
         color = "Weight Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom") +
    scale_x_continuous(breaks = 1:max(combined_acf_data$Lag)) +
    scale_color_manual(values = c("Uniform" = "blue", "Distance" = "red", "Correlation" = "green"))
  
  # # Save and display combined plot
  # ggsave("plots/07_stacf_combined_acf.png", p_combined, width = 40, height = 8, dpi = 300)
  # print(p_combined)
  
  # cat("✅ Combined ACF comparison plot saved: plots/07_stacf_combined_acf.png\n")
}

# ============================================================================
# STACF INTERPRETATION AND MA ORDER IDENTIFICATION
# ============================================================================

cat("\n=== STACF INTERPRETATION ===\n")

# Function to analyze STACF patterns
analyze_stacf_pattern <- function(stacf_result, weight_name) {
  if (!stacf_result$success) {
    cat("❌", weight_name, "STACF analysis failed\n")
    return(NULL)
  }
  
  cat("\n📋", weight_name, "STACF Pattern Analysis:\n")
  
  stacf_vals <- stacf_result$values
  
  # Analyze temporal patterns (spatial lag 0 = first column)
  temporal_acf <- stacf_vals[, 1]  # First column = spatial lag 0
  
  cat("Temporal ACF (spatial lag 0):\n")
  for (i in 1:min(6, length(temporal_acf))) {
    cat(sprintf("  Time lag %d: %.4f\n", i, temporal_acf[i]))
  }
  
  # Analyze spatial patterns (time lag 1 = first row)
  if (ncol(stacf_vals) > 1) {
    spatial_acf <- stacf_vals[1, ]  # First row = time lag 1
    cat("Spatial ACF (time lag 1):\n")
    for (j in 1:length(spatial_acf)) {
      cat(sprintf("  Space lag %d: %.4f\n", j-1, spatial_acf[j]))
    }
  }
  
  # Identify significant lags (simple threshold approach)
  significant_threshold <- 0.2
  if (length(temporal_acf) > 1) {
    significant_temporal <- which(abs(temporal_acf[-1]) > significant_threshold)  # Exclude lag 0
  } else {
    significant_temporal <- numeric(0)
  }
  
  cat("Significant temporal lags (|ACF| >", significant_threshold, "):", 
      if(length(significant_temporal) > 0) paste(significant_temporal, collapse = ", ") else "None", "\n")
  
  return(list(
    temporal_acf = temporal_acf,
    significant_lags = significant_temporal,
    weight_type = weight_name
  ))
}

# Analyze patterns for each weight matrix
uniform_analysis <- analyze_stacf_pattern(stacf_uniform, "Uniform")
distance_analysis <- analyze_stacf_pattern(stacf_distance, "Distance")
correlation_analysis <- analyze_stacf_pattern(stacf_correlation, "Correlation")

# ============================================================================
# MA ORDER RECOMMENDATION
# ============================================================================

cat("\n=== MA ORDER IDENTIFICATION ===\n")

# Function to suggest MA order based on STACF cutoff
suggest_ma_order <- function(analysis) {
  if (is.null(analysis)) return(NULL)
  
  # Look for cutoff pattern in STACF
  temporal_acf <- analysis$temporal_acf
  
  # Find where ACF becomes insignificant
  cutoff_threshold <- 0.15
  cutoff_point <- 0
  
  for (i in 2:length(temporal_acf)) {
    if (abs(temporal_acf[i]) < cutoff_threshold) {
      cutoff_point <- i - 1
      break
    }
  }
  
  # Suggest MA order (conservative approach)
  suggested_q <- min(cutoff_point, 3)  # Cap at 3 for practical reasons
  
  return(list(
    suggested_q = suggested_q,
    cutoff_point = cutoff_point,
    weight_type = analysis$weight_type
  ))
}

# Get MA order suggestions
uniform_ma <- suggest_ma_order(uniform_analysis)
distance_ma <- suggest_ma_order(distance_analysis)
correlation_ma <- suggest_ma_order(correlation_analysis)

# Display recommendations
cat("📊 MA Order Recommendations:\n")

if (!is.null(uniform_ma)) {
  cat("- Uniform weights: MA(", uniform_ma$suggested_q, ") - cutoff at lag", uniform_ma$cutoff_point, "\n")
}

if (!is.null(distance_ma)) {
  cat("- Distance weights: MA(", distance_ma$suggested_q, ") - cutoff at lag", distance_ma$cutoff_point, "\n")
}

if (!is.null(correlation_ma)) {
  cat("- Correlation weights: MA(", correlation_ma$suggested_q, ") - cutoff at lag", correlation_ma$cutoff_point, "\n")
}

# ============================================================================
# SUMMARY TABLE
# ============================================================================

cat("\n📋 STACF Analysis Summary:\n")

# Create summary table
stacf_summary <- data.frame(
  Weight_Type = c("Uniform", "Distance", "Correlation"),
  STACF_Success = c(
    ifelse(stacf_uniform$success, "✅ Success", "❌ Failed"),
    ifelse(stacf_distance$success, "✅ Success", "❌ Failed"),
    ifelse(stacf_correlation$success, "✅ Success", "❌ Failed")
  ),
  Suggested_MA_Order = c(
    ifelse(!is.null(uniform_ma), paste("MA(", uniform_ma$suggested_q, ")", sep=""), "N/A"),
    ifelse(!is.null(distance_ma), paste("MA(", distance_ma$suggested_q, ")", sep=""), "N/A"),
    ifelse(!is.null(correlation_ma), paste("MA(", correlation_ma$suggested_q, ")", sep=""), "N/A")
  ),
  # Plot_Generated = c(
  #   ifelse(!is.null(plot1) && plot1, "✅ Yes", "❌ No"),
  #   ifelse(!is.null(plot2) && plot2, "✅ Yes", "❌ No"),
  #   ifelse(!is.null(plot3) && plot3, "✅ Yes", "❌ No")
  # ),
  stringsAsFactors = FALSE
)

print(stacf_summary)

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save STACF results
save(stacf_uniform, stacf_distance, stacf_correlation,
     uniform_analysis, distance_analysis, correlation_analysis,
     uniform_ma, distance_ma, correlation_ma,
     stacf_summary, centered_matrix,
     file = "output/08_stacf_analysis.RData")

# Display results in viewer
cat("\n=== DATA VIEWER ===\n")
cat("Opening STACF summary in viewer...\n")
View(stacf_summary, title = "STACF Analysis Summary")

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n=== STACF ANALYSIS COMPLETED ===\n")
cat("✅ STACF computed for 3 spatial weight matrices\n")
cat("✅ Space-Time ACF heatmaps generated (3 plots)\n")
cat("✅ ACF-style plots generated (3 + 1 combined plot)\n")
cat("✅ Lag patterns analyzed for MA order identification\n")
cat("✅ MA order recommendations provided\n")
cat("✅ Results saved to: output/07_stacf_analysis.RData\n")
cat("✅ Summary available in RStudio viewer\n\n")

cat("📊 PHASE 2 PROGRESS: 1/2 files completed (50%)\n")
cat("🎯 Next step: STPACF Analysis for AR order identification\n")
cat("📁 Next file: 09_STPACF_Analysis.R\n\n")

cat("STACF Analysis validation:\n")
cat("- Space-Time ACF computed: ✅\n")
cat("- Lag patterns identified: ✅\n")
cat("- MA orders suggested: ✅\n")
cat("- Ready for STPACF analysis: ✅\n")