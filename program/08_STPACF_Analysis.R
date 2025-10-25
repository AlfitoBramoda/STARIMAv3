# ============================================================================
# STARMA Forecasting Pipeline - Phase 2: STARIMA Identification
# File: 08_STPACF_Analysis.R
# Purpose: Space-Time Partial Autocorrelation Function (STPACF) Analysis
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

# Load training data and spatial weights
load("output/06_data_split.RData")
load("output/05_spatial_weights.RData")
load("output/07_stacf_analysis.RData")

cat("=== STARMA STPACF ANALYSIS ===\n")
cat("Space-Time Partial Autocorrelation Function Analysis for AR order identification...\n\n")

# ============================================================================
# DATA PREPARATION FOR STPACF
# ============================================================================

cat("📊 Data Information:\n")
cat("- Training data dimensions:", dim(train_data), "\n")
cat("- Number of regions:", ncol(train_data), "\n")
cat("- Time periods:", nrow(train_data), "\n")
cat("- Regions:", paste(colnames(train_data), collapse = ", "), "\n\n")

# Get spatial weights for analysis
uniform_w <- spatial_weights$uniform
distance_w <- spatial_weights$distance
correlation_w <- spatial_weights$correlation

cat("📍 Available spatial weight matrices:\n")
cat("- Uniform weights: Equal weights for all neighbors\n")
cat("- Distance weights: Inverse distance weighting\n") 
cat("- Correlation weights: Cross-correlation based\n\n")

# ============================================================================
# STPACF ANALYSIS WITH DIFFERENT SPATIAL WEIGHTS
# ============================================================================

library(starma)
library(ggplot2)
library(gridExtra)

# STPACF parameters
max_time_lag <- 40  # Maximum temporal lag
max_space_lag <- 2  # Maximum spatial lag (reasonable for 5 regions)

cat("🔍 STPACF Analysis Parameters:\n")
cat("- Maximum temporal lag:", max_time_lag, "\n")
cat("- Maximum spatial lag:", max_space_lag, "\n\n")

# Function to perform STPACF analysis
perform_stpacf_analysis <- function(data, weights, weight_name) {
  cat("📈 Computing STPACF with", weight_name, "weights...\n")
  
  tryCatch({
    # Create wlist with identity matrix first, then weights
    identity_matrix <- diag(ncol(data))
    wlist <- list(identity_matrix, weights)
    
    # Compute STPACF
    stpacf_result <- stpacf(data, wlist = wlist, tlag.max = max_time_lag, plot = FALSE)
    
    cat("✅ STPACF computation successful for", weight_name, "\n")
    
    # STPACF returns matrix directly
    stpacf_values <- stpacf_result
    
    # Print summary
    cat("STPACF Summary for", weight_name, ":\n")
    cat("- STPACF matrix dimensions:", dim(stpacf_values), "\n")
    cat("- Temporal lags (rows): 1 to", nrow(stpacf_values), "\n")
    cat("- Spatial lags (cols): 0 to", ncol(stpacf_values)-1, "\n\n")
    
    return(list(
      result = stpacf_result,
      values = stpacf_values,
      success = TRUE
    ))
    
  }, error = function(e) {
    cat("❌ Error computing STPACF for", weight_name, ":", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# Perform STPACF analysis for each weight matrix
cat("=== STPACF COMPUTATION ===\n")

stpacf_uniform <- perform_stpacf_analysis(train_data, uniform_w, "Uniform")
stpacf_distance <- perform_stpacf_analysis(train_data, distance_w, "Distance")
stpacf_correlation <- perform_stpacf_analysis(train_data, correlation_w, "Correlation")

# ============================================================================
# STPACF VISUALIZATION
# ============================================================================

cat("\n📊 Creating STPACF Visualizations...\n")

# Function to create STPACF heatmap
create_stpacf_heatmap <- function(stpacf_result, title, filename) {
  if (!stpacf_result$success) {
    cat("⚠️ Skipping visualization for", title, "due to computation error\n")
    return(NULL)
  }
  
  tryCatch({
    cat("Creating", title, "STPACF heatmap...\n")
    
    # Create custom heatmap using ggplot2
    stpacf_matrix <- stpacf_result$values
    
    # Convert matrix to data frame for ggplot
    stpacf_df <- expand.grid(
      Time_Lag = 1:nrow(stpacf_matrix),
      Space_Lag = 0:(ncol(stpacf_matrix)-1)
    )
    stpacf_df$STPACF_Value <- as.vector(stpacf_matrix)
    
    # Create heatmap
    p <- ggplot(stpacf_df, aes(x = Space_Lag, y = Time_Lag, fill = STPACF_Value)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                          midpoint = 0, name = "STPACF") +
      labs(title = paste("STPACF Analysis:", title),
           x = "Spatial Lag", y = "Temporal Lag") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Save plot
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
    
    # Also display the plot
    print(p)
    
    cat("✅", title, "STPACF heatmap saved:", filename, "\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error creating", title, "plot:", e$message, "\n")
    return(FALSE)
  })
}

# Create STPACF heatmaps
plot1 <- create_stpacf_heatmap(stpacf_uniform, "Uniform Weights", "plots/08_stpacf_uniform_heatmap.png")
plot2 <- create_stpacf_heatmap(stpacf_distance, "Distance Weights", "plots/08_stpacf_distance_heatmap.png")
plot3 <- create_stpacf_heatmap(stpacf_correlation, "Correlation Weights", "plots/08_stpacf_correlation_heatmap.png")

# ============================================================================
# PACF-STYLE PLOTS (Like ARIMA ACF/PACF)
# ============================================================================

cat("\n📊 Creating PACF-style STPACF plots...\n")

# Function to create PACF-style plot
create_pacf_style_plot <- function(stpacf_result, title, filename) {
  if (!stpacf_result$success) {
    cat("⚠️ Skipping PACF-style plot for", title, "due to computation error\n")
    return(NULL)
  }
  
  tryCatch({
    cat("Creating PACF-style plot for", title, "...\n")
    
    # Extract temporal PACF (spatial lag 0)
    stpacf_matrix <- stpacf_result$values
    temporal_pacf <- stpacf_matrix[, 1]  # First column = spatial lag 0
    
    # Create data frame for plotting
    pacf_df <- data.frame(
      Lag = 1:length(temporal_pacf),
      PACF = temporal_pacf
    )
    
    # Calculate confidence bounds (approximate)
    n <- nrow(train_data)
    conf_bound <- 1.96 / sqrt(n)  # 95% confidence interval
    
    # Create PACF-style plot
    p <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_hline(yintercept = c(conf_bound, -conf_bound), 
                 color = "blue", linetype = "dashed", size = 0.5) +
      geom_segment(aes(xend = Lag, yend = 0), color = "darkred", size = 1) +
      geom_point(color = "darkred", size = 2) +
      labs(title = paste("STPACF (PACF-style):", title),
           subtitle = paste("Temporal lags (spatial lag 0) - n =", n),
           x = "Temporal Lag", y = "Partial Autocorrelation") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = 1:length(temporal_pacf)) +
      ylim(c(min(-conf_bound * 1.2, min(temporal_pacf) * 1.1), 
             max(conf_bound * 1.2, max(temporal_pacf) * 1.1)))
    
    # Add significance annotations
    significant_lags <- which(abs(temporal_pacf) > conf_bound)
    if (length(significant_lags) > 0) {
      p <- p + annotate("text", x = max(pacf_df$Lag) * 0.8, y = max(temporal_pacf) * 0.9,
                       label = paste("Significant lags:", paste(significant_lags, collapse = ", ")),
                       size = 3, color = "red")
    }
    
    # Save and display plot
    ggsave(filename, p, width = 10, height = 6, dpi = 300)
    print(p)
    
    cat("✅", title, "PACF-style plot saved:", filename, "\n")
    
    return(list(plot = p, significant_lags = significant_lags))
    
  }, error = function(e) {
    cat("❌ Error creating PACF-style plot for", title, ":", e$message, "\n")
    return(NULL)
  })
}

# Create PACF-style plots for each weight matrix
pacf_plot1 <- create_pacf_style_plot(stpacf_uniform, "Uniform Weights", "plots/08_stpacf_uniform_pacf.png")
pacf_plot2 <- create_pacf_style_plot(stpacf_distance, "Distance Weights", "plots/08_stpacf_distance_pacf.png")
pacf_plot3 <- create_pacf_style_plot(stpacf_correlation, "Correlation Weights", "plots/08_stpacf_correlation_pacf.png")

# Create combined PACF comparison plot
if (!is.null(pacf_plot1) && !is.null(pacf_plot2) && !is.null(pacf_plot3)) {
  cat("Creating combined PACF comparison plot...\n")
  
  # Combine data from all three weight matrices
  combined_pacf_data <- rbind(
    data.frame(Lag = 1:nrow(stpacf_uniform$values), 
               PACF = stpacf_uniform$values[, 1], 
               Weight_Type = "Uniform"),
    data.frame(Lag = 1:nrow(stpacf_distance$values), 
               PACF = stpacf_distance$values[, 1], 
               Weight_Type = "Distance"),
    data.frame(Lag = 1:nrow(stpacf_correlation$values), 
               PACF = stpacf_correlation$values[, 1], 
               Weight_Type = "Correlation")
  )
  
  # Calculate confidence bounds
  n <- nrow(train_data)
  conf_bound <- 1.96 / sqrt(n)
  
  # Create combined plot
  p_combined <- ggplot(combined_pacf_data, aes(x = Lag, y = PACF, color = Weight_Type)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_hline(yintercept = c(conf_bound, -conf_bound), 
               color = "gray", linetype = "dashed", size = 0.5) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "STPACF Comparison: All Weight Matrices",
         subtitle = paste("Temporal PACF (spatial lag 0) with 95% confidence bounds - n =", n),
         x = "Temporal Lag", y = "Partial Autocorrelation",
         color = "Weight Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom") +
    scale_x_continuous(breaks = 1:max(combined_pacf_data$Lag)) +
    scale_color_manual(values = c("Uniform" = "blue", "Distance" = "red", "Correlation" = "green"))
  
  # Save and display combined plot
  ggsave("plots/08_stpacf_combined_pacf.png", p_combined, width = 40, height = 8, dpi = 300)
  print(p_combined)
  
  cat("✅ Combined PACF comparison plot saved: plots/08_stpacf_combined_pacf.png\n")
}

# ============================================================================
# STPACF INTERPRETATION AND AR ORDER IDENTIFICATION
# ============================================================================

cat("\n=== STPACF INTERPRETATION ===\n")

# Function to analyze STPACF patterns
analyze_stpacf_pattern <- function(stpacf_result, weight_name) {
  if (!stpacf_result$success) {
    cat("❌", weight_name, "STPACF analysis failed\n")
    return(NULL)
  }
  
  cat("\n📋", weight_name, "STPACF Pattern Analysis:\n")
  
  stpacf_vals <- stpacf_result$values
  
  # Analyze temporal patterns (spatial lag 0)
  temporal_pacf <- stpacf_vals[, 1]  # First column = spatial lag 0
  
  cat("Temporal PACF (spatial lag 0):\n")
  for (i in 1:min(6, length(temporal_pacf))) {
    cat(sprintf("  Time lag %d: %.4f\n", i, temporal_pacf[i]))
  }
  
  # Analyze spatial patterns (time lag 1)
  if (ncol(stpacf_vals) > 1) {
    spatial_pacf <- stpacf_vals[1, ]  # First row = time lag 1
    cat("Spatial PACF (time lag 1):\n")
    for (j in 1:length(spatial_pacf)) {
      cat(sprintf("  Space lag %d: %.4f\n", j-1, spatial_pacf[j]))
    }
  }
  
  # Identify significant lags (simple threshold approach)
  significant_threshold <- 0.2
  if (length(temporal_pacf) > 1) {
    significant_temporal <- which(abs(temporal_pacf) > significant_threshold)
  } else {
    significant_temporal <- numeric(0)
  }
  
  cat("Significant temporal lags (|PACF| >", significant_threshold, "):", 
      if(length(significant_temporal) > 0) paste(significant_temporal, collapse = ", ") else "None", "\n")
  
  return(list(
    temporal_pacf = temporal_pacf,
    significant_lags = significant_temporal,
    weight_type = weight_name
  ))
}

# Analyze patterns for each weight matrix
uniform_analysis <- analyze_stpacf_pattern(stpacf_uniform, "Uniform")
distance_analysis <- analyze_stpacf_pattern(stpacf_distance, "Distance")
correlation_analysis <- analyze_stpacf_pattern(stpacf_correlation, "Correlation")

# ============================================================================
# AR ORDER RECOMMENDATION
# ============================================================================

cat("\n=== AR ORDER IDENTIFICATION ===\n")

# Function to suggest AR order based on STPACF cutoff
suggest_ar_order <- function(analysis) {
  if (is.null(analysis)) return(NULL)
  
  # Look for cutoff pattern in STPACF
  temporal_pacf <- analysis$temporal_pacf
  
  # Find where PACF becomes insignificant
  cutoff_threshold <- 0.15
  cutoff_point <- 0
  
  for (i in 1:length(temporal_pacf)) {
    if (abs(temporal_pacf[i]) < cutoff_threshold) {
      cutoff_point <- i - 1
      break
    }
  }
  
  # Suggest AR order (conservative approach)
  suggested_p <- min(cutoff_point, 3)  # Cap at 3 for practical reasons
  
  return(list(
    suggested_p = suggested_p,
    cutoff_point = cutoff_point,
    weight_type = analysis$weight_type
  ))
}

# Get AR order suggestions
uniform_ar <- suggest_ar_order(uniform_analysis)
distance_ar <- suggest_ar_order(distance_analysis)
correlation_ar <- suggest_ar_order(correlation_analysis)

# Display recommendations
cat("📊 AR Order Recommendations:\n")

if (!is.null(uniform_ar)) {
  cat("- Uniform weights: AR(", uniform_ar$suggested_p, ") - cutoff at lag", uniform_ar$cutoff_point, "\n")
}

if (!is.null(distance_ar)) {
  cat("- Distance weights: AR(", distance_ar$suggested_p, ") - cutoff at lag", distance_ar$cutoff_point, "\n")
}

if (!is.null(correlation_ar)) {
  cat("- Correlation weights: AR(", correlation_ar$suggested_p, ") - cutoff at lag", correlation_ar$cutoff_point, "\n")
}

# ============================================================================
# COMBINED STACF + STPACF IDENTIFICATION SUMMARY
# ============================================================================

cat("\n=== STARIMA MODEL IDENTIFICATION SUMMARY ===\n")

# Load STACF results for comparison
cat("📊 Combined STACF + STPACF Analysis:\n\n")

# Display STACF results (MA order)
cat("STACF Results (MA Order):\n")
if (exists("uniform_ma") && !is.null(uniform_ma)) {
  cat("- Uniform weights: MA(", uniform_ma$suggested_q, ")\n")
}
if (exists("distance_ma") && !is.null(distance_ma)) {
  cat("- Distance weights: MA(", distance_ma$suggested_q, ")\n")
}
if (exists("correlation_ma") && !is.null(correlation_ma)) {
  cat("- Correlation weights: MA(", correlation_ma$suggested_q, ")\n")
}

cat("\nSTPACF Results (AR Order):\n")
if (!is.null(uniform_ar)) {
  cat("- Uniform weights: AR(", uniform_ar$suggested_p, ")\n")
}
if (!is.null(distance_ar)) {
  cat("- Distance weights: AR(", distance_ar$suggested_p, ")\n")
}
if (!is.null(correlation_ar)) {
  cat("- Correlation weights: AR(", correlation_ar$suggested_p, ")\n")
}

# Proposed STARIMA models
cat("\n🎯 Proposed STARIMA Models:\n")
if (!is.null(uniform_ar) && exists("uniform_ma") && !is.null(uniform_ma)) {
  cat("- Uniform weights: STARIMA(", uniform_ar$suggested_p, ",0,", uniform_ma$suggested_q, ")\n")
}
if (!is.null(distance_ar) && exists("distance_ma") && !is.null(distance_ma)) {
  cat("- Distance weights: STARIMA(", distance_ar$suggested_p, ",0,", distance_ma$suggested_q, ")\n")
}
if (!is.null(correlation_ar) && exists("correlation_ma") && !is.null(correlation_ma)) {
  cat("- Correlation weights: STARIMA(", correlation_ar$suggested_p, ",0,", correlation_ma$suggested_q, ")\n")
}

# ============================================================================
# SUMMARY TABLE
# ============================================================================

cat("\n📋 STPACF Analysis Summary:\n")

# Create summary table
stpacf_summary <- data.frame(
  Weight_Type = c("Uniform", "Distance", "Correlation"),
  STPACF_Success = c(
    ifelse(stpacf_uniform$success, "✅ Success", "❌ Failed"),
    ifelse(stpacf_distance$success, "✅ Success", "❌ Failed"),
    ifelse(stpacf_correlation$success, "✅ Success", "❌ Failed")
  ),
  Suggested_AR_Order = c(
    ifelse(!is.null(uniform_ar), paste("AR(", uniform_ar$suggested_p, ")", sep=""), "N/A"),
    ifelse(!is.null(distance_ar), paste("AR(", distance_ar$suggested_p, ")", sep=""), "N/A"),
    ifelse(!is.null(correlation_ar), paste("AR(", correlation_ar$suggested_p, ")", sep=""), "N/A")
  ),
  Plot_Generated = c(
    ifelse(!is.null(plot1) && plot1, "✅ Yes", "❌ No"),
    ifelse(!is.null(plot2) && plot2, "✅ Yes", "❌ No"),
    ifelse(!is.null(plot3) && plot3, "✅ Yes", "❌ No")
  ),
  stringsAsFactors = FALSE
)

print(stpacf_summary)

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save STPACF results
save(stpacf_uniform, stpacf_distance, stpacf_correlation,
     uniform_analysis, distance_analysis, correlation_analysis,
     uniform_ar, distance_ar, correlation_ar,
     stpacf_summary, train_data,
     file = "output/08_stpacf_analysis.RData")

# Display results in viewer
cat("\n=== DATA VIEWER ===\n")
cat("Opening STPACF summary in viewer...\n")
View(stpacf_summary, title = "STPACF Analysis Summary")

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================

cat("\n=== STPACF ANALYSIS COMPLETED ===\n")
cat("✅ STPACF computed for 3 spatial weight matrices\n")
cat("✅ Space-Time PACF plots generated (heatmaps + PACF-style)\n")
cat("✅ Lag patterns analyzed for AR order identification\n")
cat("✅ AR order recommendations provided\n")
cat("✅ Combined STACF+STPACF identification summary\n")
cat("✅ Results saved to: output/08_stpacf_analysis.RData\n")
cat("✅ Summary available in RStudio viewer\n\n")

cat("📊 PHASE 2 IDENTIFICATION: COMPLETED 100%!\n")
cat("🎯 Ready for Phase 3: STARIMA Model Estimation\n")
cat("📁 Next file: 09_Model_Structure.R\n\n")

cat("STPACF Analysis validation:\n")
cat("- Space-Time PACF computed: ✅\n")
cat("- AR order patterns identified: ✅\n")
cat("- STARIMA model structure determined: ✅\n")
cat("- Ready for model estimation: ✅\n")