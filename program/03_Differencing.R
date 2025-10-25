# ============================================================================
# 03_Differencing.R - Data Differencing for Stationarity
# ============================================================================
# Purpose: Apply differencing to achieve stationarity based on Python-style ADF results
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🔄 Data Differencing Started...\n")

# Load required libraries
library(tseries)
library(urca)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load stationarity test results
load("output/02_stationarity_results.RData")
cat("📊 Data loaded: stationarity_results and rainfall_matrix\n")

# Simple stationarity test function (same as in 02_Stationarity_Test.R)
test_stationarity <- function(ts_data) {
  adf_test <- adf.test(ts_data, alternative = "stationary")
  return(list(
    statistic = adf_test$statistic,
    p_value = adf_test$p.value,
    is_stationary = adf_test$p.value < 0.05
  ))
}

# Initialize differencing results
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

differencing_results <- data.frame(
  Region = regions,
  Original_Stationary = stationarity_results$Overall_Stationary,
  Integration_Order = numeric(n_regions),
  Final_Stationary = logical(n_regions),
  Differencing_Applied = character(n_regions),
  stringsAsFactors = FALSE
)

# Store differenced data
differenced_data <- list()
integration_order <- numeric(n_regions)
names(integration_order) <- regions

cat("\n🔄 Applying Differencing Process:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

for (i in 1:n_regions) {
  region <- regions[i]
  ts_data <- rainfall_matrix[, i]
  
  cat("\n📍 Processing", region, "region:\n")
  
  if (stationarity_results$Overall_Stationary[i]) {
    # Already stationary
    differenced_data[[region]] <- ts_data
    integration_order[i] <- 0
    differencing_results$Integration_Order[i] <- 0
    differencing_results$Final_Stationary[i] <- TRUE
    differencing_results$Differencing_Applied[i] <- "None (already stationary)"
    
    cat("  ✅ Already stationary, no differencing needed\n")
    cat("  📊 Integration order (d):", 0, "\n")
    
  } else {
    # Need differencing
    cat("  🔄 Applying differencing...\n")
    
    current_data <- ts_data
    d_order <- 0
    max_diff <- 2  # Maximum differencing order
    
    # Apply differencing until stationary or max order reached
    while (d_order < max_diff) {
      d_order <- d_order + 1
      current_data <- diff(current_data)
      
      cat("    Testing after", d_order, "differencing...\n")
      
      # Test stationarity with ADF
      adf_result <- test_stationarity(current_data)
      
      cat("    ADF p-value:", round(adf_result$p_value, 4), "\n")
      
      if (adf_result$is_stationary) {
        cat("    ✅ Achieved stationarity after", d_order, "differencing\n")
        break
      } else {
        cat("    ❌ Still non-stationary, trying higher order...\n")
      }
    }
    
    # Store results
    differenced_data[[region]] <- current_data
    integration_order[i] <- d_order
    differencing_results$Integration_Order[i] <- d_order
    differencing_results$Final_Stationary[i] <- d_order <= max_diff
    
    if (d_order == 1) {
      differencing_results$Differencing_Applied[i] <- "First difference"
    } else if (d_order == 2) {
      differencing_results$Differencing_Applied[i] <- "Second difference"
    } else {
      differencing_results$Differencing_Applied[i] <- paste(d_order, "differences")
    }
    
    cat("  📊 Final integration order (d):", d_order, "\n")
    
    if (d_order > max_diff) {
      cat("  ⚠️ Warning: Maximum differencing reached, may need alternative approach\n")
    }
  }
}

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("📋 DIFFERENCING RESULTS SUMMARY:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Display results table
print(differencing_results)

# Create differenced matrix (handle different lengths due to differencing)
cat("\n🔧 Creating Differenced Data Matrix:\n")

# Find minimum length after differencing
min_length <- min(sapply(differenced_data, length))
cat("Minimum length after differencing:", min_length, "\n")

# Create matrix with equal lengths
differenced_matrix <- matrix(NA, nrow = min_length, ncol = n_regions)
colnames(differenced_matrix) <- regions

for (i in 1:n_regions) {
  region <- regions[i]
  data_length <- length(differenced_data[[region]])
  # Take the last min_length observations
  start_idx <- data_length - min_length + 1
  differenced_matrix[, i] <- differenced_data[[region]][start_idx:data_length]
}

cat("Differenced matrix dimensions:", nrow(differenced_matrix), "x", ncol(differenced_matrix), "\n")

# Integration order summary
cat("\n📊 Integration Order Summary:\n")
cat("Integration order vector [d1, d2, d3, d4, d5]:", integration_order, "\n")

stationary_after_diff <- sum(differencing_results$Final_Stationary)
cat("✅ Stationary regions after differencing:", stationary_after_diff, "out of", n_regions, "\n")

if (stationary_after_diff == n_regions) {
  cat("🎉 All regions achieved stationarity!\n")
  cat("🔄 Next step: Data centering with stcenter()\n")
} else {
  non_stationary <- regions[!differencing_results$Final_Stationary]
  cat("⚠️ Still non-stationary regions:", paste(non_stationary, collapse = ", "), "\n")
  cat("🔄 Consider alternative approaches for these regions\n")
}

# Adjust dates for differenced data
if (exists("dates")) {
  # Calculate how many observations were lost due to differencing
  max_diff_order <- max(integration_order)
  if (max_diff_order > 0) {
    differenced_dates <- dates[(max_diff_order + 1):length(dates)]
    # Adjust to match matrix length
    date_length <- length(differenced_dates)
    start_idx <- date_length - min_length + 1
    differenced_dates <- differenced_dates[start_idx:date_length]
  } else {
    differenced_dates <- dates
  }
  
  cat("Date range after differencing:", as.character(min(as.Date(differenced_dates))), 
      "to", as.character(max(as.Date(differenced_dates))), "\n")
} else {
  differenced_dates <- NULL
}

# ============================================================================
# VISUALIZATION: Time Series Plot After Differencing
# ============================================================================

cat("\n📈 Creating Time Series Plot After Differencing...\n")

library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data for plotting
if (!is.null(differenced_dates)) {
  plot_data <- data.frame(
    Date = as.Date(differenced_dates),
    differenced_matrix
  )
} else {
  # Create time index if dates not available
  plot_data <- data.frame(
    Date = 1:nrow(differenced_matrix),
    differenced_matrix
  )
}

# Reshape for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Differenced_Rainfall")

# Time series plot
p_diff <- ggplot(plot_data_long, aes(x = Date, y = Differenced_Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Differenced Rainfall Time Series by Region",
       x = "Date", y = "Differenced Rainfall",
       subtitle = paste("After differencing for stationarity (", nrow(differenced_matrix), " observations)")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

if (!is.null(differenced_dates)) {
  p_diff <- p_diff + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

print(p_diff)
ggsave("plots/03_differenced_timeseries.png", p_diff, width = 12, height = 8, dpi = 300)
cat("✅ Differenced time series plot saved: plots/03_differenced_timeseries.png\n")

# Save results
save(differenced_matrix, integration_order, differencing_results, 
     differenced_dates, coordinates,
     file = "output/03_differencing_results.RData")

cat("\n💾 Results saved to: output/03_differencing_results.RData\n")
cat("✅ Differencing process completed!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")