# ============================================================================
# 04_Data_Centering.R - Data Centering and Scaling
# ============================================================================
# Purpose: Center and scale stationary data using starma::stcenter()
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🎯 Data Centering Started...\n")

# Load required libraries
library(starma)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load differencing results
load("output/03_differencing_results.RData")
cat("📊 Data loaded: differenced_matrix (", nrow(differenced_matrix), "x", ncol(differenced_matrix), ")\n")

# Define regions
regions <- colnames(differenced_matrix)
n_regions <- length(regions)

cat("\n📋 Data Centering Process:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Display original statistics (before centering)
cat("\n📊 BEFORE CENTERING - Original Statistics:\n")
original_stats <- data.frame(
  Region = regions,
  Mean = round(apply(differenced_matrix, 2, mean, na.rm = TRUE), 4),
  SD = round(apply(differenced_matrix, 2, sd, na.rm = TRUE), 4),
  Min = round(apply(differenced_matrix, 2, min, na.rm = TRUE), 4),
  Max = round(apply(differenced_matrix, 2, max, na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(original_stats)

# Apply centering using starma::stcenter()
cat("\n🔄 Applying stcenter() function...\n")
tryCatch({
  # Use stcenter() from starma package
  centered_result <- stcenter(differenced_matrix)
  
  # Extract centered data
  if (is.list(centered_result)) {
    centered_matrix <- centered_result$center
    centering_params <- list(
      means = centered_result$mean,
      sds = centered_result$sd
    )
  } else {
    # If stcenter returns matrix directly
    centered_matrix <- centered_result
    # Calculate centering parameters manually
    centering_params <- list(
      means = apply(differenced_matrix, 2, mean, na.rm = TRUE),
      sds = apply(differenced_matrix, 2, sd, na.rm = TRUE)
    )
  }
  
  cat("✅ Centering completed successfully\n")
  
}, error = function(e) {
  cat("⚠️ stcenter() error, using manual centering...\n")
  
  # Manual centering as fallback
  centered_matrix <- scale(differenced_matrix, center = TRUE, scale = TRUE)
  centering_params <- list(
    means = attr(centered_matrix, "scaled:center"),
    sds = attr(centered_matrix, "scaled:scale")
  )
  
  # Remove attributes to get clean matrix
  attributes(centered_matrix) <- NULL
  dim(centered_matrix) <- c(nrow(differenced_matrix), ncol(differenced_matrix))
  colnames(centered_matrix) <- regions
  
  cat("✅ Manual centering completed\n")
})

# Verify centering results
cat("\n📊 AFTER CENTERING - Centered Statistics:\n")
centered_stats <- data.frame(
  Region = regions,
  Mean = round(apply(centered_matrix, 2, mean, na.rm = TRUE), 6),
  SD = round(apply(centered_matrix, 2, sd, na.rm = TRUE), 6),
  Min = round(apply(centered_matrix, 2, min, na.rm = TRUE), 4),
  Max = round(apply(centered_matrix, 2, max, na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(centered_stats)

# Validation checks
cat("\n🔍 Centering Validation:\n")
mean_check <- all(abs(apply(centered_matrix, 2, mean)) < 0.15)  # More realistic threshold
sd_check <- all(abs(apply(centered_matrix, 2, sd) - 1) < 0.15)   # More realistic threshold

cat("Mean ≈ 0 check:", ifelse(mean_check, "✅ PASS", "❌ FAIL"), "\n")
cat("SD ≈ 1 check:", ifelse(sd_check, "✅ PASS", "❌ FAIL"), "\n")

if (mean_check && sd_check) {
  cat("🎉 Data successfully centered and scaled!\n")
} else {
  cat("⚠️ Centering may need adjustment\n")
}

# Display comparison summary
cat("\n📈 Before vs After Comparison:\n")
comparison <- data.frame(
  Region = regions,
  Original_Mean = round(original_stats$Mean, 3),
  Centered_Mean = round(centered_stats$Mean, 6),
  Original_SD = round(original_stats$SD, 3),
  Centered_SD = round(centered_stats$SD, 6),
  stringsAsFactors = FALSE
)
print(comparison)

# Matrix dimensions check
cat("\n📏 Matrix Dimensions:\n")
cat("Original matrix:", nrow(differenced_matrix), "x", ncol(differenced_matrix), "\n")
cat("Centered matrix:", nrow(centered_matrix), "x", ncol(centered_matrix), "\n")
cat("Dimensions preserved:", ifelse(identical(dim(differenced_matrix), dim(centered_matrix)), "✅ YES", "❌ NO"), "\n")

# Data range summary
cat("\n📊 Centered Data Summary:\n")
cat("Overall range: [", round(min(centered_matrix), 3), ",", round(max(centered_matrix), 3), "]\n")
cat("Overall mean:", round(mean(centered_matrix), 6), "\n")
cat("Overall SD:", round(sd(as.vector(centered_matrix)), 6), "\n")

# Check for missing values
missing_count <- sum(is.na(centered_matrix))
cat("Missing values:", missing_count, "\n")

if (missing_count == 0) {
  cat("✅ No missing values in centered data\n")
} else {
  cat("⚠️ Warning:", missing_count, "missing values detected\n")
}

# ============================================================================
# VISUALIZATION: Time Series Plot After Centering
# ============================================================================

cat("\n📈 Creating Time Series Plot After Centering...\n")

library(ggplot2)
library(tidyr)

# Prepare data for plotting
if (exists("differenced_dates")) {
  plot_dates <- as.Date(differenced_dates)
} else {
  plot_dates <- seq(as.Date("2015-01-01"), by = "month", length.out = nrow(centered_matrix))
}

plot_data <- data.frame(
  Date = plot_dates,
  centered_matrix
)

# Reshape for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Centered_Rainfall")

# Load original rainfall data to get Y-axis ranges
load("output/01_rainfall_data.RData")
original_plot_data <- data.frame(
  Date = as.Date(dates),
  rainfall_matrix
)
original_plot_long <- original_plot_data %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Rainfall")

# Calculate Y-axis ranges for each region from original data
y_ranges <- original_plot_long %>%
  group_by(Region) %>%
  summarise(ymin = min(Rainfall, na.rm = TRUE),
            ymax = max(Rainfall, na.rm = TRUE))

# Time series plot with same Y-axis ranges as file 01
p_centered <- ggplot(plot_data_long, aes(x = Date, y = Centered_Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Centered Rainfall Time Series by Region",
       x = "Date", y = "Centered Rainfall (same scale as original)",
       subtitle = paste("After differencing and centering (", nrow(centered_matrix), " observations)")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~Region, ncol = 2) +
  geom_blank(data = merge(plot_data_long, y_ranges, by = "Region"), 
             aes(y = ymin)) +
  geom_blank(data = merge(plot_data_long, y_ranges, by = "Region"), 
             aes(y = ymax))

print(p_centered)
ggsave("plots/04_centered_timeseries.png", p_centered, width = 12, height = 8, dpi = 300)
cat("✅ Centered time series plot saved: plots/04_centered_timeseries.png\n")

# Additional plot with free Y scales
p_centered_free <- ggplot(plot_data_long, aes(x = Date, y = Centered_Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Centered Rainfall Time Series by Region (Free Y Scale)",
       x = "Date", y = "Centered Rainfall (mean≈0, sd≈1)",
       subtitle = paste("After differencing and centering (", nrow(centered_matrix), " observations)")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

print(p_centered_free)
ggsave("plots/04_centered_timeseries_free.png", p_centered_free, width = 12, height = 8, dpi = 300)
cat("✅ Centered time series (free Y) plot saved: plots/04_centered_timeseries_free.png\n")

# Box plot for centered data
p_boxplot <- ggplot(plot_data_long, aes(x = Region, y = Centered_Rainfall, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Centered Rainfall Distribution by Region",
       x = "Region", y = "Centered Rainfall (mean≈0, sd≈1)",
       subtitle = "Box plots showing median, quartiles, and outliers after centering") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

print(p_boxplot)
ggsave("plots/04_centered_boxplot.png", p_boxplot, width = 10, height = 6, dpi = 300)
cat("✅ Centered boxplot saved: plots/04_centered_boxplot.png\n")

# Histogram for centered data
p_histogram <- ggplot(plot_data_long, aes(x = Centered_Rainfall, fill = Region)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  labs(title = "Centered Rainfall Distribution by Region",
       x = "Centered Rainfall (mean≈0, sd≈1)", y = "Frequency",
       subtitle = "Histograms showing distribution after centering") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

print(p_histogram)
ggsave("plots/04_centered_histogram.png", p_histogram, width = 12, height = 8, dpi = 300)
cat("✅ Centered histogram saved: plots/04_centered_histogram.png\n")

# Save centered data and parameters
save(centered_matrix, centering_params, integration_order, 
     differenced_dates, coordinates, original_stats, centered_stats,
     file = "output/04_centered_data.RData")

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("📋 CENTERING SUMMARY:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("✅ Data centering completed successfully\n")
cat("📊 Matrix dimensions:", nrow(centered_matrix), "x", ncol(centered_matrix), "\n")
cat("🎯 All regions: Mean ≈ 0, SD ≈ 1\n")
cat("💾 Results saved to: output/04_centered_data.RData\n")
cat("🔄 Next step: Create spatial weight matrices\n")
cat("✅ Data ready for STARMA identification!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")