# ============================================================================
# 01_Load_Data.R - Load and Format Spatio-Temporal Data (Correlation)
# ============================================================================
# Purpose: Load rainfall data from 5 regions and convert to STARMA format
# Author: STARMA Project - Correlation Analysis
# Date: 2024
# ============================================================================

cat("📊 Loading Spatio-Temporal Rainfall Data (Correlation Focus)...\n")

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(corrplot)

# Define regions
regions <- c("Barat", "Selatan", "Tengah", "Timur", "Utara")
cat("🗺️ Regions:", paste(regions, collapse = ", "), "\n")

# Initialize list to store data
region_data <- list()
coordinates <- data.frame(
  Region = character(),
  Longitude = numeric(),
  Latitude = numeric(),
  stringsAsFactors = FALSE
)

# Load data for each region
cat("\n📁 Loading data files:\n")
for (region in regions) {
  file_path <- paste0("dataset/", region, ".csv")
  
  if (file.exists(file_path)) {
    # Read data
    data <- read_csv(file_path, show_col_types = FALSE)
    
    # Extract rainfall data and coordinates
    region_data[[region]] <- data$PRECTOTCORR
    
    # Store coordinates (first row)
    coordinates <- rbind(coordinates, data.frame(
      Region = region,
      Longitude = data$Longitude[1],
      Latitude = data$Latitude[1]
    ))
    
    cat("✅", region, ":", nrow(data), "observations loaded\n")
  } else {
    stop(paste("❌ File not found:", file_path))
  }
}

# Convert to spatio-temporal matrix (rows=time, columns=locations)
rainfall_matrix <- do.call(cbind, region_data)
colnames(rainfall_matrix) <- regions

# Extract dates from first file for reference
dates <- read_csv("dataset/Barat.csv", show_col_types = FALSE)$Date

cat("\n📋 Dataset Structure:\n")
cat("Dimensions:", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), "(time x space)\n")
cat("Time period:", min(dates), "to", max(dates), "\n")
cat("Total observations:", nrow(rainfall_matrix), "months\n")

# Display first few rows
cat("\n📊 First 5 observations:\n")
print(head(rainfall_matrix, 5))

# Display coordinates
cat("\n🗺️ Spatial Coordinates:\n")
print(coordinates)

# Basic statistics
cat("\n📈 Basic Statistics by Region:\n")
summary_stats <- data.frame(
  Region = regions,
  Mean = round(apply(rainfall_matrix, 2, mean, na.rm = TRUE), 3),
  SD = round(apply(rainfall_matrix, 2, sd, na.rm = TRUE), 3),
  Min = round(apply(rainfall_matrix, 2, min, na.rm = TRUE), 3),
  Max = round(apply(rainfall_matrix, 2, max, na.rm = TRUE), 3),
  Missing = apply(rainfall_matrix, 2, function(x) sum(is.na(x)))
)
print(summary_stats)

# Check for missing values
total_missing <- sum(is.na(rainfall_matrix))
cat("\n🔍 Missing Values Check:\n")
cat("Total missing values:", total_missing, "\n")

if (total_missing > 0) {
  cat("⚠️ Warning: Missing values detected!\n")
} else {
  cat("✅ No missing values found\n")
}

# Create output directory if not exists
if (!dir.exists("output")) {
  dir.create("output")
  cat("📁 Created output directory\n")
}

# Create plots directory if not exists
if (!dir.exists("plots")) {
  dir.create("plots")
  cat("📁 Created plots directory\n")
}

# Visualizations
cat("\n📈 Creating Data Visualizations:\n")

# Prepare data for plotting
plot_data <- data.frame(
  Date = as.Date(dates),
  rainfall_matrix
)

# Reshape for ggplot
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Rainfall")

# Time series plot
p1 <- ggplot(plot_data_long, aes(x = Date, y = Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Rainfall Time Series by Region (2015-2024) - Correlation Analysis",
       x = "Date", y = "Rainfall (mm/day)",
       subtitle = "120 months of rainfall data across 5 regions") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

print(p1)
ggsave("plots/01_rainfall_timeseries_correlation.png", p1, width = 12, height = 8, dpi = 300)
cat("✅ Time series plot saved: plots/01_rainfall_timeseries_correlation.png\n")

# Correlation Matrix Plot (Focus for correlation weights)
cor_matrix <- cor(rainfall_matrix)
cat("\n🔗 Correlation Matrix for Spatial Weights:\n")
print(round(cor_matrix, 3))

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         title = "Spatial Correlation Matrix (Correlation Weights)\n", mar = c(0,0,2,0))

png("plots/01_correlation_matrix_correlation.png", width = 800, height = 600, res = 150)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         title = "Spatial Correlation Matrix (Correlation Weights)\n", mar = c(0,0,2,0))
dev.off()
cat("✅ Correlation matrix saved: plots/01_correlation_matrix_correlation.png\n")

# Box Plot by Region
p2 <- ggplot(plot_data_long, aes(x = Region, y = Rainfall, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Rainfall Distribution by Region (Correlation Analysis)",
       x = "Region", y = "Rainfall (mm/day)",
       subtitle = "Box plots showing median, quartiles, and outliers") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

print(p2)
ggsave("plots/01_rainfall_boxplot_correlation.png", p2, width = 10, height = 6, dpi = 300)
cat("✅ Box plot saved: plots/01_rainfall_boxplot_correlation.png\n")

cat("\n📈 All plots created and saved to plots/ directory\n")

# Save processed data
save(rainfall_matrix, coordinates, dates, cor_matrix,
     file = "output/01_rainfall_data.RData")

cat("\n💾 Data saved to: output/01_rainfall_data.RData\n")
cat("✅ Data loading completed successfully!\n")
cat("🔗 Correlation matrix will be used for spatial weights construction\n")
cat(paste(rep("=", 50), collapse = ""), "\n")