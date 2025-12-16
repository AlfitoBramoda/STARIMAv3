# ============================================================================
# 01_Load_Data.R - Load and Format Spatio-Temporal Data
# ============================================================================
# Purpose: Load rainfall data from 5 regions and convert to STARMA format
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("📊 Loading Spatio-Temporal Rainfall Data...\n")

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

# Display last few rows
cat("\n📊 Last 5 observations:\n")
print(tail(rainfall_matrix, 5))

# Display coordinates
cat("\n🗺️ Spatial Coordinates:\n")
print(coordinates)

# Display data structure
cat("\n📋 Data Structure:\n")
str(rainfall_matrix)

# Show a preview of what will be opened in viewers
cat("\n👀 Data Preview (will be opened in viewers later):\n")
cat("- Rainfall Matrix:", nrow(rainfall_matrix), "rows x", ncol(rainfall_matrix), "columns\n")
cat("- Coordinates Table:", nrow(coordinates), "regions with lat/lon\n")
cat("- Complete Dataset: Dates +", ncol(rainfall_matrix), "rainfall columns\n")

# Display sample of middle data
cat("\n📊 Sample from middle (rows 58-62):\n")
print(rainfall_matrix[58:62, ])

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

# Data range and distribution info
cat("\n📈 Data Range Analysis:\n")
cat("Overall minimum:", round(min(rainfall_matrix, na.rm = TRUE), 3), "mm/day\n")
cat("Overall maximum:", round(max(rainfall_matrix, na.rm = TRUE), 3), "mm/day\n")
cat("Overall mean:", round(mean(rainfall_matrix, na.rm = TRUE), 3), "mm/day\n")
cat("Overall std dev:", round(sd(rainfall_matrix, na.rm = TRUE), 3), "mm/day\n")

# Check data completeness
cat("\n📋 Data Completeness:\n")
completeness <- (1 - total_missing / (nrow(rainfall_matrix) * ncol(rainfall_matrix))) * 100
cat("Data completeness:", round(completeness, 2), "%\n")

# Display data type confirmation
cat("\n🔍 Data Type Verification:\n")
cat("Matrix class:", class(rainfall_matrix), "\n")
cat("Data type:", typeof(rainfall_matrix), "\n")
cat("Dimensions confirmed:", dim(rainfall_matrix)[1], "time periods x", dim(rainfall_matrix)[2], "spatial locations\n")

# Final dataset summary
cat("\n🎆 DATASET LOADING SUMMARY:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat("✅ Successfully loaded", ncol(rainfall_matrix), "regions\n")
cat("✅ Time series length:", nrow(rainfall_matrix), "observations\n")
cat("✅ Date range:", as.character(min(as.Date(dates))), "to", as.character(max(as.Date(dates))), "\n")
cat("✅ Data quality:", round(completeness, 1), "% complete\n")
cat("✅ Ready for STARIMA analysis\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

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

# 1. Time Series Plot
library(ggplot2)
library(gridExtra)

# Prepare data for plotting
plot_data <- data.frame(
  Date = as.Date(dates),
  rainfall_matrix
)

# Reshape for ggplot
library(tidyr)
plot_data_long <- plot_data %>%
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Rainfall")

# Time series plot
p1 <- ggplot(plot_data_long, aes(x = Date, y = Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Rainfall Time Series by Region (2015-2024)",
       x = "Date", y = "Rainfall (mm/day)",
       subtitle = "120 months of rainfall data across 5 regions") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

print(p1)
ggsave("plots/01_rainfall_timeseries.png", p1, width = 12, height = 8, dpi = 300)
cat("✅ Time series plot saved: plots/01_rainfall_timeseries.png\n")

# 2. Correlation Matrix Plot
library(corrplot)
cor_matrix <- cor(rainfall_matrix)

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         title = "Spatial Correlation Matrix\n", mar = c(0,0,2,0))

png("plots/01_correlation_matrix.png", width = 800, height = 600, res = 150)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         title = "Spatial Correlation Matrix\n", mar = c(0,0,2,0))
dev.off()
cat("✅ Correlation matrix saved: plots/01_correlation_matrix.png\n")


# 3. Box Plot by Region
p2 <- ggplot(plot_data_long, aes(x = Region, y = Rainfall, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Rainfall Distribution by Region",
       x = "Region", y = "Rainfall (mm/day)",
       subtitle = "Box plots showing median, quartiles, and outliers") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

print(p2)
ggsave("plots/01_rainfall_boxplot.png", p2, width = 10, height = 6, dpi = 300)
cat("✅ Box plot saved: plots/01_rainfall_boxplot.png\n")

cat("\n📈 All plots created and saved to plots/ directory\n")

# Display data in viewer for inspection
cat("\n👁️ Opening data viewers...\n")

# View rainfall matrix
View(rainfall_matrix)
cat("✅ Rainfall matrix opened in viewer\n")

# View coordinates
View(coordinates)
cat("✅ Coordinates opened in viewer\n")

# Create a comprehensive data frame for viewing
rainfall_df <- data.frame(
  Date = dates,
  rainfall_matrix
)
View(rainfall_df)
cat("✅ Complete dataset (with dates) opened in viewer\n")

# Save processed data
save(rainfall_matrix, coordinates, dates, 
     file = "output/01_rainfall_data.RData")

cat("\n💾 Data saved to: output/01_rainfall_data.RData\n")
cat("✅ Data loading completed successfully!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")