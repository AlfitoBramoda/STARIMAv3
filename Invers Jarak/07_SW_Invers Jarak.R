# ============================================================================
# 07_SW_Distance.R - Create Distance-Based Spatial Weight Matrix
# ============================================================================
# Purpose: Create distance-based spatial weight matrix for STARMA modeling
# Author: STARMA Project
# Date: 2024
# ============================================================================

cat("🗺️ Spatial Weights Creation (Distance-Based) Started...\n")

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions and coordinates
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

cat("\n🗺️ Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# Display coordinates
cat("\n📍 Coordinates:\n")
print(coordinates)

# ============================================================================
# CALCULATE DISTANCE MATRIX
# ============================================================================
cat("\n1️⃣ Calculating Distance Matrix...\n")

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(lat1, lon1, lat2, lon2) {
  sqrt((lat2 - lat1)^2 + (lon2 - lon1)^2)
}

# Create distance matrix
distance_matrix <- matrix(0, nrow = n_regions, ncol = n_regions)
rownames(distance_matrix) <- colnames(distance_matrix) <- regions

for (i in 1:n_regions) {
  for (j in 1:n_regions) {
    if (i != j) {
      lat1 <- coordinates$Latitude[coordinates$Region == regions[i]]
      lon1 <- coordinates$Longitude[coordinates$Region == regions[i]]
      lat2 <- coordinates$Latitude[coordinates$Region == regions[j]]
      lon2 <- coordinates$Longitude[coordinates$Region == regions[j]]
      
      distance_matrix[i, j] <- euclidean_distance(lat1, lon1, lat2, lon2)
    }
  }
}

cat("✅ Distance matrix calculated\n")
cat("Distance range: [", round(min(distance_matrix[distance_matrix > 0]), 3), 
    ",", round(max(distance_matrix), 3), "]\n")

# Display distance matrix
cat("\n📏 Distance Matrix:\n")
print(round(distance_matrix, 3))

# ============================================================================
# CREATE DISTANCE-BASED WEIGHTS MATRIX
# ============================================================================
cat("\n2️⃣ Creating Distance-Based Weights Matrix...\n")

# Method: Inverse distance weighting (closer regions get higher weights)
# Formula: w_ij = 1/d_ij^alpha (where alpha = 1 for simplicity)
distance_weights <- matrix(0, nrow = n_regions, ncol = n_regions)
rownames(distance_weights) <- colnames(distance_weights) <- regions

# Calculate inverse distance weights
for (i in 1:n_regions) {
  row_weights <- numeric(n_regions)
  for (j in 1:n_regions) {
    if (i != j) {
      # Inverse distance: closer regions get higher weights
      row_weights[j] <- 1 / distance_matrix[i, j]
    }
  }
  
  # Normalize weights so each row sums to 1
  if (sum(row_weights) > 0) {
    distance_weights[i, ] <- row_weights / sum(row_weights)
  }
}

cat("✅ Distance-based weights matrix created\n")
cat("Matrix dimensions:", dim(distance_weights), "\n")
cat("Row sums:", round(rowSums(distance_weights), 3), "\n")
cat("Weight range: [", round(min(distance_weights[distance_weights > 0]), 4), 
    ",", round(max(distance_weights[distance_weights > 0]), 4), "]\n")

# ============================================================================
# VALIDATION
# ============================================================================
cat("\n🔍 VALIDATING DISTANCE WEIGHTS...\n")

diag_check <- all(diag(distance_weights) == 0)
sum_check <- all(abs(rowSums(distance_weights) - 1) < 1e-10)
non_neg_check <- all(distance_weights >= 0)

cat("  Diagonal = 0:", ifelse(diag_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Row sums = 1:", ifelse(sum_check, "✅ PASS", "❌ FAIL"), "\n")
cat("  Non-negative:", ifelse(non_neg_check, "✅ PASS", "❌ FAIL"), "\n")

cat("  Min weight:", round(min(distance_weights[distance_weights > 0]), 6), "\n")
cat("  Max weight:", round(max(distance_weights), 6), "\n")
cat("  Mean weight:", round(mean(distance_weights[distance_weights > 0]), 6), "\n")

# Display weights matrix
cat("\n📊 Distance-Based Weights Matrix:\n")
print(round(distance_weights, 4))

# ============================================================================
# DISTANCE ANALYSIS
# ============================================================================
cat("\n📈 Distance Analysis:\n")

# Find closest and farthest neighbors for each region
distance_analysis <- data.frame(
  Region = regions,
  Closest_Neighbor = character(n_regions),
  Closest_Distance = numeric(n_regions),
  Closest_Weight = numeric(n_regions),
  Farthest_Neighbor = character(n_regions),
  Farthest_Distance = numeric(n_regions),
  Farthest_Weight = numeric(n_regions),
  stringsAsFactors = FALSE
)

for (i in 1:n_regions) {
  region <- regions[i]
  distances <- distance_matrix[i, ]
  weights <- distance_weights[i, ]
  
  # Remove self (distance = 0)
  distances_others <- distances[distances > 0]
  weights_others <- weights[weights > 0]
  names_others <- names(distances_others)
  
  # Closest neighbor (minimum distance)
  closest_idx <- which.min(distances_others)
  distance_analysis$Closest_Neighbor[i] <- names_others[closest_idx]
  distance_analysis$Closest_Distance[i] <- distances_others[closest_idx]
  distance_analysis$Closest_Weight[i] <- weights_others[closest_idx]
  
  # Farthest neighbor (maximum distance)
  farthest_idx <- which.max(distances_others)
  distance_analysis$Farthest_Neighbor[i] <- names_others[farthest_idx]
  distance_analysis$Farthest_Distance[i] <- distances_others[farthest_idx]
  distance_analysis$Farthest_Weight[i] <- weights_others[farthest_idx]
}

print(distance_analysis)

# ============================================================================
# VISUALIZATION
# ============================================================================
cat("\n📊 Creating Heatmap Visualization...\n")

library(ggplot2)
library(tidyr)

# Prepare data frame for distance matrix
distance_df <- as.data.frame(distance_matrix)
distance_df$From <- rownames(distance_df)
melted_distance <- distance_df %>%
  pivot_longer(cols = -From, names_to = "To", values_to = "Distance")

# Plot distance matrix
p_distance <- ggplot(melted_distance, aes(x = To, y = From, fill = Distance)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red",
                       midpoint = max(distance_matrix) / 2, name = "Distance") +
  labs(title = "Distance Matrix Between Regions",
       subtitle = paste("Range: [", round(min(distance_matrix[distance_matrix > 0]), 3), 
                        ",", round(max(distance_matrix), 3), "]"),
       x = "To Region", y = "From Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_fixed()

# Prepare data frame for weights matrix
weights_df <- as.data.frame(distance_weights)
weights_df$From <- rownames(weights_df)
melted_weights <- weights_df %>%
  pivot_longer(cols = -From, names_to = "To", values_to = "Weight")

# Plot weights matrix
p_weights <- ggplot(melted_weights, aes(x = To, y = From, fill = Weight)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = max(distance_weights) / 2, name = "Weight") +
  labs(title = "Distance-Based Spatial Weight Matrix",
       subtitle = paste("Range: [", round(min(distance_weights[distance_weights > 0]), 4), 
                        ",", round(max(distance_weights), 4), "]"),
       x = "To Region", y = "From Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_fixed()

# Save heatmaps
ggsave("plots/07_distance_matrix.png", p_distance, width = 8, height = 6, dpi = 300)
ggsave("plots/07_distance_weights.png", p_weights, width = 8, height = 6, dpi = 300)
cat("✅ Heatmaps saved: plots/07_distance_matrix.png & plots/07_distance_weights.png\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================
spatial_weights <- list(distance = distance_weights)

weights_summary <- data.frame(
  Weight_Type = "Distance-Based",
  Min_Weight = round(min(distance_weights[distance_weights > 0]), 6),
  Max_Weight = round(max(distance_weights), 6),
  Mean_Weight = round(mean(distance_weights[distance_weights > 0]), 6),
  Validation = ifelse(diag_check && sum_check && non_neg_check, "✅ PASS", "❌ FAIL"),
  stringsAsFactors = FALSE
)

print(weights_summary)

save(spatial_weights, coordinates, distance_weights, distance_matrix, 
     distance_analysis, weights_summary, rainfall_matrix,
     file = "output/07_spatial_weights_distance.RData")

cat("\n✅ Distance-Based Spatial Weights successfully created and saved!\n")
cat("💾 Saved to: output/07_spatial_weights_distance.RData\n")
cat("📈 Visualizations saved to: plots/07_distance_matrix.png & plots/07_distance_weights.png\n")
cat("🔄 Next step: Space-Time ACF analysis\n")
cat(paste(rep("=", 60), collapse = ""), "\n")