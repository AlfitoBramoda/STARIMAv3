# ============================================================================
# convert_coordinates_to_km.R
# ============================================================================
# Purpose : Convert longitude/latitude (degrees) → pairwise distances (km)
# Author  : STARMA Project
# Date    : 2024
# ============================================================================

cat("🌍 Coordinate Conversion to Kilometers Script Loaded\n")

# ============================================================================
# LOAD DATA
# ============================================================================
load("output/01_rainfall_data.RData")
cat("📊 Data loaded: rainfall_matrix (", nrow(rainfall_matrix), "x", ncol(rainfall_matrix), ")\n")

# Define regions and coordinates
regions <- colnames(rainfall_matrix)
n_regions <- length(regions)

cat("\n🗺 Spatial Information:\n")
cat("Regions:", paste(regions, collapse = ", "), "\n")
cat("Number of regions:", n_regions, "\n")

# Display coordinates
cat("\n📍 Coordinates:\n")
print(coordinates)

# ============================================================================
# FUNCTION: compute_distance_matrix_km
# ============================================================================
compute_distance_matrix_km <- function(coordinates) {
  if (!all(c("Region", "Latitude", "Longitude") %in% colnames(coordinates))) {
    stop("❌ Input must contain columns: Region, Latitude, Longitude")
  }
  
  regions <- coordinates$Region
  n_regions <- length(regions)
  
  distance_matrix_km <- matrix(0, nrow = n_regions, ncol = n_regions)
  rownames(distance_matrix_km) <- colnames(distance_matrix_km) <- regions
  
  # ----------------------------
  # 🔹 Convert degrees → km using Euclidean approximation
  # ----------------------------
  for (i in 1:n_regions) {
    for (j in 1:n_regions) {
      if (i != j) {
        delta_lat <- coordinates$Latitude[j] - coordinates$Latitude[i]
        delta_lon <- coordinates$Longitude[j] - coordinates$Longitude[i]
        
        # konversi derajat ke kilometer
        lat_km <- delta_lat * 111.32
        lon_km <- delta_lon * 111.32 * cos(mean(c(coordinates$Latitude[i], coordinates$Latitude[j])) * pi / 180)
        
        # jarak Euclidean (km)
        distance_matrix_km[i, j] <- sqrt(lat_km^2 + lon_km^2)
      }
    }
  }
  
  cat("\n✅ Distance matrix (km) computed\n")
  cat("Range:", round(min(distance_matrix_km[distance_matrix_km > 0]), 3), "–",
      round(max(distance_matrix_km), 3), "km\n")
  
  return(distance_matrix_km)
}

# ============================================================================
# COMPUTE DISTANCE MATRIX
# ============================================================================
distance_matrix_km <- compute_distance_matrix_km(coordinates)

cat("\n--- Distance Matrix (km) ---\n")
print(round(distance_matrix_km, 2))

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(distance_matrix_km, file = "output/06_distance_matrix_km.RData")
cat("\n💾 Distance matrix saved to 'output/06_distance_matrix_km.RData'\n")
