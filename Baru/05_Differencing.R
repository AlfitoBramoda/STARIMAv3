# ============================================================================
# 05_Differencing.R - Seasonal Differencing (s = 12)
# ============================================================================

cat("🔄 Seasonal Differencing (lag = 12) Started...\n")

# Clear any existing dates_diff object to avoid conflicts
if (exists("dates_diff")) {
  rm(dates_diff)
}

# Load Box-Cox or previous data
load("output/04_boxcox_data.RData")

data_in <- final_data
regions <- colnames(data_in)
n_regions <- length(regions)

# Seasonal differencing parameter
diff_lag_seasonal <- 12

# Prepare output containers
differenced_matrix <- matrix(NA, nrow = nrow(data_in) - diff_lag_seasonal, ncol = n_regions)
colnames(differenced_matrix) <- regions
integration_order <- 1  # karena hanya 1 tahap differencing musiman

# Perform seasonal differencing
for (i in 1:n_regions) {
  ts_data <- data_in[, i]
  
  if (length(ts_data) > diff_lag_seasonal) {
    diff_seasonal <- diff(ts_data, lag = diff_lag_seasonal)
    differenced_matrix[, i] <- diff_seasonal
  } else {
    warning(paste("⚠️ Data region", regions[i], "terlalu pendek untuk differencing musiman."))
    differenced_matrix[, i] <- rep(NA, nrow(differenced_matrix))
    integration_order[i] <- 0
  }
}

# Hapus baris NA yang muncul karena differencing
valid_rows <- complete.cases(differenced_matrix)
differenced_matrix <- differenced_matrix[valid_rows, ]

# Inisiasi plot_dates sesuai dengan data setelah differencing
plot_dates <- seq(as.Date("2016-01-31"), by = "month", length.out = nrow(differenced_matrix))

# Save results
save(differenced_matrix, integration_order, plot_dates, regions,
     file = "output/05_differencing_results.RData")

cat("✅ Seasonal differencing completed with lag = 12\n")
cat("💾 Saved to output/05_differencing_results.RData\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
