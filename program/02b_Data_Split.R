# ----------------------------------------------------------------------------
# Define Train/Test by Date Range (instead of fixed index)
# ----------------------------------------------------------------------------
train_start <- as.Date("2015-01-01")
train_end   <- as.Date("2023-12-31")
test_start  <- as.Date("2024-01-01")
test_end    <- as.Date("2024-12-31")

# Pastikan objek 'dates' ada (biasanya dari 01_rainfall_data.RData)
if (!exists("dates")) stop("❌ 'dates' variable not found in dataset!")

# Filter data berdasarkan rentang waktu
train_idx <- which(dates >= train_start & dates <= train_end)
test_idx  <- which(dates >= test_start & dates <= test_end)

train_data <- rainfall_matrix[train_idx, ]
test_data  <- rainfall_matrix[test_idx, ]

train_time <- dates[train_idx]
test_time  <- dates[test_idx]

# Validasi hasil
if (nrow(train_data) == 0) stop("❌ Tidak ada data pada rentang training (2015–2023)")
if (nrow(test_data) == 0) warning("⚠️ Tidak ada data pada rentang testing (2024)")

cat("✅ Training range:", min(train_time), "→", max(train_time), "(", nrow(train_data), "rows)\n")
cat("✅ Testing range :", min(test_time), "→", max(test_time), "(", nrow(test_data), "rows)\n\n")

# ----------------------------------------------------------------------------
# Save train/test datasets to output folder
# ----------------------------------------------------------------------------
if (!dir.exists("output")) dir.create("output")

save(train_data, train_time, file = "output/train_data_2015_2023.RData")
save(test_data,  test_time,  file = "output/test_data_2024.RData")

cat("💾 Training data saved to: output/train_data_2015_2023.RData\n")
cat("💾 Testing  data saved to: output/test_data_2024.RData\n")

# ----------------------------------------------------------------------------
# Combine into one file (optional, for easier load later)
# ----------------------------------------------------------------------------
save(train_data, test_data, train_time, test_time,
     file = "output/02b_data_split.RData")

cat("✅ Combined train/test saved to: ooutput/02b_data_split.RData")
cat("🎯 Data split by date range completed successfully!\n")
