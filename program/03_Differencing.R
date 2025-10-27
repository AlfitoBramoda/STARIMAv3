# ============================================================================
# 03_Differencing.R - Seasonal Differencing for Stationarity (Train 2015–2023 Only, D=1, s=12)
# ============================================================================
# Purpose : Apply first-order seasonal differencing (D=1, s=12) on Box-Cox-transformed
#           or raw training data (2015–2023) to stabilize seasonal mean.
# Author  : STARMA Project
# Date    : 2025
# ============================================================================

cat("🔄 Data Differencing Started (Seasonal D=1, s=12, Train 2015–2023 Only)...\n")

# ----------------------------------------------------------------------------
# 1️⃣ Libraries
# ----------------------------------------------------------------------------
library(tseries)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)

# ----------------------------------------------------------------------------
# 2️⃣ Load Source Data (Box-Cox first, fallback to raw)
# ----------------------------------------------------------------------------
if (file.exists("output/02c_boxcox_data.RData")) {
  load("output/02c_boxcox_data.RData")
  if (exists("final_data")) {
    input_for_diff <- final_data
    cat("✅ Using Box-Cox transformed data for differencing.\n")
  } else {
    input_for_diff <- train_data
    cat("⚠️ Box-Cox file found, but 'final_data' not detected. Using raw train_data.\n")
  }
} else {
  load("output/02b_data_split.RData")
  input_for_diff <- train_data
  cat("⚠️ Box-Cox file not found. Using raw train_data for differencing.\n")
}

# Validate
if (!exists("train_time")) stop("❌ 'train_time' not found — please re-run 02b_Data_Split.R")
if (!is.matrix(input_for_diff) && !is.data.frame(input_for_diff)) {
  stop("❌ Input data for differencing must be a matrix or data frame.")
}

cat("📊 Input data dimensions:", dim(input_for_diff), "\n")
cat("📅 Training time range:", min(train_time), "→", max(train_time), "\n\n")

# ----------------------------------------------------------------------------
# 3️⃣ Seasonal Differencing Parameters
# ----------------------------------------------------------------------------
D_order <- 1     # Seasonal differencing order
s_period <- 12   # Periodicity (12 bulan)
cat("📋 Seasonal Differencing Parameters:\n")
cat("- Seasonal Order (D):", D_order, "\n")
cat("- Seasonal Period (s):", s_period, "\n\n")

# ----------------------------------------------------------------------------
# 4️⃣ Apply Seasonal Differencing
# ----------------------------------------------------------------------------
regions <- colnames(train_data)
n_regions <- length(regions)

cat("🔁 Applying seasonal differencing (D=1, s=12) for all regions...\n")

differenced_data <- list()
for (r in regions) {
  differenced_data[[r]] <- diff(train_data[, r], lag = s_period, differences = D_order)
}

integration_order <- rep(D_order, n_regions)
names(integration_order) <- regions

cat("✅ Seasonal differencing complete for all", n_regions, "regions (D=1, s=12)\n")

# ----------------------------------------------------------------------------
# 5️⃣ Create Differenced Matrix
# ----------------------------------------------------------------------------
min_length <- min(sapply(differenced_data, length))
differenced_matrix <- matrix(NA, nrow = min_length, ncol = n_regions)
colnames(differenced_matrix) <- regions

for (i in 1:n_regions) {
  differenced_matrix[, i] <- tail(differenced_data[[i]], min_length)
}

diff_dates <- tail(train_time, min_length)

cat("📅 Differenced date range:", min(diff_dates), "to", max(diff_dates), "\n")

# ----------------------------------------------------------------------------
# 6️⃣ Visualization
# ----------------------------------------------------------------------------
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

plot_data <- data.frame(Date = diff_dates, differenced_matrix)
plot_data_long <- pivot_longer(plot_data, cols = -Date,
                               names_to = "Region", values_to = "Differenced_Rainfall")

p <- ggplot(plot_data_long, aes(Date, Differenced_Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(
    title = "Seasonally Differenced Rainfall (Train 2015–2023, D=1, s=12)",
    subtitle = paste0("After seasonal differencing (", min_length, " obs)"),
    x = "Date", y = "Differenced Rainfall"
  ) +
  theme_minimal() + theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

ggsave("plots/03_differenced_train_D1S12.png", p, width = 12, height = 8, dpi = 300)
print(p)
cat("✅ Saved: plots/03_differenced_train_D1S12.png\n")

# ----------------------------------------------------------------------------
# 7️⃣ Save Results (same variable naming convention as non-seasonal)
# ----------------------------------------------------------------------------
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

save(differenced_matrix, integration_order, diff_dates,
     file = "output/03_differencing_results.RData")

cat("\n💾 Saved to output/03_differencing_results.RData\n")
cat("✅ Seasonal differencing (D=1, s=12) completed successfully!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
