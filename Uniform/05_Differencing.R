# ============================================================================
# 05_Differencing.R - Seasonal Differencing after Box-Cox (Train 2015–2023)
# ============================================================================
# Purpose : Use Box-Cox transformed data (final_data) and apply seasonal
#           differencing D=1 with period s=12 on TRAIN set only
# Author  : STARMA Project
# Date    : 2024
# ============================================================================

cat("🔄 Seasonal Differencing (D=1, s=12) using Box-Cox data...\n")

# ----------------------------------------------------------------------------
# 1️⃣ Libraries
# ----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

# ----------------------------------------------------------------------------
# 2️⃣ Load Box-Cox Data (TRAIN)
# ----------------------------------------------------------------------------
# Diharapkan file ini memuat: final_data (hasil Box-Cox untuk TRAIN),
# train_data (asli), train_time (tanggal TRAIN), dst.
load("output/04_boxcox_data.RData")  
cat("📦 Loaded output/04_boxcox_data.RData\n")

# Pilih sumber data untuk differencing:
# - Gunakan final_data (hasil Box-Cox) jika tersedia
# - Jika tidak ada, fallback ke train_data
if (exists("final_data") && is.matrix(final_data)) {
  source_matrix <- final_data
  cat("✅ Using Box-Cox transformed TRAIN data (final_data)\n")
} else if (exists("train_data") && is.matrix(train_data)) {
  source_matrix <- train_data
  cat("ℹ️ final_data not found — using raw TRAIN data (train_data)\n")
} else {
  stop("❌ Neither 'final_data' nor 'train_data' is available in 04_boxcox_data.RData")
}

# Tanggal TRAIN (opsional, untuk plotting)
if (exists("train_time")) {
  train_dates <- as.Date(train_time)
  cat("🗓️ Train date range:", as.character(min(train_dates)), "→", as.character(max(train_dates)), "\n")
} else {
  train_dates <- NULL
  cat("⚠️ 'train_time' not found — plots will use index instead of dates.\n")
}

regions    <- colnames(source_matrix)
n_regions  <- length(regions)
n_obs      <- nrow(source_matrix)

cat("📊 Data dims (TRAIN after Box-Cox / source):", n_obs, "×", n_regions, "\n\n")

# ----------------------------------------------------------------------------
# 3️⃣ Seasonal Differencing Parameters
# ----------------------------------------------------------------------------
D_order  <- 1   # seasonal difference order
s_period <- 12  # monthly seasonality
cat("📋 Seasonal Differencing Parameters: D =", D_order, "| s =", s_period, "\n\n")

# ----------------------------------------------------------------------------
# 4️⃣ Apply Seasonal Differencing (D=1, s=12)
# ----------------------------------------------------------------------------
cat("🔁 Applying seasonal differencing to each region...\n")

differenced_list <- vector("list", n_regions)
names(differenced_list) <- regions

for (r in regions) {
  # diff(x, lag = s, differences = D)
  differenced_list[[r]] <- diff(source_matrix[, r], lag = s_period, differences = D_order)
}

# Ukuran "kerugian" observasi karena seasonal differencing
lost <- D_order * s_period
cat("ℹ️ Observations lost due to seasonal differencing:", lost, "\n")

# ----------------------------------------------------------------------------
# 5️⃣ Build Differenced Matrix (equal lengths)
# ----------------------------------------------------------------------------
min_length <- min(sapply(differenced_list, length))
differenced_matrix <- matrix(NA_real_, nrow = min_length, ncol = n_regions)
colnames(differenced_matrix) <- regions

for (i in seq_along(regions)) {
  x <- differenced_list[[ regions[i] ]]
  if (length(x) >= min_length) {
    differenced_matrix[, i] <- tail(x, min_length)
  } else {
    # (Jarang terjadi) jika ada yang lebih pendek — pad tail
    pad <- min_length - length(x)
    differenced_matrix[, i] <- c(rep(NA_real_, pad), x)
  }
}

# Buat vektor tanggal untuk hasil differencing (jika train_dates ada)
if (!is.null(train_dates)) {
  # Seasonal differencing menggeser mulai sebanyak 'lost' observasi
  diff_dates_full <- train_dates[(lost + 1):length(train_dates)]
  # Selaraskan panjang dengan min_length (take tail)
  diff_dates <- tail(diff_dates_full, min_length)
  cat("📅 Differenced TRAIN date range:", as.character(min(diff_dates)), "→", as.character(max(diff_dates)), "\n")
} else {
  diff_dates <- NULL
}

# ----------------------------------------------------------------------------
# 6️⃣ integration_order (menjaga kompatibilitas pipeline)
# ----------------------------------------------------------------------------
# Banyak skrip downstream memeriksa integration_order sebagai SCALAR (length==1).
# Di sini kita simpan sebagai 1 (D=1) agar bisa dibaca sebagai d_order/deteksi differencing.
integration_order <- 1

cat("\n✅ Seasonal differencing completed.\n")
cat("- Differenced matrix dims:", nrow(differenced_matrix), "×", ncol(differenced_matrix), "\n")
cat("- integration_order (scalar):", integration_order, "\n\n")

# ----------------------------------------------------------------------------
# 7️⃣ Visualization (faceted per region)
# ----------------------------------------------------------------------------
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

plot_df <- data.frame(
  Index = seq_len(nrow(differenced_matrix)),
  differenced_matrix
)

if (!is.null(diff_dates)) {
  plot_df$Date <- diff_dates
  x_var <- "Date"
} else {
  x_var <- "Index"
}

plot_long <- plot_df |>
  pivot_longer(cols = all_of(regions), names_to = "Region", values_to = "Value")

p <- ggplot(plot_long, aes_string(x = x_var, y = "Value", color = "Region")) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  labs(
    title = "Seasonally Differenced Rainfall (TRAIN, D=1, s=12)",
    subtitle = paste0("Source: ", if (exists("final_data")) "Box-Cox (final_data)" else "train_data",
                      " | Obs: ", nrow(differenced_matrix)),
    x = if (!is.null(diff_dates)) "Date" else "Index",
    y = "Differenced Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

if (!is.null(diff_dates)) {
  p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
}

ggsave("plots/05_differenced_train_D1S12_boxcox.png", p, width = 12, height = 8, dpi = 300)
print(p)
cat("📈 Saved plot: plots/05_differenced_train_D1S12_boxcox.png\n")

# ----------------------------------------------------------------------------
# 8️⃣ Save Results (sama seperti sebelumnya untuk kompatibilitas)
# ----------------------------------------------------------------------------
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

save(
  differenced_matrix,   # matriks hasil seasonal differencing (TRAIN)
  integration_order,    # scalar = 1 (untuk mendeteksi ada differencing)
  diff_dates,           # tanggal TRAIN sesudah differencing (atau NULL)
  file = "output/05_differencing_results.RData"
)

cat("\n💾 Saved to: output/05_differencing_results.RData\n")
cat("🎯 Done: Seasonal differencing after Box-Cox (D=1, s=12) on TRAIN data.\n")
cat(paste(rep("=", 60), collapse = ""), "\n")