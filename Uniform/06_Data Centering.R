# ============================================================================
# 04_Data_Centering.R - Data Centering and Scaling (Fixed Dates Alignment)
# Purpose: Center and scale stationary data using starma::stcenter()
# Author : STARMA Project
# Date   : 2024
# ============================================================================

cat("🎯 Data Centering Started...\n")

# ----------------------------------------------------------------------------
# 1) Libraries
# ----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(starma)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

# ----------------------------------------------------------------------------
# 2) Load differencing results
# ----------------------------------------------------------------------------
load("output/05_differencing_results.RData")
if (!exists("differenced_matrix"))
  stop("❌ 'differenced_matrix' tidak ditemukan di output/05_differencing_results.RData")
cat("📊 Data loaded: differenced_matrix (", nrow(differenced_matrix), "x", ncol(differenced_matrix), ")\n")

regions <- colnames(differenced_matrix)
n_regions <- length(regions)
# ----------------------------------------------------------------------------
# 3) BEFORE CENTERING - Stats
# ----------------------------------------------------------------------------
cat("\n📊 BEFORE CENTERING - Original Statistics:\n")
original_stats <- data.frame(
  Region = regions,
  Mean = round(apply(differenced_matrix, 2, mean, na.rm = TRUE), 4),
  SD   = round(apply(differenced_matrix, 2, sd,   na.rm = TRUE), 4),
  Min  = round(apply(differenced_matrix, 2, min,  na.rm = TRUE), 4),
  Max  = round(apply(differenced_matrix, 2, max,  na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(original_stats)

# ----------------------------------------------------------------------------
# 4) Centering (stcenter → fallback manual)
# ----------------------------------------------------------------------------
cat("\n🔄 Applying stcenter()...\n")
centered_matrix <- NULL
centering_params <- NULL

tryCatch({
  centered_result <- stcenter(differenced_matrix)
  
  if (is.list(centered_result)) {
    centered_matrix <- centered_result$center
    centering_params <- list(
      means = centered_result$mean,
      sds   = centered_result$sd
    )
  } else {
    centered_matrix <- centered_result
    centering_params <- list(
      means = apply(differenced_matrix, 2, mean, na.rm = TRUE),
      sds   = apply(differenced_matrix, 2, sd,   na.rm = TRUE)
    )
  }
  cat("✅ Centering completed (stcenter)\n")
}, error = function(e) {
  cat("⚠️ stcenter() error -> using manual centering. Message:", e$message, "\n")
  centered_matrix <- scale(differenced_matrix, center = TRUE, scale = TRUE)
  centering_params <- list(
    means = attr(centered_matrix, "scaled:center"),
    sds   = attr(centered_matrix, "scaled:scale")
  )
  attributes(centered_matrix) <- NULL
  dim(centered_matrix) <- c(nrow(differenced_matrix), ncol(differenced_matrix))
  colnames(centered_matrix) <- regions
  cat("✅ Manual centering completed\n")
})

# ----------------------------------------------------------------------------
# 5) AFTER CENTERING - Stats & Validation
# ----------------------------------------------------------------------------
cat("\n📊 AFTER CENTERING - Centered Statistics:\n")
centered_stats <- data.frame(
  Region = regions,
  Mean = round(apply(centered_matrix, 2, mean, na.rm = TRUE), 6),
  SD   = round(apply(centered_matrix, 2, sd,   na.rm = TRUE), 6),
  Min  = round(apply(centered_matrix, 2, min,  na.rm = TRUE), 4),
  Max  = round(apply(centered_matrix, 2, max,  na.rm = TRUE), 4),
  stringsAsFactors = FALSE
)
print(centered_stats)

cat("\n🔍 Centering Validation:\n")
mean_check <- all(abs(apply(centered_matrix, 2, mean)) < 0.15)
sd_check   <- all(abs(apply(centered_matrix, 2, sd)   - 1) < 0.15)
cat("Mean ≈ 0 check:", ifelse(mean_check, "✅ PASS", "❌ FAIL"), "\n")
cat("SD   ≈ 1 check:", ifelse(sd_check,   "✅ PASS", "❌ FAIL"), "\n")
if (mean_check && sd_check) cat("🎉 Data successfully centered and scaled!\n") else cat("⚠️ Centering may need adjustment\n")

cat("\n📈 Before vs After Comparison:\n")
comparison <- data.frame(
  Region        = regions,
  Original_Mean = round(original_stats$Mean, 3),
  Centered_Mean = round(centered_stats$Mean, 6),
  Original_SD   = round(original_stats$SD, 3),
  Centered_SD   = round(centered_stats$SD, 6),
  stringsAsFactors = FALSE
)
print(comparison)

cat("\n📏 Matrix Dimensions:\n")
cat("Original matrix:", nrow(differenced_matrix), "x", ncol(differenced_matrix), "\n")
cat("Centered matrix:", nrow(centered_matrix), "x", ncol(centered_matrix), "\n")
cat("Dimensions preserved:", ifelse(identical(dim(differenced_matrix), dim(centered_matrix)), "✅ YES", "❌ NO"), "\n")

cat("\n📊 Centered Data Summary:\n")
cat("Overall range: [", round(min(centered_matrix), 3), ",", round(max(centered_matrix), 3), "]\n")
cat("Overall mean:", round(mean(centered_matrix), 6), "\n")
cat("Overall SD  :", round(sd(as.vector(centered_matrix)), 6), "\n")
missing_count <- sum(is.na(centered_matrix))
cat("Missing values:", missing_count, ifelse(missing_count==0,"(none) ✅","\n"))

# ----------------------------------------------------------------------------
# 6) Build plot dates that ALWAYS match centered_matrix rows
# ----------------------------------------------------------------------------
cat("\n\n🗓️ Aligning dates for plotting...\n")

n_centered <- nrow(centered_matrix)

# Prioritas nama variabel tanggal:
# 1) diff_dates (baru)
# 2) differenced_dates (legacy)
plot_dates <- NULL
if (exists("diff_dates")) {
  plot_dates <- as.Date(diff_dates)
} else if (exists("differenced_dates")) {
  plot_dates <- as.Date(differenced_dates)
}

# Jika tidak ada tanggal, buat tanggal sintetik bulanan
if (is.null(plot_dates)) {
  plot_dates <- seq(as.Date("2015-01-01"), by = "month", length.out = n_centered)
} else {
  # Sesuaikan panjangnya dengan centered_matrix
  if (length(plot_dates) > n_centered) {
    plot_dates <- tail(plot_dates, n_centered)
  } else if (length(plot_dates) < n_centered) {
    centered_matrix <- tail(centered_matrix, length(plot_dates))
    n_centered <- nrow(centered_matrix)
  }
}

# simpan tanggal yang sudah selaras sebagai diff_dates
diff_dates <- plot_dates

# ----------------------------------------------------------------------------
# 7) VISUALIZATION
# ----------------------------------------------------------------------------
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

cat("\n📈 Creating Time Series Plots...\n")

plot_data <- data.frame(
  Date = diff_dates,
  centered_matrix
)

plot_data_long <- plot_data |>
  pivot_longer(cols = -Date, names_to = "Region", values_to = "Centered_Rainfall")

# (Opsional) Ambil rentang Y dari data asli untuk referensi skala
if (file.exists("output/01_rainfall_data.RData")) {
  load("output/01_rainfall_data.RData")  # dates, rainfall_matrix
  original_plot_long <- data.frame(Date = as.Date(dates), rainfall_matrix) |>
    pivot_longer(cols = -Date, names_to = "Region", values_to = "Rainfall")
  
  y_ranges <- original_plot_long |>
    group_by(Region) |>
    summarise(ymin = min(Rainfall, na.rm = TRUE),
              ymax = max(Rainfall, na.rm = TRUE), .groups = "drop")
  
  p_centered <- ggplot(plot_data_long, aes(x = Date, y = Centered_Rainfall, color = Region)) +
    geom_line(size = 0.8) +
    labs(title = "Centered Rainfall Time Series by Region",
         x = "Date", y = "Centered Rainfall (same scale as original)",
         subtitle = paste("After differencing and centering (", n_centered, " observations)")) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(~Region, ncol = 2) +
    geom_blank(data = merge(plot_data_long, y_ranges, by = "Region"),
               aes(y = ymin)) +
    geom_blank(data = merge(plot_data_long, y_ranges, by = "Region"),
               aes(y = ymax))
} else {
  p_centered <- ggplot(plot_data_long, aes(x = Date, y = Centered_Rainfall, color = Region)) +
    geom_line(size = 0.8) +
    labs(title = "Centered Rainfall Time Series by Region",
         x = "Date", y = "Centered Rainfall",
         subtitle = paste("After differencing and centering (", n_centered, " observations)")) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(~Region, ncol = 2)
}

ggsave("plots/04_centered_timeseries.png", p_centered, width = 12, height = 8, dpi = 300)
print(p_centered)
cat("✅ Centered time series plot saved: plots/04_centered_timeseries.png\n")

# Free Y scale plot
p_centered_free <- ggplot(plot_data_long, aes(x = Date, y = Centered_Rainfall, color = Region)) +
  geom_line(size = 0.8) +
  labs(title = "Centered Rainfall Time Series by Region (Free Y Scale)",
       x = "Date", y = "Centered Rainfall (mean≈0, sd≈1)",
       subtitle = paste("After differencing and centering (", n_centered, " observations)")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

ggsave("plots/04_centered_timeseries_free.png", p_centered_free, width = 12, height = 8, dpi = 300)
print(p_centered_free)
cat("✅ Centered time series (free Y) plot saved: plots/04_centered_timeseries_free.png\n")

# Boxplot
p_boxplot <- ggplot(plot_data_long, aes(x = Region, y = Centered_Rainfall, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Centered Rainfall Distribution by Region",
       x = "Region", y = "Centered Rainfall (mean≈0, sd≈1)",
       subtitle = "Box plots after centering") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

ggsave("plots/04_centered_boxplot.png", p_boxplot, width = 10, height = 6, dpi = 300)
print(p_boxplot)
cat("✅ Centered boxplot saved: plots/04_centered_boxplot.png\n")

# Histogram
p_histogram <- ggplot(plot_data_long, aes(x = Centered_Rainfall, fill = Region)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  labs(title = "Centered Rainfall Distribution by Region",
       x = "Centered Rainfall (mean≈0, sd≈1)", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Region, scales = "free_y", ncol = 2)

ggsave("plots/04_centered_histogram.png", p_histogram, width = 12, height = 8, dpi = 300)
print(p_histogram)
cat("✅ Centered histogram saved: plots/04_centered_histogram.png\n")

# ----------------------------------------------------------------------------
# 8) Save outputs (note: save diff_dates yang SUDAH SELARAS)
# ----------------------------------------------------------------------------
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

save(centered_matrix, centering_params, integration_order,
     diff_dates, coordinates, original_stats, centered_stats,
     file = "output/06_centered_data.RData")

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("📋 CENTERING SUMMARY:\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("✅ Data centering completed successfully\n")
cat("📊 Matrix dimensions:", nrow(centered_matrix), "x", ncol(centered_matrix), "\n")
cat("🎯 All regions: Mean ≈ 0, SD ≈ 1\n")
cat("💾 Results saved to: output/06_centered_data.RData\n")
cat("🔄 Next step: Create spatial weight matrices\n")
cat("✅ Data ready for STARMA identification!\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
