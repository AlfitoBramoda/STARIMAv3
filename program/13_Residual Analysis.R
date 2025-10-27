# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4b: Residual Visualization per Region
# File: 11b_STARIMA_Residual_Visualization.R
# Purpose: Visualize residual diagnostics for STARIMA model (Correlation Weights)
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

cat("🚀 Starting STARIMA Residual Visualization (Correlation Weights)...\n\n")

# ============================================================================
# LOAD REQUIRED LIBRARIES
# ============================================================================
required_pkgs <- c("ggplot2", "gridExtra", "forecast")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# LOAD MODEL OUTPUT
# ============================================================================
cat("📦 Loading correlation model output...\n")
load("output/10c_starima_correlation.RData")

# ----------------------------------------------------------------------------
# Detect residuals dynamically (auto-scan structure)
# ----------------------------------------------------------------------------
if (!exists("correlation_results")) {
  stop("❌ 'correlation_results' object not found in 10c_starima_correlation.RData")
}

if (!is.null(correlation_results$residuals)) {
  resid_matrix <- correlation_results$residuals
  cat("✅ Residuals found in correlation_results$residuals\n")
} else if (!is.null(correlation_results$model$residuals)) {
  resid_matrix <- correlation_results$model$residuals
  cat("✅ Residuals found in correlation_results$model$residuals\n")
} else if (!is.null(correlation_results$model$resid)) {
  resid_matrix <- correlation_results$model$resid
  cat("✅ Residuals found in correlation_results$model$resid\n")
} else if (!is.null(correlation_results$resid)) {
  resid_matrix <- correlation_results$resid
  cat("✅ Residuals found in correlation_results$resid\n")
} else {
  cat("⚠️ Residuals not found in typical locations. Structure of correlation_results:\n")
  print(str(correlation_results, max.level = 2))
  stop("❌ Could not locate residuals inside correlation_results object.")
}

# ============================================================================
# CHECK STRUCTURE
# ============================================================================
cat("📊 Residual matrix loaded with dimensions:", dim(resid_matrix), "\n")

if (is.null(colnames(resid_matrix))) {
  regions <- paste0("Region_", seq_len(ncol(resid_matrix)))
  colnames(resid_matrix) <- regions
  cat("⚠️ Region names not found — assigned generic names.\n")
} else {
  regions <- colnames(resid_matrix)
}

cat("📍 Regions detected:", paste(regions, collapse = ", "), "\n\n")

if (!dir.exists("plots")) dir.create("plots")

# ============================================================================
# 1️⃣ TIME SERIES PLOTS OF RESIDUALS
# ============================================================================
cat("📈 Generating residual time-series plots per region...\n")

ts_plots <- list()
for (r in regions) {
  df <- data.frame(Time = 1:nrow(resid_matrix),
                   Residual = resid_matrix[, r])
  
  p <- ggplot(df, aes(x = Time, y = Residual)) +
    geom_line(color = "darkred", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = c(-2 * sd(df$Residual, na.rm = TRUE),
                              2 * sd(df$Residual, na.rm = TRUE)),
               color = "gray40", linetype = "dotted") +
    labs(title = paste("Residual Time Series -", r),
         subtitle = "Dashed lines = ±2σ bounds",
         x = "Time (Index)", y = "Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(paste0("plots/11b_residual_timeseries_", r, "_correlation.png"), 
         p, width = 8, height = 4, dpi = 300)
  ts_plots[[r]] <- p
  cat("✅ Time-series plot saved for:", r, "\n")
}

# ============================================================================
# 2️⃣ HISTOGRAM + DENSITY OF RESIDUALS
# ============================================================================
cat("\n📊 Generating histogram and density plots per region...\n")

hist_plots <- list()
for (r in regions) {
  df <- data.frame(Residual = resid_matrix[, r])
  
  p <- ggplot(df, aes(x = Residual)) +
    geom_histogram(aes(y = ..density..),
                   bins = 25, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = paste("Residual Distribution -", r),
         subtitle = "Histogram with density overlay",
         x = "Residual", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(paste0("plots/11b_residual_histogram_", r, "_correlation.png"),
         p, width = 7, height = 4, dpi = 300)
  hist_plots[[r]] <- p
  cat("✅ Histogram plot saved for:", r, "\n")
}

# ============================================================================
# 3️⃣ ACF & PACF PLOTS OF RESIDUALS
# ============================================================================
cat("\n🔁 Generating ACF/PACF residual diagnostics...\n")

for (r in regions) {
  ts_resid <- ts(resid_matrix[, r])
  png(paste0("plots/11b_residual_acf_pacf_", r, "_correlation.png"),
      width = 1000, height = 400)
  par(mfrow = c(1, 2))
  Acf(ts_resid, main = paste("ACF Residual -", r))
  Pacf(ts_resid, main = paste("PACF Residual -", r))
  dev.off()
  cat("✅ ACF/PACF plot saved for:", r, "\n")
}
par(mfrow = c(1, 1))

# ============================================================================
# 4️⃣ GRID VISUALIZATION PREVIEW
# ============================================================================
cat("\n🖼️ Displaying combined preview (first two regions)...\n")

if (length(ts_plots) >= 2) {
  gridExtra::grid.arrange(ts_plots[[1]], hist_plots[[1]],
                          ts_plots[[2]], hist_plots[[2]],
                          ncol = 2)
}

# ============================================================================
# SAVE RESULTS
# ============================================================================
save(resid_matrix, ts_plots, hist_plots,
     file = "output/11b_starima_residual_visualization_correlation.RData")

cat("\n💾 All residual plots saved to 'plots/' folder.\n")
cat("📁 Results summary saved to: output/11b_starima_residual_visualization_correlation.RData\n\n")
cat("🎯 Residual diagnostics completed successfully.\n")
cat("Next: proceed to 12_STARIMA_Forecasting_Per_Region.R for forecasting.\n")
