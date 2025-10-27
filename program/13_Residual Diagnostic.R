# ============================================================================
# STARIMA Forecasting Pipeline - Phase 4b: Residual Diagnostics (FINAL)
# File   : 13_Residual_Diagnostics_Final.R
# Purpose: Evaluate STARIMA residual adequacy (per region, fixed colnames)
# Author : STARMA Analysis (Final)
# Date   : 2024
# ============================================================================

cat("=== PHASE 4b: STARIMA RESIDUAL DIAGNOSTICS (FINAL FIX) ===\n\n")

# ----------------------------------------------------------------------------
# 1️⃣ Dependencies
# ----------------------------------------------------------------------------
req <- c("ggplot2", "tseries")
for (p in req) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

if (!dir.exists("plots")) dir.create("plots")

# ----------------------------------------------------------------------------
# 2️⃣ Load Selected Model from Phase 12
# ----------------------------------------------------------------------------
if (!file.exists("output/12_model_selection.RData")) {
  stop("❌ Missing required file: output/12_model_selection.RData. Please run Phase 12 first.")
}

load("output/12_model_selection.RData")

selected_model <- model_selection_results$selected_model$results_object
selected_name  <- model_selection_results$selected_model$name
selected_weight <- model_selection_results$selected_model$weight_scheme

cat("✅ Loaded selected model:", selected_name, "(", selected_weight, ")\n")

# ----------------------------------------------------------------------------
# 3️⃣ Extract Residuals
# ----------------------------------------------------------------------------
extract_residuals <- function(obj) {
  if (!is.null(obj$residuals)) return(obj$residuals)
  if (!is.null(obj$model$residuals)) return(obj$model$residuals)
  if (!is.null(obj$fit$residuals)) return(obj$fit$residuals)
  stop("❌ Residuals not found in model object.")
}

residuals_model <- extract_residuals(selected_model)

# Convert to matrix safely
residuals_model <- as.matrix(residuals_model)
n_regions <- ncol(residuals_model)

# Add default names if missing
if (is.null(colnames(residuals_model))) {
  colnames(residuals_model) <- c("Barat", "Selatan", "Tengah", "Timur", "Utara")[1:n_regions]
}
regions <- colnames(residuals_model)

cat("✅ Residuals extracted for regions:", paste(regions, collapse = ", "), "\n")

# ----------------------------------------------------------------------------
# 4️⃣ Diagnostics per Region
# ----------------------------------------------------------------------------
diagnostic_summary <- data.frame(
  Region = character(),
  Shapiro_Stat = numeric(),
  Shapiro_p = numeric(),
  LjungBox_Stat = numeric(),
  LjungBox_p = numeric(),
  Normality = character(),
  White_Noise = character(),
  stringsAsFactors = FALSE
)

cat("\n🔬 Running diagnostics per region...\n")

for (r in regions) {
  res <- as.numeric(na.omit(residuals_model[, r]))
  
  # --- Shapiro-Wilk (Normality)
  if (length(res) >= 3 && length(res) <= 5000) {
    sh <- shapiro.test(res)
    sh_p <- sh$p.value
    sh_stat <- sh$statistic
    normality <- ifelse(sh_p > 0.05, "✅ Normal", "❌ Non-normal")
  } else {
    sh_p <- NA
    sh_stat <- NA
    normality <- "⚠️ Sample too large/small"
  }
  
  # --- Ljung-Box (White Noise)
  if (length(res) >= 13) {
    lb <- Box.test(res, lag = 12, type = "Ljung-Box")
    lb_p <- lb$p.value
    lb_stat <- lb$statistic
    whitenoise <- ifelse(lb_p > 0.05, "✅ White noise", "❌ Autocorrelated")
  } else {
    lb_p <- NA
    lb_stat <- NA
    whitenoise <- "⚠️ Sample too short"
  }
  
  diagnostic_summary <- rbind(
    diagnostic_summary,
    data.frame(
      Region = r,
      Shapiro_Stat = round(sh_stat, 4),
      Shapiro_p = round(sh_p, 5),
      LjungBox_Stat = round(lb_stat, 4),
      LjungBox_p = round(lb_p, 5),
      Normality = normality,
      White_Noise = whitenoise
    )
  )
  
  # --- Plot ACF/PACF
  png(paste0("plots/13_residual_acf_pacf_", r, ".png"), width = 1200, height = 600)
  par(mfrow = c(1, 2))
  acf(res, main = paste("ACF Residuals -", r))
  pacf(res, main = paste("PACF Residuals -", r))
  dev.off()
  
  # --- Plot Histogram
  png(paste0("plots/13_residual_histogram_", r, ".png"), width = 800, height = 600)
  hist(res, breaks = 20, col = "lightblue", border = "white",
       main = paste("Residual Distribution -", r),
       xlab = "Residuals", ylab = "Frequency")
  dev.off()
}

# ----------------------------------------------------------------------------
# 5️⃣ Output Summary
# ----------------------------------------------------------------------------
cat("\n📋 Diagnostic Summary:\n")
print(diagnostic_summary)

write.csv(diagnostic_summary, "output/13_residual_diagnostics_summary.csv", row.names = FALSE)
save(diagnostic_summary, residuals_model, file = "output/13_residual_diagnostics.RData")

cat("\n💾 Results saved:\n")
cat("- output/13_residual_diagnostics_summary.csv\n")
cat("- output/13_residual_diagnostics.RData\n")
cat("📊 Plots saved in: plots/13_residual_*.png\n")
cat("✅ Residual diagnostics (final) completed successfully!\n")
