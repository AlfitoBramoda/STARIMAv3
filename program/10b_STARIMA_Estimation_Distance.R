# ============================================================================
# STARMA Forecasting Pipeline - Phase 4: STARIMA Estimation (Automatic p,d,q)
# File   : 10a_STARIMA_Estimation_distance.R
# Purpose: Estimate STARIMA(p,d,q) automatically for distance spatial weights
# Author : STARMA Analysis
# Date   : 2025
# ============================================================================

cat("🚀 PHASE 4: STARIMA ESTIMATION (distance Weights, Automatic p,d,q)\n\n")

# ============================================================================
# 1️⃣ LOAD REQUIRED DATA
# ============================================================================
load("output/09_model_structure_all_weights.RData")  # contains model_structures
load("output/05_spatial_weights.RData")              # contains spatial_weights
load("output/02b_data_split.RData")                  # contains train_data, train_time

library(starma)
library(ggplot2)
library(gridExtra)

# Extract model structure for distance weights
if (!"distance" %in% names(model_structures)) {
  stop("❌ No distance model structure found in model_structures.")
}

distance_spec <- model_structures$distance

p_order <- distance_spec$p_order
d_order <- distance_spec$d_order
q_order <- distance_spec$q_order
ar_mask <- distance_spec$ar_mask
ma_mask <- distance_spec$ma_mask

cat("📊 Model Specification Loaded:\n")
cat("- Weight type  :", "distance\n")
cat("- STARIMA Order:", paste0("(", p_order, ",", d_order, ",", q_order, ")\n"))
cat("- AR mask size :", paste(dim(ar_mask), collapse = " × "), "\n")
cat("- MA mask size :", paste(dim(ma_mask), collapse = " × "), "\n\n")

# ============================================================================
# 2️⃣ DATA & SPATIAL WEIGHTS PREPARATION
# ============================================================================
distance_matrix <- spatial_weights$distance
max_spatial_lag <- nrow(ar_mask) - 1
wlist_distance <- list()

# Spatial lag 0 = Identity matrix
wlist_distance[[1]] <- diag(nrow(distance_matrix))

# Spatial lag 1 = Original weights
wlist_distance[[2]] <- distance_matrix

# Spatial lag 2 = Squared weights (second-order neighbors)
if (max_spatial_lag >= 2) {
  wlist_distance[[3]] <- distance_matrix %*% distance_matrix
}

# Normalize all spatial weight matrices
for (i in 2:length(wlist_distance)) {
  wlist_distance[[i]] <- sweep(wlist_distance[[i]], 1, rowSums(wlist_distance[[i]]), FUN = "/")
}

cat("📦 Data & Weight Info:\n")
cat("- Training observations:", nrow(train_data), "\n")
cat("- Regions              :", ncol(train_data), "\n")
cat("- Spatial lag matrices :", length(wlist_distance), "\n\n")

# ============================================================================
# 3️⃣ STARIMA MODEL ESTIMATION
# ============================================================================
cat("🔧 Estimating STARIMA model automatically...\n")

start_time <- Sys.time()
starima_distance <- NULL

tryCatch({
  starima_distance <- starma(
    data = train_data,
    wlist = wlist_distance,
    ar = ar_mask,
    ma = ma_mask
  )
}, error = function(e) {
  stop(paste("❌ Model estimation failed:", e$message))
})

end_time <- Sys.time()
estimation_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

cat("✅ Model estimation completed successfully!\n")
cat("⏱️ Estimation time:", estimation_time, "seconds\n\n")

# ============================================================================
# 4️⃣ MODEL SUMMARY & COEFFICIENTS
# ============================================================================
cat("📊 MODEL SUMMARY\n")
summary_starima <- summary(starima_distance)
print(summary_starima)

coef_table <- data.frame(
  Parameter   = rownames(summary_starima$coefficients),
  Estimate    = round(summary_starima$coefficients[, "Estimate"], 4),
  Std_Error   = round(summary_starima$coefficients[, "Std..Error"], 4),
  t_value     = round(summary_starima$coefficients[, "t.value"], 3),
  p_value     = round(summary_starima$coefficients[, "p.value"], 4),
  Significant = ifelse(summary_starima$coefficients[, "p.value"] < 0.05, "***",
                       ifelse(summary_starima$coefficients[, "p.value"] < 0.1, "*", "")),
  stringsAsFactors = FALSE
)

cat("\n📋 Parameter Estimates:\n")
print(coef_table)

# ============================================================================
# 5️⃣ MODEL FIT STATISTICS
# ============================================================================
# ============================================================================
# 5️⃣ MODEL FIT STATISTICS (Safe Extraction)
# ============================================================================
cat("\n📈 Fit Statistics:\n")

# --- Safe extractors ---
extract_numeric <- function(obj, name) {
  # Try to get directly
  if (!is.null(obj[[name]]) && is.numeric(obj[[name]])) return(obj[[name]])
  
  # Try inside nested model component
  if (!is.null(obj$model) && !is.null(obj$model[[name]]) && is.numeric(obj$model[[name]])) {
    return(obj$model[[name]])
  }
  
  # Try inside summary
  if (exists("summary_starima")) {
    s <- summary_starima
    if (!is.null(s[[name]]) && is.numeric(s[[name]])) return(s[[name]])
  }
  
  # Not found → return NA
  return(NA_real_)
}

loglik <- extract_numeric(starima_distance, "loglik")
aic <- extract_numeric(starima_distance, "aic")
bic <- extract_numeric(starima_distance, "bic")

# --- Calculate if missing ---
if (is.na(aic) && !is.na(loglik)) {
  n_params <- nrow(coef_table)
  aic <- -2 * loglik + 2 * n_params
}
if (is.na(bic) && !is.na(loglik)) {
  bic <- -2 * loglik + log(nrow(train_data)) * nrow(coef_table)
}

# --- Safety conversion to numeric ---
loglik <- suppressWarnings(as.numeric(loglik))
aic <- suppressWarnings(as.numeric(aic))
bic <- suppressWarnings(as.numeric(bic))

cat("- Log-likelihood:", ifelse(is.na(loglik), "N/A", round(loglik, 4)), "\n")
cat("- AIC:", ifelse(is.na(aic), "N/A", round(aic, 4)), "\n")
cat("- BIC:", ifelse(is.na(bic), "N/A", round(bic, 4)), "\n")
cat("- Parameters:", nrow(coef_table), "\n")
cat("- Observations:", nrow(train_data), "\n\n")


# ============================================================================
# 6️⃣ RESIDUAL ANALYSIS
# ============================================================================
residuals_distance <- starima_distance$residuals
residual_stats <- data.frame(
  Statistic = c("Mean", "Std Dev", "Min", "Max"),
  Value = c(mean(residuals_distance, na.rm = TRUE),
            sd(residuals_distance, na.rm = TRUE),
            min(residuals_distance, na.rm = TRUE),
            max(residuals_distance, na.rm = TRUE))
)

cat("🔍 Residual summary:\n")
print(residual_stats)

# ============================================================================
# 7️⃣ VISUALIZATION
# ============================================================================
cat("\n📊 Generating visualizations...\n")

# 1️⃣ Coefficient Plot
coef_plot <- ggplot(coef_table, aes(x = Parameter, y = Estimate)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = Estimate - 1.96*Std_Error,
                    ymax = Estimate + 1.96*Std_Error), width = 0.2) +
  labs(title = paste0("STARIMA(", p_order, ",", d_order, ",", q_order, ") Coefficients - distance"),
       subtitle = paste("Parameters:", nrow(coef_table)),
       x = "Parameter", y = "Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/10a_distance_coefficients.png", coef_plot, width = 10, height = 6, dpi = 300)

# 2️⃣ Residual Plot
resid_df <- data.frame(Time = 1:length(residuals_distance), Residuals = as.vector(residuals_distance))
resid_plot <- ggplot(resid_df, aes(Time, Residuals)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Residuals over Time - distance Weights",
       subtitle = "Dashed line = 0",
       x = "Time", y = "Residuals") +
  theme_minimal()

ggsave("plots/10a_distance_residuals.png", resid_plot, width = 10, height = 6, dpi = 300)

cat("✅ Plots saved to: plots/10a_distance_coefficients.png and plots/10a_distance_residuals.png\n")

# ============================================================================
# 8️⃣ SAVE RESULTS
# ============================================================================
distance_results <- list(
  model = starima_distance,
  specification = distance_spec,
  coefficients = coef_table,
  fit_statistics = list(loglik = loglik, aic = aic, bic = bic),
  residuals = residuals_distance,
  residual_stats = residual_stats,
  estimation_time = estimation_time
)

save(distance_results, file = "output/10b_starima_distance.RData")
cat("\n💾 Results saved to: output/10b_starima_distance.RData\n")

# ============================================================================
# 🔚 COMPLETION SUMMARY
# ============================================================================
cat("\n=== STARIMA ESTIMATION COMPLETED ===\n")
cat("✅ Weight type  : distance\n")
cat("✅ STARIMA order: (", p_order, ",", d_order, ",", q_order, ")\n")
cat("✅ Parameters   :", nrow(coef_table), "\n")
cat("✅ AIC          :", round(aic, 3), "\n")
cat("✅ BIC          :", round(bic, 3), "\n")
cat("✅ Time elapsed :", estimation_time, "sec\n")
cat("✅ Results saved to output/10abstarima_distance.RData\n")
cat("🎯 Ready for: 10c_STARIMA_Estimation_Distance.R\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
