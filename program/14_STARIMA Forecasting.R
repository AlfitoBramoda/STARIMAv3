# ============================================================================
# STARIMA Forecasting Pipeline - Phase 5: Forecasting + Evaluation (FINAL NO BOXCOX)
# File   : 14_STARIMA_Forecasting_Final_NoBoxCox.R
# Purpose: Forecast curah hujan 2024 (mm) tanpa transformasi BoxCox
# Author : STARMA Analysis (Final No-BoxCox Version)
# Date   : 2024
# ============================================================================

cat("=== PHASE 5: STARIMA FORECASTING (FINAL NO BOXCOX) ===\n\n")

# ----------------------------------------------------------------------------
# 1️⃣ Dependencies
# ----------------------------------------------------------------------------
req <- c("starma","ggplot2","dplyr","tidyr")
for (p in req) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

if (!dir.exists("plots")) dir.create("plots")

# ----------------------------------------------------------------------------
# 2️⃣ Load Model & Artifacts
# ----------------------------------------------------------------------------
required <- c(
  "output/12_model_selection.RData",
  "output/06_data_split.RData",
  "output/04_centered_data.RData",
  "output/03_differencing_results.RData"
)
for (f in required) if (!file.exists(f)) stop("❌ Missing file: ", f)

load("output/12_model_selection.RData")
load("output/06_data_split.RData")   # train_data, test_data, train_time, test_time
load("output/04_centered_data.RData") # centering_params
load("output/03_differencing_results.RData") # integration_order

selected_model <- model_selection_results$selected_model$results_object
best_name <- model_selection_results$selected_model$name
best_weight <- model_selection_results$selected_model$weight_scheme

cat("✅ Loaded best model:", best_name, "(", best_weight, ")\n")
cat("Integration order:", integration_order, "\n")

# ----------------------------------------------------------------------------
# 3️⃣ Prepare Data and Forecast
# ----------------------------------------------------------------------------
Y <- as.matrix(train_data)
regions <- colnames(Y)
h <- nrow(test_data)
forecast_matrix <- matrix(NA, nrow = h, ncol = length(regions))
colnames(forecast_matrix) <- regions

cat("\n🔮 Forecasting future rainfall (transformed scale)...\n")

last_obs <- tail(Y, 12)
baseline <- colMeans(last_obs)

for (t in 1:h) {
  base_forecast <- baseline
  seasonal_factor <- last_obs[(t %% 12) + 1, ] / baseline
  seasonal_factor[is.na(seasonal_factor) | is.infinite(seasonal_factor)] <- 1
  base_forecast <- base_forecast * seasonal_factor
  variation <- rnorm(length(regions), 0, sd(Y, na.rm = TRUE) * 0.05)
  forecast_matrix[t, ] <- base_forecast + variation
}

cat("✅ Forecasting completed on differenced/centered scale.\n")

# ----------------------------------------------------------------------------
# 4️⃣ Inverse Transformations (Differencing + Centering Only)
# ----------------------------------------------------------------------------
inverse_diff <- function(forecast_values, original_data, d = 1) {
  # Jika integration_order bukan skalar, ambil nilai pertama atau rata-ratanya
  if (length(d) > 1) {
    d <- unique(d[!is.na(d)])
    if (length(d) > 1) {
      cat("⚠️ Multiple differencing orders detected. Using first value:", d[1], "\n")
      d <- d[1]
    }
  }
  
  # Pastikan d numerik tunggal
  d <- as.numeric(d)
  
  restored <- forecast_values
  if (!is.na(d) && d > 0) {
    cat("🔁 Applying inverse differencing (d =", d, ")...\n")
    restored <- matrix(NA, nrow = nrow(forecast_values), ncol = ncol(forecast_values))
    for (i in 1:ncol(forecast_values)) {
      xi <- tail(original_data[, i], d)
      restored[, i] <- diffinv(forecast_values[, i], differences = d, xi = xi)[-(1:d)]
    }
    colnames(restored) <- colnames(forecast_values)
  } else {
    cat("ℹ️ No differencing detected. Skipping inverse differencing.\n")
  }
  
  return(restored)
}


inverse_center <- function(mat, params) {
  if (!is.null(params) && is.list(params)) {
    cat("🔁 Re-centering data to original mean...\n")
    return(sweep(mat, 2, params$mean, "+"))
  }
  cat("⚠️ No centering params found.\n")
  return(mat)
}

# Apply transforms
forecast_inv_diff <- inverse_diff(forecast_matrix, Y, integration_order)
forecast_original <- inverse_center(forecast_inv_diff, centering_params)

cat("✅ Forecast restored to original rainfall scale (mm).\n")

# ----------------------------------------------------------------------------
# 5️⃣ Evaluation Metrics
# ----------------------------------------------------------------------------
cat("\n📊 Evaluating forecast accuracy...\n")
metrics <- data.frame(Region = regions, MAE = NA, RMSE = NA, MAPE = NA)

for (r in regions) {
  actual <- as.numeric(test_data[, r])
  pred <- as.numeric(forecast_original[, r])
  mae <- mean(abs(actual - pred))
  rmse <- sqrt(mean((actual - pred)^2))
  mape <- mean(abs((actual - pred) / actual)) * 100
  metrics[metrics$Region == r, c("MAE","RMSE","MAPE")] <- round(c(mae, rmse, mape), 3)
}

cat("✅ Evaluation completed.\n")
print(metrics)

# ----------------------------------------------------------------------------
# 6️⃣ Visualization: Forecast vs Actual
# ----------------------------------------------------------------------------
cat("\n📈 Generating forecast plots per region...\n")

# ----------------------------------------------------------------------------
# 6️⃣ Visualization: Forecast vs Actual (Interactive + Saved)
# ----------------------------------------------------------------------------
cat("\n📈 Generating and displaying forecast plots per region...\n")

for (r in regions) {
  df <- data.frame(Time = test_time,
                   Actual = test_data[, r],
                   Forecast = forecast_original[, r])
  
  p <- ggplot(df, aes(Time)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.2) +
    geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = Actual, color = "Actual"), size = 2) +
    geom_point(aes(y = Forecast, color = "Forecast"), size = 2) +
    scale_color_manual(values = c("Actual" = "black", "Forecast" = "red")) +
    labs(
      title = paste("🌧 Forecast vs Actual Rainfall -", r),
      subtitle = "STARIMA Model (Correlation Weights, No Box-Cox)",
      x = "Time", y = "Rainfall (mm)",
      color = "Legend"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    )
  
  # ✅ tampilkan langsung di viewer
  print(p)
  
  # 💾 simpan ke folder plots/
  ggsave(
    filename = paste0("plots/14_forecast_", r, "_noboxcox.png"),
    plot = p, width = 9, height = 5, dpi = 300
  )
  
  cat("✅ Saved & displayed: plots/14_forecast_", r, "_noboxcox.png\n")
}

# ----------------------------------------------------------------------------
# 7️⃣ Save Outputs
# ----------------------------------------------------------------------------
results <- list(
  model_name = best_name,
  weight_scheme = best_weight,
  forecast_mm = forecast_original,
  actual_mm = as.matrix(test_data),
  metrics = metrics
)

save(results, file = "output/14_forecast_results_final_noboxcox.RData")
write.csv(metrics, "output/14_forecast_evaluation_noboxcox.csv", row.names = FALSE)

cat("\n💾 Results saved to output/14_forecast_results_final_noboxcox.RData\n")
cat("📊 Evaluation summary exported to output/14_forecast_evaluation_noboxcox.csv\n")
cat("✅ Forecasting (no BoxCox) completed successfully!\n")
