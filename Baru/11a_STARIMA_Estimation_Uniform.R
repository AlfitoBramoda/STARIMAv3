# ============================================================================
# 10a_STARIMA_Estimation_Uniform.R - STARIMA Estimation using Uniform Weights
# ============================================================================
# Purpose : Fit STARIMA model using uniform spatial weights (1st–3rd order)
# Author  : STARIMA Project
# Date    : 2025
# ============================================================================

cat("🚀 STARIMA Estimation - Uniform Weight Started...\n")

# ============================================================================
# GLOBAL PARAMETERS
# ============================================================================
MAX_SPATIAL_LAG <- 2
MAX_TEMPORAL_LAG <- 40

cat("📋 Global Parameters:\n")
cat("- MAX_SPATIAL_LAG:", MAX_SPATIAL_LAG, "\n")
cat("- MAX_TEMPORAL_LAG:", MAX_TEMPORAL_LAG, "\n\n")

# ----------------------------------------------------------------------------
# 1️⃣ Load Required Libraries
# ----------------------------------------------------------------------------
suppressMessages({
  library(starma)
})

# ----------------------------------------------------------------------------
# 2️⃣ Load Data and Model Structure
# ----------------------------------------------------------------------------
required_files <- c(
  "output/02_data_split.RData",
  "output/10_model_structure.RData",
  "output/07_spatial_weights.RData"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0)
  stop(paste("❌ File berikut belum tersedia:", paste(missing_files, collapse = ", ")))

load("output/02_data_split.RData")      # centered_matrix, test_data
load("output/10_model_structure.RData") # ar_mask, ma_mask, p_order, q_order, dll
load("output/07_spatial_weights.RData") # spatial_weights list (uniform, distance, correlation)

cat("📦 Data & struktur model berhasil dimuat.\n\n")

# ----------------------------------------------------------------------------
# Load Model Structure
# ----------------------------------------------------------------------------
load("output/10_model_structure.RData") # ar_mask, ma_mask, p_order, q_order, dll

# Tambahkan integration order jika belum ada
if (!exists("d_order")) d_order <- 1

cat("📋 Diagnostic Setup:\n")
cat("===================\n")
cat("- Model: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n")

# ----------------------------------------------------------------------------
# 3️⃣ Data Validation
# ----------------------------------------------------------------------------
if (!is.matrix(centered_matrix)) centered_matrix <- as.matrix(centered_matrix)
if (any(is.na(centered_matrix))) stop("❌ Terdapat missing values pada centered_matrix.")
if (!is.numeric(centered_matrix)) stop("❌ centered_matrix harus berupa numeric matrix.")
cat("✅ Data validation passed. Dimensi data:", dim(centered_matrix), "\n\n")

# ----------------------------------------------------------------------------
# 4️⃣ Prepare Spatial Weights (Uniform)
# ----------------------------------------------------------------------------
uniform_matrix <- spatial_weights$uniform
max_spatial_lag <- MAX_SPATIAL_LAG

# Generate lagged weights properly via matrix multiplication
wlist_uniform <- list()
wlist_uniform[[1]] <- diag(nrow(uniform_matrix)) # slag 0 (identitas)

for (lag in 1:max_spatial_lag) {
  w_temp <- uniform_matrix
  if (lag > 1) {
    for (k in 2:lag) w_temp <- w_temp %*% uniform_matrix
  }
  # Normalisasi baris
  for (i in 1:nrow(w_temp)) {
    s <- sum(w_temp[i, ])
    if (s > 0) w_temp[i, ] <- w_temp[i, ] / s
  }
  wlist_uniform[[lag + 1]] <- w_temp
}

cat("✅ Spatial weights (uniform) berhasil disiapkan.\n\n")

# ----------------------------------------------------------------------------
# 5️⃣ Fit STARIMA Model with Error Handling
# ----------------------------------------------------------------------------
cat("🧩 Estimasi STARIMA sedang berjalan...\n")

starima_uniform <- tryCatch(
  {
    starma(
      data = centered_matrix,
      ar = ar_mask,
      ma = ma_mask,
      wlist = wlist_uniform,
      iterate = 50   # tanpa argumen 'tol'
    )
  },
  error = function(e) {
    cat("❌ Estimation error:", e$message, "\n")
    return(NULL)
  },
  warning = function(w) {
    cat("⚠️ Estimation warning:", w$message, "\n")
    NULL
  }
)

# ----------------------------------------------------------------------------
# 6️⃣ Validate Model Output
# ----------------------------------------------------------------------------
estimation_success <- !is.null(starima_uniform) &&
  !is.null(starima_uniform$phi) &&
  !is.null(starima_uniform$theta)

if (estimation_success) {
  cat("📈 Estimasi STARIMA berhasil.\n\n")
} else {
  cat("⚠️ Estimasi gagal atau parameter kosong.\n\n")
}

# ----------------------------------------------------------------------------
# 7️⃣ Compute Fit Statistics (Manual LogLik)
# ----------------------------------------------------------------------------
fit_stats <- list()

if (estimation_success) {
  residuals <- as.vector(starima_uniform$residuals)
  n <- length(residuals)
  k <- length(starima_uniform$phi) + length(starima_uniform$theta)
  
  SSE <- sum(residuals^2)
  MSE <- mean(residuals^2)
  RMSE <- sqrt(MSE)
  sigma2 <- SSE / n
  
  # Manual log-likelihood (assuming Gaussian errors)
  logLik_manual <- -0.5 * n * (log(2 * pi * sigma2) + 1)
  AIC_manual <- -2 * logLik_manual + 2 * k
  BIC_manual <- -2 * logLik_manual + log(n) * k
  
  fit_stats <- list(
    n = n, k = k, SSE = SSE, MSE = MSE, RMSE = RMSE,
    logLik = logLik_manual,
    AIC = AIC_manual,
    BIC = BIC_manual
  )
  
  cat("📊 Fit statistics calculated manually.\n\n")
} else {
  fit_stats <- list(SSE = NA, MSE = NA, RMSE = NA, AIC = NA, BIC = NA)
}

# ----------------------------------------------------------------------------
# 8️⃣ Build Comprehensive Results Object
# ----------------------------------------------------------------------------
uniform_results <- list(
  model = starima_uniform,
  wlist = wlist_uniform,
  ar_mask = ar_mask,
  ma_mask = ma_mask,
  fit_stats = fit_stats,
  estimation_success = estimation_success,
  timestamp = Sys.time()
)

# ----------------------------------------------------------------------------
# 9️⃣ Save Results
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 9️⃣ Save Results
# ----------------------------------------------------------------------------
save(
  uniform_results, ar_mask, ma_mask, p_order, q_order, d_order,
  file = "output/11a_starima_uniform.RData"
)

cat("💾 Hasil disimpan di: output/11a_starima_uniform.RData\n\n")
cat("🎯 STARIMA Estimation - Uniform Weight Finished!\n")

# ----------------------------------------------------------------------------
# 🔟 Show Summary in Console
# ----------------------------------------------------------------------------
cat("\n📤 Ringkasan Hasil Estimasi STARIMA (Uniform Weight):\n")
if (uniform_results$estimation_success) {
  cat("\n📍 Parameter AR (phi):\n")
  print(uniform_results$model$phi)
  
  cat("\n📍 Parameter MA (theta):\n")
  print(uniform_results$model$theta)
  
  cat("\n📊 Fit Statistics:\n")
  print(uniform_results$fit_stats)
} else {
  cat("❌ Estimasi gagal, tidak ada parameter yang dapat ditampilkan.\n")
}
