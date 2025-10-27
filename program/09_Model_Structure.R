# ============================================================================
# STARMA Forecasting Pipeline - Phase 3b: Automatic STARIMA Order Selection (All Weights)
# File   : 09_Auto_STARIMA_Selection_All_Weights.R
# Purpose: Automatically select optimal STARIMA(p,d,q) per spatial weight (Uniform, Distance, Correlation)
# Author : STARMA Analysis
# Date   : 2025
# ============================================================================

cat("🚀 Starting AUTO STARIMA ORDER SELECTION (ALL WEIGHTS)...\n\n")

# ----------------------------------------------------------------------------
# 1️⃣ Libraries
# ----------------------------------------------------------------------------
required_packages <- c("starma", "dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------------------------------------------------------
# 2️⃣ Load Data and Spatial Weights
# ----------------------------------------------------------------------------
load("output/04_centered_data.RData")   # centered_matrix
load("output/05_spatial_weights.RData") # spatial_weights

cat("📦 Data & spatial weights loaded successfully!\n")
cat("- Data dimensions:", dim(centered_matrix), "\n\n")

# ----------------------------------------------------------------------------
# 3️⃣ Define Weight Matrices
# ----------------------------------------------------------------------------
wlist_uniform <- list(diag(ncol(centered_matrix)), spatial_weights$uniform)
wlist_distance <- list(diag(ncol(centered_matrix)), spatial_weights$distance)
wlist_correlation <- list(diag(ncol(centered_matrix)), spatial_weights$correlation)

weight_types <- list(
  uniform = wlist_uniform,
  distance = wlist_distance,
  correlation = wlist_correlation
)

# ============================================================================
# 4️⃣ Define Auto Selection Function
# ============================================================================
auto_starima_selection <- function(data, wlist, max_p = 3, max_d = 1, max_q = 3) {
  cat("🔍 Running auto STARIMA selection...\n")
  cat("Testing p:", 0:max_p, "d:", 0:max_d, "q:", 0:max_q, "\n\n")
  
  results <- data.frame(p = integer(), d = integer(), q = integer(),
                        AIC = numeric(), BIC = numeric(), Shapiro_P = numeric(),
                        Success = logical(), stringsAsFactors = FALSE)
  
  best_model <- NULL
  best_bic <- Inf
  
  for (p in 0:max_p) {
    for (d in 0:max_d) {
      for (q in 0:max_q) {
        if (p == 0 && q == 0) next
        cat(sprintf("Testing STARIMA(%d,%d,%d)... ", p, d, q))
        
        tryCatch({
          ar_mask <- if (p > 0) array(1, dim = c(p, length(wlist))) else NULL
          ma_mask <- if (q > 0) array(1, dim = c(q, length(wlist))) else NULL
          
          model <- starma(data = data, ar = ar_mask, ma = ma_mask, wlist = wlist, iterate = 50)
          
          residuals <- as.vector(model$residuals)
          n <- length(residuals)
          k <- length(c(model$phi, model$theta))
          sigma2 <- sum(residuals^2, na.rm = TRUE) / n
          loglik <- -0.5 * n * (log(2 * pi * sigma2) + 1)
          aic <- -2 * loglik + 2 * k
          bic <- -2 * loglik + log(n) * k
          shapiro_p <- if (n > 3 && n < 5000) shapiro.test(residuals)$p.value else NA
          
          results <- rbind(results, data.frame(
            p = p, d = d, q = q,
            AIC = aic, BIC = bic, Shapiro_P = shapiro_p,
            Success = TRUE
          ))
          
          if (bic < best_bic) {
            best_bic <- bic
            best_model <- list(model = model, order = c(p, d, q),
                               aic = aic, bic = bic, loglik = loglik, shapiro_p = shapiro_p)
          }
          cat("✅ BIC:", round(bic, 3), "\n")
          
        }, error = function(e) {
          cat("❌ Error:", substr(e$message, 1, 40), "...\n")
          results <<- rbind(results, data.frame(
            p = p, d = d, q = q,
            AIC = NA, BIC = NA, Shapiro_P = NA, Success = FALSE
          ))
        })
      }
    }
  }
  
  results <- results[order(results$BIC, na.last = TRUE), ]
  cat("\n🏁 Auto-selection completed. Total models tested:", nrow(results), "\n")
  
  list(
    best_model = best_model,
    all_results = results,
    top_5 = head(results[results$Success == TRUE, ], 5)
  )
}

# ============================================================================
# 5️⃣ Execute Auto Selection for Each Weight Type
# ============================================================================
auto_all_results <- list()
summary_table <- data.frame()

for (w in names(weight_types)) {
  cat("\n=============================================================\n")
  cat("⚙️ Running AUTO STARIMA for", toupper(w), "weights...\n")
  cat("=============================================================\n")
  
  res <- auto_starima_selection(centered_matrix, weight_types[[w]], max_p = 3, max_d = 1, max_q = 3)
  auto_all_results[[w]] <- res
  
  if (!is.null(res$best_model)) {
    ord <- res$best_model$order
    summary_table <- rbind(summary_table, data.frame(
      Weight = toupper(w),
      p = ord[1], d = ord[2], q = ord[3],
      AIC = round(res$best_model$aic, 2),
      BIC = round(res$best_model$bic, 2),
      Shapiro_P = round(res$best_model$shapiro_p, 4)
    ))
    
    save(res, file = paste0("output/auto_starima_selection_", w, ".RData"))
    cat("💾 Saved:", paste0("output/auto_starima_selection_", w, ".RData"), "\n")
  }
}

# ============================================================================
# 6️⃣ Summary Comparison
# ============================================================================
cat("\n📊 STARIMA ORDER SELECTION SUMMARY (ALL WEIGHTS)\n")
cat("=============================================================\n")
print(summary_table)

# Save combined summary
save(auto_all_results, summary_table, file = "output/auto_starima_selection_all_weights.RData")

cat("\n✅ All results saved to: output/auto_starima_selection_all_weights.RData\n")
cat("🎯 Auto STARIMA model selection completed for all spatial weights.\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
