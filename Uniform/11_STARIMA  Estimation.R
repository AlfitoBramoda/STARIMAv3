# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation
# File   : 11_STARIMA_Estimation_Uniform.R
# Purpose: Estimate STARIMA(p,d,q) model using uniform spatial weights
# Author : STARMA Analysis
# Date   : 2024
# ============================================================================

suppressPackageStartupMessages({
  library(starma)
  library(spdep)
  library(ggplot2)
  library(gridExtra)
})

# ------------------------------- Guardrails ---------------------------------
`%nz%` <- function(x, y) if (is.null(x)) y else x   # portable null-coalescing
nznum  <- function(x, val=0) ifelse(is.finite(x), x, val)

halt <- function(...) { message(paste0("❌ ", paste(..., collapse=" "))); stop(invisible(NULL)) }
note <- function(...)  message(paste0("ℹ️ ", paste(..., collapse=" ")))
ok   <- function(...)  message(paste0("✅ ", paste(..., collapse=" ")))

# ------------------------------ Data Loading --------------------------------
req_files <- c(
  "output/09_stpacf_uniform_only.RData",
  "output/07_spatial_weights_uniform.RData",
  "output/05_differencing_results.RData"
)
missing <- req_files[!file.exists(req_files)]
if (length(missing)) halt("Missing required files: ", paste(missing, collapse=", "))

load("output/09_stpacf_uniform_only.RData")
load("output/07_spatial_weights_uniform.RData")
load("output/05_differencing_results.RData")

if (!exists("differenced_matrix")) halt("'differenced_matrix' not found in 05_differencing_results.RData")
if (!exists("spatial_weights") || is.null(spatial_weights$uniform)) halt("'spatial_weights$uniform' not found")

data_input <- differenced_matrix   # gunakan hasil differencing
d_order <- 1

# ------------------------------ Model Orders (EXPERIMENT HERE!) --------------------------------
# 🧪 CHANGE THESE VALUES TO EXPERIMENT WITH DIFFERENT ORDERS:
p_order <- 1          # AR order (try: 1, 2, 3, 4)
q_order <- 0          # MA order (try: 1, 2, 3)
max_spatial_lag <- 1  # Spatial lags (usually keep at 1)

# Model order validation
if (p_order < 0 || q_order < 0) {
  halt("Model orders must be non-negative integers")
}
if (p_order == 0 && q_order == 0) {
  halt("Invalid model: STARIMA(0,", d_order, ",0) has no temporal structure (pure white noise)")
}

cat("=== STARIMA ESTIMATION - UNIFORM WEIGHTS ===\n\n")
cat("📋 Estimation Setup:\n")
cat("===================\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d)\n", p_order, d_order, q_order))
cat("- Spatial weights: Uniform-based\n")
cat(sprintf("- Training data: %d observations\n", nrow(data_input)))
cat(sprintf("- Regions: %d (%s)\n", ncol(data_input),
            paste(head(colnames(data_input) %nz% paste0("Region_", seq_len(ncol(data_input)))), collapse=", ")))
cat(sprintf("- Max spatial lag: %d (lags 0..%d)\n\n", max_spatial_lag, max_spatial_lag))

# --------------------------- Build/Validate wlist ----------------------------
U <- spatial_weights$uniform
if (!is.matrix(U) || nrow(U) != ncol(U)) halt("Uniform weight matrix must be square")

# lag-0 = I, lag-1 = U, lag-k = row-normalized U^k
wlist_uniform <- vector("list", max_spatial_lag + 1L)
wlist_uniform[[1]] <- diag(nrow(U))
wlist_uniform[[2]] <- U

for (i in seq_along(wlist_uniform)) {
  rs <- rowSums(wlist_uniform[[i]])
  rs[rs == 0] <- 1
  wlist_uniform[[i]] <- wlist_uniform[[i]] / rs
}

ok("Spatial weights list constructed with ", length(wlist_uniform), " lags")

# ------------------------------ Dynamic Masks (AR/MA) --------------------------------
# Create masks dynamically based on p_order and q_order
ar_mask <- if (p_order > 0) {
  matrix(FALSE, p_order, max_spatial_lag + 1)
} else {
  matrix(FALSE, 0, max_spatial_lag + 1)  # Empty but valid matrix
}

ma_mask <- if (q_order > 0) {
  matrix(FALSE, q_order, max_spatial_lag + 1)
} else {
  matrix(FALSE, 0, max_spatial_lag + 1)  # Empty but valid matrix
}

# Activate temporal lags for both spatial lag 0 and 1
if (p_order > 0) {
  # SLAG 0: Activate all temporal lags (within-region effects)
  for (p in seq_len(p_order)) {
    ar_mask[p, 1] <- TRUE  # tlag_p-slag0
  }
  
  # SLAG 1: Activate all temporal lags (cross-region effects)
  for (p in seq_len(p_order)) {
    ar_mask[p, 2] <- TRUE  # tlag_p-slag1
  }
}

if (q_order > 0) {
  # SLAG 0: Activate all temporal lags (within-region effects)
  for (q in seq_len(q_order)) {
    ma_mask[q, 1] <- TRUE  # tlag_q-slag0
  }
  
  # SLAG 1: Activate all temporal lags (cross-region effects)
  for (q in seq_len(q_order)) {
    ma_mask[q, 2] <- TRUE  # tlag_q-slag1
  }
}

cat("🎯 Dynamic Mask Configuration:\n")
cat(sprintf("- AR mask: %dx%d (p=%d, spatial_lags=%d)\n", 
           nrow(ar_mask), ncol(ar_mask), p_order, max_spatial_lag))
cat(sprintf("- MA mask: %dx%d (q=%d, spatial_lags=%d)\n", 
           nrow(ma_mask), ncol(ma_mask), q_order, max_spatial_lag))
cat(sprintf("- Total AR parameters: %d\n", sum(ar_mask)))
cat(sprintf("- Total MA parameters: %d\n", sum(ma_mask)))
cat(sprintf("- Total parameters: %d\n\n", sum(ar_mask) + sum(ma_mask)))

# Debug: Show actual mask matrices
cat("🔍 AR Mask Matrix:\n")
print(ar_mask)
cat("\n🔍 MA Mask Matrix:\n")
print(ma_mask)
cat("\n")

# Debug: Show expected parameters
if (p_order > 0) {
  cat("📋 Expected AR Parameters:\n")
  for (p in 1:p_order) {
    for (s in 0:max_spatial_lag) {
      if (ar_mask[p, s+1]) {
        cat(sprintf("  - phi%d%d (temporal lag %d, spatial lag %d)\n", p, s, p, s))
      }
    }
  }
}

if (q_order > 0) {
  cat("📋 Expected MA Parameters:\n")
  for (q in 1:q_order) {
    for (s in 0:max_spatial_lag) {
      if (ma_mask[q, s+1]) {
        cat(sprintf("  - theta%d%d (temporal lag %d, spatial lag %d)\n", q, s, q, s))
      }
    }
  }
}
cat("\n")

ok("Dynamic masks created successfully")

# ----------------------------- Data Hygiene ----------------------------------
na_rows <- which(!stats::complete.cases(data_input))
if (length(na_rows)) {
  note(length(na_rows), " rows with NA detected — removing for estimation")
  data_input <- data_input[-na_rows, , drop = FALSE]
}
if (nrow(data_input) <= max(p_order, q_order) + 2)
  halt("Not enough observations after NA handling: ", nrow(data_input))

# --------------------------- Model Estimation --------------------------------
cat("\n🔧 Estimating STARIMA Models (Separate + Joint)...\n")

# =============================================================================
# SEPARATE ESTIMATION: SLAG 0 ONLY (Original phi10)
# =============================================================================
cat("\n1️⃣ Estimating SLAG 0 only model...\n")

# Create SLAG 0 only masks
ar_mask_slag0 <- if (p_order > 0) {
  matrix(FALSE, p_order, 1)  # Only spatial lag 0
} else {
  matrix(FALSE, 0, 1)
}

ma_mask_slag0 <- if (q_order > 0) {
  matrix(FALSE, q_order, 1)  # Only spatial lag 0
} else {
  matrix(FALSE, 0, 1)
}

# Activate only SLAG 0 parameters
if (p_order > 0) {
  for (p in seq_len(p_order)) {
    ar_mask_slag0[p, 1] <- TRUE  # Only tlag_p-slag0
  }
}

if (q_order > 0) {
  for (q in seq_len(q_order)) {
    ma_mask_slag0[q, 1] <- TRUE  # Only tlag_q-slag0
  }
}

# SLAG 0 wlist (Identity matrix only)
wlist_slag0 <- list(diag(nrow(U)))

# Estimate SLAG 0 model
fit_slag0 <- try(
  starma(
    data = data_input,
    wlist = wlist_slag0,
    ar    = ar_mask_slag0,
    ma    = ma_mask_slag0
  ),
  silent = TRUE
)

if (inherits(fit_slag0, "try-error")) {
  cat("⚠️ SLAG 0 estimation failed:\n")
  cat(as.character(fit_slag0), "\n")
  fit_slag0 <- NULL
} else {
  cat("✅ SLAG 0 model estimated successfully\n")
  cat("🔍 SLAG 0 parameters:", paste(rownames(summary(fit_slag0)$coefficients), collapse=", "), "\n")
}

# =============================================================================
# SEPARATE ESTIMATION: SLAG 1 ONLY (Pure spatial)
# =============================================================================
cat("\n2️⃣ Estimating SLAG 1 only model...\n")

# Create SLAG 1 only masks
ar_mask_slag1 <- if (p_order > 0) {
  matrix(FALSE, p_order, 1)  # Only spatial lag 1
} else {
  matrix(FALSE, 0, 1)
}

ma_mask_slag1 <- if (q_order > 0) {
  matrix(FALSE, q_order, 1)  # Only spatial lag 1
} else {
  matrix(FALSE, 0, 1)
}

# Activate SLAG 1 parameters (all temporal lags for spatial lag 1)
if (p_order > 0) {
  for (p in seq_len(p_order)) {
    ar_mask_slag1[p, 1] <- TRUE  # tlag_p-slag1
  }
}

if (q_order > 0) {
  for (q in seq_len(q_order)) {
    ma_mask_slag1[q, 1] <- TRUE  # tlag_q-slag1
  }
}

# SLAG 1 wlist (Uniform matrix only)
wlist_slag1 <- list(U)

# Estimate SLAG 1 model
fit_slag1 <- try(
  starma(
    data = data_input,
    wlist = wlist_slag1,
    ar    = ar_mask_slag1,
    ma    = ma_mask_slag1
  ),
  silent = TRUE
)

if (inherits(fit_slag1, "try-error")) {
  cat("⚠️ SLAG 1 estimation failed:\n")
  cat(as.character(fit_slag1), "\n")
  fit_slag1 <- NULL
} else {
  cat("✅ SLAG 1 model estimated successfully\n")
  cat("🔍 SLAG 1 parameters:", paste(rownames(summary(fit_slag1)$coefficients), collapse=", "), "\n")
}

# =============================================================================
# JOINT ESTIMATION: SLAG 0 + SLAG 1 (Current approach)
# =============================================================================
cat("\n3️⃣ Estimating joint SLAG 0+1 model...\n")

estimation_start_time <- Sys.time()

# Standard estimation
fit <- try(
  starma(
    data = data_input,
    wlist = wlist_uniform,
    ar    = ar_mask,
    ma    = ma_mask
  ),
  silent = TRUE
)

if (inherits(fit, "try-error")) {
  cat("⚠️ Joint estimation failed:\n")
  cat(as.character(fit), "\n")
  halt("Joint model estimation failed. Try reducing model complexity.")
} else {
  cat("✅ Joint model estimation successful\n")
}

estimation_time <- Sys.time() - estimation_start_time
ok(sprintf("Joint model estimation completed in %.2f sec", as.numeric(estimation_time)))

# ------------------------------- Summary -------------------------------------
cat("\n📊 Joint Model Summary:\n========================\n")
print(summary(fit))

if (!is.null(fit_slag0)) {
  cat("\n📊 SLAG 0 Only Model Summary:\n==============================\n")
  print(summary(fit_slag0))
} else {
  cat("\n⚠️ SLAG 0 model failed - no summary available\n")
}

if (!is.null(fit_slag1)) {
  cat("\n📊 SLAG 1 Only Model Summary:\n==============================\n")
  print(summary(fit_slag1))
} else {
  cat("\n⚠️ SLAG 1 model failed - no summary available\n")
}

# Extract coefficients for comparison
sm <- summary(fit)
coef_df <- as.data.frame(sm$coefficients, stringsAsFactors = FALSE)
std_names <- c("Estimate","Std. Error","t value","Pr(>|t|)")
for (nm in std_names) if (!nm %in% names(coef_df)) coef_df[[nm]] <- NA_real_
names(coef_df)[match(std_names, names(coef_df), nomatch = 0)] <- c("Estimate","Std.Error","t.value","p.value")

coef_table <- data.frame(
  Parameter   = rownames(coef_df),
  Estimate    = round(coef_df$Estimate, 6),
  Std_Error   = round(coef_df$Std.Error, 6),
  t_value     = round(coef_df$t.value, 4),
  p_value     = round(coef_df$p.value, 6),
  Significant = ifelse(is.finite(coef_df$p.value) & coef_df$p.value < 0.05, "***",
                       ifelse(is.finite(coef_df$p.value) & coef_df$p.value < 0.10, "*","")),
  row.names = NULL, check.names = FALSE
)

# Fit stats
loglik <- fit$loglik %nz% NA_real_
aic <- fit$aic %nz% (-2*loglik + 2*nrow(coef_df))
bic <- fit$bic %nz% (-2*loglik + log(nrow(data_input))*nrow(coef_df))

cat("\n📈 Model Fit Statistics:\n")
cat("- Log-likelihood:", round(loglik, 4), "\n")
cat("- AIC:", round(aic, 4), "\n")
cat("- BIC:", round(bic, 4), "\n")
cat("- Parameters:", nrow(coef_table), "\n")
cat("- Observations:", nrow(data_input), "\n\n")

# =============================================================================
# COMPARISON OF ALL THREE APPROACHES
# =============================================================================
cat("\n📊 Comparing SLAG 0, SLAG 1, and Joint models...\n")

# Extract coefficients from separate models
if (!is.null(fit_slag0)) {
  coef_slag0 <- summary(fit_slag0)$coefficients
  phi10_original <- coef_slag0["phi10", 1]  # Original phi10
  phi10_pvalue_original <- coef_slag0["phi10", 4]
  phi20_original <- if("phi20" %in% rownames(coef_slag0)) coef_slag0["phi20", 1] else NA
  phi20_pvalue_original <- if("phi20" %in% rownames(coef_slag0)) coef_slag0["phi20", 4] else NA
  aic_slag0 <- fit_slag0$aic
  cat(sprintf("SLAG 0 only: phi10 = %.4f (p = %.4f), phi20 = %.4f (p = %.4f), AIC = %.2f\n", 
              phi10_original, phi10_pvalue_original, 
              ifelse(is.na(phi20_original), 0, phi20_original), 
              ifelse(is.na(phi20_pvalue_original), 1, phi20_pvalue_original), aic_slag0))
} else {
  phi10_original <- NA; phi10_pvalue_original <- NA
  phi20_original <- NA; phi20_pvalue_original <- NA
  aic_slag0 <- NA
  cat("SLAG 0 only: FAILED\n")
}

if (!is.null(fit_slag1)) {
  coef_slag1 <- summary(fit_slag1)$coefficients
  # SLAG 1 model generates phi10, phi20 (not phi11, phi21) because it only has one spatial weight
  phi11_separate <- coef_slag1["phi10", 1]  # Actually phi10 in SLAG 1 model
  phi11_pvalue_separate <- coef_slag1["phi10", 4]
  phi21_separate <- if("phi20" %in% rownames(coef_slag1)) coef_slag1["phi20", 1] else NA
  phi21_pvalue_separate <- if("phi20" %in% rownames(coef_slag1)) coef_slag1["phi20", 4] else NA
  aic_slag1 <- fit_slag1$aic
  cat(sprintf("SLAG 1 only: phi11 = %.4f (p = %.4f), phi21 = %.4f (p = %.4f), AIC = %.2f\n", 
              phi11_separate, phi11_pvalue_separate,
              ifelse(is.na(phi21_separate), 0, phi21_separate),
              ifelse(is.na(phi21_pvalue_separate), 1, phi21_pvalue_separate), aic_slag1))
} else {
  phi11_separate <- NA; phi11_pvalue_separate <- NA
  phi21_separate <- NA; phi21_pvalue_separate <- NA
  aic_slag1 <- NA
  cat("SLAG 1 only: FAILED\n")
}

# Joint model coefficients (current)
get_coef <- function(name) {
  idx <- which(coef_table$Parameter == name)
  if(length(idx) > 0) c(coef_table$Estimate[idx], coef_table$p_value[idx]) else c(NA, NA)
}

phi10_joint <- get_coef("phi10")
phi11_joint <- get_coef("phi11")
phi20_joint <- get_coef("phi20")
phi21_joint <- get_coef("phi21")

aic_joint <- aic

cat(sprintf("Joint model: phi10 = %.4f (p = %.4f), phi11 = %.4f (p = %.4f), phi20 = %.4f (p = %.4f), phi21 = %.4f (p = %.4f), AIC = %.2f\n", 
            phi10_joint[1], phi10_joint[2], phi11_joint[1], phi11_joint[2],
            phi20_joint[1], phi20_joint[2], phi21_joint[1], phi21_joint[2], aic_joint))

# Create comparison table
comparison_table <- data.frame(
  Model = c("SLAG 0 only", "SLAG 1 only", "Joint SLAG 0+1"),
  phi10 = c(phi10_original, NA, phi10_joint[1]),
  phi10_pvalue = c(phi10_pvalue_original, NA, phi10_joint[2]),
  phi11 = c(NA, phi11_separate, phi11_joint[1]),
  phi11_pvalue = c(NA, phi11_pvalue_separate, phi11_joint[2]),
  phi20 = c(phi20_original, NA, phi20_joint[1]),
  phi20_pvalue = c(phi20_pvalue_original, NA, phi20_joint[2]),
  phi21 = c(NA, phi21_separate, phi21_joint[1]),
  phi21_pvalue = c(NA, phi21_pvalue_separate, phi21_joint[2]),
  AIC = c(aic_slag0, aic_slag1, aic_joint),
  Parameters = c(ifelse(is.null(fit_slag0), 0, p_order), 
                 ifelse(is.null(fit_slag1), 0, p_order), 
                 nrow(coef_table)),
  Status = c(ifelse(is.null(fit_slag0), "FAILED", "SUCCESS"),
             ifelse(is.null(fit_slag1), "FAILED", "SUCCESS"),
             "SUCCESS")
)

cat("\n📋 Model Comparison Table:\n")
print(comparison_table)

# Determine best model (only among successful ones)
valid_aic <- comparison_table$AIC[!is.na(comparison_table$AIC)]
if (length(valid_aic) > 0) {
  best_aic_idx <- which.min(comparison_table$AIC)
  best_model <- comparison_table$Model[best_aic_idx]
  cat(sprintf("\n🏆 Best model by AIC: %s (AIC = %.2f)\n", best_model, min(comparison_table$AIC, na.rm = TRUE)))
} else {
  cat("\n⚠️ No valid AIC values for comparison\n")
  best_model <- "Joint SLAG 0+1"
}

# --------------------------- Residual Diagnostics ----------------------------
resid_mat <- residuals(fit)
if (is.null(dim(resid_mat))) {
  Tn <- nrow(data_input); Rn <- ncol(data_input)
  resid_mat <- matrix(resid_mat, nrow = Tn, ncol = Rn, byrow = FALSE)
}
colnames(resid_mat) <- colnames(data_input) %nz% paste0("Region_", seq_len(ncol(resid_mat)))

rvec <- as.vector(resid_mat)
calc_skew <- function(x){ x<-x[is.finite(x)]; m<-mean(x); s<-sd(x); if (s==0) 0 else sum((x-m)^3)/(length(x)*s^3) }
calc_kurt <- function(x){ x<-x[is.finite(x)]; m<-mean(x); s<-sd(x); if (s==0) 0 else sum((x-m)^4)/(length(x)*s^4)-3 }

residual_stats <- data.frame(
  Statistic = c("Mean","Std Dev","Min","Max","Skewness","Kurtosis"),
  Value = c(round(mean(rvec, na.rm=TRUE),6),
            round(sd(rvec,   na.rm=TRUE),6),
            round(min(rvec,  na.rm=TRUE),6),
            round(max(rvec,  na.rm=TRUE),6),
            round(calc_skew(rvec),6),
            round(calc_kurt(rvec),6))
)

# Save results
uniform_results <- list(
  # Joint model (main result)
  model          = fit,
  coefficients   = coef_table,
  fit_statistics = list(loglik = loglik, aic = aic, bic = bic,
                        parameters = nrow(coef_table), observations = nrow(data_input)),
  
  # Separate models for comparison
  model_slag0    = fit_slag0,
  model_slag1    = fit_slag1,
  
  # Comparison results
  comparison_table = comparison_table,
  best_model     = best_model,
  
  # Original phi10 for reference
  phi10_original = phi10_original,
  phi10_pvalue_original = phi10_pvalue_original,
  
  # Other results
  residuals      = resid_mat,
  residual_stats = residual_stats,
  estimation_time= estimation_time,
  spatial_weights= "uniform",
  orders         = list(p=p_order, d=d_order, q=q_order, max_spatial_lag=max_spatial_lag)
)

save(uniform_results, file = "output/11_starima_uniform.RData")
ok("Results saved → output/11_starima_uniform.RData")

cat("\n=== STARIMA ESTIMATION COMPLETED - UNIFORM WEIGHTS ===\n")
cat("✅ Parameters estimated:", nrow(coef_table), "\n")
cat("✅ Significant (<0.05):", sum(is.finite(coef_table$p_value) & coef_table$p_value < 0.05), "\n")
cat("✅ LogLik:", round(loglik, 4),
    " | AIC:", round(aic, 2),
    " | BIC:", round(bic, 2), "\n")
cat("📊 Next step: 12_Residual_Diagnostic.R\n")