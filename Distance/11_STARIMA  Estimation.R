# ============================================================================
# STARMA Forecasting Pipeline - Phase 3: STARIMA Estimation
# File   : 11_STARIMA_Estimation_Distance.R
# Purpose: Estimate STARIMA(2,1,2) model using distance spatial weights
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
  "output/09_stpacf_distance_only.RData",
  "output/07_spatial_weights_distance.RData",
  "output/05_differencing_results.RData"
)
missing <- req_files[!file.exists(req_files)]
if (length(missing)) halt("Missing required files: ", paste(missing, collapse=", "))

load("output/09_stpacf_distance_only.RData")
load("output/07_spatial_weights_distance.RData")
load("output/05_differencing_results.RData")

if (!exists("differenced_matrix")) halt("'differenced_matrix' not found in 05_differencing_results.RData")
if (!exists("spatial_weights") || is.null(spatial_weights$distance)) halt("'spatial_weights$distance' not found")

data_input <- differenced_matrix   # gunakan hasil differencing
d_order <- 1

# ------------------------------ Model Orders (EXPERIMENT HERE!) --------------------------------
# 🧪 CHANGE THESE VALUES TO EXPERIMENT WITH DIFFERENT ORDERS:
p_order <- 3          # AR order (try: 1, 2, 3, 4)
q_order <- 2          # MA order (try: 1, 2, 3)
max_spatial_lag <- 1  # Spatial lags (usually keep at 1)

# 📊 Popular combinations to try:
# STARIMA(1,1,1) - Simple model
# STARIMA(2,1,2) - Balanced model  
# STARIMA(3,1,1) - AR-heavy model
# STARIMA(1,1,3) - MA-heavy model
# STARIMA(3,1,2) - Complex model

cat("=== STARIMA ESTIMATION - DISTANCE WEIGHTS ===\n\n")
cat("📋 Estimation Setup:\n")
cat("===================\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d)\n", p_order, d_order, q_order))
cat("- Spatial weights: Distance-based\n")
cat(sprintf("- Training data: %d observations\n", nrow(data_input)))
cat(sprintf("- Regions: %d (%s)\n", ncol(data_input),
            paste(head(colnames(data_input) %nz% paste0("Region_", seq_len(ncol(data_input)))), collapse=", ")))
cat(sprintf("- Max spatial lag: %d (lags 0..%d)\n\n", max_spatial_lag, max_spatial_lag))

# --------------------------- Build/Validate wlist ----------------------------
D <- spatial_weights$distance
if (!is.matrix(D) || nrow(D) != ncol(D)) halt("Distance weight matrix must be square")

# lag-0 = I, lag-1 = D, lag-k = row-normalized D^k
wlist_distance <- vector("list", max_spatial_lag + 1L)
wlist_distance[[1]] <- diag(nrow(D))
wlist_distance[[2]] <- D

for (i in seq_along(wlist_distance)) {
  rs <- rowSums(wlist_distance[[i]])
  rs[rs == 0] <- 1
  wlist_distance[[i]] <- wlist_distance[[i]] / rs
}

ok("Spatial weights list constructed with ", length(wlist_distance), " lags")

# ------------------------------ Dynamic Masks (AR/MA) --------------------------------
# Create masks dynamically based on p_order and q_order
ar_mask <- matrix(FALSE, p_order, max_spatial_lag + 1)
ma_mask <- matrix(FALSE, q_order, max_spatial_lag + 1)

# Activate all temporal lags for spatial lag 0 (within-region effects)
for (p in 1:p_order) {
  ar_mask[p, 1] <- TRUE  # tlag_p-slag0
}
for (q in 1:q_order) {
  ma_mask[q, 1] <- TRUE  # tlag_q-slag0
}

# Optional: Activate some spatial lags (uncomment if needed)
# ar_mask[1, 2] <- TRUE  # tlag1-slag1 (first AR lag with spatial lag 1)
# ma_mask[1, 2] <- TRUE  # tlag1-slag1 (first MA lag with spatial lag 1)

cat("🎯 Dynamic Mask Configuration:\n")
cat(sprintf("- AR mask: %dx%d (p=%d, spatial_lags=%d)\n", 
           nrow(ar_mask), ncol(ar_mask), p_order, max_spatial_lag))
cat(sprintf("- MA mask: %dx%d (q=%d, spatial_lags=%d)\n", 
           nrow(ma_mask), ncol(ma_mask), q_order, max_spatial_lag))
cat(sprintf("- Total AR parameters: %d\n", sum(ar_mask)))
cat(sprintf("- Total MA parameters: %d\n", sum(ma_mask)))
cat(sprintf("- Total parameters: %d\n\n", sum(ar_mask) + sum(ma_mask)))

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
cat("\n🔧 Estimating STARIMA Model...\n")
estimation_start_time <- Sys.time()

fit <- try(
  starma(
    data = data_input,
    wlist = wlist_distance,
    ar    = ar_mask,
    ma    = ma_mask
  ),
  silent = TRUE
)

if (inherits(fit, "try-error")) {
  cat(as.character(fit), "\n")
  halt("Model estimation failed. Common fixes: check NA rows or mask dimensions.")
}

estimation_time <- Sys.time() - estimation_start_time
ok(sprintf("Model estimation completed in %.2f sec", as.numeric(estimation_time)))

# ------------------------------- Summary -------------------------------------
cat("\n📊 Model Summary:\n=================\n")
print(summary(fit))

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
distance_results <- list(
  model          = fit,
  coefficients   = coef_table,
  fit_statistics = list(loglik = loglik, aic = aic, bic = bic,
                        parameters = nrow(coef_table), observations = nrow(data_input)),
  residuals      = resid_mat,
  residual_stats = residual_stats,
  estimation_time= estimation_time,
  spatial_weights= "distance",
  orders         = list(p=p_order, d=d_order, q=q_order, max_spatial_lag=max_spatial_lag)
)

save(distance_results, file = "output/11_starima_distance.RData")
ok("Results saved → output/11_starima_distance.RData")

cat("\n=== STARIMA ESTIMATION COMPLETED - DISTANCE WEIGHTS ===\n")
cat("✅ Parameters estimated:", nrow(coef_table), "\n")
cat("✅ Significant (<0.05):", sum(is.finite(coef_table$p_value) & coef_table$p_value < 0.05), "\n")
cat("✅ LogLik:", round(loglik, 4),
    " | AIC:", round(aic, 2),
    " | BIC:", round(bic, 2), "\n")
cat("📊 Next step: 12_Residual_Diagnostic.R\n")