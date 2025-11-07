# ============================================================================
# STARMA Forecasting Pipeline - Phase 3
# STARIMA Estimation (distance Weights, SLAG 1 Only)
# ============================================================================
suppressPackageStartupMessages({
  library(starma)
  library(spdep)
  library(ggplot2)
  library(gridExtra)
})

# ------------------------------- Guardrails ---------------------------------
'%nz%' <- function(x, y) if (is.null(x)) y else x
nznum  <- function(x, val=0) ifelse(is.finite(x), x, val)

halt <- function(...) { message(paste0("❌ ", paste(..., collapse=" "))); stop(invisible(NULL)) }
note <- function(...)  message(paste0("ℹ ", paste(..., collapse=" ")))
ok   <- function(...)  message(paste0("✅ ", paste(..., collapse=" ")))

# ------------------------------ Data Loading --------------------------------
req_files <- c(
  "output/09_stpacf_distance_only.RData",
  "output/07_spatial_weights_idw.RData",
  "output/05_differencing_results.RData"
)
missing <- req_files[!file.exists(req_files)]
if (length(missing)) halt("Missing required files: ", paste(missing, collapse=", "))

load("output/09_stpacf_distance_only.RData")
load("output/07_spatial_weights_idw.RData")
load("output/05_differencing_results.RData")

if (!exists("differenced_matrix")) halt("'differenced_matrix' not found")
if (!exists("spatial_weights") || is.null(spatial_weights$distance)) halt("'spatial_weights$distance' not found")

data_input <- differenced_matrix
d_order <- 1
p_order <- 3
q_order <- 3

cat("=== STARIMA ESTIMATION - distance WEIGHTS (SLAG 1 ONLY) ===\n\n")
cat(sprintf("- Model: STARIMA(%d,%d,%d)\n", p_order, d_order, q_order))
cat("- Spatial weights: distance-based (SLAG 1 only)\n")
cat(sprintf("- Training data: %d obs × %d regions\n\n", nrow(data_input), ncol(data_input)))

# --------------------------- Spatial Weights (SLAG 1) -----------------------
U <- spatial_weights$distance
if (!is.matrix(U) || nrow(U) != ncol(U)) halt("distance weight matrix must be square")

# Row-normalize U (to get valid weight matrix)
rs <- rowSums(U)
rs[rs == 0] <- 1
U_norm <- U / rs

# Build wlist with only SLAG 1
wlist_distance <- list(U_norm)
ok("Spatial weights list constructed with only SLAG 1")

# --------------------------- Build Masks (AR/MA) ----------------------------
ar_mask <- if (p_order > 0) matrix(TRUE, p_order, 1) else matrix(FALSE, 0, 1)
ma_mask <- if (q_order > 0) matrix(TRUE, q_order, 1) else matrix(FALSE, 0, 1)
ok("Dynamic masks configured for slag=1 only")

cat("🎯 Dynamic Mask Configuration:\n")
cat(sprintf("- AR mask: %dx%d (slag=1)\n", nrow(ar_mask), ncol(ar_mask)))
cat(sprintf("- MA mask: %dx%d (slag=1)\n", nrow(ma_mask), ncol(ma_mask)))
cat(sprintf("- Total parameters: %d\n\n", sum(ar_mask) + sum(ma_mask)))

# ----------------------------- Data Hygiene ---------------------------------
na_rows <- which(!stats::complete.cases(data_input))
if (length(na_rows)) {
  note(length(na_rows), " rows with NA detected — removing for estimation")
  data_input <- data_input[-na_rows, , drop = FALSE]
}
if (nrow(data_input) <= max(p_order, q_order) + 2)
  halt("Not enough observations after NA handling: ", nrow(data_input))

# --------------------------- Model Estimation -------------------------------
cat("\n🔧 Estimating STARIMA Model (SLAG 1)...\n")
estimation_start_time <- Sys.time()

fit <- try(
  starma(data = data_input, wlist = wlist_distance, ar = ar_mask, ma = ma_mask),
  silent = TRUE
)

if (inherits(fit, "try-error")) {
  cat(as.character(fit), "\n")
  halt("Model estimation failed. Common fixes: check NA rows or mask dimensions.")
}

estimation_time <- Sys.time() - estimation_start_time
ok(sprintf("Model estimation completed in %.2f sec", as.numeric(estimation_time)))

# ------------------------------- Summary ------------------------------------
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
  Significant = ifelse(is.finite(coef_df$p.value) & coef_df$p.value < 0.05, "*",
                       ifelse(is.finite(coef_df$p.value) & coef_df$p.value < 0.10, "*","")),
  row.names = NULL, check.names = FALSE
)

# --------------------------- Fit Statistics ---------------------------------
loglik <- fit$loglik %nz% NA_real_
aic <- fit$aic %nz% (-2*loglik + 2*nrow(coef_df))
bic <- fit$bic %nz% (-2*loglik + log(nrow(data_input))*nrow(coef_df))

cat("\n📈 Model Fit Statistics:\n")
cat("- Log-likelihood:", round(loglik, 4), "\n")
cat("- AIC:", round(aic, 4), "\n")
cat("- BIC:", round(bic, 4), "\n")
cat("- Parameters:", nrow(coef_table), "\n")
cat("- Observations:", nrow(data_input), "\n\n")

# --------------------------- Residual Diagnostics ---------------------------
resid_mat <- residuals(fit)
if (is.null(dim(resid_mat))) {
  resid_mat <- matrix(resid_mat, nrow = nrow(data_input), ncol = ncol(data_input))
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

# ----------------------------- Save Results ---------------------------------
distance_results_slag1 <- list(
  model          = fit,
  coefficients   = coef_table,
  fit_statistics = list(loglik = loglik, aic = aic, bic = bic,
                        parameters = nrow(coef_table), observations = nrow(data_input)),
  residuals      = resid_mat,
  residual_stats = residual_stats,
  estimation_time= estimation_time,
  spatial_weights= "distance",
  orders         = list(p=p_order, d=d_order, q=q_order, max_spatial_lag=1)
)

save(distance_results_slag1, file = "output/11_starima_distance_slag1.RData")
ok("Results saved → output/11_starima_distance_slag1.RData")

cat("\n=== STARIMA ESTIMATION COMPLETED - distance WEIGHTS (SLAG 1 ONLY) ===\n")
cat("✅ Parameters estimated:", nrow(coef_table), "\n")
cat("✅ Significant (<0.05):", sum(is.finite(coef_table$p_value) & coef_table$p_value < 0.05), "\n")
cat("✅ LogLik:", round(loglik, 4),
    " | AIC:", round(aic, 2),
    " | BIC:", round(bic, 2), "\n")
cat("📊 Next step: 12_Residual_Diagnostic.R\n")