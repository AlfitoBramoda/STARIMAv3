# ============================================================================
# 07_IDW_From_Distance_KM.R
# Purpose : Build inverse-distance row-standardized weights
#           using distance_matrix_km from Step 06 output
# Input   : output/06_distance_matrix_km.RData
# Output  : output/07_spatial_weights_idw.RData
# ============================================================================

cat("🚀 Building IDW weights from distance_matrix_km...\n")

dir.create("artifacts", showWarnings = FALSE, recursive = TRUE)

# ------------------------------- Load ---------------------------------------
infile <- "output/06_distance_matrix_km.RData"
if (!file.exists(infile)) stop("❌ Missing input: ", infile)

load(infile)
if (!exists("distance_matrix_km")) stop("❌ Object 'distance_matrix_km' not found in file.")

D <- distance_matrix_km
if (!is.matrix(D)) stop("❌ distance_matrix_km must be a matrix.")

# ----------------------------- Governance -----------------------------------
if (nrow(D) != ncol(D)) stop("❌ Distance matrix must be square.")
if (any(D < 0, na.rm = TRUE)) stop("❌ Distances must be non-negative.")
if (!isTRUE(all.equal(D, t(D), tolerance = 1e-8))) {
  warning("⚠️ Matrix not symmetric; forcing symmetry (D + t(D))/2.")
  D <- (D + t(D)) / 2
}

# Ensure diagonal is 0 and add small epsilon to avoid division by zero
diag(D) <- 0
epsilon <- 1e-6  # Small value to avoid 1/0
D[D == 0 & row(D) != col(D)] <- epsilon  # Only for off-diagonal zeros

# --------------------------- IDW per formula --------------------------------
# Formula: w_ij = (1/d_ij) / sum_{k != i}(1/d_ik), i != j
n <- nrow(D)
regions <- rownames(D)
if (is.null(regions)) regions <- paste0("R", seq_len(n))

invD <- matrix(0, n, n)
nz_mask <- row(D) != col(D)
invD[nz_mask] <- 1 / D[nz_mask]

row_sums <- rowSums(invD)
row_sums[row_sums == 0] <- 1
W <- invD / row_sums

# ------------------------------- Validation ---------------------------------
diag_ok   <- all(diag(W) == 0)
row_ok    <- all(abs(rowSums(W) - 1) < 1e-10)
nonneg_ok <- all(W >= 0 | is.nan(W))

met <- W[W > 0]
cat("✅ Weights computed successfully.\n",
    "  Diagonal=0  : ", ifelse(diag_ok, "PASS", "FAIL"), "\n",
    "  Row sums=1  : ", ifelse(row_ok,  "PASS", "FAIL"), "\n",
    "  Non-negative: ", ifelse(nonneg_ok,"PASS", "FAIL"), "\n",
    sprintf("  Weight range: [%.6f, %.6f]\n",
            min(met, na.rm = TRUE), max(met, na.rm = TRUE)), sep = "")

# Display Spatial Weights Matrix in console
cat("\n--- 🎯 SPATIAL WEIGHTS MATRIX (IDW) ---\n")
rownames(W) <- regions
colnames(W) <- regions
print(round(W, 4))

# Verify row sums = 1
cat("\n--- ✅ ROW SUMS VERIFICATION ---\n")
row_sums_check <- rowSums(W)
for(i in 1:length(regions)) {
  cat(sprintf("%s: %.6f\n", regions[i], row_sums_check[i]))
}
cat(sprintf("All rows sum to 1: %s\n", ifelse(all(abs(row_sums_check - 1) < 1e-10), "✅ YES", "❌ NO")))

# ------------------------------ Analytics -----------------------------------
closest <- apply(D + diag(Inf, n), 1, which.min)
farthest <- apply(D, 1, which.max)

distance_analysis <- data.frame(
  Region              = regions,
  Closest_Neighbor    = regions[closest],
  Closest_Distance    = D[cbind(seq_len(n), closest)],
  Closest_Weight      = W[cbind(seq_len(n), closest)],
  Farthest_Neighbor   = regions[farthest],
  Farthest_Distance   = D[cbind(seq_len(n), farthest)],
  Farthest_Weight     = W[cbind(seq_len(n), farthest)],
  row.names = NULL
)
print(distance_analysis)

# ------------------------------- Persist ------------------------------------
spatial_weights <- list(distance_idw = W)
weights_summary <- data.frame(
  Weight_Type = "Inverse Distance (km input)",
  Min_Weight  = min(met, na.rm = TRUE),
  Max_Weight  = max(met, na.rm = TRUE),
  Mean_Weight = mean(met, na.rm = TRUE),
  Validation  = ifelse(diag_ok && row_ok && nonneg_ok, "✅ PASS", "❌ CHECK"),
  stringsAsFactors = FALSE
)

save(spatial_weights, W, D, distance_analysis, weights_summary, distance_matrix_km,
     file = "output/07_spatial_weights_distance.RData")
write.csv(W, file = "artifacts/07_idw_weights.csv", row.names = TRUE)

cat("💾 Saved -> output/07_spatial_weights_idw.RData\n")
cat("🧾 Exported -> artifacts/07_idw_weights.csv\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
