# ============================================================================
# STARMA Forecasting Pipeline - Phase 4: Residual Analysis (FIXED)
# File: 13_Residual_Analysis_FIXED.R
# Purpose: Comprehensive residual analysis for selected model
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

cat("=== COMPREHENSIVE RESIDUAL ANALYSIS (FIXED) ===\n")
cat("Analyzing residuals from selected STARMA model...\n\n")

# ----------------------------------------------------------------------------
# 1) Load Required Data with Error Handling
# ----------------------------------------------------------------------------
required_files <- c(
  "output/14_model_selection.RData",
  "output/10_model_structure.RData"
)

# Check file existence
for (file in required_files) {
  if (!file.exists(file)) {
    cat("❌ Required file not found:", file, "\n")
    stop("Cannot proceed without required files")
  }
}

# Load model selection results
load("output/14_model_selection.RData")
load("output/10_model_structure.RData")

cat("✅ Model selection results loaded\n")
cat("📊 Selected model:", model_selection_results$selected_model, "\n")
cat("📊 Model structure: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n\n")

# ----------------------------------------------------------------------------
# 2) Load Selected Model Data with Robust Error Handling
# ----------------------------------------------------------------------------
cat("🔍 Loading selected model data...\n")

# Determine which model file to load based on selection
selected_model_name <- model_selection_results$selected_model
model_file <- NULL
model_loaded <- FALSE
residuals_data <- NULL
model_object <- NULL

if (grepl("Uniform", selected_model_name, ignore.case = TRUE)) {
  model_file <- "output/11a_starima_uniform.RData"
  weight_type <- "uniform"
} else if (grepl("Distance", selected_model_name, ignore.case = TRUE)) {
  model_file <- "output/11b_starima_distance.RData"
  weight_type <- "distance"
} else if (grepl("Correlation", selected_model_name, ignore.case = TRUE)) {
  model_file <- "output/11c_starima_correlation.RData"
  weight_type <- "correlation"
} else {
  # Default to uniform if selection unclear
  model_file <- "output/11a_starima_uniform.RData"
  weight_type <- "uniform"
  cat("⚠️ Model selection unclear, defaulting to uniform weights\n")
}

cat("📁 Loading model file:", model_file, "\n")

# Load model with error handling
if (file.exists(model_file)) {
  tryCatch({
    load(model_file)
    model_loaded <- TRUE
    cat("✅ Model file loaded successfully\n")
  }, error = function(e) {
    cat("❌ Error loading model file:", e$message, "\n")
    model_loaded <- FALSE
  })
} else {
  cat("❌ Model file not found:", model_file, "\n")
  model_loaded <- FALSE
}

# ----------------------------------------------------------------------------
# 3) Extract Residuals with Multiple Fallback Options
# ----------------------------------------------------------------------------
cat("\n🔍 Extracting residuals from loaded data...\n")

if (model_loaded) {
  # Try multiple ways to extract residuals
  residuals_extracted <- FALSE
  
  # Method 1: Direct model object (starima_uniform, starima_distance, etc.)
  model_var_name <- paste0("starima_", weight_type)
  if (exists(model_var_name)) {
    model_object <- get(model_var_name)
    if (!is.null(model_object) && !is.null(model_object$residuals)) {
      residuals_data <- as.vector(model_object$residuals)
      residuals_extracted <- TRUE
      cat("✅ Method 1: Residuals extracted from", model_var_name, "\n")
      cat("   - Residuals length:", length(residuals_data), "\n")
    }
  }
  
  # Method 2: Results object (uniform_results, distance_results, etc.)
  if (!residuals_extracted) {
    results_var_name <- paste0(weight_type, "_results")
    if (exists(results_var_name)) {
      results_object <- get(results_var_name)
      if (!is.null(results_object)) {
        # Try results$model$residuals
        if (!is.null(results_object$model) && !is.null(results_object$model$residuals)) {
          residuals_data <- as.vector(results_object$model$residuals)
          model_object <- results_object$model
          residuals_extracted <- TRUE
          cat("✅ Method 2a: Residuals extracted from", results_var_name, "$model\n")
        }
        # Try results$residuals directly
        else if (!is.null(results_object$residuals)) {
          residuals_data <- as.vector(results_object$residuals)
          residuals_extracted <- TRUE
          cat("✅ Method 2b: Residuals extracted from", results_var_name, "$residuals\n")
        }
      }
    }
  }
  
  # Method 3: Search all loaded objects for residuals
  if (!residuals_extracted) {
    cat("🔍 Searching all loaded objects for residuals...\n")
    all_objects <- ls()
    
    for (obj_name in all_objects) {
      obj <- get(obj_name)
      if (is.list(obj)) {
        # Check if object has residuals component
        if (!is.null(obj$residuals) && is.numeric(obj$residuals)) {
          residuals_data <- as.vector(obj$residuals)
          model_object <- obj
          residuals_extracted <- TRUE
          cat("✅ Method 3: Residuals found in object:", obj_name, "\n")
          break
        }
        # Check if object has model$residuals
        if (!is.null(obj$model) && !is.null(obj$model$residuals)) {
          residuals_data <- as.vector(obj$model$residuals)
          model_object <- obj$model
          residuals_extracted <- TRUE
          cat("✅ Method 3: Residuals found in", obj_name, "$model\n")
          break
        }
      }
    }
  }
  
  # Final check
  if (!residuals_extracted || is.null(residuals_data) || length(residuals_data) == 0) {
    cat("❌ No residuals found in any loaded objects\n")
    cat("🔍 Available objects:\n")
    print(ls())
    stop("Cannot proceed without residuals data")
  }
  
} else {
  stop("❌ Cannot proceed: Model file could not be loaded")
}

cat("✅ Residuals successfully extracted:", length(residuals_data), "observations\n")

# ----------------------------------------------------------------------------
# 4) Load Spatial Weights
# ----------------------------------------------------------------------------
if (file.exists("output/07_spatial_weights.RData")) {
  load("output/07_spatial_weights.RData")
  cat("✅ Spatial weights loaded\n")
} else {
  cat("⚠️ Spatial weights file not found, creating default weights\n")
  # Create minimal spatial weights if file missing
  spatial_weights <- list(
    uniform = matrix(0.25, nrow = 5, ncol = 5),
    distance = matrix(0.2, nrow = 5, ncol = 5),
    correlation = matrix(0.2, nrow = 5, ncol = 5)
  )
  diag(spatial_weights$uniform) <- 0
  diag(spatial_weights$distance) <- 0
  diag(spatial_weights$correlation) <- 0
}

# ----------------------------------------------------------------------------
# 5) Comprehensive Residual Analysis
# ----------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

cat("\n📊 Performing comprehensive residual analysis...\n")

# Basic statistics
residuals_stats <- list(
  mean = mean(residuals_data, na.rm = TRUE),
  sd = sd(residuals_data, na.rm = TRUE),
  min = min(residuals_data, na.rm = TRUE),
  max = max(residuals_data, na.rm = TRUE),
  skewness = sum((residuals_data - mean(residuals_data))^3) / (length(residuals_data) * sd(residuals_data)^3),
  kurtosis = sum((residuals_data - mean(residuals_data))^4) / (length(residuals_data) * sd(residuals_data)^4) - 3
)

cat("📈 Residual Statistics:\n")
cat("   - Mean:", round(residuals_stats$mean, 6), "\n")
cat("   - SD:", round(residuals_stats$sd, 4), "\n")
cat("   - Range: [", round(residuals_stats$min, 4), ",", round(residuals_stats$max, 4), "]\n")
cat("   - Skewness:", round(residuals_stats$skewness, 4), "\n")
cat("   - Kurtosis:", round(residuals_stats$kurtosis, 4), "\n")

# Normality tests
cat("\n🔬 Normality Tests:\n")

# Shapiro-Wilk test (if sample size allows)
if (length(residuals_data) <= 5000) {
  shapiro_result <- shapiro.test(residuals_data)
  cat("   - Shapiro-Wilk p-value:", round(shapiro_result$p.value, 6), "\n")
  normality_shapiro <- shapiro_result$p.value > 0.05
} else {
  cat("   - Sample too large for Shapiro-Wilk test\n")
  normality_shapiro <- TRUE
}

# Jarque-Bera test
n <- length(residuals_data)
jb_statistic <- n * (residuals_stats$skewness^2/6 + residuals_stats$kurtosis^2/24)
jb_p_value <- 1 - pchisq(jb_statistic, df = 2)
cat("   - Jarque-Bera p-value:", round(jb_p_value, 6), "\n")
normality_jb <- jb_p_value > 0.05

# Autocorrelation analysis
cat("\n📊 Autocorrelation Analysis:\n")
residual_acf <- acf(residuals_data, plot = FALSE, lag.max = 20)
residual_pacf <- pacf(residuals_data, plot = FALSE, lag.max = 20)

# Count significant lags
conf_bound <- 1.96 / sqrt(length(residuals_data))
acf_significant <- sum(abs(residual_acf$acf[-1]) > conf_bound)
pacf_significant <- sum(abs(residual_pacf$acf) > conf_bound)

cat("   - ACF significant lags:", acf_significant, "out of 20\n")
cat("   - PACF significant lags:", pacf_significant, "out of 20\n")

# ----------------------------------------------------------------------------
# 6) Create Comprehensive Visualizations
# ----------------------------------------------------------------------------
cat("\n📊 Creating comprehensive diagnostic plots...\n")

if (!dir.exists("plots")) dir.create("plots")

# 1. Residual time series plot
ts_data <- data.frame(
  Index = 1:length(residuals_data),
  Residuals = residuals_data
)

p1 <- ggplot(ts_data, aes(x = Index, y = Residuals)) +
  geom_line(color = "darkblue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = c(-2*residuals_stats$sd, 2*residuals_stats$sd), 
             color = "orange", linetype = "dotted") +
  labs(title = paste("Residual Time Series -", selected_model_name),
       x = "Observation", y = "Residuals") +
  theme_minimal()

# 2. Residual histogram
p2 <- ggplot(ts_data, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_density(aes(y = ..density.. * length(residuals_data) * diff(range(residuals_data))/30), 
               color = "red", size = 1) +
  labs(title = "Residual Distribution",
       x = "Residuals", y = "Frequency") +
  theme_minimal()

# 3. Q-Q plot
qq_data <- data.frame(
  Theoretical = qnorm(ppoints(length(residuals_data))),
  Sample = sort(residuals_data)
)

p3 <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot (Normal Distribution)",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# 4. ACF plot
acf_data <- data.frame(
  Lag = 1:length(residual_acf$acf[-1]),
  ACF = residual_acf$acf[-1]
)

p4 <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(conf_bound, -conf_bound), 
             color = "blue", linetype = "dashed") +
  geom_segment(aes(xend = Lag, yend = 0), color = "darkblue") +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "Residual Autocorrelation Function",
       x = "Lag", y = "ACF") +
  theme_minimal()

# Save plots
ggsave("plots/14_residual_timeseries.png", p1, width = 10, height = 6, dpi = 300)
ggsave("plots/14_residual_histogram.png", p2, width = 8, height = 6, dpi = 300)
ggsave("plots/14_residual_qqplot.png", p3, width = 8, height = 6, dpi = 300)
ggsave("plots/14_residual_acf.png", p4, width = 10, height = 6, dpi = 300)

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

cat("✅ Diagnostic plots saved to plots/ directory\n")

# ----------------------------------------------------------------------------
# 7) Summary and Conclusions
# ----------------------------------------------------------------------------
cat("\n📋 RESIDUAL ANALYSIS SUMMARY\n")
cat("============================\n")

analysis_results <- list(
  selected_model = selected_model_name,
  model_structure = paste0("STARIMA(", p_order, ",", d_order, ",", q_order, ")"),
  residuals_count = length(residuals_data),
  statistics = residuals_stats,
  normality_tests = list(
    shapiro_wilk = if(length(residuals_data) <= 5000) normality_shapiro else NA,
    jarque_bera = normality_jb
  ),
  autocorrelation = list(
    acf_significant_lags = acf_significant,
    pacf_significant_lags = pacf_significant
  ),
  overall_assessment = "Model residuals analyzed"
)

# Print summary
cat("Selected Model:", selected_model_name, "\n")
cat("Model Structure: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n")
cat("Residuals Analyzed:", length(residuals_data), "observations\n")
cat("Normality (Jarque-Bera):", ifelse(normality_jb, "✅ PASS", "❌ FAIL"), "\n")
cat("Autocorrelation:", ifelse(acf_significant <= 2, "✅ ACCEPTABLE", "⚠️ CONCERNING"), "\n")

# ----------------------------------------------------------------------------
# 8) Save Results
# ----------------------------------------------------------------------------
save(analysis_results, residuals_data, residuals_stats,
     file = "output/14_residual_analysis.RData")

cat("\n💾 Results saved to: output/14_residual_analysis.RData\n")
cat("📊 Plots saved in plots/ directory\n")
cat("✅ Comprehensive residual analysis completed!\n")