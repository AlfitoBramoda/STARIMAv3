# ============================================================================
# STARMA Forecasting Pipeline - Phase 4: Model Selection & Comparison (FIXED)
# File: 12_Model_Selection_FIXED.R
# Purpose: Compare STARIMA models and select best model for forecasting
# Author: STARMA Analysis
# Date: 2024
# ============================================================================

cat("=== STARIMA MODEL SELECTION & COMPARISON (FIXED) ===\n")
cat("Comparing STARIMA models for best model selection...\n\n")

# ----------------------------------------------------------------------------
# 1) Load Required Data with Error Handling
# ----------------------------------------------------------------------------
required_files <- c(
  "output/11a_starima_uniform.RData",
  "output/11b_starima_distance.RData", 
  "output/11c_starima_correlation.RData",
  "output/13a_diagnostic_uniform.RData",
  "output/13b_diagnostic_distance.RData",
  "output/13c_diagnostic_correlation.RData",
  "output/10_model_structure.RData"
)

# Check which files exist
existing_files <- sapply(required_files, file.exists)
missing_files <- required_files[!existing_files]

if (length(missing_files) > 0) {
  cat("⚠️ Missing files:\n")
  for (file in missing_files) {
    cat("   -", file, "\n")
  }
  cat("\n🔄 Proceeding with available files only...\n\n")
}

# Load available files
if (file.exists("output/10_model_structure.RData")) {
  load("output/10_model_structure.RData")
  cat("✅ Model structure loaded: STARIMA(", p_order, ",", d_order, ",", q_order, ")\n")
}

# Load model results with error handling
models_loaded <- list()

if (file.exists("output/11a_starima_uniform.RData")) {
  load("output/11a_starima_uniform.RData")
  models_loaded$uniform <- TRUE
  cat("✅ Uniform model loaded\n")
} else {
  models_loaded$uniform <- FALSE
  cat("❌ Uniform model missing\n")
}

if (file.exists("output/11b_starima_distance.RData")) {
  load("output/11b_starima_distance.RData")
  models_loaded$distance <- TRUE
  cat("✅ Distance model loaded\n")
} else {
  models_loaded$distance <- FALSE
  cat("❌ Distance model missing\n")
}

if (file.exists("output/11c_starima_correlation.RData")) {
  load("output/11c_starima_correlation.RData")
  models_loaded$correlation <- TRUE
  cat("✅ Correlation model loaded\n")
} else {
  models_loaded$correlation <- FALSE
  cat("❌ Correlation model missing\n")
}

# Load diagnostic results
if (file.exists("output/13a_diagnostic_uniform.RData")) {
  load("output/13a_diagnostic_uniform.RData")
  cat("✅ Uniform diagnostics loaded\n")
}

library(ggplot2)
library(gridExtra)

# ----------------------------------------------------------------------------
# 2) Extract Model Statistics with Error Handling
# ----------------------------------------------------------------------------
cat("\n📊 Extracting model statistics...\n")

# Function to safely extract statistics
extract_stats <- function(model_obj, results_obj, model_name) {
  stats <- list(
    model_name = model_name,
    loglik = NA,
    aic = NA,
    bic = NA,
    parameters = NA,
    observations = NA,
    estimation_success = FALSE
  )
  
  tryCatch({
    # Try to get from results object first
    if (!is.null(results_obj)) {
      if (!is.null(results_obj$loglik)) stats$loglik <- as.numeric(results_obj$loglik)
      if (!is.null(results_obj$aic)) stats$aic <- as.numeric(results_obj$aic)
      if (!is.null(results_obj$bic)) stats$bic <- as.numeric(results_obj$bic)
      if (!is.null(results_obj$n_params)) stats$parameters <- as.numeric(results_obj$n_params)
      if (!is.null(results_obj$n_obs)) stats$observations <- as.numeric(results_obj$n_obs)
      if (!is.null(results_obj$estimation_success)) stats$estimation_success <- results_obj$estimation_success
    }
    
    # Try to get from model object if results not available
    if (!is.null(model_obj)) {
      if (is.na(stats$parameters) && !is.null(model_obj$phi) && !is.null(model_obj$theta)) {
        stats$parameters <- length(model_obj$phi) + length(model_obj$theta)
      }
      
      if (!is.null(model_obj$residuals)) {
        n_obs <- length(as.vector(model_obj$residuals))
        if (is.na(stats$observations)) stats$observations <- n_obs
        
        # Calculate basic statistics if missing
        if (is.na(stats$loglik) && !is.null(model_obj$residuals)) {
          residuals <- as.vector(model_obj$residuals)
          sse <- sum(residuals^2, na.rm = TRUE)
          mse <- sse / n_obs
          stats$loglik <- -0.5 * n_obs * log(2 * pi * mse) - 0.5 * sse / mse
        }
        
        if (is.na(stats$aic) && !is.na(stats$loglik) && !is.na(stats$parameters)) {
          stats$aic <- -2 * stats$loglik + 2 * stats$parameters
        }
        
        if (is.na(stats$bic) && !is.na(stats$loglik) && !is.na(stats$parameters)) {
          stats$bic <- -2 * stats$loglik + log(n_obs) * stats$parameters
        }
      }
      
      stats$estimation_success <- TRUE
    }
    
  }, error = function(e) {
    cat("⚠️ Error extracting stats for", model_name, ":", e$message, "\n")
  })
  
  return(stats)
}

# Extract statistics for each model
uniform_stats <- if (models_loaded$uniform) {
  extract_stats(
    if(exists("starima_uniform")) starima_uniform else NULL,
    if(exists("uniform_results")) uniform_results else NULL,
    "Uniform"
  )
} else {
  list(model_name = "Uniform", loglik = NA, aic = NA, bic = NA, parameters = NA, observations = NA, estimation_success = FALSE)
}

distance_stats <- if (models_loaded$distance) {
  extract_stats(
    if(exists("starima_distance")) starima_distance else NULL,
    if(exists("distance_results")) distance_results else NULL,
    "Distance"
  )
} else {
  list(model_name = "Distance", loglik = NA, aic = NA, bic = NA, parameters = NA, observations = NA, estimation_success = FALSE)
}

correlation_stats <- if (models_loaded$correlation) {
  extract_stats(
    if(exists("starima_correlation")) starima_correlation else NULL,
    if(exists("correlation_results")) correlation_results else NULL,
    "Correlation"
  )
} else {
  list(model_name = "Correlation", loglik = NA, aic = NA, bic = NA, parameters = NA, observations = NA, estimation_success = FALSE)
}

# ----------------------------------------------------------------------------
# 3) Create Model Comparison Table
# ----------------------------------------------------------------------------
cat("\n📋 Creating model comparison table...\n")

model_comparison <- data.frame(
  Model = c("Uniform Weights", "Distance Weights", "Correlation Weights"),
  Spatial_Weight_Type = c("Equal (0.25)", "Inverse Distance", "Cross-Correlation"),
  Log_Likelihood = c(
    ifelse(is.na(uniform_stats$loglik), "N/A", round(uniform_stats$loglik, 4)),
    ifelse(is.na(distance_stats$loglik), "N/A", round(distance_stats$loglik, 4)),
    ifelse(is.na(correlation_stats$loglik), "N/A", round(correlation_stats$loglik, 4))
  ),
  AIC = c(
    ifelse(is.na(uniform_stats$aic), "N/A", round(uniform_stats$aic, 2)),
    ifelse(is.na(distance_stats$aic), "N/A", round(distance_stats$aic, 2)),
    ifelse(is.na(correlation_stats$aic), "N/A", round(correlation_stats$aic, 2))
  ),
  BIC = c(
    ifelse(is.na(uniform_stats$bic), "N/A", round(uniform_stats$bic, 2)),
    ifelse(is.na(distance_stats$bic), "N/A", round(distance_stats$bic, 2)),
    ifelse(is.na(correlation_stats$bic), "N/A", round(correlation_stats$bic, 2))
  ),
  Parameters = c(
    ifelse(is.na(uniform_stats$parameters), "N/A", uniform_stats$parameters),
    ifelse(is.na(distance_stats$parameters), "N/A", distance_stats$parameters),
    ifelse(is.na(correlation_stats$parameters), "N/A", correlation_stats$parameters)
  ),
  Estimation_Status = c(
    ifelse(uniform_stats$estimation_success, "✅ Success", "❌ Failed"),
    ifelse(distance_stats$estimation_success, "✅ Success", "❌ Failed"),
    ifelse(correlation_stats$estimation_success, "✅ Success", "❌ Failed")
  ),
  stringsAsFactors = FALSE
)

print(model_comparison)

# ----------------------------------------------------------------------------
# 4) Model Selection Decision
# ----------------------------------------------------------------------------
cat("\n🎯 Model Selection Decision:\n")
cat("============================\n")

# Count successful models
successful_models <- sum(c(uniform_stats$estimation_success, 
                           distance_stats$estimation_success, 
                           correlation_stats$estimation_success))

cat("📊 Estimation Summary:\n")
cat("- Successful models:", successful_models, "out of 3\n")

if (successful_models == 0) {
  cat("❌ No models estimated successfully!\n")
  cat("🔄 Recommendation: Review estimation process\n")
  selected_model <- "None"
} else if (successful_models == 1) {
  # Only one model succeeded
  if (uniform_stats$estimation_success) {
    selected_model <- "Uniform Weights"
  } else if (distance_stats$estimation_success) {
    selected_model <- "Distance Weights"
  } else {
    selected_model <- "Correlation Weights"
  }
  cat("✅ Selected model:", selected_model, "(only successful model)\n")
} else {
  # Multiple models succeeded - compare AIC
  valid_aics <- c()
  model_names <- c()
  
  if (uniform_stats$estimation_success && !is.na(uniform_stats$aic)) {
    valid_aics <- c(valid_aics, uniform_stats$aic)
    model_names <- c(model_names, "Uniform Weights")
  }
  if (distance_stats$estimation_success && !is.na(distance_stats$aic)) {
    valid_aics <- c(valid_aics, distance_stats$aic)
    model_names <- c(model_names, "Distance Weights")
  }
  if (correlation_stats$estimation_success && !is.na(correlation_stats$aic)) {
    valid_aics <- c(valid_aics, correlation_stats$aic)
    model_names <- c(model_names, "Correlation Weights")
  }
  
  if (length(valid_aics) > 0) {
    best_idx <- which.min(valid_aics)
    selected_model <- model_names[best_idx]
    cat("✅ Selected model:", selected_model, "(lowest AIC =", round(valid_aics[best_idx], 2), ")\n")
  } else {
    # Default to uniform if no AIC available
    selected_model <- "Uniform Weights"
    cat("✅ Selected model:", selected_model, "(default choice)\n")
  }
}

# ----------------------------------------------------------------------------
# 5) Create Selection Summary
# ----------------------------------------------------------------------------
selection_summary <- data.frame(
  Criterion = c("Estimation Success", "Information Criteria", "Practical Implementation", "Overall Recommendation"),
  Result = c(
    paste(successful_models, "out of 3 models successful"),
    ifelse(successful_models > 1, "AIC-based selection", "Single model available"),
    "Uniform weights simplest to implement",
    paste("✅", selected_model)
  ),
  stringsAsFactors = FALSE
)

print(selection_summary)

# ----------------------------------------------------------------------------
# 6) Save Results
# ----------------------------------------------------------------------------
model_selection_results <- list(
  comparison_table = model_comparison,
  selection_summary = selection_summary,
  selected_model = selected_model,
  uniform_stats = uniform_stats,
  distance_stats = distance_stats,
  correlation_stats = correlation_stats,
  successful_models = successful_models
)

save(model_selection_results, model_comparison, selection_summary,
     file = "output/14_model_selection.RData")

cat("\n💾 Results saved to: output/14_model_selection.RData\n")
cat("✅ Model selection completed!\n")
cat("🎯 Selected model:", selected_model, "\n")
cat("📁 Next step: Proceed to forecasting with selected model\n")