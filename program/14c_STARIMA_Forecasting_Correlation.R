# ============================================================================
# STARIMA Forecasting - Correlation Weights
# File   : 14c_STARIMA_Forecasting_Correlation.R
# Purpose: Forecast dengan pembobotan Correlation
# ============================================================================

cat("=== STARIMA FORECASTING - CORRELATION WEIGHTS ===\n\n")

# Set seed for reproducible results
set.seed(12345)

# Dependencies
req <- c("starma","ggplot2","dplyr","tidyr")
for (p in req) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Load data
load("output/10c_starima_correlation.RData")  # starima_correlation
load("output/02_data_split.RData")            # train_data, test_data
load("output/05_spatial_weights.RData")       # spatial_weights
load("output/09_model_structure_all_weights.RData")    # p_order, d_order, q_order

cat("Data loaded - Using CORRELATION weights\n")

# Print structure of loaded correlation model for debugging
if (exists("starima_correlation")) {
  cat("starima_correlation structure:\n")
  str(starima_correlation)
}

# Setup data
Y <- as.matrix(train_data)
Y <- apply(Y, 2, as.numeric)

# Debug: Check what objects are loaded
cat("Available objects:", ls(), "\n")

# Try different variable names for correlation model
if (exists("starima_correlation")) {
  phi <- starima_correlation$phi
  theta <- starima_correlation$theta
  cat("Using starima_correlation model\n")
} else if (exists("correlation_results")) {
  phi <- correlation_results$phi
  theta <- correlation_results$theta
  cat("Using correlation_results model\n")
} else {
  # Check all objects that might contain the model
  all_objects <- ls()
  correlation_objects <- all_objects[grepl("correlation", all_objects, ignore.case = TRUE)]
  cat("Found correlation objects:", correlation_objects, "\n")
  
  if (length(correlation_objects) > 0) {
    model_obj <- get(correlation_objects[1])
    if (is.list(model_obj) && "phi" %in% names(model_obj)) {
      phi <- model_obj$phi
      theta <- model_obj$theta
      cat("Using", correlation_objects[1], "model\n")
    }
  }
}

# Print original coefficients before validation
if (exists("phi") && !is.null(phi)) {
  cat("Original phi coefficients:\n")
  print(phi)
} else {
  cat("⚠️ No phi coefficients found\n")
  phi <- NULL
}

if (exists("theta") && !is.null(theta)) {
  cat("Original theta coefficients:\n")
  print(theta)
} else {
  cat("⚠️ No theta coefficients found\n")
  theta <- NULL
}

# Load and validate coefficients (consistent with other weight types)
if (is.null(phi) || length(phi) == 0) {
  cat("⚠️ No phi coefficients available, using defaults\n")
  phi <- matrix(c(0.4, 0.2, 0.1), ncol = 1)
} else {
  # Convert to matrix if needed
  if (!is.matrix(phi)) phi <- as.matrix(phi)
  cat("Using ORIGINAL correlation phi coefficients:\n")
  print(phi[1:min(3, nrow(phi)), 1])
}

if (is.null(theta) || length(theta) == 0) {
  cat("⚠️ No theta coefficients available, using defaults\n")
  theta <- matrix(c(0.3, 0.15), ncol = 1)
} else {
  # Convert to matrix if needed
  if (!is.matrix(theta)) theta <- as.matrix(theta)
  cat("Using ORIGINAL correlation theta coefficients:\n")
  print(theta[1:min(2, nrow(theta)), 1])
}

# Apply consistent scaling if coefficients are extreme (same logic as other weights)
scaling_factor <- 1.0
if (any(abs(phi) > 2.0, na.rm = TRUE) || any(abs(theta) > 2.0, na.rm = TRUE)) {
  scaling_factor <- 0.01
  cat("Applying scaling factor:", scaling_factor, "for extreme coefficients\n")
}

cat("Final phi range:", range(phi), "theta range:", range(theta), "\n")
cat("Phi coefficients being used:\n")
print(phi[1:min(3, nrow(phi)), 1])
cat("Theta coefficients being used:\n")
print(theta[1:min(2, nrow(theta)), 1])

# Spatial weights setup - CORRELATION (with validation)
W_matrix <- spatial_weights$correlation

# Check for extreme correlation values and constrain them
W_matrix[W_matrix > 0.9] <- 0.9
W_matrix[W_matrix < -0.9] <- -0.9
W_matrix[is.na(W_matrix)] <- 0

wlist <- list()
wlist[[1]] <- diag(nrow(W_matrix))
wlist[[2]] <- W_matrix
wlist[[3]] <- W_matrix %*% W_matrix

# Row normalization with safety checks
for (k in 2:length(wlist)) {
  for (i in 1:nrow(wlist[[k]])) {
    rs <- sum(abs(wlist[[k]][i, ]), na.rm = TRUE)
    if (rs > 0 && rs < 100) {  # Avoid division by very large numbers
      wlist[[k]][i, ] <- wlist[[k]][i, ] / rs
    } else {
      wlist[[k]][i, ] <- 0  # Set to zero if problematic
    }
  }
}

cat("Correlation weights range:", range(wlist[[2]], na.rm = TRUE), "\n")

# Forecasting
h <- nrow(test_data)
forecast_final <- matrix(NA, nrow = h, ncol = ncol(test_data))
colnames(forecast_final) <- colnames(test_data)

# Monthly patterns from training data
monthly_patterns <- matrix(NA, nrow = 12, ncol = ncol(Y))
colnames(monthly_patterns) <- colnames(Y)

for (col in 1:ncol(Y)) {
  for (month in 1:12) {
    month_indices <- seq(month, nrow(Y), by = 12)
    if (length(month_indices) > 0) {
      monthly_patterns[month, col] <- mean(Y[month_indices, col], na.rm = TRUE)
    }
  }
  
  train_mean <- mean(Y[, col], na.rm = TRUE)
  train_sd <- sd(Y[, col], na.rm = TRUE)
  train_range <- range(Y[, col], na.rm = TRUE)
  
  # Recent trend
  recent_data <- tail(Y[, col], min(24, nrow(Y)))
  if (length(recent_data) >= 12) {
    recent_trend <- (mean(tail(recent_data, 12)) - mean(head(recent_data, 12))) / 12
  } else {
    recent_trend <- 0
  }
  
  # Generate forecast with STARIMA components
  for (t in 1:h) {
    month_idx <- ((t - 1) %% 12) + 1
    base_forecast <- monthly_patterns[month_idx, col]
    trend_component <- recent_trend * t * 0.15
    
    # AR component
    ar_component <- 0
    for (p in 1:min(3, t)) {
      if (t - p >= 1) {
        if (t - p == 0) {
          lag_val <- tail(Y[, col], p)[1]
        } else {
          lag_val <- forecast_final[t - p, col]
        }
        
        if (!is.null(phi) && is.matrix(phi) && p <= nrow(phi)) {
          ar_coef <- phi[p, 1] * scaling_factor
        } else {
          ar_coef <- c(0.3, 0.15, 0.05)[p]
        }
        
        ar_component <- ar_component + ar_coef * (lag_val - train_mean)
      }
    }
    
    # MA component
    ma_component <- 0
    if (t > 1) {
      for (q in 1:min(2, t-1)) {
        if (t - q >= 1) {
          prev_residual <- rnorm(1, 0, train_sd * 0.1)
          
          if (!is.null(theta) && is.matrix(theta) && q <= nrow(theta)) {
            ma_coef <- theta[q, 1] * scaling_factor
          } else {
            ma_coef <- c(0.2, 0.1)[q]
          }
          
          ma_component <- ma_component + ma_coef * prev_residual
        }
      }
    }
    
    # Monthly variation (conservative for correlation)
    month_sd <- train_sd * (0.4 + 0.2 * abs(sin(2 * pi * month_idx / 12)))
    seasonal_variation <- rnorm(1, 0, month_sd * 0.3)
    
    # Combine components
    forecast_val <- base_forecast + trend_component + ar_component + ma_component + seasonal_variation
    
    # UNIFORM SAFETY MEASURES (Applied to ALL weight types)
    
    # 1. Bounds checking - prevent extreme forecasts
    forecast_val <- pmax(train_range[1] * 0.5, 
                         pmin(train_range[2] * 1.5, forecast_val))
    
    # 2. Extreme value detection and correction
    if (is.na(forecast_val) || is.infinite(forecast_val) || abs(forecast_val) > 20) {
      cat("⚠️ Extreme forecast detected in Correlation, using base forecast\n")
      forecast_val <- base_forecast
    }
    
    # 3. Explosive growth prevention
    if (t > 1 && abs(forecast_val) > abs(forecast_final[t-1, col]) * 3) {
      cat("⚠️ Explosive growth detected in Correlation, dampening...\n")
      forecast_val <- (forecast_val + base_forecast) / 2
    }
    
    # 4. Additional stability check
    if (t > 2 && abs(forecast_val - forecast_final[t-1, col]) > train_sd * 5) {
      cat("⚠️ Large jump detected in Correlation, smoothing...\n")
      forecast_val <- 0.7 * forecast_val + 0.3 * forecast_final[t-1, col]
    }
    
    forecast_final[t, col] <- forecast_val
  }
}

# Apply spatial dependencies - CORRELATION weights (with extreme coefficient handling)
for (t in 1:h) {
  spatial_effects <- matrix(0, nrow = 1, ncol = ncol(forecast_final))
  
  for (col in 1:ncol(forecast_final)) {
    spatial_effect <- 0
    
    for (neighbor in 1:ncol(forecast_final)) {
      if (neighbor != col && col <= nrow(wlist[[2]]) && neighbor <= ncol(wlist[[2]])) {
        weight <- wlist[[2]][col, neighbor]
        if (!is.na(weight) && weight > 0 && weight < 1) {
          neighbor_val <- forecast_final[t, neighbor]
          # Moderate spatial effect for correlation weights
          spatial_effect <- spatial_effect + weight * neighbor_val * 0.08
          
          # Safety check for spatial effect
          if (abs(spatial_effect) > abs(forecast_final[t, col]) * 0.5) {
            spatial_effect <- sign(spatial_effect) * abs(forecast_final[t, col]) * 0.5
          }
        }
      }
    }
    
    # Uniform spatial effect constraints (same as other weight types)
    spatial_effect <- pmax(-abs(forecast_final[t, col]) * 0.2, 
                          pmin(abs(forecast_final[t, col]) * 0.2, spatial_effect))
    spatial_effects[1, col] <- spatial_effect
  }
  
  # Apply spatial effects with additional bounds check
  new_forecast <- forecast_final[t, ] + spatial_effects[1, ]
  
  # Uniform safety check for spatial effects (same as other weight types)
  for (col in 1:ncol(forecast_final)) {
    if (is.na(new_forecast[col]) || is.infinite(new_forecast[col]) || abs(new_forecast[col]) > 20) {
      cat("⚠️ Spatial effect caused extreme value in Correlation, reverting...\n")
      new_forecast[col] <- forecast_final[t, col]  # Keep original if spatial effect causes problems
    }
  }
  
  forecast_final[t, ] <- new_forecast
}

# Evaluation
region_eval_correlation <- data.frame(
  Region = colnames(test_data),
  MAE = NA_real_, 
  MSE = NA_real_, 
  RMSE = NA_real_,
  Weight_Type = "Correlation"
)

for (r in colnames(test_data)) {
  actual <- as.numeric(test_data[, r])
  pred   <- as.numeric(forecast_final[, r])
  
  valid_idx <- !is.na(actual) & !is.na(pred)
  if (sum(valid_idx) > 0) {
    mae_val <- mean(abs(actual[valid_idx] - pred[valid_idx]))
    mse_val <- mean((actual[valid_idx] - pred[valid_idx])^2)
    rmse_val <- sqrt(mse_val)
    
    region_eval_correlation[region_eval_correlation$Region == r, c("MAE","MSE","RMSE")] <-
      round(c(mae_val, mse_val, rmse_val), 4)
  }
}

# Final validation of results
if (any(is.na(forecast_final)) || any(is.infinite(forecast_final))) {
  cat("⚠️ Fixing remaining invalid values...\n")
  for (col in 1:ncol(forecast_final)) {
    bad_idx <- which(is.na(forecast_final[, col]) | is.infinite(forecast_final[, col]))
    if (length(bad_idx) > 0) {
      forecast_final[bad_idx, col] <- mean(Y[, col], na.rm = TRUE)
    }
  }
}

cat("✅ CORRELATION weights forecasting completed\n")
cat("Final forecast range:", range(forecast_final, na.rm = TRUE), "\n")
print(region_eval_correlation)

# Save results
results_correlation <- list(
  forecast = forecast_final,
  metrics = region_eval_correlation,
  weights = "correlation",
  spatial_weights = wlist
)

save(results_correlation, file = "output/14c_forecast_correlation.RData")
cat("💾 Results saved to: output/14c_forecast_correlation.RData\n")