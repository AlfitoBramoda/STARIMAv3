# ============================================================================
# 18_MODEL_ORDER_SELECTION.R
# Automated STARIMA Model Order Selection
# ============================================================================

cat("=== AUTOMATED STARIMA MODEL ORDER SELECTION ===\n")
cat("Testing different p and q orders to find optimal configuration...\n\n")

# Load required libraries
library(starma)
suppressMessages(library(spdep))

# Load data
load("output/05_spatial_weights.RData")
load("output/04_centered_data.RData")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Define order ranges to test
p_orders <- c(1, 2, 3)  # AR orders
q_orders <- c(1, 2, 3)  # MA orders
max_spatial_lag <- 2
d_order <- 0
D_order <- 1

# Spatial weights to test
weight_types <- c("uniform", "distance", "correlation")

# Results storage
results <- data.frame()

cat("📋 Testing Configuration:\n")
cat("- AR orders:", paste(p_orders, collapse=", "), "\n")
cat("- MA orders:", paste(q_orders, collapse=", "), "\n")
cat("- Spatial weights:", paste(weight_types, collapse=", "), "\n")
cat("- Max spatial lag:", max_spatial_lag, "\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

create_masks <- function(p_order, q_order, max_spatial_lag) {
  # AR mask: p_order x 1 (only temporal AR, no spatial AR)
  ar_mask <- array(FALSE, dim = c(p_order, max_spatial_lag + 1))
  ar_mask[, 1] <- TRUE  # Only temporal AR parameters
  
  # MA mask: q_order x (max_spatial_lag + 1)
  ma_mask <- array(TRUE, dim = c(q_order, max_spatial_lag + 1))
  
  return(list(ar_mask = ar_mask, ma_mask = ma_mask))
}

create_wlist <- function(weight_type) {
  # Get spatial weights matrix
  if (weight_type == "uniform") {
    W <- spatial_weights$uniform
  } else if (weight_type == "distance") {
    W <- spatial_weights$distance
  } else if (weight_type == "correlation") {
    W <- spatial_weights$correlation
  }
  
  # Create wlist
  wlist <- list()
  wlist[[1]] <- diag(nrow(W))  # Spatial lag 0
  wlist[[2]] <- W              # Spatial lag 1
  wlist[[3]] <- W %*% W        # Spatial lag 2
  
  # Row normalize higher order lags
  for (i in 2:length(wlist)) {
    for (j in 1:nrow(wlist[[i]])) {
      row_sum <- sum(wlist[[i]][j, ])
      if (row_sum > 0) {
        wlist[[i]][j, ] <- wlist[[i]][j, ] / row_sum
      }
    }
  }
  
  return(wlist)
}

# ============================================================================
# MODEL TESTING LOOP
# ============================================================================

cat("🔧 Starting model testing...\n")
cat("Testing", length(p_orders) * length(q_orders), "model orders with", length(weight_types), "weight types\n\n")

total_models <- length(p_orders) * length(q_orders) * length(weight_types)
model_count <- 0
start_time <- Sys.time()

for (weight_type in weight_types) {
  cat("Testing", weight_type, "weights...\n")
  
  # Create spatial weights list
  wlist <- create_wlist(weight_type)
  
  for (p in p_orders) {
    for (q in q_orders) {
      model_count <- model_count + 1
      
      cat(sprintf("  Model %d/%d: STARIMA(%d,0,%d) with %s weights... ", 
                  model_count, total_models, p, q, weight_type))
      
      # Create masks
      masks <- create_masks(p, q, max_spatial_lag)
      total_params <- sum(masks$ar_mask) + sum(masks$ma_mask)
      
      # Skip if too many parameters (rule of thumb: n_obs/params > 4)
      if (total_params > 20) {  # Allow more parameters
        cat("SKIPPED (too many parameters)\n")
        next
      }
      
      # Skip if degrees of freedom too low
      if ((96 - total_params) < 5) {
        cat("SKIPPED (insufficient df)\n")
        next
      }
      
      # Estimate model
      tryCatch({
        model <- starma(
          data = centered_matrix,
          wlist = wlist,
          ar = masks$ar_mask,
          ma = masks$ma_mask
        )
        
        # Extract model statistics
        loglik <- if(is.null(model$loglik)) NA else model$loglik
        aic <- if(is.null(model$aic)) NA else model$aic
        bic <- if(is.null(model$bic)) NA else model$bic
        
        # Calculate AIC/BIC if missing
        if (is.na(aic) && !is.na(loglik)) {
          n_obs <- 96
          aic <- -2 * loglik + 2 * total_params
          bic <- -2 * loglik + log(n_obs) * total_params
        }
        
        # Count significant parameters and extract p-values
        summary_model <- summary(model)
        coef_matrix <- summary_model$coefficients
        if(is.null(dim(coef_matrix))) {
          n_significant <- 0
          min_pvalue <- NA
          max_pvalue <- NA
        } else {
          p_col <- which(colnames(coef_matrix) %in% c("p.value", "Pr(>|t|)"))
          if(length(p_col) > 0) {
            p_values <- coef_matrix[, p_col[1]]
            n_significant <- sum(p_values < 0.05, na.rm = TRUE)
            min_pvalue <- round(min(p_values, na.rm = TRUE), 4)
            max_pvalue <- round(max(p_values, na.rm = TRUE), 4)
          } else {
            n_significant <- 0
            min_pvalue <- NA
            max_pvalue <- NA
          }
        }
        
        # Store results
        results <- rbind(results, data.frame(
          Weight_Type = weight_type,
          p_order = p,
          q_order = q,
          Total_Params = total_params,
          LogLik = round(loglik, 2),
          AIC = round(aic, 2),
          BIC = round(bic, 2),
          Significant_Params = n_significant,
          Min_PValue = min_pvalue,
          Max_PValue = max_pvalue,
          Convergence = "SUCCESS",
          stringsAsFactors = FALSE
        ))
        
        cat("SUCCESS\n")
        
      }, error = function(e) {
        # Store failed estimation
        results <- rbind(results, data.frame(
          Weight_Type = weight_type,
          p_order = p,
          q_order = q,
          Total_Params = total_params,
          LogLik = NA,
          AIC = NA,
          BIC = NA,
          Significant_Params = NA,
          Min_PValue = NA,
          Max_PValue = NA,
          Convergence = "FAILED",
          stringsAsFactors = FALSE
        ))
        assign("results", results, envir = parent.frame())
        cat("FAILED\n")
      })
    }
  }
}

# ============================================================================
# RESULTS ANALYSIS
# ============================================================================

cat("\n📊 MODEL COMPARISON RESULTS\n")
cat("============================\n")

# Filter successful models only
successful_models <- results[results$Convergence == "SUCCESS" & !is.na(results$AIC), ]

if (nrow(successful_models) == 0) {
  cat("❌ No models converged successfully!\n")
  stop("All models failed to converge")
}

# Sort by AIC (best first)
successful_models <- successful_models[order(successful_models$AIC), ]

cat("Top 10 models by AIC:\n")
print(successful_models[, c("Weight_Type", "p_order", "q_order", "Total_Params", 
                                "AIC", "BIC", "Significant_Params", "Min_PValue", "Max_PValue")])

# Find best model for each weight type
cat("\n🏆 Best model for each weight type:\n")
for (wt in weight_types) {
  wt_models <- successful_models[successful_models$Weight_Type == wt, ]
  if (nrow(wt_models) > 0) {
    best <- wt_models[1, ]
    cat(sprintf("- %s: STARIMA(%d,0,%d) | AIC=%.1f | BIC=%.1f | Sig=%d/%d | p-val: %.3f-%.3f\n",
                best$Weight_Type, best$p_order, best$q_order, 
                best$AIC, best$BIC, best$Significant_Params, best$Total_Params,
                best$Min_PValue, best$Max_PValue))
  }
}

# Overall best model
best_model <- successful_models[1, ]
cat(sprintf("\n🥇 OVERALL BEST MODEL:\n"))
cat(sprintf("   STARIMA(%d,0,%d) with %s weights\n", 
            best_model$p_order, best_model$q_order, best_model$Weight_Type))
cat(sprintf("   AIC: %.1f | BIC: %.1f | Significant: %d/%d parameters\n",
            best_model$AIC, best_model$BIC, 
            best_model$Significant_Params, best_model$Total_Params))
cat(sprintf("   P-value range: %.4f - %.4f\n", 
            best_model$Min_PValue, best_model$Max_PValue))

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save all results
save(results, successful_models, best_model, 
     file = "output/18_model_order_selection.RData")

# Create summary for next steps
model_selection_summary <- list(
  best_overall = best_model,
  best_by_weight = lapply(weight_types, function(wt) {
    wt_models <- successful_models[successful_models$Weight_Type == wt, ]
    if (nrow(wt_models) > 0) wt_models[1, ] else NULL
  }),
  all_results = successful_models
)

names(model_selection_summary$best_by_weight) <- weight_types

save(model_selection_summary, file = "output/18_best_models.RData")

cat("\n✅ Results saved to:\n")
cat("- output/18_model_order_selection.RData (complete results)\n")
cat("- output/18_best_models.RData (summary for next steps)\n")

cat("\n🎯 RECOMMENDATION:\n")
cat("Update 09_Model_Structure.R with the best model configuration:\n")
cat(sprintf("p_order <- %d\n", best_model$p_order))
cat(sprintf("q_order <- %d\n", best_model$q_order))
cat(sprintf("# Best weight type: %s\n", best_model$Weight_Type))

end_time <- Sys.time()
total_time <- round(as.numeric(end_time - start_time), 2)

cat("\n⏱️ Total testing time:", total_time, "seconds\n")
cat("📊 Models tested:", nrow(results), "total,", nrow(successful_models), "successful\n")
cat("\n=== MODEL ORDER SELECTION COMPLETED ===\n")