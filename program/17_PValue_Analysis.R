# ============================================================================
# P-Value Analysis for All Spatial Weights
# File: 17_PValue_Analysis.R
# Purpose: Analyze parameter significance across all three spatial weighting schemes
# ============================================================================

cat("=== P-VALUE ANALYSIS FOR ALL SPATIAL WEIGHTS ===\n")
cat("Analyzing parameter significance across Uniform, Distance, and Correlation weights...\n\n")

# ============================================================================
# LOAD REQUIRED DATA
# ============================================================================

# Load estimation results from all three models
load("output/10a_starima_uniform.RData")     # uniform_results
load("output/10b_starima_distance.RData")    # distance_results  
load("output/10c_starima_correlation.RData") # correlation_results

library(dplyr)
library(ggplot2)

cat("📊 Loaded estimation results from all three spatial weighting schemes\n\n")

# ============================================================================
# EXTRACT P-VALUES FROM EACH MODEL
# ============================================================================

cat("📈 Extracting p-values from coefficient tables...\n")

# Extract coefficient tables
uniform_coef <- uniform_results$coefficients
distance_coef <- distance_results$coefficients
correlation_coef <- correlation_results$coefficients

# Display coefficient tables with p-values
cat("\n🔵 UNIFORM WEIGHTS - Parameter Significance:\n")
cat("==========================================\n")
uniform_summary <- uniform_coef[, c("Parameter", "Estimate", "p_value", "Significant")]
print(uniform_summary)

cat("\n🟢 DISTANCE WEIGHTS - Parameter Significance:\n")
cat("=============================================\n")
distance_summary <- distance_coef[, c("Parameter", "Estimate", "p_value", "Significant")]
print(distance_summary)

cat("\n🟠 CORRELATION WEIGHTS - Parameter Significance:\n")
cat("================================================\n")
correlation_summary <- correlation_coef[, c("Parameter", "Estimate", "p_value", "Significant")]
print(correlation_summary)

# ============================================================================
# P-VALUE COMPARISON ANALYSIS
# ============================================================================

cat("\n📊 P-VALUE COMPARISON ANALYSIS:\n")
cat("===============================\n")

# Create comprehensive p-value comparison table
pvalue_comparison <- data.frame(
  Parameter = uniform_coef$Parameter,
  Uniform_PValue = round(uniform_coef$p_value, 4),
  Uniform_Sig = uniform_coef$Significant,
  Distance_PValue = round(distance_coef$p_value, 4),
  Distance_Sig = distance_coef$Significant,
  Correlation_PValue = round(correlation_coef$p_value, 4),
  Correlation_Sig = correlation_coef$Significant,
  stringsAsFactors = FALSE
)

print(pvalue_comparison)

# ============================================================================
# SIGNIFICANCE STATISTICS
# ============================================================================

cat("\n📈 SIGNIFICANCE STATISTICS:\n")
cat("===========================\n")

# Count significant parameters for each model
uniform_sig_count <- sum(uniform_coef$p_value < 0.05, na.rm = TRUE)
distance_sig_count <- sum(distance_coef$p_value < 0.05, na.rm = TRUE)
correlation_sig_count <- sum(correlation_coef$p_value < 0.05, na.rm = TRUE)

total_params <- nrow(uniform_coef)

significance_summary <- data.frame(
  Spatial_Weight = c("Uniform", "Distance", "Correlation"),
  Significant_Params = c(uniform_sig_count, distance_sig_count, correlation_sig_count),
  Total_Params = c(total_params, total_params, total_params),
  Significance_Rate = round(c(uniform_sig_count/total_params, 
                             distance_sig_count/total_params, 
                             correlation_sig_count/total_params) * 100, 1),
  stringsAsFactors = FALSE
)

print(significance_summary)

cat("\n🎯 SIGNIFICANCE RATE SUMMARY:\n")
for(i in 1:nrow(significance_summary)) {
  cat("- ", significance_summary$Spatial_Weight[i], ": ", 
      significance_summary$Significant_Params[i], "/", significance_summary$Total_Params[i], 
      " (", significance_summary$Significance_Rate[i], "%)\n", sep="")
}

# ============================================================================
# P-VALUE DISTRIBUTION ANALYSIS
# ============================================================================

cat("\n📊 P-VALUE DISTRIBUTION ANALYSIS:\n")
cat("=================================\n")

# Analyze p-value ranges
pvalue_ranges <- data.frame(
  Range = c("< 0.001 (Highly Sig)", "0.001-0.01 (Very Sig)", "0.01-0.05 (Significant)", 
            "0.05-0.1 (Marginal)", "> 0.1 (Not Sig)"),
  Uniform = c(
    sum(uniform_coef$p_value < 0.001, na.rm = TRUE),
    sum(uniform_coef$p_value >= 0.001 & uniform_coef$p_value < 0.01, na.rm = TRUE),
    sum(uniform_coef$p_value >= 0.01 & uniform_coef$p_value < 0.05, na.rm = TRUE),
    sum(uniform_coef$p_value >= 0.05 & uniform_coef$p_value < 0.1, na.rm = TRUE),
    sum(uniform_coef$p_value >= 0.1, na.rm = TRUE)
  ),
  Distance = c(
    sum(distance_coef$p_value < 0.001, na.rm = TRUE),
    sum(distance_coef$p_value >= 0.001 & distance_coef$p_value < 0.01, na.rm = TRUE),
    sum(distance_coef$p_value >= 0.01 & distance_coef$p_value < 0.05, na.rm = TRUE),
    sum(distance_coef$p_value >= 0.05 & distance_coef$p_value < 0.1, na.rm = TRUE),
    sum(distance_coef$p_value >= 0.1, na.rm = TRUE)
  ),
  Correlation = c(
    sum(correlation_coef$p_value < 0.001, na.rm = TRUE),
    sum(correlation_coef$p_value >= 0.001 & correlation_coef$p_value < 0.01, na.rm = TRUE),
    sum(correlation_coef$p_value >= 0.01 & correlation_coef$p_value < 0.05, na.rm = TRUE),
    sum(correlation_coef$p_value >= 0.05 & correlation_coef$p_value < 0.1, na.rm = TRUE),
    sum(correlation_coef$p_value >= 0.1, na.rm = TRUE)
  )
)

print(pvalue_ranges)

# ============================================================================
# PARAMETER CONSISTENCY ANALYSIS
# ============================================================================

cat("\n🔍 PARAMETER CONSISTENCY ANALYSIS:\n")
cat("==================================\n")

# Check which parameters are consistently significant across all models
consistent_significant <- rep(TRUE, nrow(pvalue_comparison))
consistent_nonsignificant <- rep(TRUE, nrow(pvalue_comparison))

for(i in 1:nrow(pvalue_comparison)) {
  # Check if significant in all models
  if(pvalue_comparison$Uniform_PValue[i] >= 0.05 || 
     pvalue_comparison$Distance_PValue[i] >= 0.05 || 
     pvalue_comparison$Correlation_PValue[i] >= 0.05) {
    consistent_significant[i] <- FALSE
  }
  
  # Check if non-significant in all models
  if(pvalue_comparison$Uniform_PValue[i] < 0.05 || 
     pvalue_comparison$Distance_PValue[i] < 0.05 || 
     pvalue_comparison$Correlation_PValue[i] < 0.05) {
    consistent_nonsignificant[i] <- FALSE
  }
}

consistency_analysis <- data.frame(
  Parameter = pvalue_comparison$Parameter,
  Consistently_Significant = consistent_significant,
  Consistently_NonSignificant = consistent_nonsignificant,
  Mixed_Results = !consistent_significant & !consistent_nonsignificant,
  stringsAsFactors = FALSE
)

cat("Parameters consistently SIGNIFICANT across all models:\n")
sig_params <- consistency_analysis$Parameter[consistency_analysis$Consistently_Significant]
if(length(sig_params) > 0) {
  cat(paste(sig_params, collapse = ", "), "\n")
} else {
  cat("None\n")
}

cat("\nParameters consistently NON-SIGNIFICANT across all models:\n")
nonsig_params <- consistency_analysis$Parameter[consistency_analysis$Consistently_NonSignificant]
if(length(nonsig_params) > 0) {
  cat(paste(nonsig_params, collapse = ", "), "\n")
} else {
  cat("None\n")
}

cat("\nParameters with MIXED results across models:\n")
mixed_params <- consistency_analysis$Parameter[consistency_analysis$Mixed_Results]
if(length(mixed_params) > 0) {
  cat(paste(mixed_params, collapse = ", "), "\n")
} else {
  cat("None\n")
}

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("\n📊 Creating p-value visualization...\n")

# Prepare data for plotting
plot_data <- rbind(
  data.frame(Parameter = uniform_coef$Parameter, PValue = uniform_coef$p_value, Model = "Uniform"),
  data.frame(Parameter = distance_coef$Parameter, PValue = distance_coef$p_value, Model = "Distance"),
  data.frame(Parameter = correlation_coef$Parameter, PValue = correlation_coef$p_value, Model = "Correlation")
)

# P-value comparison plot
pvalue_plot <- ggplot(plot_data, aes(x = Parameter, y = -log10(PValue), fill = Model)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred", size = 1) +
  labs(title = "Parameter Significance Comparison Across Spatial Weights",
       subtitle = "Higher bars = more significant (red line = p=0.05, dark red = p=0.01)",
       x = "Parameters", y = "-log10(p-value)",
       fill = "Spatial Weight") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_manual(values = c("Uniform" = "steelblue", "Distance" = "forestgreen", "Correlation" = "darkorange"))

ggsave("plots/17_pvalue_analysis.png", pvalue_plot, width = 16, height = 10, dpi = 300)
print(pvalue_plot)

# ============================================================================
# SAVE RESULTS
# ============================================================================

pvalue_analysis_results <- list(
  pvalue_comparison = pvalue_comparison,
  significance_summary = significance_summary,
  pvalue_ranges = pvalue_ranges,
  consistency_analysis = consistency_analysis,
  uniform_coef = uniform_coef,
  distance_coef = distance_coef,
  correlation_coef = correlation_coef
)

save(pvalue_analysis_results, file = "output/17_pvalue_analysis.RData")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n=== P-VALUE ANALYSIS COMPLETED ===\n")
cat("✅ Parameter significance analyzed for all three spatial weights\n")
cat("📊 Significance rates:\n")
cat("   - Uniform:", significance_summary$Significance_Rate[1], "%\n")
cat("   - Distance:", significance_summary$Significance_Rate[2], "%\n") 
cat("   - Correlation:", significance_summary$Significance_Rate[3], "%\n")
cat("🔍 Consistently significant parameters:", length(sig_params), "\n")
cat("🔍 Mixed significance parameters:", length(mixed_params), "\n")
cat("💾 Results saved to: output/17_pvalue_analysis.RData\n")
cat("📊 Visualization saved to: plots/17_pvalue_analysis.png\n")