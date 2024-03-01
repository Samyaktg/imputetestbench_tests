multicomp <- function(response_var, grouping_vars, data) {
  # Anova 
  model <- aov(formula(paste(response_var, "~", paste(grouping_vars, collapse = "+"))), data = data)
  
  # Pairwise comparisons using Tukey's HSD
  comparison_result <- pairwise.t.test(data[[response_var]], interaction(data[grouping_vars]), p.adj = "holm")
  
  # Store the results in a dataframe
  results_df <- as.data.frame(comparison_result$p.value)
  results_df$group1 <- rownames(results_df)
  results_df$group2 <- colnames(results_df)[-ncol(results_df)]
  
  # Filter rows where p-value is less than 0.05
  significant_results <- results_df[results_df$p.value < 0.05, ]
  
  # Print only the significant results
  print(significant_results)
  
  # Return the full dataframe if needed
  View(results_df)
}