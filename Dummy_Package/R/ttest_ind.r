ttest_ind <- function(data, critical_value) {
  # Function to perform t-test between two groups
  perform_t_test <- function(group1, group2) {
    result <- t.test(group1, group2)
    return(result$p.value)
  }

  # Loop through all pairs of variables
  for (i in 2:(ncol(data) - 1)) {
    for (j in (i + 1):ncol(data)) {
      p_value <- perform_t_test(data[, i], data[, j])
      if (p_value < critical_value) {
        cat("Significant difference found between", names(data)[i], "and", names(data)[j], "\n")
        cat("  p-value:", p_value, "\n")
        
        # Check for missing values before calculating mean difference
        if (all(!is.na(data[, i])) && all(!is.na(data[, j]))) {
          cat("  Mean difference:", mean(data[, i]) - mean(data[, j]), "\n\n")
        } else {
          cat("  Mean difference: NAs present in data, unable to calculate.\n\n")
        }
      } else {
        cat("No significant difference found between", names(data)[i], "and", names(data)[j], "\n")
        cat("  p-value:", p_value, "\n\n")
      }
    }
  }
}

