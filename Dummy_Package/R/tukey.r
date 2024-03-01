tukeyhsd <- function(response_var, grouping_vars, data) {
    # warning against categorical data
    if (length(grouping_vars) > 1) {
        warning("Only one grouping variable is accepted.")
    }
    # Create the ANOVA model using the provided parameters
    anova_result <- aov(as.formula(paste(response_var, "~", grouping_vars)), data = data)

    # Perform Tukey's HSD (Honestly Significant Difference) test for multiple comparisons
    tukey_result <- TukeyHSD(anova_result)
    
    # Extract the p-values using the summary function
    p_values <- tukey_result[[grouping_vars]][, "p adj"]

    # Print the p-values
    print(p_values)
}

