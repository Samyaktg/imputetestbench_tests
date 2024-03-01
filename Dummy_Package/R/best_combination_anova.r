best_comb <- function(data, target_var) {
    # Function to perform ANOVA and return p-value
    perform_anova <- function(data, target_var, independent_vars) {
        model <- lm(paste(target_var, "~", paste(independent_vars, collapse = "+")), data = data)
        anova_result <- anova(model)
        return(anova_result$"Pr(>F)"[1])  
    }

    # extracting a list of independent variables
    independent_vars <- colnames(data)[colnames(data) != target_var]
    anova_results <- list()

    # Iterate through all combinations of independent variables
    for (size in 1:length(independent_vars)) {
        combinations <- combn(independent_vars, size, simplify = TRUE)
        
        # Perform ANOVA for each combination
        for (i in 1:ncol(combinations)) {
            vars <- combinations[, i]
            p_value <- perform_anova(data, target_var = target_var, independent_vars = vars)
            anova_results[[paste(vars, collapse = "_")]] <- p_value
        }
    }

    # Sort combinations by p-value in ascending order
    sorted_combinations <- names(sort(unlist(anova_results), decreasing = FALSE))

    # Print the list of combinations from least significance to most significance
    print(sorted_combinations)
}

