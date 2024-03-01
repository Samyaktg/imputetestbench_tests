anova_plot <- function(df, group_col, value_col) {
  # ANOVA
  aov_result <- aov(df[[value_col]] ~ df[[group_col]], data = df)
  summary(aov_result)
  
  # Calculate mean and standard error
  means <- tapply(df[[value_col]], df[[group_col]], mean)
  errors <- tapply(df[[value_col]], df[[group_col]], function(x) sd(x)/sqrt(length(x)))
  
  # Create plot
  plot(1:length(means), means, ylim = range(c(means-errors, means+errors)), pch = 19, xaxt = "n", xlab = group_col, ylab = value_col)
  axis(1, at=1:length(means), labels=names(means))
  arrows(1:length(means), means-errors, 1:length(means), means+errors, length = 0.05, angle = 90, code = 3)
}


