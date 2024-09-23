#' Statistical Testing for mfcurve Analysis
#'
#' This function performs statistical testing on grouped data, comparing each group's mean
#' against the mean of all other groups, calculates confidence intervals, and flags significant results.
#'
#' @param data A data frame containing the results of the preprocessing step.
#' @param test The type of test to perform: "mean" (against other groups' means) or "zero" (against zero).
#' @param level Confidence level for significance testing (e.g., 0.95 for 95% CI).
#' @return A data frame with test results, including group means, t-values, p-values, and confidence intervals.
#' @export
mfcurve_stat_test <- function(data, test = "mean", level = 0.95) {

  # Check for valid test specification
  if (!test %in% c("mean", "zero")) {
    stop("Test must be either 'mean' or 'zero'.")
  }

  # Initialize vectors to store results
  t_values <- numeric(nrow(data))
  p_values <- numeric(nrow(data))
  ci_lower <- numeric(nrow(data))
  ci_upper <- numeric(nrow(data))
  significant <- logical(nrow(data))

  # Perform t-tests for each group
  for (i in seq_len(nrow(data))) {
    group_mean <- data$mean_outcome[i]
    group_sd <- data$sd_outcome[i]
    group_n <- data$n[i]

    # Exclude the current group to calculate the mean of other groups
    other_groups <- data[-i, ]
    other_mean <- mean(other_groups$mean_outcome, na.rm = TRUE)
    other_n <- sum(other_groups$n, na.rm = TRUE)
    other_sd <- sqrt(sum((other_groups$n - 1) * (other_groups$sd_outcome^2), na.rm = TRUE) / (other_n - length(na.omit(other_groups$mean_outcome))))

    # Skip testing if group_n or group_sd is not suitable for t-test
    if (group_n < 2 || is.na(group_sd) || group_sd == 0 || is.na(other_sd) || other_sd == 0) {
      t_values[i] <- NA
      p_values[i] <- NA
      ci_lower[i] <- NA
      ci_upper[i] <- NA
      significant[i] <- FALSE
      next
    }

    # Calculate t-test statistics
    se <- sqrt((group_sd^2 / group_n) + (other_sd^2 / other_n))
    t_values[i] <- (group_mean - other_mean) / se
    df <- (group_sd^2 / group_n + other_sd^2 / other_n)^2 /
      (((group_sd^2 / group_n)^2 / (group_n - 1)) + ((other_sd^2 / other_n)^2 / (other_n - 1)))
    p_values[i] <- 2 * (1 - stats::pt(abs(t_values[i]), df))

    # Calculate confidence intervals
    ci_margin <- qt(1 - (1 - level) / 2, df) * se
    ci_lower[i] <- group_mean - ci_margin
    ci_upper[i] <- group_mean + ci_margin

    # Flag significance based on the confidence level
    significant[i] <- p_values[i] < (1 - level)
  }

  # Add test results to the data frame
  data <- data %>%
    dplyr::mutate(
      t_value = t_values,
      p_value = p_values,
      significant = significant,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )

  return(data)
}
