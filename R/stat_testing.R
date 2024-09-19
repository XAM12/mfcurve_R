# File: R/stat_testing.R

# Correct @importFrom tags, ensuring they are placed correctly
#' @importFrom dplyr rowwise mutate ungroup
#' @importFrom stats pt qt

# Title line for the function documentation
#' Conduct Statistical Tests for Group Differences
#'
#' This function performs statistical tests (e.g., t-tests) between group means
#' and flags significant differences.
#'
#' @param data A data frame containing grouped data with means, standard deviations, and counts.
#' @param level Significance level (default is 0.05).
#' @param test Type of test ("mean" or "zero") to compare group means or test against zero.
#' @return A data frame with test results, confidence intervals, and significance flags.
#' @export
mfcurve_stat_test <- function(data, level = 0.05, test = "mean") {
  # Basic check for required columns
  if (!all(c("group", "mean_outcome", "sd_outcome", "n") %in% colnames(data))) {
    stop("Data must contain 'group', 'mean_outcome', 'sd_outcome', and 'n' columns.")
  }

  # Simplified code for testing
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      t_value = if (test == "mean") {
        mean_outcome / (sd_outcome / sqrt(n))
      } else if (test == "zero") {
        mean_outcome / (sd_outcome / sqrt(n))
      } else {
        NA
      },
      p_value = 2 * stats::pt(-abs(t_value), df = n - 1),
      significant = p_value < level,
      ci_lower = mean_outcome - stats::qt(1 - level / 2, df = n - 1) * (sd_outcome / sqrt(n)),
      ci_upper = mean_outcome + stats::qt(1 - level / 2, df = n - 1) * (sd_outcome / sqrt(n))
    ) %>%
    dplyr::ungroup()

  return(data)
}
