#' Statistical Testing for mfcurve Analysis
#'
#' This function performs statistical testing on grouped data, comparing each group's mean
#' against either the overall grand mean or zero, calculates confidence intervals, and flags significant results.
#'
#' @param data A data frame containing the summarized results of the preprocessing step, with columns for group means, standard deviations, and counts.
#' @param test The type of test to perform: "mean" (compares each group's mean against the grand mean) or "zero" (compares each group's mean against zero).
#' @param level Significance level for confidence intervals and hypothesis testing (e.g., 0.05 for 95% confidence).
#' @return A data frame with the original data and added columns for t-values, p-values, significance flags, and confidence intervals.
#' @examples
#' # Sample data
#' data <- data.frame(
#'   group = 1:4,
#'   mean_outcome = c(10, 20, 15, 25),
#'   sd_outcome = c(5, 6, 7, 8),
#'   n = c(10, 12, 15, 8)
#' )
#'
#' # Test each group against the grand mean
#' test_result <- mfcurve_stat_test(data, test = "mean", level = 0.05)
#' print(test_result)
#'
#' # Test each group against zero
#' test_result_zero <- mfcurve_stat_test(data, test = "zero", level = 0.05)
#' print(test_result_zero)
#' @export
mfcurve_stat_test <- function(data, test = "mean", level = 0.05) {

  # Validate test type
  if (!test %in% c("mean", "zero")) {
    stop("Invalid test specified. Choose either 'mean' or 'zero'.")
  }

  # Calculate grand mean if testing against the mean
  grand_mean <- if (test == "mean") mean(data$mean_outcome, na.rm = TRUE) else 0

  # Statistical testing
  data <- data %>%
    dplyr::mutate(
      # Calculate t-value based on the chosen test
      t_value = if (test == "mean") {
        (mean_outcome - grand_mean) / (sd_outcome / sqrt(n))
      } else {
        mean_outcome / (sd_outcome / sqrt(n))
      },
      # Calculate two-tailed p-value
      p_value = 2 * stats::pt(-abs(t_value), df = n - 1),
      # Significance flag
      significant = p_value < level,
      # Confidence intervals
      ci_lower = mean_outcome - stats::qt(1 - level / 2, df = n - 1) * (sd_outcome / sqrt(n)),
      ci_upper = mean_outcome + stats::qt(1 - level / 2, df = n - 1) * (sd_outcome / sqrt(n))
    )

  return(data)
}
