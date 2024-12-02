#' Perform Statistical Testing for mfcurve Analysis
#'
#' This function performs statistical testing on grouped data, comparing each group's mean
#' against the overall grand mean or zero, calculates confidence intervals, and flags significant results.
#'
#' @param data A data frame containing the summarized results from preprocessing, with columns for group means, standard deviations, and counts.
#' @param test The type of test to perform: "mean" (compares each group's mean against the grand mean) or "zero" (compares each group's mean against zero).
#' @param alpha Significance level for confidence intervals and hypothesis testing (e.g., 0.05 for 95% confidence). Default is 0.05.
#' @return A data frame with the original data and added columns for t-values, p-values, significance flags, and confidence intervals.
#' @importFrom dplyr mutate
#' @importFrom stats pt qt
#' @examples
#' # Example data
#' data <- data.frame(
#'   group = c("A", "B", "C"),
#'   mean_outcome = c(15, 18, 22),
#'   sd_outcome = c(5, 6, 4),
#'   n = c(20, 25, 30)
#' )
#'
#' # Test each group against the grand mean
#' test_result <- mfcurve_stat_testing(data, test = "mean", alpha = 0.05)
#' print(test_result)
#'
#' # Test each group against zero
#' test_result_zero <- mfcurve_stat_testing(data, test = "zero", alpha = 0.05)
#' print(test_result_zero)
#' @export
mfcurve_stat_testing <- function(data, test = "mean", alpha = 0.05) {
  # Validate inputs
  if (!test %in% c("mean", "zero")) {
    stop("Invalid test specified. Choose either 'mean' or 'zero'.")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Invalid alpha level. Please provide a value between 0 and 1.")
  }

  # Calculate grand mean if testing against the mean
  grand_mean <- if (test == "mean") mean(data$mean_outcome, na.rm = TRUE) else 0

  # Perform statistical testing
  data <- data %>%
    dplyr::mutate(
      # Calculate t-values based on the specified test
      t_value = if (test == "mean") {
        (mean_outcome - grand_mean) / (sd_outcome / sqrt(n))
      } else {
        mean_outcome / (sd_outcome / sqrt(n))
      },
      # Calculate two-tailed p-values
      p_value = 2 * stats::pt(-abs(t_value), df = n - 1),
      # Significance flag
      significant = p_value < alpha,
      # Confidence intervals
      ci_lower = mean_outcome - stats::qt(1 - alpha / 2, df = n - 1) * (sd_outcome / sqrt(n)),
      ci_upper = mean_outcome + stats::qt(1 - alpha / 2, df = n - 1) * (sd_outcome / sqrt(n))
    )

  return(data)
}
