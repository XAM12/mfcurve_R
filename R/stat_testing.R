#' Perform Statistical Testing for Multi-Factor Curve Analysis
#'
#' This function conducts statistical testing on grouped data to:
#' - Compare each group's mean against the overall grand mean or zero.
#' - Calculate confidence intervals for group means.
#' - Identify statistically significant differences using hypothesis tests.
#'
#' @param data A data frame containing the summarized group-level data, typically output from `mfcurve_preprocessing`.
#' @param test A character string specifying the type of test to perform:
#'   - "mean": Compares each group's mean against the grand mean.
#'   - "zero": Compares each group's mean against zero.
#' @param alpha A numeric value indicating the significance level for confidence intervals and hypothesis testing (e.g., 0.05 for 95% confidence). Default is 0.05.
#' @return A data frame with the following columns added to the input data:
#' - `t_value`: The calculated t-value for each group.
#' - `p_value`: The two-tailed p-value for the test.
#' - `significant`: A logical flag indicating whether the result is statistically significant (`TRUE` or `FALSE`).
#' - `ci_lower`: The lower bound of the confidence interval for the group mean.
#' - `ci_upper`: The upper bound of the confidence interval for the group mean.
#'
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
  if (anyNA(data$mean_outcome) || anyNA(data$sd_outcome) || anyNA(data$n)) {
    stop("Input data contains missing values in required columns.")
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
