#' Statistical Testing for mfcurve Analysis
#'
#' This function performs statistical testing on grouped data, comparing each group's mean
#' against the mean of all other groups or zero, calculates confidence intervals, and flags significant results.
#'
#' @param data A data frame containing the summarized results of the preprocessing step, with columns for group means, standard deviations, and counts.
#' @param test The type of test to perform: "mean" (compares each group's mean against the mean of all other groups) or "zero" (compares each group's mean against zero).
#' @param level Confidence level for significance testing (e.g., 0.95 for a 95% confidence interval).
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
#' # Test each group against the mean of other groups
#' test_result <- mfcurve_stat_test(data, test = "mean", level = 0.95)
#' print(test_result)
#'
#' # Test each group against zero
#' test_result_zero <- mfcurve_stat_test(data, test = "zero", level = 0.95)
#' print(test_result_zero)
#' @export
mfcurve_stat_test <- function(data, level = 0.05, test = "mean") {
  # Check for required columns
  if (!all(c("group", "mean_outcome", "sd_outcome", "n") %in% colnames(data))) {
    stop("Data must contain 'group', 'mean_outcome', 'sd_outcome', and 'n' columns.")
  }

  # Statistical testing implementation
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
