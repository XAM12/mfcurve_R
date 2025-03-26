#' Perform Statistical Testing for Multi-Factor Curve Analysis
#'
#' This function conducts statistical tests on group-level data (typically the output from mfcurve_preprocessing).
#' It compares each group's mean outcome either against the overall grand mean or against zero, and computes:
#' - The t-statistic.
#' - The two-tailed p-value.
#' - A flag indicating statistical significance based on the specified alpha level.
#' - Confidence intervals (lower and upper bounds) for the group means.
#' In addition, the function rounds numerical results to a specified number of decimal places for improved readability.
#'
#' @param data A data frame containing the summarized group-level data.
#' @param test A character string specifying the test type:
#'   - "mean": Compares each group's mean against the grand mean.
#'   - "zero": Compares each group's mean against zero.
#'   Default is "mean".
#' @param alpha A numeric value indicating the significance level (e.g., 0.05 for a 95% confidence interval). Default is 0.05.
#' @param rounding An integer specifying the number of decimal places to round the output statistics (default: 2).
#'
#' @return A data frame with additional columns:
#' \item{t_value}{The computed t-statistic for each group.}
#' \item{p_value}{The two-tailed p-value corresponding to the t-statistic.}
#' \item{significant}{A logical value indicating whether the test result is statistically significant (TRUE if p < alpha, else FALSE).}
#' \item{ci_lower}{The lower bound of the confidence interval for the group mean.}
#' \item{ci_upper}{The upper bound of the confidence interval for the group mean.}
#'
#' @examples
#' \dontrun{
#' preprocessed_data <- mfcurve_preprocessing(df, outcome_var = "wage",
#'                                            factors = c("race", "south", "union"))
#' test_results <- mfcurve_stat_testing(preprocessed_data, test = "mean", alpha = 0.05)
#' }
#'
#' @export
mfcurve_stat_testing <- function(data, test = "mean", alpha = 0.05, rounding = 2) {
  if (!test %in% c("mean", "zero")) {
    stop("Invalid test specified. Choose either 'mean' or 'zero'.")
  }

  grand_mean <- if (test == "mean") mean(data$mean_outcome, na.rm = TRUE) else 0

  data <- data %>%
    dplyr::mutate(
      t_value = if (test == "mean") {
        (mean_outcome - grand_mean) / (sd_outcome / sqrt(n))
      } else {
        mean_outcome / (sd_outcome / sqrt(n))
      },
      p_value = 2 * stats::pt(-abs(t_value), df = n - 1),
      significant = p_value < alpha,
      ci_lower = mean_outcome - stats::qt(1 - alpha / 2, df = n - 1) * (sd_outcome / sqrt(n)),
      ci_upper = mean_outcome + stats::qt(1 - alpha / 2, df = n - 1) * (sd_outcome / sqrt(n))
    ) %>%
    dplyr::mutate(
      mean_outcome = round(mean_outcome, rounding),
      ci_lower = round(ci_lower, rounding),
      ci_upper = round(ci_upper, rounding)
    )

  return(data)
}
