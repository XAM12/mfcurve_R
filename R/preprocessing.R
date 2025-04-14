#' Preprocess Data and Compute Group Statistics
#'
#' This function prepares data and computes descriptive statistics and t-tests
#' for groups defined by combinations of categorical factors.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string indicating the name of the outcome variable (numeric).
#' @param factors A character vector of factor variable names to define groups.
#' @param alpha Significance level for the t-tests and confidence intervals. Default is 0.05.
#' @param test A string indicating the reference value for t-tests: "mean" (group mean vs. grand mean) or "zero" (group mean vs. 0).
#'
#' @return A list with:
#' \describe{
#'   \item{group_stats}{A data frame with computed statistics and CI bounds.}
#'   \item{group_stats_vis}{A visualization-ready version with rounded numbers.}
#'   \item{lower_data}{Data for the lower panel of the plot (without y positions).}
#'   \item{grand_mean}{The overall mean of the outcome variable.}
#'   \item{level}{The confidence level used.}
#' }
#' @export
mfcurve_preprocessing <- function(data, outcome, factors, alpha = 0.05, test = "mean") {
  # Input validation
  if (!is.numeric(data[[outcome]])) {
    stop("The outcome variable must be numeric.")
  }
  if (!test %in% c("mean", "zero")) {
    stop('Argument "test" must be either "mean" or "zero".')
  }

  # Remove missing values
  vars <- c(outcome, factors)
  data <- tidyr::drop_na(data, tidyselect::all_of(vars))

  # Create group variable
  data <- dplyr::mutate(data, group = interaction(dplyr::across(tidyselect::all_of(factors)), sep = "_"))

  # Compute grand mean
  grand_mean <- mean(data[[outcome]])

  # Group statistics
  group_stats <- data |>
    dplyr::group_by(group) |>
    dplyr::summarize(
      mean_outcome = mean(.data[[outcome]]),
      sd_outcome = sd(.data[[outcome]]),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      test_value = if (test == "mean") grand_mean else 0,
      t_stat = (mean_outcome - test_value) / (sd_outcome / sqrt(n)),
      p_value = 2 * stats::pt(-abs(t_stat), df = n - 1),
      sig = p_value < alpha
    ) |>
    dplyr::arrange(mean_outcome) |>
    dplyr::mutate(rank = dplyr::row_number())

  # Confidence intervals
  level <- 1 - alpha
  group_stats <- dplyr::mutate(group_stats,
                               se = sd_outcome / sqrt(n),
                               ci_lower = mean_outcome - stats::qt(1 - alpha / 2, df = n - 1) * se,
                               ci_upper = mean_outcome + stats::qt(1 - alpha / 2, df = n - 1) * se,
                               ci_width = (ci_upper - ci_lower) / 2
  )

  # Visualization-ready version (not rounded here; rounding is applied in plotting)
  group_stats_vis <- group_stats

  # Separate group back into factors
  group_stats <- tidyr::separate(group_stats, group, into = factors, sep = "_")
  group_stats_vis <- tidyr::separate(group_stats_vis, group, into = factors, sep = "_")

  # Prepare lower panel data (no y positions)
  lower_data <- group_stats |>
    dplyr::select(rank, tidyselect::all_of(factors)) |>
    tidyr::pivot_longer(cols = -rank, names_to = "factor", values_to = "level") |>
    dplyr::group_by(factor) |>
    dplyr::mutate(level_code = as.numeric(factor(level))) |>
    dplyr::ungroup()

  return(list(
    group_stats = group_stats,
    group_stats_vis = group_stats_vis,
    lower_data = lower_data,
    grand_mean = grand_mean,
    level = level
  ))
}
