#' Create an Interactive Plot for Multi-Factor Curve Analysis
#'
#' This function generates an interactive plot for multi-factor curve analysis.
#' @param stats A data frame containing the summarized group statistics and ranks.
#' @param factors A character vector specifying the factor variables used in the analysis.
#' @param outcome A character string specifying the outcome variable for the analysis.
#' @param alpha A numeric value indicating the significance level for confidence intervals.
#' @param showTitle Logical; whether to display a title on the plot.
#' @return A `plotly` object containing the plot.
#' @export
mfcurve_plotting <- function(stats, factors, outcome, alpha = 0.05, showTitle = TRUE) {
  # Ensure required columns are present
  required_cols <- c("rank", "mean_outcome", "ci_lower", "ci_upper", "group")
  if (!all(required_cols %in% names(stats))) {
    stop("The input data must include the columns: rank, mean_outcome, ci_lower, ci_upper, and group.")
  }

  # Calculate the grand mean
  grand_mean <- mean(stats$mean_outcome, na.rm = TRUE)

  # Separate group variable into individual factors
  stats <- stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  # Prepare data for the lower panel
  lower_data <- stats %>%
    dplyr::select(rank, dplyr::all_of(factors)) %>%
    tidyr::pivot_longer(cols = factors, names_to = "factor", values_to = "level") %>%
    dplyr::mutate(level_code = as.numeric(as.factor(level)))

  # Assign positions to factors
  factor_positions <- unique(lower_data$factor)
  lower_data <- lower_data %>%
    dplyr::mutate(y = match(factor, factor_positions))

  # Create plot
  # (The code remains the same, but the conflicts with magrittr have been resolved)
}
