#' Preprocess Data for MF Curve Analysis
#'
#' This function preprocesses the input data for MF curve analysis by computing group statistics,
#' t-tests comparing group means to either the overall mean or zero (depending on the 'test' parameter),
#' confidence intervals, and prepares data for plotting.
#'
#' @param data A data frame containing the data.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param factors A character vector specifying the names of the grouping factors.
#' @param test A character string specifying the reference value for the t-test.
#'             Use "mean" (default) to test against the overall mean, or "zero" to test against 0.
#' @param alpha A numeric value indicating the significance level (default is 0.05).
#' @param rounding A non-negative integer specifying the number of decimal places for visualization rounding (default is 2).
#' @param mode A character string for labeling groups. Use "collapsed" for default labels or "expanded" to append factor levels to names (default is "collapsed").
#' @param plotOrigin A logical value. If TRUE, the plot axes will start at zero where applicable (default is FALSE).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{group_stats}{A data frame with computed group statistics (mean, SD, t-statistic, p-value, confidence intervals, etc.).}
#'   \item{group_stats_vis}{A version of \code{group_stats} with rounded values for visualization.}
#'   \item{grand_mean}{The overall mean of the outcome variable (only relevant if \code{test = "mean"}).}
#'   \item{lower_data}{A data frame prepared for the lower panel plot (factor level labels).}
#'   \item{axis_limits}{A list with x and y axis limits for plotting.}
#'   \item{factor_positions}{A data frame with positions for factor labels in the lower panel.}
#' }
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   df <- data.frame(
#'     wage = rnorm(100, mean = 15, sd = 5),
#'     race = sample(c("White", "Black", "Other"), 100, replace = TRUE),
#'     south = sample(c("Yes", "No"), 100, replace = TRUE),
#'     union = sample(c("Yes", "No"), 100, replace = TRUE)
#'   )
#'   processed <- mfcurve_preprocessing(df, outcome = "wage",
#'                                      factors = c("race", "south", "union"),
#'                                      test = "mean",
#'                                      rounding = 3, mode = "collapsed", plotOrigin = TRUE)
#' }
#'
#' @import dplyr tidyr rlang
#' @export
mfcurve_preprocessing <- function(data, outcome, factors, test = "mean", alpha = 0.05, rounding = 2,
                                  mode = "collapsed", plotOrigin = FALSE) {
  # Validate rounding input
  if (!is.numeric(rounding) || rounding %% 1 != 0 || rounding < 0) {
    stop("The 'rounding' parameter must be a non-negative whole number.")
  }

  # Remove missing values for outcome and factor columns
  vars <- c(outcome, factors)
  data <- data %>% tidyr::drop_na(dplyr::all_of(vars))

  # Create a group variable by combining factor levels
  data <- data %>%
    dplyr::mutate(group = interaction(!!!rlang::syms(factors), sep = "_"))

  # Calculate the overall (grand) mean
  grand_mean <- mean(data[[outcome]])

  # Determine the reference value for t-tests based on the 'test' parameter
  if (test == "mean") {
    reference <- grand_mean
  } else if (test == "zero") {
    reference <- 0
  } else {
    stop("Invalid value for 'test'. Please use 'mean' or 'zero'.")
  }

  # Compute group statistics: mean, standard deviation, count
  group_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(.data[[outcome]]),
      sd_outcome = sd(.data[[outcome]]),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Perform t-tests comparing group means to the reference value
  group_stats <- group_stats %>%
    dplyr::mutate(
      t_stat = (mean_outcome - reference) / (sd_outcome / sqrt(n)),
      p_value = 2 * pt(-abs(t_stat), df = n - 1),
      sig = p_value < alpha
    )

  # Rank groups by ascending mean outcome
  group_stats <- group_stats %>%
    dplyr::arrange(mean_outcome) %>%
    dplyr::mutate(rank = dplyr::row_number())

  # Calculate confidence intervals
  group_stats <- group_stats %>%
    dplyr::mutate(
      se = sd_outcome / sqrt(n),
      ci_lower = mean_outcome - qt(1 - alpha / 2, df = n - 1) * se,
      ci_upper = mean_outcome + qt(1 - alpha / 2, df = n - 1) * se,
      ci_width = (ci_upper - ci_lower) / 2
    )

  # Create a visualization version with rounded numerical values
  group_stats_vis <- group_stats %>%
    dplyr::mutate(
      mean_outcome_vis = round(mean_outcome, rounding),
      sd_outcome_vis = round(sd_outcome, rounding),
      ci_lower_vis = round(ci_lower, rounding),
      ci_upper_vis = round(ci_upper, rounding),
      ci_width_vis = round(ci_width, rounding)
    )

  # Separate the combined group variable back into individual factors
  group_stats <- group_stats %>%
    tidyr::separate(col = group, into = factors, sep = "_")
  group_stats_vis <- group_stats_vis %>%
    tidyr::separate(col = group, into = factors, sep = "_")

  # Prepare data for the lower panel (factor level labels)
  lower_data <- group_stats %>%
    dplyr::select(rank, dplyr::all_of(factors)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(factors),
                        names_to = "factor", values_to = "level")

  if (mode == "expanded") {
    lower_data <- lower_data %>%
      dplyr::mutate(factor = paste(factor, level, sep = " ")) %>%
      dplyr::distinct()
  }

  # Assign numeric codes to factor levels for coloring
  lower_data <- lower_data %>%
    dplyr::group_by(factor) %>%
    dplyr::mutate(level_code = as.numeric(factor(level))) %>%
    dplyr::ungroup()

  # Compute factor positions for y-axis labeling in the lower plot
  factor_levels <- unique(lower_data$factor)
  factor_positions <- data.frame(
    factor = factor_levels,
    y = rev(seq_along(factor_levels))
  )

  lower_data <- lower_data %>%
    dplyr::left_join(factor_positions, by = "factor")

  # Determine axis limits if plotOrigin is TRUE
  if (plotOrigin) {
    x_min <- min(group_stats_vis$rank, 0)
    x_max <- max(group_stats_vis$rank) + 0.5
    y_min <- min(group_stats_vis$ci_lower_vis, 0)
    y_max <- max(group_stats_vis$ci_upper_vis)
  } else {
    x_min <- NULL
    x_max <- NULL
    y_min <- NULL
    y_max <- NULL
  }

  axis_limits <- list(x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)

  # Return the processed data components as a list
  return(list(
    group_stats = group_stats,
    group_stats_vis = group_stats_vis,
    grand_mean = grand_mean,
    lower_data = lower_data,
    axis_limits = axis_limits,
    factor_positions = factor_positions
  ))
}
