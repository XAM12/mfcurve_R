#' Create an Interactive Plot for mfcurve Analysis
#'
#' This function generates an interactive plot for visualizing the results of an mfcurve analysis.
#' The plot consists of two panels: the upper panel displays group means with error bars, and the lower
#' panel maps group ranks to their factor-level combinations. The lower panel is fixed (not scrollable),
#' while the upper panel's y-axis scrollability can be optionally fixed.
#'
#' @param stats A data frame containing the summarized group statistics and ranks, typically output from `mfcurve_stat_testing`.
#' @param factors A character vector specifying the factor variables used in the analysis.
#' @param outcome A string specifying the outcome variable for the analysis (e.g., "wage").
#' @param alpha Significance level for confidence intervals (default is 0.05).
#' @param showTitle Logical; whether to display a title on the plot (default is TRUE).
#' @param mode Character; controls the mode of the lower plot ("collapsed" or "expanded"). Default is "collapsed".
#' @param upper_fixed_range Logical; if TRUE the y-axis in the upper panel is fixed (no scroll/drag), if FALSE it remains scrollable. Default is FALSE.
#' @return A `plotly` object containing the combined plot.
#' @importFrom plotly plot_ly subplot layout
#' @importFrom tidyr gather
#' @examples
#' # Example data
#' stats <- data.frame(
#'   group = c("A_B", "A_C", "B_C"),
#'   mean_outcome = c(10, 15, 20),
#'   sd_outcome = c(2, 3, 4),
#'   n = c(20, 25, 30),
#'   ci_lower = c(9, 14, 19),
#'   ci_upper = c(11, 16, 21),
#'   rank = c(1, 2, 3)
#' )
#' factors <- c("factor1", "factor2")
#'
#' # Generate plot with a scrollable upper panel (default)
#' mfcurve_plotting(stats, factors, outcome = "wage", upper_fixed_range = FALSE)
#'
#' # Generate plot with a fixed (non-scrollable) upper panel
#' mfcurve_plotting(stats, factors, outcome = "wage", upper_fixed_range = TRUE)
#' @export
mfcurve_plotting <- function(stats, factors, outcome, alpha = 0.05, showTitle = TRUE, mode = "collapsed", upper_fixed_range = FALSE) {
  require(plotly)
  require(tidyr)

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
    tidyr::gather(key = "factor", value = "level", -rank)

  if (mode == "expanded") {
    lower_data <- lower_data %>%
      dplyr::mutate(factor = paste(factor, level, sep = " ")) %>%
      dplyr::distinct()
  }

  lower_data <- lower_data %>%
    dplyr::group_by(factor) %>%
    dplyr::mutate(level_code = as.numeric(factor(level))) %>%
    dplyr::ungroup()

  # Assign positions to factors for the lower panel
  factor_levels <- unique(lower_data$factor)
  factor_positions <- data.frame(
    factor = factor_levels,
    y = seq(length(factor_levels), 1)
  )
  lower_data <- lower_data %>%
    dplyr::left_join(factor_positions, by = "factor")

  # Upper panel plot (group means with error bars)
  upper_plot <- plotly::plot_ly(
    data = stats,
    x = ~rank,
    y = ~mean_outcome,
    error_y = ~list(
      type = "data",
      symmetric = FALSE,
      array = ci_upper - mean_outcome,
      arrayminus = mean_outcome - ci_lower
    ),
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'blue'),
    name = 'Group Means'
  ) %>%
    plotly::add_trace(
      x = c(min(stats$rank), max(stats$rank)),
      y = c(grand_mean, grand_mean),
      type = 'scatter',
      mode = 'lines',
      line = list(dash = 'dash', color = 'orange'),
      name = 'Grand Mean',
      inherit = FALSE  # Prevent inheriting error bars
    )

  # Lower panel plot (factor combinations) with fixed y-axis (no scrolling)
  lower_plot <- plotly::plot_ly(
    data = lower_data,
    x = ~rank,
    y = ~y,
    text = ~paste("Factor:", factor, "<br>Level:", level),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 10, color = ~level_code, colorscale = 'Viridis'),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      yaxis = list(
        tickvals = factor_positions$y,
        ticktext = factor_positions$factor,
        autorange = "reversed",
        fixedrange = TRUE  # Lower panel: keine Scroll-/Drag-Funktion
      ),
      xaxis = list(title = "Group Rank")
    )

  # Combine the upper and lower panels
  combined_plot <- plotly::subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # Add title and configure axis properties, including optional fixedrange for upper panel
  if (showTitle) {
    title <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))
    combined_plot <- combined_plot %>%
      plotly::layout(
        title = list(text = title, x = 0.5),
        yaxis = list(title = outcome, fixedrange = upper_fixed_range),
        yaxis2 = list(title = "Factors", fixedrange = TRUE)
      )
  } else {
    combined_plot <- combined_plot %>%
      plotly::layout(
        yaxis = list(title = outcome, fixedrange = upper_fixed_range),
        yaxis2 = list(title = "Factors", fixedrange = TRUE)
      )
  }

  return(combined_plot)
}
