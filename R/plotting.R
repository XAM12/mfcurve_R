#' Create an Interactive Plot with Plotly
#'
#' This function generates an interactive plot based on the `mfcurve` analysis results.
#'
#' @param data A data frame containing the results of the analysis.
#' @param upper_panel_cols A named list specifying the x and y columns for the upper panel plot.
#'        Example: list(x = "group", y = "mean_value").
#' @param lower_panel_cols A named list specifying the x and y columns for the lower panel plot.
#'        Example: list(x = "group", y = "ci_lower").
#' @param show_title Logical; if TRUE, adds a title to the combined plot.
#' @return A `plotly` object representing the combined plot.
#' @export
create_interactive_plot <- function(data, upper_panel_cols, lower_panel_cols, show_title = TRUE) {
  library(plotly)

  # Upper panel plot
  upper_plot <- plot_ly(
    data = data,
    x = ~ get(upper_panel_cols$x),
    y = ~ get(upper_panel_cols$y),
    type = 'scatter',
    mode = 'markers+lines',
    name = 'Group Means'
  )

  # Lower panel plot
  lower_plot <- plot_ly(
    data = data,
    x = ~ get(lower_panel_cols$x),
    y = ~ get(lower_panel_cols$y),
    type = 'scatter',
    mode = 'lines',
    line = list(dash = 'dash'),
    name = 'Confidence Interval'
  )

  # Combine plots
  combined_plot <- subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    titleX = TRUE
  )

  # Add title to the combined plot if show_title is TRUE
  if (show_title) {
    combined_plot <- combined_plot %>% layout(title = "Interactive Plotly Visualization")
  }

  # Return the combined plot object
  return(combined_plot)
}
