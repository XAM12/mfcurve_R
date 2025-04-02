#' Plot MF Curve Analysis Results
#'
#' This function creates an interactive Plotly visualization of MF curve analysis results
#' produced by \code{mfcurve_preprocessing}. It constructs an upper panel showing group means
#' with confidence intervals and a lower panel displaying factor level labels.
#'
#' @param processed_data A list returned by \code{mfcurve_preprocessing}.
#' @param outcome A character string specifying the name of the outcome variable (used for y-axis labeling).
#' @param showTitle A logical value indicating whether to display a title on the combined plot (default is TRUE).
#'
#' @return A Plotly object representing the combined MF curve plot.
#'
#' @examples
#' \dontrun{
#'   # Assuming 'processed' is created by mfcurve_preprocessing
#'   plot_obj <- mfcurve_plotting(processed, outcome = "wage", showTitle = TRUE)
#'   print(plot_obj)
#' }
#'
#' @import plotly
#' @export
mfcurve_plotting <- function(processed_data, outcome, showTitle = TRUE) {
  # Extract components from the processed data list
  group_stats_vis <- processed_data$group_stats_vis
  lower_data <- processed_data$lower_data
  grand_mean <- processed_data$grand_mean
  axis_limits <- processed_data$axis_limits
  factor_positions <- processed_data$factor_positions

  # Create the upper panel: plot group means with confidence intervals
  upper_plot <- plotly::plot_ly(
    data = group_stats_vis,
    x = ~rank,
    y = ~mean_outcome_vis,
    error_y = list(
      type = "data",
      symmetric = FALSE,
      array = group_stats_vis$ci_upper_vis - group_stats_vis$mean_outcome_vis,
      arrayminus = group_stats_vis$mean_outcome_vis - group_stats_vis$ci_lower_vis
    ),
    text = ~paste0("Mean: ", mean_outcome_vis, "<br>",
                   "SD: ", sd_outcome_vis, "<br>",
                   "CI: [", ci_lower_vis, ", ", ci_upper_vis, "] / ±", ci_width_vis),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(color = ifelse(group_stats_vis$sig, 'red', 'blue')),
    name = 'Group Means'
  ) %>%
    plotly::add_trace(
      x = c(min(group_stats_vis$rank), max(group_stats_vis$rank)),
      y = c(grand_mean, grand_mean),
      type = 'scatter',
      mode = 'lines',
      line = list(dash = 'dash'),
      name = 'Grand Mean',
      inherit = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(
        range = if (!is.null(axis_limits$x_min)) c(axis_limits$x_min, axis_limits$x_max) else NULL,
        showgrid = FALSE
      ),
      yaxis = list(
        range = if (!is.null(axis_limits$y_min)) c(axis_limits$y_min, axis_limits$y_max) else NULL,
        showgrid = TRUE
      )
    )

  # Create the lower panel: display factor level labels
  lower_plot <- plotly::plot_ly(
    data = lower_data,
    x = ~rank,
    y = ~y,
    text = ~paste("Factor:", factor, "<br>Level:", level),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 10,
      color = ~level_code,
      colorscale = 'Viridis'
    ),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      yaxis = list(
        tickvals = factor_positions$y,
        ticktext = factor_positions$factor,
        autorange = "reversed",
        showgrid = TRUE
      ),
      xaxis = list(
        title = "Group Rank",
        tickvals = group_stats_vis$rank,
        ticktext = as.character(group_stats_vis$rank),
        showgrid = FALSE
      )
    )

  # Combine the two panels into one subplot with shared x-axis
  combined_plot <- plotly::subplot(
    upper_plot, lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # Add a title and axis labels if requested
  title_text <- paste("Mean", outcome, "by the combination of factors")
  if (showTitle) {
    combined_plot <- combined_plot %>%
      plotly::layout(
        title = list(text = title_text, x = 0.5),
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors", showticklabels = TRUE)
      )
  } else {
    combined_plot <- combined_plot %>%
      plotly::layout(
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors", showticklabels = TRUE)
      )
  }

  return(combined_plot)
}
