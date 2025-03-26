#' Create an Interactive Plot for mfcurve Analysis (with Two Panels)
#'
#' This function generates an interactive Plotly plot that combines two panels:
#' - The upper panel displays group means with error bars (confidence intervals).
#' - The lower panel visualizes the factor-level combinations mapped to group ranks.
#'
#' The function offers the following customizations:
#' - \code{mode}: "collapsed" (shows only factor names) or "expanded" (appends factor levels to the names).
#' - \code{upper_fixed_range}: If TRUE, the y-axis of the upper panel is fixed (no scroll/drag).
#' - \code{color_scheme}: "default" for a colored palette, "bw" for grayscale.
#' - \code{plotOrigin}: If TRUE, the coordinate origin (0,0) is included in the plot.
#' - \code{rounding}: Number of decimal places for rounding numeric values.
#'
#' @param stats A data frame containing the summarized group statistics (e.g., output from mfcurve_stat_testing).
#' @param factors A character vector specifying the factor variables used in the analysis.
#' @param outcome A string specifying the outcome variable (e.g., "wage").
#' @param alpha A numeric value for the significance level (default: 0.05).
#' @param rounding An integer specifying the number of decimal places for rounding numeric values (default: 2).
#' @param plotOrigin Logical. If TRUE, includes the coordinate origin (0,0) in the plot (default: FALSE).
#' @param showTitle Logical. If TRUE, displays a title on the plot (default: TRUE).
#' @param mode Character; controls the display of the lower panel ("collapsed" or "expanded") (default: "collapsed").
#' @param upper_fixed_range Logical. If TRUE, fixes the y-axis of the upper panel (default: FALSE).
#' @param color_scheme Character; determines the color scheme ("default" for color, "bw" for black-and-white) (default: "default").
#'
#' @return A Plotly object containing the combined interactive plot.
#'
#' @examples
#' \dontrun{
#' preprocessed_data <- mfcurve_preprocessing(df, outcome_var = "wage",
#'                                            factors = c("race", "south", "union"))
#' test_results <- mfcurve_stat_testing(preprocessed_data, test = "mean", alpha = 0.05)
#' plot1 <- mfcurve_plotting(test_results, factors = c("race", "south", "union"),
#'                           outcome = "wage", alpha = 0.05, showTitle = TRUE,
#'                           mode = "collapsed", upper_fixed_range = FALSE,
#'                           color_scheme = "default")
#' plot2 <- mfcurve_plotting(test_results, factors = c("race", "south", "union"),
#'                           outcome = "wage", alpha = 0.05, showTitle = TRUE,
#'                           mode = "expanded", upper_fixed_range = TRUE,
#'                           color_scheme = "bw")
#' }
#'
#' @export
mfcurve_plotting <- function(stats, factors, outcome, alpha = 0.05, rounding = 2,
                             plotOrigin = FALSE, showTitle = TRUE, mode = "collapsed",
                             upper_fixed_range = FALSE, color_scheme = "default") {
  require(plotly)
  require(tidyr)
  require(dplyr)
  require(RColorBrewer)

  # Round numeric values for visualization
  stats <- stats %>%
    dplyr::mutate(
      mean_outcome = round(mean_outcome, rounding),
      ci_lower = round(ci_lower, rounding),
      ci_upper = round(ci_upper, rounding)
    )

  # Determine axis ranges if plotOrigin is TRUE
  x_min <- if (plotOrigin) min(stats$rank, 0) else NULL
  x_max <- if (plotOrigin) max(stats$rank) + 0.5 else NULL
  y_min <- if (plotOrigin) min(stats$ci_lower, 0) else NULL
  y_max <- if (plotOrigin) max(stats$ci_upper) else NULL

  # Compute grand mean for reference line
  grand_mean <- mean(stats$mean_outcome, na.rm = TRUE)

  # Split group variable into individual factors
  stats <- stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  # Determine binary factors and their level counts
  factor_level_info <- lapply(factors, function(fct) {
    n_lvls <- dplyr::n_distinct(stats[[fct]])
    data.frame(orig_factor = fct, n_levels = n_lvls)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(is_binary = (n_levels == 2))

  # Prepare lower panel data
  lower_data <- stats %>%
    dplyr::select(rank, dplyr::all_of(factors)) %>%
    tidyr::gather(key = "orig_factor", value = "orig_level", -rank) %>%
    dplyr::left_join(factor_level_info, by = "orig_factor")

  # Assign marker symbols for binary factors
  lower_data <- lower_data %>%
    dplyr::mutate(
      symbol = dplyr::case_when(
        is_binary & orig_level == "Yes" ~ "circle",
        is_binary & orig_level == "No" ~ "circle-open",
        TRUE ~ "circle"
      )
    )

  # Adjust labels based on mode
  if (mode == "expanded") {
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = paste(orig_factor, orig_level, sep = " ")) %>%
      dplyr::distinct()
  } else {
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = orig_factor)
  }

  # Determine color mapping
  lower_data <- lower_data %>%
    dplyr::mutate(
      color_key = dplyr::if_else(is_binary, orig_factor, paste(orig_factor, orig_level, sep = "_"))
    )
  unique_keys <- unique(lower_data$color_key)
  n_keys <- length(unique_keys)
  if (color_scheme == "default") {
    palette <- brewer.pal(n = max(n_keys, 3), name = "Set1")[1:n_keys]
  } else if (color_scheme == "bw") {
    palette <- gray.colors(n_keys, start = 0, end = 0.7)
  } else {
    stop("Invalid color_scheme specified. Use 'default' or 'bw'.")
  }
  color_map <- setNames(palette, unique_keys)
  lower_data <- lower_data %>%
    dplyr::mutate(final_color = color_map[color_key])

  # Set y positions for lower panel factors
  factor_labels <- unique(lower_data$factor_display)
  factor_positions <- data.frame(
    factor_display = factor_labels,
    y = seq(length(factor_labels), 1)
  )
  lower_data <- lower_data %>%
    dplyr::left_join(factor_positions, by = "factor_display")

  # Create upper panel: group means with error bars
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
      inherit = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(
        range = if (!is.null(x_min)) c(x_min, x_max) else NULL,
        showgrid = FALSE
      ),
      yaxis = list(
        range = if (!is.null(y_min)) c(y_min, y_max) else NULL,
        showgrid = FALSE,
        fixedrange = upper_fixed_range
      )
    )

  # Create lower panel: factor-level mapping
  lower_plot <- plotly::plot_ly(
    data = lower_data,
    x = ~rank,
    y = ~y,
    text = ~paste("Factor:", orig_factor, "<br>Level:", orig_level),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 10,
      color = ~final_color,
      symbol = ~symbol,
      line = list(width = 2, color = ~final_color)
    ),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      yaxis = list(
        tickvals = factor_positions$y,
        ticktext = factor_positions$factor_display,
        autorange = "reversed",
        fixedrange = TRUE
      ),
      xaxis = list(
        title = "Group Rank",
        fixedrange = TRUE
      )
    )

  # Combine the two panels into one plot
  combined_plot <- plotly::subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # Add a title if requested
  if (showTitle) {
    title_text <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))
    combined_plot <- combined_plot %>%
      plotly::layout(
        title = list(text = title_text, x = 0.5)
      )
  }

  return(combined_plot)
}
