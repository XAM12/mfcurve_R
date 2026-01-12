#' Create a two-panel mfcurve plot from processed statistics
#'
#' Generates an interactive two-panel plot showing group means (with optional confidence intervals)
#' and corresponding factor combinations.
#'
#' @param group_stats_vis Data frame containing group-level summary statistics.
#' @param lower_data      Data frame defining the factor structure for the lower panel.
#' @param grand_mean      Numeric. The overall mean of the outcome variable.
#' @param outcome         Name of the outcome variable (string).
#' @param factors         Character vector of factor variable names.
#' @param level           Level for confidence intervals (e.g., 0.95).
#' @param rounding        Number of digits to round outcome values. Default is 2.
#' @param showTitle       Logical. Show the plot title? Default is TRUE.
#' @param plotOrigin      Logical. Force axes to include 0? Default is FALSE.
#' @param CI              Logical. Display confidence intervals? Default is TRUE.
#' @param mode            Labeling mode for the lower panel: "collapsed" (default) or "expanded".
#' @param showGrandMean   Logical. Show the grand mean overall groups. Default is TRUE.
#' @param showSigStars    Logical. Flag significant results. Default is TRUE.
#'
#' @return A plotly object (invisible).
#' @export

mfcurve_plotting <- function(group_stats_vis, lower_data, grand_mean,
                             outcome, factors, level,
                             rounding = 2, showTitle = TRUE,
                             plotOrigin = FALSE, CI = TRUE,
                             mode = "collapsed",
                             showGrandMean = TRUE,
                             showSigStars = TRUE) {

  if (mode == "expanded") {
    lower_data <- lower_data %>%
      dplyr::mutate(factor = paste(factor, level, sep = " ")) %>%
      dplyr::mutate(
        factor_var = sub("^(.*?) .*", "\\1", factor),
        factor_lbl = factor
      ) %>%
      dplyr::group_by(factor_var) %>%
      dplyr::arrange(factor_var, factor_lbl, .by_group = TRUE) %>%
      dplyr::ungroup()

    factor_blocks <- split(lower_data, lower_data$factor_var)
    y_pos <- c()
    factor_order <- c()
    spacing <- 0.6
    current_y <- 0
    for (block in factor_blocks) {
      n_levels <- length(unique(block$factor_lbl))
      block_y <- current_y + seq(n_levels, 1)
      y_pos <- c(y_pos, block_y)
      factor_order <- c(factor_order, unique(block$factor_lbl))
      current_y <- max(block_y) + spacing
    }

    factor_positions <- data.frame(factor = factor_order, y = y_pos)
    lower_data$factor <- lower_data$factor_lbl
    lower_data <- dplyr::select(lower_data, -factor_var, -factor_lbl)
    lower_data <- lower_data %>%
      dplyr::mutate(factor_group = sub("^(.*?) .*", "\\1", factor)) %>%
      dplyr::mutate(level_code = as.numeric(factor(factor_group)))

  } else {
    factor_levels <- unique(lower_data$factor)
    factor_positions <- data.frame(
      factor = factor_levels,
      y = seq(length(factor_levels), 1)
    )

    lower_data <- lower_data %>%
      dplyr::mutate(factor_level_combo = paste(factor, level, sep = ":")) %>%
      dplyr::mutate(level_code = as.numeric(factor(factor_level_combo)))
  }

  lower_data <- dplyr::left_join(lower_data, factor_positions, by = "factor")

  group_stats_vis <- dplyr::mutate(group_stats_vis,
                                   mean_outcome_vis = round(mean_outcome, rounding),
                                   sd_outcome_vis = round(sd_outcome, rounding),
                                   ci_lower_vis = round(ci_lower, rounding),
                                   ci_upper_vis = round(ci_upper, rounding),
                                   ci_width_vis = round(ci_width, rounding)
  )

  x_min <- min(group_stats_vis$rank) - 0.5
  x_max <- max(group_stats_vis$rank) + 0.5
  y_min <- min(group_stats_vis$ci_lower_vis)
  y_max <- max(group_stats_vis$ci_upper_vis)

  offset <- 0.08 * (y_max - y_min)
  y_max <- y_max + offset + 0.05 * (y_max - y_min)

  if (plotOrigin) {
    x_min <- min(x_min, 0)
    y_min <- min(y_min, 0)
  }

  upper_plot <- plotly::plot_ly(data = group_stats_vis)

  if (CI) {
    upper_plot <- upper_plot %>%
      plotly::add_trace(
        x = ~rank,
        y = ~mean_outcome_vis,
        error_y = list(
          type = 'data',
          symmetric = FALSE,
          array = ~ci_upper_vis - mean_outcome_vis,
          arrayminus = ~mean_outcome_vis - ci_lower_vis
        ),
        type = 'scatter',
        mode = 'markers',
        marker = list(symbol = ~ifelse(sig, 'diamond', 'circle')),
        text = ~paste0(
          "Mean: ", mean_outcome_vis, "<br>",
          "SD: ", sd_outcome_vis, "<br>",
          round(level * 100), "%-CI: [", ci_lower_vis, ", ", ci_upper_vis, "]<br>",
          "Group size: ", n
        ),
        hoverinfo = 'text',
        name = 'Group Means'
      )
  } else {
    upper_plot <- upper_plot %>%
      plotly::add_trace(
        x = ~rank,
        y = ~mean_outcome_vis,
        type = 'scatter',
        mode = 'markers',
        marker = list(symbol = ~ifelse(sig, 'diamond', 'circle')),
        text = ~paste0(
          "Mean: ", mean_outcome_vis, "<br>",
          "SD: ", sd_outcome_vis, "<br>",
          "Group size: ", n
        ),
        hoverinfo = 'text',
        name = 'Group Means'
      )
  }

  # Grand Mean zuerst, dann Significant values
    if (showGrandMean) {
      upper_plot <- upper_plot %>%
        plotly::add_trace(
          x = c(x_min, x_max),
          y = c(grand_mean, grand_mean),
          type = 'scatter',
          mode = 'lines',
          line = list(dash = 'dash'),
          name = 'Grand Mean'
        )
    }
    if (showSigStars) {
      upper_plot <- upper_plot %>%
        plotly::add_trace(
          data = dplyr::filter(group_stats_vis, sig),
          x = ~rank,
          y = ~ci_upper_vis + offset,
          mode = 'markers',
          type = 'scatter',
          marker = list(symbol = "star-dot", size = 8, color = "black"),
          name = "Significant values",
          hoverinfo = 'skip',
          showlegend = TRUE
        )
    }
  upper_plot <- upper_plot %>%
    plotly::layout(
      xaxis = list(range = c(x_min, x_max), fixedrange = TRUE),
      yaxis = list(range = c(y_min, y_max), fixedrange = TRUE)
    )

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
        fixedrange = TRUE
      ),
      xaxis = list(
        title = "Group Rank",
        range = c(x_min, x_max),
        fixedrange = TRUE
      )
    )

  title <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))
  combined <- plotly::subplot(upper_plot, lower_plot, nrows = 2, shareX = TRUE, heights = c(0.7, 0.3))

  if (showTitle) {
    combined <- combined %>%
      plotly::layout(
        title = list(text = title, x = 0.5),
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors")
      )
  } else {
    combined <- combined %>%
      plotly::layout(
        yaxis = list(title = outcome),
        yaxis2 = list(title = "Factors")
      )
  }

  print(combined)
  invisible(combined)
}
