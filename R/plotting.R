#' Create an Interactive Plot for mfcurve Analysis (Mixed Binary/Multi-Level Coloring)
#'
#' This function generates an interactive plot for visualizing the results of an mfcurve analysis.
#' The plot consists of two panels: the upper panel displays group means with error bars, and the lower
#' panel maps group ranks to their factor-level combinations. The lower panel is fixed (not scrollable),
#' while the upper panel's y-axis scrollability can be optionally fixed.
#'
#' Coloring logic:
#' - If a factor is binary (exactly 2 distinct levels), all its levels share the same color.
#' - If a factor has more than 2 levels, each level gets its own color.
#'
#' Additionally:
#' - "expanded" mode appends the level name to the factor name in the lower panel (e.g., "race White").
#' - "collapsed" mode shows only the factor name (e.g., "race").
#' - Binary factors (e.g., "south", "union") show "Yes" as a filled circle and "No" as an open circle.
#' - color_scheme = "default" for a colored palette, "bw" for grayscale.
#'
#' @param stats A data frame containing the summarized group statistics and ranks, typically output from `mfcurve_stat_testing`.
#' @param factors A character vector specifying the factor variables used in the analysis.
#' @param outcome A string specifying the outcome variable for the analysis (e.g., "wage").
#' @param alpha Significance level for confidence intervals (default is 0.05).
#' @param showTitle Logical; whether to display a title on the plot (default is TRUE).
#' @param mode Character; controls the mode of the lower plot ("collapsed" or "expanded"). Default is "collapsed".
#' @param upper_fixed_range Logical; if TRUE the y-axis in the upper panel is fixed (no scroll/drag), if FALSE it remains scrollable. Default is FALSE.
#' @param color_scheme Character; determines the color scheme for the lower panel markers.
#'        Options: "default" for colored mode, "bw" for black-and-white (grayscale) mode. Default is "default".
#' @return A `plotly` object containing the combined plot.
#'
#' @importFrom plotly plot_ly subplot layout add_trace
#' @importFrom tidyr gather separate
#' @importFrom dplyr select mutate group_by ungroup left_join distinct n_distinct
#' @import RColorBrewer
#' @examples
#' # Example usage:
#' # plot_obj <- mfcurve_plotting(
#' #   stat_test_results,
#' #   factors = c("race", "south", "union"),
#' #   outcome = "wage",
#' #   mode = "expanded",
#' #   upper_fixed_range = TRUE,
#' #   color_scheme = "default"
#' # )
#' # print(plot_obj)
#'
#' @export
mfcurve_plotting <- function(stats,
                             factors,
                             outcome,
                             alpha = 0.05,
                             showTitle = TRUE,
                             mode = "collapsed",
                             upper_fixed_range = FALSE,
                             color_scheme = "default") {
  require(plotly)
  require(tidyr)
  require(dplyr)
  require(RColorBrewer)

  # -----------------------------
  # 1) Input-Checks
  # -----------------------------
  required_cols <- c("rank", "mean_outcome", "ci_lower", "ci_upper", "group")
  if (!all(required_cols %in% names(stats))) {
    stop("The input data must include the columns: rank, mean_outcome, ci_lower, ci_upper, and group.")
  }

  # Grand Mean
  grand_mean <- mean(stats$mean_outcome, na.rm = TRUE)

  # Zerlege die Gruppenvariable in die einzelnen Faktoren
  stats <- stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  # -----------------------------
  # 2) Ermittle, welche Faktoren binär sind
  # -----------------------------
  factor_level_info <- lapply(factors, function(fct) {
    n_lvls <- dplyr::n_distinct(stats[[fct]])
    data.frame(orig_factor = fct, n_levels = n_lvls)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(is_binary = (n_levels == 2))

  # -----------------------------
  # 3) "Long" Format für das untere Panel
  # -----------------------------
  lower_data <- stats %>%
    dplyr::select(rank, dplyr::all_of(factors)) %>%
    tidyr::gather(key = "orig_factor", value = "orig_level", -rank) %>%
    dplyr::left_join(factor_level_info, by = "orig_factor")

  # -----------------------------
  # 4) Marker-Symbole (Yes/No)
  # -----------------------------
  lower_data <- lower_data %>%
    dplyr::mutate(
      symbol = dplyr::case_when(
        is_binary & orig_level == "Yes" ~ "circle",
        is_binary & orig_level == "No"  ~ "circle-open",
        TRUE ~ "circle"
      )
    )

  # -----------------------------
  # 5) "expanded" vs. "collapsed" => Label
  # -----------------------------
  if (mode == "expanded") {
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = paste(orig_factor, orig_level, sep = " ")) %>%
      dplyr::distinct()
  } else {
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = orig_factor)
  }

  # -----------------------------
  # 6) Farb-Logik
  # -----------------------------
  lower_data <- lower_data %>%
    dplyr::mutate(
      color_key = dplyr::if_else(
        is_binary,
        true = orig_factor,
        false = paste(orig_factor, orig_level, sep = "_")
      )
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

  # -----------------------------
  # 7) y-Position für das untere Panel
  # -----------------------------
  factor_labels <- unique(lower_data$factor_display)
  factor_positions <- data.frame(
    factor_display = factor_labels,
    y = seq(length(factor_labels), 1)
  )
  lower_data <- lower_data %>%
    dplyr::left_join(factor_positions, by = "factor_display")

  # -----------------------------
  # 8) Oberes Panel: Gruppenmittelwerte
  # -----------------------------
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
    )

  # -----------------------------
  # 9) Unteres Panel
  # -----------------------------
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
        fixedrange = TRUE  # X-Achse im unteren Panel fixieren
      )
    )

  # -----------------------------
  # 10) Zusammenführen der Panels
  # -----------------------------
  combined_plot <- plotly::subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # -----------------------------
  # 11) Titel und Layout
  # -----------------------------
  if (showTitle) {
    title_text <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))
    combined_plot <- combined_plot %>%
      plotly::layout(
        title = list(text = title_text, x = 0.5),
        yaxis = list(title = outcome, fixedrange = upper_fixed_range),
        yaxis2 = list(title = "Factors", fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE),
        xaxis2 = list(fixedrange = TRUE)
      )
  } else {
    combined_plot <- combined_plot %>%
      plotly::layout(
        yaxis = list(title = outcome, fixedrange = upper_fixed_range),
        yaxis2 = list(title = "Factors", fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE),
        xaxis2 = list(fixedrange = TRUE)
      )
  }

  return(combined_plot)
}
