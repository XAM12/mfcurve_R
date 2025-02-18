#' Create an Interactive Plot for mfcurve Analysis
#'
#' This function generates an interactive plot for visualizing the results of an mfcurve analysis.
#' The plot consists of two panels: the upper panel displays group means with error bars, and the lower
#' panel maps group ranks to their factor-level combinations. The lower panel is fixed (not scrollable),
#' while the upper panel's y-axis scrollability can be optionally fixed.
#'
#' It supports two modes:
#' - "collapsed": Each factor is labeled only by its name, but each factor-level combination gets its own color.
#' - "expanded": Each factor is combined with its level in the label (e.g. "race White").
#'
#' It also supports a black-and-white mode for simpler (Stata-ähnliche) Darstellungen.
#' Binary factors (e.g. "south", "union") are marked with a full circle ("circle") if "Yes" and an open circle ("circle-open") if "No".
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
#' @importFrom plotly plot_ly subplot layout add_trace
#' @importFrom tidyr gather separate
#' @importFrom dplyr select mutate group_by ungroup left_join distinct
#' @import RColorBrewer
#' @examples
#' # Example usage:
#' # plot_obj <- mfcurve_plotting(stat_test_results,
#' #                   factors = c("race", "south", "union"),
#' #                   outcome = "wage",
#' #                   mode = "expanded",
#' #                   upper_fixed_range = TRUE,
#' #                   color_scheme = "bw")
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

  # ---------------------------------------
  # 1) Grundlegende Checks & Vorbereitung
  # ---------------------------------------
  required_cols <- c("rank", "mean_outcome", "ci_lower", "ci_upper", "group")
  if (!all(required_cols %in% names(stats))) {
    stop("The input data must include the columns: rank, mean_outcome, ci_lower, ci_upper, and group.")
  }

  grand_mean <- mean(stats$mean_outcome, na.rm = TRUE)

  # Zerlege "group" in die einzelnen Faktoren
  stats <- stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  # Erstelle "lower_data", in dem wir rank und alle Faktoren "aufschmelzen"
  lower_data <- stats %>%
    dplyr::select(rank, dplyr::all_of(factors)) %>%
    tidyr::gather(key = "orig_factor", value = "orig_level", -rank)

  # ---------------------------------------
  # 2) "expanded" vs. "collapsed"
  #    => factor_display Spalte erstellen
  # ---------------------------------------
  if (mode == "expanded") {
    # Erzeuge z. B. "race White" anstelle von "race" + "White"
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = paste(orig_factor, orig_level, sep = " ")) %>%
      dplyr::distinct()
  } else {
    # Lasse es bei nur dem Factor-Namen
    lower_data <- lower_data %>%
      dplyr::mutate(factor_display = orig_factor)
  }

  # ---------------------------------------
  # 3) Symbol-Logik für binäre Faktoren
  # ---------------------------------------
  # Hier identifizieren wir "Yes"/"No" bei "south" / "union"
  lower_data <- lower_data %>%
    dplyr::mutate(
      symbol = dplyr::case_when(
        # z. B. orig_factor == "south" & orig_level == "Yes"
        # => "circle" (ausgefüllt)
        orig_factor %in% c("south", "union") & orig_level == "Yes" ~ "circle",
        orig_factor %in% c("south", "union") & orig_level == "No"  ~ "circle-open",
        TRUE ~ "circle"
      )
    )

  # ---------------------------------------
  # 4) Farb-Logik
  # ---------------------------------------
  # "expanded": Jede (orig_factor, orig_level) => factor_display bekommt EINE Farbe
  # "collapsed": Ebenfalls, nur die ID ist etwas anders
  # Wir können ein "key" definieren, das wir zur Unterscheidung nutzen
  # - expanded: key = factor_display
  # - collapsed: key = paste(orig_factor, orig_level, sep = "_")
  #   (bzw. factor_display, wenn wir es 1:1 abbilden wollen)
  if (mode == "expanded") {
    color_key_col <- "factor_display"
  } else {
    # => Wir nutzen orig_factor + orig_level als ID
    color_key_col <- "factor_key"
    lower_data <- lower_data %>%
      dplyr::mutate(factor_key = paste(orig_factor, orig_level, sep = "_"))
  }

  # Falls es "expanded" ist, existiert factor_display (z. B. "race White")
  # => wir können color_key_col = "factor_display" nutzen
  # Falls "collapsed", existiert factor_key (z. B. "race_White")

  # Erzeuge Vektor aller unique-Keys
  unique_keys <- unique(lower_data[[color_key_col]])
  n_colors <- length(unique_keys)

  # Bestimme Palette
  if (color_scheme == "default") {
    palette <- RColorBrewer::brewer.pal(n = max(n_colors, 3), name = "Set1")[1:n_colors]
  } else if (color_scheme == "bw") {
    palette <- gray.colors(n_colors, start = 0, end = 0.7)
  } else {
    stop("Invalid color_scheme specified. Use 'default' or 'bw'.")
  }

  # Mapping Key -> Farbe
  color_map <- setNames(palette, unique_keys)

  # final_color Spalte
  lower_data <- lower_data %>%
    dplyr::mutate(final_color = color_map[ .data[[color_key_col]] ])

  # ---------------------------------------
  # 5) y-Position im unteren Panel
  # ---------------------------------------
  # => Wir nutzen factor_display als Achsen-Label
  factor_labels <- unique(lower_data$factor_display)
  factor_positions <- data.frame(
    factor_display = factor_labels,
    y = seq(length(factor_labels), 1)
  )

  lower_data <- lower_data %>%
    dplyr::left_join(factor_positions, by = "factor_display")

  # ---------------------------------------
  # 6) Oberes Panel (Group Means + Fehlerbalken + Grand Mean)
  # ---------------------------------------
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

  # ---------------------------------------
  # 7) Unteres Panel
  # ---------------------------------------
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
      line = list(width = 2, color = ~final_color)  # offener Kreis mit Rand in gleicher Farbe
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
      xaxis = list(title = "Group Rank")
    )

  # ---------------------------------------
  # 8) Zusammenfügen beider Panels
  # ---------------------------------------
  combined_plot <- plotly::subplot(
    upper_plot,
    lower_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.7, 0.3)
  )

  # ---------------------------------------
  # 9) Titel und Layout
  # ---------------------------------------
  if (showTitle) {
    title_text <- paste("Mean", outcome, "by the combination of", paste(factors, collapse = " / "))
    combined_plot <- combined_plot %>%
      plotly::layout(
        title = list(text = title_text, x = 0.5),
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
