# File: R/plotting.R

#' Plot Results of mfcurve Analysis
#'
#' This function creates a plot of group means with confidence intervals,
#' highlighting significant differences, replicating the graph logic from the Stata `mfcurve` ado.
#'
#' @param data A data frame containing the results of the statistical tests.
#' @param show_mean_line Logical; if TRUE, adds a line showing the overall mean.
#' @param title The title of the plot.
#' @return A ggplot object visualizing the group means and confidence intervals.
#' @export
mfcurve_plot <- function(data, show_mean_line = TRUE, title = "mfcurve Analysis Results") {

  # Ensure required columns are present
  if (!all(c("group", "mean_outcome", "ci_lower", "ci_upper", "significant") %in% colnames(data))) {
    stop("Data must contain 'group', 'mean_outcome', 'ci_lower', 'ci_upper', and 'significant' columns.")
  }

  # Calculate overall mean for optional mean line
  overall_mean <- mean(data$mean_outcome, na.rm = TRUE)

  # Create the base plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(group, mean_outcome), y = mean_outcome)) +
    ggplot2::geom_point(ggplot2::aes(color = significant, shape = significant), size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper, color = significant), width = 0.2) +
    ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1)) +
    ggplot2::labs(title = title, x = "Group", y = "Mean Outcome") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Optionally add overall mean line
  if (show_mean_line) {
    plot <- plot + ggplot2::geom_hline(yintercept = overall_mean, linetype = "dashed", color = "blue")
  }

  return(plot)
}
