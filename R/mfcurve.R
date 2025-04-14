#' Wrapper for mfcurve preprocessing and plotting
#'
#' Calls \code{mfcurve_preprocessing()} and \code{mfcurve_plotting()} in sequence
#' to create the full two-panel interactive plot.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string indicating the name of the outcome variable (numeric).
#' @param factors A character vector of factor variable names to define groups.
#' @param test A placeholder argument for future testing types (currently unused).
#' @param alpha Significance level for t-tests and confidence intervals. Default is 0.05.
#' @param showTitle Logical. Should the title be shown in the plot? Default is TRUE.
#' @param SaveProcessedData Logical. If TRUE, saves \code{group_stats} to the global environment. Default is FALSE.
#' @param mode Either "collapsed" (default) or "expanded". Controls factor labeling in lower panel.
#' @param rounding Number of digits to round the outcome statistics in the plot. Default is 2.
#' @param plotOrigin Logical. If TRUE, axes will include origin (0). Default is FALSE.
#' @param CI Logical. Whether to display confidence intervals. Default is TRUE.
#'
#' @return Invisibly returns the plotly object.
#' @export
mfcurve <- function(data, outcome, factors, test = "mean", alpha = 0.05, showTitle = TRUE,
                    SaveProcessedData = FALSE, mode = "collapsed", rounding = 2,
                    plotOrigin = FALSE, CI = TRUE) {

  # Run preprocessing
  results <- mfcurve_preprocessing(data = data, outcome = outcome, factors = factors, alpha = alpha)

  # Optionally save to global environment
  if (SaveProcessedData) {
    assign("group_stats", results$group_stats, envir = .GlobalEnv)
  }

  # Plot
  mfcurve_plotting(
    group_stats_vis = results$group_stats_vis,
    lower_data = results$lower_data,
    grand_mean = results$grand_mean,
    outcome = outcome,
    factors = factors,
    level = results$level,
    rounding = rounding,
    showTitle = showTitle,
    plotOrigin = plotOrigin,
    CI = CI,
    mode = mode
  )
}
