#' Wrapper for mfcurve preprocessing and plotting
#'
#' Calls \code{mfcurve_preprocessing()} and \code{mfcurve_plotting()} in sequence
#' to generate a two-panel interactive mfcurve plot.
#'
#' @param data              A data frame containing the variables.
#' @param outcome           Name of the numeric outcome variable (string).
#' @param factors           Character vector of factor variable names for grouping.
#' @param test              Reference for t-tests: "mean", "zero", or "leave-one-out". Passed to preprocessing. Default is "mean".
#' @param alpha             Significance level for t-tests and confidence intervals. Default is 0.05.
#' @param showTitle         Logical. Show the plot title? Default is TRUE.
#' @param SaveProcessedData Logical. Save group-level statistics to the global environment? Default is FALSE.
#' @param mode              Factor labeling mode: "collapsed" (default) or "expanded".
#' @param rounding          Number of digits to round outcome statistics. Default is 2.
#' @param plotOrigin        Logical. Force axes to include 0? Default is FALSE.
#' @param CI                Logical. Display confidence intervals? Default is TRUE.
#' @param showGrandMean     Logical. Show the grand mean line? Default is TRUE.
#' @param showSigStars      Logical. Show markers for significant values? Default is TRUE.
#'
#' @return Invisibly returns the plotly object representing the two-panel plot.
#' If \code{SaveProcessedData = TRUE}, also saves the group statistics as \code{group_stats} in the global environment.
#'
#' @details
#' \code{mfcurve()} plots the mean of an outcome variable across all combinations of multiple grouping factors, producing a two-panel interactive plot.
#'
#' The upper panel shows group means (and confidence intervals, if requested); the lower panel marks which factor levels are present in each group.
#' In the lower panel, factor labels can be displayed in two modes:
#'
#' - In \strong{collapsed} mode, each factor occupies only one row. Factor levels are differentiated by marker color.
#'
#' - In \strong{expanded} mode, each factor is split into its levels (dummy-coded), with levels listed below each other.
#'   Markers indicate whether a specific factor level is present or absent in the group.
#'
#' While collapsed mode saves space when many factors or levels are present,
#' expanded mode may be more intuitive (especially for readers familiar with specification curves).
#'
#' \code{mfcurve()} allows optional significance testing (t-tests). Group-level statistics can be saved if needed.
#'
#' @seealso \code{\link{mfcurve_preprocessing}}, \code{\link{mfcurve_plotting}}
#'
#' @examples
#' # Simulate data for a 3 x 2 experimental design: 3 treatments (A, B, C), 2 doses (low, high)
#' set.seed(123)
#' df <- data.frame(
#'   treatment = sample(c("A", "B", "C"), 1000, replace = TRUE),
#'   dose      = sample(c("low", "high"), 1000, replace = TRUE)
#' )
#'
#' # Generate self-rated health (scale 1–10) with small group differences
#' df$self_rated_health <- 6 +
#'   ifelse(df$treatment == "B", 0.5, ifelse(df$treatment == "C", -0.5, 0)) +
#'   ifelse(df$dose == "high", 0.3, 0) +
#'   rnorm(1000, 0, 1.5)
#'
#' # Restrict health scores to valid range
#' df$self_rated_health <- pmin(pmax(df$self_rated_health, 1), 10)
#'
#' # Create mfcurve plot
#' mfcurve(
#'   data = df,
#'   outcome = "self_rated_health",
#'   factors = c("treatment", "dose"),
#'   test = "mean"
#' )
#'
#' @importFrom magrittr %>%
#'
#' @export

mfcurve <- function(data, outcome, factors, test = "mean", alpha = 0.05, showTitle = TRUE,
                    SaveProcessedData = FALSE, mode = "collapsed", rounding = 2,
                    plotOrigin = FALSE, CI = TRUE, showGrandMean = TRUE, showSigStars = TRUE) {

  # Run preprocessing (pass test argument)
  results <- mfcurve_preprocessing(
    data = data,
    outcome = outcome,
    factors = factors,
    alpha = alpha,
    test = test
  )

  # Optionally save to global environment
  if (SaveProcessedData) {
    assign("group_stats", results$group_stats, envir = .GlobalEnv)
    message("Saved 'group_stats' to the global environment.")
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
    mode = mode,
    showGrandMean = showGrandMean,
    showSigStars = showSigStars
  )
}
