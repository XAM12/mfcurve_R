#' mfcurve: Multi-Factor Curve Analysis for Grouped Data
#'
#' The mfcurve package provides tools for multi-factor curve analysis, including:
#' - Data preprocessing (`mfcurve_preprocessing`)
#' - Statistical testing (`mfcurve_stat_testing`)
#' - Interactive visualization (`mfcurve_plotting`)
#'
#' @docType package
#' @name mfcurve
#'
#' @import dplyr
#' @import magrittr
#' @import plotly
#' @import rlang
#' @import tidyr
#' @importFrom stats pt qt sd
NULL

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c(
  # mfcurve_preprocessing
  "group",

  # mfcurve_stat_testing
  "mean_outcome", "sd_outcome", "n", "t_value", "p_value", "ci_lower", "ci_upper",

  # mfcurve_plotting
  "factor", "level", "y"
))
