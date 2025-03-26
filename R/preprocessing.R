#' Preprocess Data for Multi-Factor Curve Analysis
#'
#' This function prepares the input dataset for a multi-factor curve analysis. It performs the following steps:
#' 1. Removes rows with missing values in the specified outcome variable and factor variables.
#' 2. Creates a "group" variable by concatenating the specified factor variables (if a group variable is not already provided).
#' 3. Calculates group-level summary statistics including the mean outcome, standard deviation, and number of observations.
#' 4. Ranks the groups in ascending order based on the mean outcome.
#' 5. Rounds numerical summary values to a specified number of decimal places for better visualization.
#' 6. Optionally ensures that the coordinate origin (0,0) is included in subsequent plots.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param outcome_var A character string indicating the outcome variable (e.g., "wage").
#' @param factors A character vector of factor variable names used to define groups (e.g., c("race", "south", "union")).
#' @param groupvar Optional. A character string indicating an existing group variable. If NULL (default), a new group variable is created by concatenating the factors.
#' @param rounding An integer specifying the number of decimal places to round computed summary statistics (default: 2).
#' @param plotOrigin Logical. If TRUE, ensures that (0,0) is included in plots (default: FALSE).
#'
#' @return A data frame containing the following columns:
#' \item{group}{A character vector that uniquely identifies each group (combination of factor levels).}
#' \item{mean_outcome}{The mean of the outcome variable for each group.}
#' \item{sd_outcome}{The standard deviation of the outcome variable for each group.}
#' \item{n}{The number of observations in each group.}
#' \item{rank}{The rank of each group based on the mean outcome (sorted in ascending order).}
#' \item{<factor variables>}{Each input factor as a separate column, extracted from the concatenated group identifier.}
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   wage = rnorm(1000, mean = 15, sd = 5),
#'   race = sample(c("White", "Black", "Other"), 1000, replace = TRUE),
#'   south = sample(c("Yes", "No"), 1000, replace = TRUE),
#'   union = sample(c("Yes", "No"), 1000, replace = TRUE)
#' )
#' preprocessed_data <- mfcurve_preprocessing(df, outcome_var = "wage",
#'                                            factors = c("race", "south", "union"))
#' }
#'
#' @export
mfcurve_preprocessing <- function(data, outcome_var, factors, groupvar = NULL, rounding = 2, plotOrigin = FALSE) {
  if (missing(data) || missing(outcome_var) || missing(factors)) {
    stop("Please provide data, outcome_var, and factors.")
  }

  outcome_var_sym <- rlang::sym(outcome_var)
  factor_syms <- rlang::syms(factors)

  data <- data %>%
    dplyr::filter(!is.na(!!outcome_var_sym), !dplyr::if_any(dplyr::all_of(factors), is.na))

  if (is.null(groupvar)) {
    data <- data %>%
      dplyr::mutate(group = do.call(paste, c(dplyr::across(dplyr::all_of(factors)), sep = "_")) %>% as.character())
  } else {
    data <- data %>%
      dplyr::rename(group = dplyr::all_of(groupvar))
  }

  group_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = stats::sd(!!outcome_var_sym, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mean_outcome) %>%
    dplyr::mutate(rank = 1:n())

  group_stats <- group_stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE) %>%
    dplyr::mutate(
      mean_outcome = round(mean_outcome, rounding),
      sd_outcome = round(sd_outcome, rounding)
    )


  return(group_stats)
}
