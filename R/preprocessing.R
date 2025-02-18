#' Preprocess Data for Multi-Factor Curve Analysis
#'
#' This function prepares the dataset for multi-factor curve analysis by:
#' - Filtering out rows with missing values in the specified outcome and factor variables.
#' - Creating a "group" variable based on the combinations of factor levels (if not already defined).
#' - Calculating group-level summary statistics (mean, standard deviation, and count).
#' - Ranking groups based on the mean outcome.
#' - Splitting the "group" variable back into its constituent factor-level combinations for easier interpretation.
#'
#' @param data A data frame containing the dataset to be preprocessed.
#' @param outcome_var A character string specifying the name of the outcome variable (e.g., "wage").
#' @param factors A character vector specifying the factor variables used to define groups (e.g., `c("factor1", "factor2")`).
#' @param groupvar Optional. A character string specifying a predefined group variable. If `NULL`, the group variable is created by combining the specified factors.
#' @return A data frame summarizing the following for each group:
#' - `group`: The group identifier, combining levels of the factor variables.
#' - `mean_outcome`: The mean value of the outcome variable for the group.
#' - `sd_outcome`: The standard deviation of the outcome variable for the group.
#' - `n`: The number of observations in the group.
#' - `rank`: The rank of the group based on the mean outcome (ascending order).
#' - Factor columns: The individual factor-level combinations derived from the "group" variable.
#' @examples
#' data <- data.frame(
#'   wage = c(10, 15, 20, 12, 18, 22),
#'   factor1 = c("A", "A", "B", "A", "B", "B"),
#'   factor2 = c("X", "Y", "X", "X", "Y", "X"),
#'   stringsAsFactors = FALSE
#' )
#' mfcurve_preprocessing(data, outcome_var = "wage", factors = c("factor1", "factor2"))
#' @export
mfcurve_preprocessing <- function(data, outcome_var, factors, groupvar = NULL) {
  # Validate inputs
  if (missing(data) || missing(outcome_var) || missing(factors)) {
    stop("Please provide data, outcome_var, and factors.")
  }

  # Convert outcome and factors to symbols for tidy evaluation
  outcome_var_sym <- rlang::sym(outcome_var)
  factor_syms <- rlang::syms(factors)

  # Filter missing values
  data <- data %>%
    dplyr::filter(
      !is.na(!!outcome_var_sym),
      !dplyr::if_any(dplyr::all_of(factors), is.na)
    )

  # Create group variable if groupvar is not provided
  if (is.null(groupvar)) {
    data <- data %>%
      dplyr::mutate(
        group = do.call(paste, c(dplyr::across(dplyr::all_of(factors)), sep = "_")) %>% as.character()
      )
  } else {
    data <- data %>%
      dplyr::rename(group = dplyr::all_of(groupvar))
  }

  # Group and summarize data
  group_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = stats::sd(!!outcome_var_sym, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Rank groups by mean outcome
  group_stats <- group_stats %>%
    dplyr::arrange(mean_outcome) %>%
    dplyr::mutate(rank = 1:n())

  # Separate group variable into individual factors
  group_stats <- group_stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  return(group_stats)
}
