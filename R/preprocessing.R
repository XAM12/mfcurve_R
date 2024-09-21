# File: R/preprocessing.R

#' Preprocess Data for mfcurve Analysis
#'
#' This function preprocesses the data by filtering missing values,
#' handling factors, and creating groups for analysis.
#'
#' @param data A data frame containing the dataset to be preprocessed.
#' @param outcome_var Character string specifying the outcome variable (e.g., "wage").
#' @param factors Character vector of factor variable names to group by (e.g., c("race", "union")).
#' @param groupvar Optional character string specifying a predefined group variable.
#' @return A data frame summarizing the mean, standard deviation, and count for each group.
#' @export
mfcurve_preprocessing <- function(data, outcome_var, factors, groupvar = NULL) {

  # Ensure outcome_var and factors are in the data
  if (!outcome_var %in% colnames(data)) {
    stop("Outcome variable not found in the dataset.")
  }
  if (!all(factors %in% colnames(data))) {
    stop("One or more factor variables not found in the dataset.")
  }

  # Convert outcome_var to a symbol
  outcome_var_sym <- rlang::sym(outcome_var)

  # Remove observations with missing values in outcome or factor variables
  data_clean <- data %>%
    dplyr::filter(!is.na(!!outcome_var_sym)) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(factors), is.na))

  # Create a group variable if not provided
  if (is.null(groupvar)) {
    # Group by interaction of factors
    data_clean <- data_clean %>%
      dplyr::mutate(group = interaction(!!!rlang::syms(factors), drop = TRUE))
  } else {
    # Ensure groupvar is present in the data
    if (!groupvar %in% colnames(data_clean)) {
      stop("Group variable not found in the dataset.")
    }
    # Use the provided group variable for grouping
    data_clean <- data_clean %>%
      dplyr::filter(!is.na(!!rlang::sym(groupvar))) %>%
      dplyr::mutate(group = !!rlang::sym(groupvar))
  }

  # Reduce data to unique factor combinations and summarize
  data_clean <- data_clean %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = sd(!!outcome_var_sym, na.rm = TRUE),
      n = dplyr::n(),
      .groups = 'drop' # Ensures ungrouped output after summarise
    )

  return(data_clean)
}
