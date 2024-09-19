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

  # Remove observations with missing values in outcome or factor variables
  data_clean <- data %>%
    dplyr::filter(!is.na(rlang::sym(outcome_var))) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(factors), is.na))

  # Create a group variable if not provided
  if (is.null(groupvar)) {
    data_clean <- data_clean %>%
      dplyr::mutate(group = interaction(!!!rlang::syms(factors), drop = TRUE))
  } else {
    data_clean <- data_clean %>%
      dplyr::filter(!is.na(rlang::sym(groupvar))) %>%
      dplyr::mutate(group = rlang::sym(groupvar))
  }

  # Reduce data to unique factor combinations
  data_clean <- data_clean %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      mean_outcome = mean(!!rlang::sym(outcome_var), na.rm = TRUE),
      sd_outcome = sd(!!rlang::sym(outcome_var), na.rm = TRUE),
      n = dplyr::n()
    ) %>%
    dplyr::ungroup()

  return(data_clean)
}
