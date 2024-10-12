#' Preprocess Data for Multi-Factor Curve Analysis
#'
#' This function preprocesses data by filtering missing values, handling factor variables,
#' and creating groups for multi-factor curve analysis. It replicates the logic of the Stata
#' 'mfcurve' ado file, allowing for flexible grouping based on combinations of factors or a predefined group variable.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom rlang sym syms
#' @param data A data frame containing the dataset to be preprocessed.
#' @param outcome_var A character string specifying the name of the outcome variable (e.g., "wage").
#' @param factors A character vector of factor variable names to group by (e.g., c("race", "union")).
#' @param groupvar Optional. A character string specifying a predefined group variable. If NULL, groups are
#' created based on the combinations of the specified factors.
#' @return A data frame summarizing the mean, standard deviation, and count for each group.
#' @examples
#' # Sample data creation
#' data <- data.frame(
#'   outcome = rnorm(100),
#'   factor1 = factor(sample(letters[1:3], 100, replace = TRUE)),
#'   factor2 = factor(sample(letters[4:5], 100, replace = TRUE))
#' )
#'
#' # Preprocess data with interaction of factors
#' result <- mfcurve_preprocessing(data, outcome_var = "outcome", factors = c("factor1", "factor2"))
#' print(result)
#'
#' # Add a predefined group variable
#' data$groupvar <- sample(1:4, 100, replace = TRUE)
#'
#' # Preprocess data using the predefined group variable
#' result_groupvar <- mfcurve_preprocessing(data, outcome_var = "outcome", factors = c("factor1"), groupvar = "groupvar")
#' print(result_groupvar)
#' @export
mfcurve_preprocessing <- function(data, outcome_var, factors, groupvar = NULL) {

  # Ensure outcome_var and factors are in the data
  if (!outcome_var %in% colnames(data)) {
    stop("Outcome variable not found in the dataset.")
  }
  if (!all(factors %in% colnames(data))) {
    stop("One or more factor variables not found in the dataset.")
  }

  # Convert outcome_var to a symbol for dplyr operations
  outcome_var_sym <- rlang::sym(outcome_var)

  # Remove observations with missing values in the outcome or factor variables
  # This replicates the Stata 'drop if missing' commands
  data_clean <- data %>%
    dplyr::filter(!is.na(!!outcome_var_sym)) %>% # Drop missing in outcome
    dplyr::filter(!dplyr::if_any(dplyr::all_of(factors), is.na)) # Drop missing in factors

  # Create a group variable if not provided
  if (is.null(groupvar)) {
    # Replicating Stata's logic for factor combinations
    # Group by the interaction of factors, similar to how Stata creates artificial groups
    data_clean <- data_clean %>%
      dplyr::mutate(group = interaction(!!!rlang::syms(factors), drop = TRUE)) %>%
      dplyr::mutate(group = as.numeric(factor(group))) # Convert to numeric similar to Stata
  } else {
    # Ensure groupvar is present in the data
    if (!groupvar %in% colnames(data_clean)) {
      stop("Group variable not found in the dataset.")
    }
    # Use the provided group variable for grouping
    data_clean <- data_clean %>%
      dplyr::filter(!is.na(!!rlang::sym(groupvar))) %>% # Drop missing in groupvar
      dplyr::mutate(group = !!rlang::sym(groupvar))
  }

  # Summarize data: mean, standard deviation, and count for each group
  # Replicating the Stata 'bysort group: egen group_mean = mean(varlist)'
  data_summary <- data_clean %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = sd(!!outcome_var_sym, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )

  # Return the summarized data
  return(data_summary)
}
