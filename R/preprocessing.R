#' Preprocess Data for Multi-Factor Curve Analysis
#'
#' This function preprocesses data by filtering missing values, creating group variables,
#' calculating summary statistics, and ranking groups. It replicates the preprocessing logic
#' used by Max and the Stata 'mfcurve' ado file.
#'
#' @param data A data frame containing the dataset to be preprocessed.
#' @param outcome_var A character string specifying the name of the outcome variable (e.g., "wage").
#' @param factors A character vector of factor variable names to group by (e.g., c("factor_1", "factor_2")).
#' @param groupvar Optional. A character string specifying a predefined group variable. If NULL, groups are
#'        created based on the combinations of the specified factors.
#' @return A data frame summarizing the mean, standard deviation, and count for each group,
#'         along with ranks and split factor variables.
#' @importFrom dplyr filter mutate group_by summarize ungroup arrange across
#' @importFrom tidyr separate
#' @importFrom rlang sym syms
#' @importFrom magrittr %>%
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
        group = do.call(paste, c(dplyr::across(all_of(factors)), sep = "_")) %>% as.character() # Ensure character labels
      )
  } else {
    data <- data %>%
      dplyr::rename(group = all_of(groupvar))
  }

  # Group and summarize data
  group_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(!!outcome_var_sym, na.rm = TRUE),
      sd_outcome = sd(!!outcome_var_sym, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Rank groups by mean outcome
  group_stats <- group_stats %>%
    dplyr::arrange(mean_outcome) %>%
    dplyr::mutate(rank = dplyr::row_number())

  # Separate group variable into individual factors
  group_stats <- group_stats %>%
    tidyr::separate(group, into = factors, sep = "_", remove = FALSE)

  return(group_stats)
}
