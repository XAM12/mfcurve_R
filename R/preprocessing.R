#' Preprocess Data and Compute Group Statistics
#'
#' This function prepares data and computes descriptive statistics and t-tests
#' for groups defined by combinations of categorical factors.
#'
#' @param data A data frame containing the variables.
#' @param outcome A string indicating the name of the outcome variable (numeric).
#' @param factors A character vector of factor variable names to define groups.
#' @param alpha Significance level for the t-tests and confidence intervals. Default is 0.05.
#' @param test A string indicating the reference value for t-tests: "mean" (grand mean),
#' "leave-one-out" (mean of all other groups), or "zero".
#'
#' @return A list with:
#' \describe{
#'   \item{group_stats}{A data frame with computed statistics and CI bounds.}
#'   \item{group_stats_vis}{A visualization-ready version with rounded numbers.}
#'   \item{lower_data}{Data for the lower panel of the plot (without y positions).}
#'   \item{grand_mean}{The overall mean of the outcome variable.}
#'   \item{level}{The confidence level used.}
#' }
#' @export
mfcurve_preprocessing <- function(data, outcome, factors, alpha = 0.05, test = "mean") {
  if (!is.numeric(data[[outcome]])) {
    stop("The outcome variable must be numeric.")
  }
  if (!test %in% c("mean", "zero", "leave-one-out")) {
    stop('Argument "test" must be one of "mean", "zero", or "leave-one-out".')
  }

  vars <- c(outcome, factors)
  data <- tidyr::drop_na(data, tidyselect::all_of(vars))

  data <- dplyr::mutate(data, group = interaction(dplyr::across(tidyselect::all_of(factors)), sep = "_"))
  grand_mean <- mean(data[[outcome]])

  group_stats <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(
      mean_outcome = mean(.data[[outcome]]),
      sd_outcome = sd(.data[[outcome]]),
      n = dplyr::n(),
      .groups = "drop"
    )

  total_N <- nrow(data)
  total_sum <- sum(data[[outcome]])
  total_ss <- sum((data[[outcome]] - grand_mean)^2)

  if (test == "mean") {
    group_stats <- group_stats %>%
      dplyr::mutate(
        t_stat = (mean_outcome - grand_mean) / (sd_outcome / sqrt(n)),
        p_value = 2 * stats::pt(-abs(t_stat), df = n - 1),
        sig = p_value < alpha
      )
  } else if (test == "zero") {
    group_stats <- group_stats %>%
      dplyr::mutate(
        t_stat = mean_outcome / (sd_outcome / sqrt(n)),
        p_value = 2 * stats::pt(-abs(t_stat), df = n - 1),
        sig = p_value < alpha
      )
  } else if (test == "leave-one-out") {
    group_stats <- group_stats %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        others_n = total_N - n,
        group_sum = mean_outcome * n,
        others_mean = (total_sum - group_sum) / others_n,
        group_ss = (n - 1) * (sd_outcome^2),
        others_ss = total_ss - group_ss,
        others_sd = ifelse(others_n > 1, sqrt(others_ss / (others_n - 1)), NA_real_),
        se_diff = sqrt((sd_outcome^2 / n) + (others_sd^2 / others_n)),
        var1 = sd_outcome^2 / n,
        var2 = others_sd^2 / others_n,
        df_calc = (var1 + var2)^2 / ((var1^2 / (n - 1)) + (var2^2 / (others_n - 1))),
        t_stat = (mean_outcome - others_mean) / se_diff,
        p_value = 2 * stats::pt(-abs(t_stat), df = df_calc),
        sig = p_value < alpha
      ) %>%
      dplyr::ungroup()
  }

  group_stats <- group_stats %>%
    dplyr::mutate(test_type = test) %>%
    dplyr::arrange(mean_outcome) %>%
    dplyr::mutate(rank = dplyr::row_number())

  level <- 1 - alpha

  group_stats <- group_stats %>%
    dplyr::mutate(
      se = sd_outcome / sqrt(n),
      df = n - 1
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci_lower = if (!is.na(df) && df >= 1) {
        mean_outcome - stats::qt(1 - alpha / 2, df = df) * se
      } else {
        NA_real_
      },
      ci_upper = if (!is.na(df) && df >= 1) {
        mean_outcome + stats::qt(1 - alpha / 2, df = df) * se
      } else {
        NA_real_
      },
      ci_width = (ci_upper - ci_lower) / 2
    ) %>%
    dplyr::ungroup()

  group_stats_vis <- group_stats

  group_stats <- tidyr::separate(group_stats, group, into = factors, sep = "_")
  group_stats_vis <- tidyr::separate(group_stats_vis, group, into = factors, sep = "_")

  lower_data <- group_stats %>%
    dplyr::select(rank, tidyselect::all_of(factors)) %>%
    tidyr::pivot_longer(cols = -rank, names_to = "factor", values_to = "level") %>%
    dplyr::group_by(factor) %>%
    dplyr::mutate(level_code = as.numeric(factor(level))) %>%
    dplyr::ungroup()

  return(list(
    group_stats = group_stats,
    group_stats_vis = group_stats_vis,
    lower_data = lower_data,
    grand_mean = grand_mean,
    level = level
  ))
}
