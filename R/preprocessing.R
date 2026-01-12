#' Preprocess data and compute group statistics
#'
#' Prepares the data and computes descriptive statistics and t-tests
#' for groups defined by combinations of categorical factors.
#'
#' @param data    Data frame containing the variables.
#' @param outcome Name of the numeric outcome variable (string).
#' @param factors Character vector of factor variable names for grouping.
#' @param alpha   Significance level for the t-tests and confidence intervals. Default is 0.05.
#' @param test    Reference for t-tests: "mean" (grand mean), "leave-one-out" (mean of all other groups), or "zero" (testing against 0).
#'
#' @return A list with:
#' \describe{
#'   \item{group_stats}{Data frame with computed statistics and CI bounds.}
#'   \item{group_stats_vis}{Visualization-ready version with rounded values.}
#'   \item{lower_data}{Data for the lower panel (without y positions).}
#'   \item{grand_mean}{Overall mean of the outcome variable.}
#'   \item{level}{Confidence level used.}
#' }
#' @importFrom rlang .data
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

  data <- dplyr::mutate(
    data,
    group = interaction(dplyr::across(tidyselect::all_of(factors)), sep = "_")
  )
  grand_mean <- mean(data[[outcome]])

  group_stats <- data %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(
      mean_outcome = mean(.data[[outcome]]),
      sd_outcome   = stats::sd(.data[[outcome]]),
      n            = dplyr::n(),
      .groups      = "drop"
    )

  total_N   <- nrow(data)
  total_sum <- sum(data[[outcome]])
  total_ss  <- sum((data[[outcome]] - grand_mean)^2)

  if (test == "mean") {
    group_stats <- group_stats %>%
      dplyr::mutate(
        t_stat  = (.data$mean_outcome - grand_mean) / (.data$sd_outcome / sqrt(.data$n)),
        p_value = 2 * stats::pt(-abs(.data$t_stat), df = .data$n - 1),
        sig     = .data$p_value < alpha
      )
  } else if (test == "zero") {
    group_stats <- group_stats %>%
      dplyr::mutate(
        t_stat  = .data$mean_outcome / (.data$sd_outcome / sqrt(.data$n)),
        p_value = 2 * stats::pt(-abs(.data$t_stat), df = .data$n - 1),
        sig     = .data$p_value < alpha
      )
  } else if (test == "leave-one-out") {
    group_stats <- group_stats %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        others_n   = total_N - .data$n,
        group_sum  = .data$mean_outcome * .data$n,
        others_mean = (total_sum - .data$group_sum) / .data$others_n,
        group_ss   = (.data$n - 1) * (.data$sd_outcome^2),
        others_ss  = total_ss - .data$group_ss,
        others_sd  = ifelse(.data$others_n > 1, sqrt(.data$others_ss / (.data$others_n - 1)), NA_real_),
        se_diff    = sqrt((.data$sd_outcome^2 / .data$n) + (others_sd^2 / .data$others_n)),
        var1       = .data$sd_outcome^2 / .data$n,
        var2       = others_sd^2 / .data$others_n,
        df_calc    = (.data$var1 + .data$var2)^2 / ((.data$var1^2 / (.data$n - 1)) + (.data$var2^2 / (.data$others_n - 1))),
        t_stat     = (.data$mean_outcome - .data$others_mean) / .data$se_diff,
        p_value    = 2 * stats::pt(-abs(.data$t_stat), df = .data$df_calc),
        sig        = .data$p_value < alpha
      ) %>%
      dplyr::ungroup()
  }

  group_stats <- group_stats %>%
    dplyr::mutate(test_type = test) %>%
    dplyr::arrange(.data$mean_outcome) %>%
    dplyr::mutate(rank = dplyr::row_number())

  level <- 1 - alpha

  group_stats <- group_stats %>%
    dplyr::mutate(
      se = .data$sd_outcome / sqrt(.data$n),
      df = .data$n - 1
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci_lower = if (!is.na(.data$df) && .data$df >= 1) {
        .data$mean_outcome - stats::qt(1 - alpha / 2, df = .data$df) * .data$se
      } else {
        NA_real_
      },
      ci_upper = if (!is.na(.data$df) && .data$df >= 1) {
        .data$mean_outcome + stats::qt(1 - alpha / 2, df = .data$df) * .data$se
      } else {
        NA_real_
      },
      ci_width = (.data$ci_upper - .data$ci_lower) / 2
    ) %>%
    dplyr::ungroup()

  group_stats_vis <- group_stats

  group_stats     <- tidyr::separate(group_stats,     .data$group, into = factors, sep = "_")
  group_stats_vis <- tidyr::separate(group_stats_vis, .data$group, into = factors, sep = "_")

  lower_data <- group_stats %>%
    dplyr::select(.data$rank, tidyselect::all_of(factors)) %>%
    tidyr::pivot_longer(cols = - .data$rank, names_to = "factor", values_to = "level") %>%
    dplyr::group_by(.data$factor) %>%
    dplyr::mutate(level_code = as.numeric(factor(.data$level))) %>%
    dplyr::ungroup()

  list(
    group_stats      = group_stats,
    group_stats_vis  = group_stats_vis,
    lower_data       = lower_data,
    grand_mean       = grand_mean,
    level            = level
  )
}