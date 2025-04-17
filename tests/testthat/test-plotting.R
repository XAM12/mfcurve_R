library(testthat)
library(mfcurve)
library(plotly)

test_that("mfcurve_plotting produces plotly object without errors", {
  data <- data.frame(
    outcome = rnorm(100),
    factor1 = sample(c("A", "B"), 100, replace = TRUE),
    factor2 = sample(c("X", "Y"), 100, replace = TRUE)
  )

  preprocessed <- mfcurve_preprocessing(data, "outcome", c("factor1", "factor2"))

  plot <- mfcurve_plotting(
    group_stats_vis = preprocessed$group_stats_vis,
    lower_data = preprocessed$lower_data,
    grand_mean = preprocessed$grand_mean,
    outcome = "outcome",
    factors = c("factor1", "factor2"),
    level = preprocessed$level
  )

  expect_s3_class(plot, "plotly")
})

test_that("mfcurve_plotting handles parameters correctly", {
  data <- data.frame(
    outcome = rnorm(50),
    factor1 = rep(c("A", "B"), 25),
    factor2 = rep(c("X", "Y"), each = 25)
  )

  preprocessed <- mfcurve_preprocessing(data, "outcome", c("factor1", "factor2"))

  expect_no_error(
    plot <- mfcurve_plotting(
      group_stats_vis = preprocessed$group_stats_vis,
      lower_data = preprocessed$lower_data,
      grand_mean = preprocessed$grand_mean,
      outcome = "outcome",
      factors = c("factor1", "factor2"),
      level = preprocessed$level,
      rounding = 3,
      showTitle = FALSE,
      plotOrigin = TRUE,
      CI = FALSE,
      mode = "expanded"
    )
  )

  expect_s3_class(plot, "plotly")
})
