# tests/testthat/test-preprocessing.R

library(testthat)
library(mfcurve)

test_that("mfcurve_preprocessing returns expected structure", {
  data <- data.frame(
    outcome = 1:8,
    factor1 = rep(c("A", "B"), each = 4),
    factor2 = rep(c("X", "Y"), times = 4)
  )

  for (test_type in c("mean", "zero", "leave-one-out")) {
    result <- mfcurve_preprocessing(data, "outcome", c("factor1", "factor2"), test = test_type)

    expect_type(result, "list")
    expect_named(result, c("group_stats", "group_stats_vis", "lower_data", "grand_mean", "level"))

    expect_s3_class(result$group_stats, "data.frame")
    expect_s3_class(result$group_stats_vis, "data.frame")
    expect_s3_class(result$lower_data, "data.frame")

    expect_true(is.numeric(result$grand_mean))
    expect_true(is.numeric(result$level))

    # Check test_type column
    expect_true(all(result$group_stats$test_type == test_type))
  }
})

test_that("mfcurve_preprocessing computes correct number of groups", {
  data <- data.frame(
    outcome = rnorm(16),
    factor1 = rep(c("A", "B"), each = 8),
    factor2 = rep(c("X", "Y"), times = 8)
  )

  result <- mfcurve_preprocessing(data, "outcome", c("factor1", "factor2"))
  expect_equal(nrow(result$group_stats), 4)
})

test_that("mfcurve_preprocessing throws errors for invalid input", {
  data_invalid <- data.frame(
    outcome = c("low", "high"),
    factor1 = c("A", "B")
  )

  expect_error(
    mfcurve_preprocessing(data_invalid, "outcome", "factor1"),
    "The outcome variable must be numeric."
  )

  expect_error(
    mfcurve_preprocessing(data.frame(outcome = 1:2, factor1 = c("A", "B")), "outcome", "factor1", test = "median"),
    'Argument "test" must be one of "mean", "zero", or "leave-one-out".'
  )
})

test_that("mfcurve_preprocessing handles NA values correctly", {
  data_na <- data.frame(
    outcome = c(1, 2, NA, 4),
    factor1 = c("A", "A", "B", NA),
    factor2 = c("X", "Y", "X", "Y")
  )

  result <- mfcurve_preprocessing(data_na, "outcome", c("factor1", "factor2"))
  expect_true(all(!is.na(result$group_stats$mean_outcome)))
})
