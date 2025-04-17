library(testthat)
library(mfcurve)

test_that("mfcurve_preprocessing correctly processes input data", {
  data <- data.frame(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8),
    factor1 = c("A", "A", "B", "B", "A", "A", "B", "B"),
    factor2 = c("X", "Y", "X", "Y", "X", "Y", "X", "Y")
  )

  # Test basic functionality
  result <- mfcurve_preprocessing(data, "outcome", c("factor1", "factor2"))

  expect_type(result, "list")
  expect_named(result, c("group_stats", "group_stats_vis", "lower_data", "grand_mean", "level"))

  # Test grand mean calculation
  expect_equal(result$grand_mean, mean(1:8))

  # Test dimensions of returned data frames
  expect_true(is.data.frame(result$group_stats))
  expect_true(is.data.frame(result$group_stats_vis))
  expect_true(is.data.frame(result$lower_data))

  # Test correct number of groups
  expect_equal(nrow(result$group_stats), 4)
})

test_that("mfcurve_preprocessing throws appropriate errors", {
  data <- data.frame(
    outcome = c("one", "two"),
    factor1 = c("A", "B")
  )

  expect_error(
    mfcurve_preprocessing(data, "outcome", c("factor1")),
    "The outcome variable must be numeric."
  )

  expect_error(
    mfcurve_preprocessing(data.frame(outcome = 1:2, factor1 = c("A", "B")), "outcome", "factor1", test = "median"),
    'Argument "test" must be either "mean" or "zero".'
  )
})
