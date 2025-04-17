library(testthat)
library(mfcurve)

test_that("mfcurve wrapper function works without errors", {
  data <- data.frame(
    outcome = rnorm(30),
    factor1 = sample(c("A", "B"), 30, replace = TRUE),
    factor2 = sample(c("X", "Y"), 30, replace = TRUE)
  )

  expect_no_error(
    plot <- mfcurve(data, "outcome", c("factor1", "factor2"))
  )

  expect_s3_class(plot, "plotly")
})

test_that("mfcurve wrapper respects SaveProcessedData argument", {
  data <- data.frame(
    outcome = rnorm(30),
    factor1 = sample(c("A", "B"), 30, replace = TRUE)
  )

  expect_message(
    mfcurve(data, "outcome", "factor1", SaveProcessedData = TRUE),
    "Saved 'group_stats' to the global environment."
  )

  expect_true(exists("group_stats", envir = .GlobalEnv))
  expect_true(is.data.frame(get("group_stats", envir = .GlobalEnv)))

  # Clean up
  rm("group_stats", envir = .GlobalEnv)
})
