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

test_that("mfcurve writes group stats to tempdir() when SaveProcessedData = TRUE", {
  data <- data.frame(
    outcome = rnorm(30),
    factor1 = sample(c("A", "B"), 30, replace = TRUE)
  )

  pattern <- "^group_stats_outcome_\\d{8}-\\d{6}\\.(csv|rds)$"
  before <- list.files(tempdir(), pattern = pattern, full.names = TRUE)

  expect_message(
    mfcurve(data, "outcome", "factor1", SaveProcessedData = TRUE),
    regexp = "Saved group statistics to:"
  )

  after <- list.files(tempdir(), pattern = pattern, full.names = TRUE)
  new_files <- setdiff(after, before)

  expect_true(length(new_files) >= 2)               # should write .csv and .rds
  expect_true(any(grepl("\\.csv$", new_files)))
  expect_true(any(grepl("\\.rds$", new_files)))

  # RDS should deserialize to a data.frame with expected columns
  rds_path <- new_files[grepl("\\.rds$", new_files)][1]
  gs <- readRDS(rds_path)
  expect_true(is.data.frame(gs))
  expect_true(all(c("rank", "mean_outcome") %in% names(gs)))
})
