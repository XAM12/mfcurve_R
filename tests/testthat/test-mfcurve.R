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

  # run in a temporary working directory so files don't leak into the repo
  tmp <- file.path(tempdir(), "mfcurve_test_save")
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  old <- getwd()
  setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  # new behavior: message lists target paths; files are written to cwd
  expect_message(
    mfcurve(data, "outcome", "factor1", SaveProcessedData = TRUE),
    regexp = "Saved group statistics to:"
  )

  files <- list.files(tmp,
                      pattern = "^group_stats_outcome_\\d{8}-\\d{6}\\.(csv|rds)$",
                      full.names = TRUE
  )
  expect_true(any(grepl("\\.csv$", files)))
  expect_true(any(grepl("\\.rds$", files)))

  # RDS should deserialize to a data.frame with expected columns
  rds_path <- files[grepl("\\.rds$", files)][1]
  gs <- readRDS(rds_path)
  expect_true(is.data.frame(gs))
  expect_true(all(c("rank", "mean_outcome") %in% names(gs)))
})
