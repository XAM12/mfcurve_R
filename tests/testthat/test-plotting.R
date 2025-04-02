library(testthat)
library(plotly)

context("mfcurve_plotting tests")

# Create a dummy processed data object simulating mfcurve_preprocessing output
dummy_processed <- list(
  group_stats_vis = data.frame(
    rank = 1:2,
    mean_outcome_vis = c(15.2, 20.5),
    ci_lower_vis = c(14.0, 19.0),
    ci_upper_vis = c(16.4, 22.0),
    ci_width_vis = c(1.2, 1.5),
    sd_outcome_vis = c(2.3, 2.8),
    sig = c(TRUE, FALSE)
  ),
  lower_data = data.frame(
    rank = c(1, 2),
    factor = c("race", "race"),
    level = c("White", "Black"),
    level_code = c(1, 2),
    y = c(2, 1)
  ),
  grand_mean = 17.85,
  axis_limits = list(x_min = 0, x_max = 3, y_min = 10, y_max = 25),
  factor_positions = data.frame(
    factor = c("race"),
    y = c(1)
  )
)

test_that("mfcurve_plotting returns a plotly object", {
  plot_obj <- mfcurve_plotting(dummy_processed, outcome = "wage", showTitle = TRUE)
  expect_s3_class(plot_obj, "plotly")
})

test_that("mfcurve_plotting sets title when showTitle is TRUE", {
  plot_obj <- mfcurve_plotting(dummy_processed, outcome = "wage", showTitle = TRUE)
  # Check that the title exists and matches the expected text.
  expect_true(!is.null(plot_obj$x$layout$title))
  expected_title <- paste("Mean", "wage", "by the combination of factors")
  expect_equal(plot_obj$x$layout$title$text, expected_title)
})

test_that("mfcurve_plotting does not set title when showTitle is FALSE", {
  plot_obj <- mfcurve_plotting(dummy_processed, outcome = "wage", showTitle = FALSE)
  # In this branch, no title should be set.
  expect_true(is.null(plot_obj$x$layout$title) ||
                is.null(plot_obj$x$layout$title$text))
})

test_that("mfcurve_plotting sets axis labels correctly", {
  plot_obj <- mfcurve_plotting(dummy_processed, outcome = "wage", showTitle = TRUE)
  # Check that the upper panel y-axis title is "wage"
  expect_equal(plot_obj$x$layout$yaxis$title, "wage")
  # And the lower panel y-axis title is "Factors"
  expect_equal(plot_obj$x$layout$yaxis2$title, "Factors")
})

test_that("mfcurve_plotting returns the correct number of traces", {
  plot_obj <- mfcurve_plotting(dummy_processed, outcome = "wage", showTitle = TRUE)
  # The function creates 2 traces for the upper panel (group means and grand mean)
  # and 1 trace for the lower panel, so we expect 3 traces in total.
  expect_equal(length(plot_obj$x$data), 3)
})
