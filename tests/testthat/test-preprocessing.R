library(testthat)
library(dplyr)
library(tidyr)

# Assume that mfcurve_preprocessing is already loaded (e.g. via your package or sourced file)

context("mfcurve_preprocessing tests")

# Test that an invalid rounding value triggers an error
test_that("Error when rounding is not a non-negative whole number", {
  dummy_data <- data.frame(wage = 1, race = "A", south = "B", union = "C")

  expect_error(
    mfcurve_preprocessing(data = dummy_data,
                          outcome = "wage",
                          factors = c("race", "south", "union"),
                          rounding = -1),
    "The 'rounding' parameter must be a non-negative whole number."
  )

  expect_error(
    mfcurve_preprocessing(data = dummy_data,
                          outcome = "wage",
                          factors = c("race", "south", "union"),
                          rounding = 2.5),
    "The 'rounding' parameter must be a non-negative whole number."
  )
})

# Test that an invalid 'test' parameter triggers an error
test_that("Error when test parameter is invalid", {
  dummy_data <- data.frame(wage = 1, race = "A", south = "B", union = "C")

  expect_error(
    mfcurve_preprocessing(data = dummy_data,
                          outcome = "wage",
                          factors = c("race", "south", "union"),
                          test = "invalid"),
    "Invalid value for 'test'. Please use 'mean' or 'zero'."
  )
})

# Test basic functionality with test = "mean"
test_that("Correct preprocessing with test = 'mean'", {
  df <- data.frame(
    wage = c(10, 20, 30, 40, NA),
    race = c("White", "Black", "White", "Black", "White"),
    south = c("Yes", "No", "Yes", "No", "Yes"),
    union = c("No", "Yes", "No", "Yes", "No")
  )

  result <- mfcurve_preprocessing(data = df,
                                  outcome = "wage",
                                  factors = c("race", "south", "union"),
                                  test = "mean",
                                  rounding = 2,
                                  mode = "collapsed",
                                  plotOrigin = TRUE)

  # Check that grand_mean is computed correctly (NA omitted)
  expect_equal(result$grand_mean, mean(c(10, 20, 30, 40)))

  # Check that group_stats is a data frame with expected columns
  expected_cols <- c("race", "south", "union", "mean_outcome", "sd_outcome", "n",
                     "t_stat", "p_value", "sig", "rank", "se", "ci_lower", "ci_upper", "ci_width")
  expect_true(all(expected_cols %in% names(result$group_stats)))

  # With one row removed due to NA, we expect two unique groups
  expect_equal(nrow(result$group_stats), 2)

  # Check that group_stats_vis contains rounded columns
  expect_true("mean_outcome_vis" %in% names(result$group_stats_vis))

  # Check that lower_data has the correct structure
  expect_true(all(c("rank", "factor", "level", "level_code", "y") %in% names(result$lower_data)))

  # Check that axis_limits is a list with expected elements
  expect_true(is.list(result$axis_limits))
  expect_true(all(c("x_min", "x_max", "y_min", "y_max") %in% names(result$axis_limits)))

  # Check that factor_positions is a data frame with columns "factor" and "y"
  expect_true(is.data.frame(result$factor_positions))
  expect_true(all(c("factor", "y") %in% names(result$factor_positions)))
})

# Test functionality when test = "zero"
test_that("Preprocessing with test = 'zero' uses zero as reference", {
  df <- data.frame(
    wage = c(10, 20, 30, 40),
    race = c("White", "Black", "White", "Black"),
    south = c("Yes", "No", "Yes", "No"),
    union = c("No", "Yes", "No", "Yes")
  )

  result <- mfcurve_preprocessing(data = df,
                                  outcome = "wage",
                                  factors = c("race", "south", "union"),
                                  test = "zero",
                                  rounding = 2,
                                  mode = "collapsed",
                                  plotOrigin = FALSE)

  # For each group, since reference = 0, the t-statistic should be positive.
  expect_true(all(result$group_stats$t_stat > 0))
})

# Test functionality for mode = "expanded"
test_that("Preprocessing with mode = 'expanded' correctly expands factor labels", {
  df <- data.frame(
    wage = c(10, 20, 30, 40),
    race = c("White", "Black", "White", "Black"),
    south = c("Yes", "No", "Yes", "No"),
    union = c("No", "Yes", "No", "Yes")
  )

  result <- mfcurve_preprocessing(data = df,
                                  outcome = "wage",
                                  factors = c("race", "south", "union"),
                                  test = "mean",
                                  rounding = 2,
                                  mode = "expanded",
                                  plotOrigin = FALSE)

  # In lower_data, the factor column should contain expanded labels (e.g., "race White")
  expect_true(any(grepl(" ", result$lower_data$factor)))
})

# Test that the rounded values in group_stats_vis are correctly rounded
test_that("Rounded values in group_stats_vis match expected rounding", {
  df <- data.frame(
    wage = c(10, 20, 30, 40),
    race = c("White", "Black", "White", "Black"),
    south = c("Yes", "No", "Yes", "No"),
    union = c("No", "Yes", "No", "Yes")
  )

  result <- mfcurve_preprocessing(data = df,
                                  outcome = "wage",
                                  factors = c("race", "south", "union"),
                                  test = "mean",
                                  rounding = 1,
                                  mode = "collapsed",
                                  plotOrigin = FALSE)

  # Compare the rounded column with the manually rounded original values
  expect_equal(result$group_stats_vis$mean_outcome_vis,
               round(result$group_stats_vis$mean_outcome, 1))
})
