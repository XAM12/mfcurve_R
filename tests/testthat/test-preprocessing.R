# File: tests/testthat/test-preprocessing.R
library(testthat)
library(mfcurve)

test_that("Preprocessing handles missing values correctly", {
  # Sample data for testing
  data <- data.frame(
    wage = c(10, NA, 30, 40),
    race = c("black", "white", "black", "white"),
    union = c(1, 0, 1, 1)
  )

  # Apply the preprocessing function
  result <- mfcurve_preprocessing(data, "wage", c("race", "union"))

  # Corrected expectation: Rows with NA values in 'wage' are removed, so 3 rows remain
  expect_equal(nrow(result), 2)  # NA row in 'wage' is removed, and only valid combinations are retained
  expect_true(all(!is.na(result$mean_outcome))) # No missing values in the results
})

test_that("Preprocessing creates correct groups", {
  # Sample data for testing
  data <- data.frame(
    wage = c(10, 20, 30, 40),
    race = c("black", "black", "white", "white"),
    union = c(1, 0, 1, 0)
  )

  # Apply the preprocessing function
  result <- mfcurve_preprocessing(data, "wage", c("race", "union"))

  # Test that the correct number of groups are created
  expect_equal(nrow(result), 4)  # Each combination of 'race' and 'union' forms a group
})
