# tests/testthat/test-mfcurve_preprocessing.R

library(testthat)
library(dplyr)

# Sample dataset for testing
set.seed(123)
data <- mtcars %>%
  dplyr::mutate(
    cyl = as.factor(cyl),
    gear = as.factor(gear),
    random_factor = sample(c("A", "B", "C"), size = nrow(mtcars), replace = TRUE)
  )

# Introducing some missing values
data$mpg[c(3, 7)] <- NA
data$cyl[c(2, 10)] <- NA

test_that("mfcurve_preprocessing handles typical data correctly", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl", "gear"))

  # Check output structure
  expect_true("mean_outcome" %in% colnames(result))
  expect_true("sd_outcome" %in% colnames(result))
  expect_true("n" %in% colnames(result))
  expect_true("rank" %in% colnames(result))

  # Check that the number of rows equals unique groups
  expect_equal(nrow(result), length(unique(result$group)))
})

test_that("mfcurve_preprocessing handles predefined group variable", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl"), groupvar = "gear")

  # Ensure 'group' column is present and correctly linked
  expect_true("group" %in% colnames(result))
  expect_equal(nrow(result), length(unique(data$gear)))
})

test_that("mfcurve_preprocessing handles missing values correctly", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl", "random_factor"))

  # Check that no missing values propagate
  expect_false(any(is.na(result$mean_outcome)))
  expect_false(any(is.na(result$sd_outcome)))
})

test_that("mfcurve_preprocessing stops for missing columns", {
  expect_error(
    mfcurve_preprocessing(data, outcome_var = "non_existent", factors = c("cyl", "gear")),
    "The specified outcome variable 'non_existent' is not found in the dataset."
  )
  expect_error(
    mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("non_existent")),
    "Some variables are missing in the dataset."
  )
})

test_that("mfcurve_preprocessing works with minimal dataset", {
  minimal_data <- data.frame(
    outcome = c(1, 2, 3, 4),
    factor1 = factor(c("A", "B", "A", "B")),
    factor2 = factor(c("X", "X", "Y", "Y"))
  )
  result <- mfcurve_preprocessing(minimal_data, outcome_var = "outcome", factors = c("factor1", "factor2"))

  # Ensure correct number of rows and structure
  expect_true(nrow(result) == 4)
  expect_true(all(c("mean_outcome", "sd_outcome", "n", "rank") %in% colnames(result)))
})

test_that("mfcurve_preprocessing stops for invalid inputs", {
  expect_error(
    mfcurve_preprocessing(NULL, outcome_var = "mpg", factors = c("cyl", "gear")),
    "Please provide data, outcome_var, and factors."
  )
  expect_error(
    mfcurve_preprocessing(data, outcome_var = NULL, factors = c("cyl", "gear")),
    "Please provide data, outcome_var, and factors."
  )
  expect_error(
    mfcurve_preprocessing(data, outcome_var = "mpg", factors = NULL),
    "Please provide data, outcome_var, and factors."
  )
})
