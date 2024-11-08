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

test_that("mfcurve_preprocessing handles valid inputs", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl", "gear"))
  expect_true("mean_outcome" %in% colnames(result))
  expect_true("sd_outcome" %in% colnames(result))
  expect_true(nrow(result) > 0)
})

test_that("mfcurve_preprocessing handles predefined group variable", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl"), groupvar = "gear")
  expect_true("group" %in% colnames(result))
  expect_true(nrow(result) > 0)
})

test_that("mfcurve_preprocessing handles missing values", {
  result <- mfcurve_preprocessing(data, outcome_var = "mpg", factors = c("cyl", "random_factor"))
  expect_false(any(is.na(result$mean_outcome)))
})

test_that("mfcurve_preprocessing stops for missing columns", {
  expect_error(
    mfcurve_preprocessing(data, outcome_var = "non_existent", factors = c("cyl", "gear")),
    "The specified outcome variable 'non_existent' is not found in the dataset."
  )
})

test_that("mfcurve_preprocessing works with minimal dataset", {
  minimal_data <- data.frame(
    outcome = c(1, 2, 3, 4),
    factor1 = factor(c("A", "B", "A", "B")),
    factor2 = factor(c("X", "X", "Y", "Y"))
  )
  result <- mfcurve_preprocessing(minimal_data, outcome_var = "outcome", factors = c("factor1", "factor2"))
  expect_true(nrow(result) == 4)
})
