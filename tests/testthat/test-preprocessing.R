test_that("mfcurve_preprocessing filters missing values correctly", {
  # Input data with missing values
  data <- data.frame(
    wage = c(10, 15, NA, 20),
    factor1 = c("A", "B", "A", NA),
    factor2 = c("X", "Y", NA, "X"),
    stringsAsFactors = FALSE
  )

  # Run preprocessing
  result <- mfcurve_preprocessing(data, outcome_var = "wage", factors = c("factor1", "factor2"))

  # Check the number of rows (only complete cases should remain)
  expect_equal(nrow(result), 2)

  # Check that no NA values remain in the specified columns
  expect_false(anyNA(result$mean_outcome))
})

test_that("mfcurve_preprocessing creates group variable correctly", {
  # Input data
  data <- data.frame(
    wage = c(10, 15, 20),
    factor1 = c("A", "A", "B"),
    factor2 = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )

  # Run preprocessing
  result <- mfcurve_preprocessing(data, outcome_var = "wage", factors = c("factor1", "factor2"))

  # Check that group variable matches expected values
  expect_equal(result$group, c("A_X", "A_Y", "B_X"))
})

test_that("mfcurve_preprocessing calculates summary statistics correctly", {
  # Input data
  data <- data.frame(
    wage = c(10, 15, 20, 25),
    factor1 = c("A", "A", "B", "B"),
    factor2 = c("X", "X", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  # Run preprocessing
  result <- mfcurve_preprocessing(data, outcome_var = "wage", factors = c("factor1", "factor2"))

  # Check calculated means
  expected_means <- c(12.5, 22.5)
  expect_equal(result$mean_outcome, expected_means)

  # Check calculated standard deviations
  expected_sds <- c(3.5355339, 3.5355339)  # sqrt of variance 12.5 for n = 2
  expect_equal(result$sd_outcome, expected_sds, tolerance = 1e-6)

  # Check group sizes
  expect_equal(result$n, c(2, 2))
})

test_that("mfcurve_preprocessing ranks groups correctly", {
  # Input data
  data <- data.frame(
    wage = c(10, 15, 20),
    factor1 = c("A", "A", "B"),
    factor2 = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )

  # Run preprocessing
  result <- mfcurve_preprocessing(data, outcome_var = "wage", factors = c("factor1", "factor2"))

  # Check ranking (ascending order by mean outcome)
  expect_equal(result$rank, c(1, 2, 3))
})
