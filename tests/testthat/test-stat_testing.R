# tests/testthat/test-stat_testing.R

# Define a test for mfcurve_stat_test function
test_that("mfcurve_stat_test performs correct statistical tests", {

  # Sample data to mimic the output from mfcurve_preprocessing
  sample_data <- tibble::tibble(
    group = c(1, 2, 3, 4),
    mean_outcome = c(10, 20, 15, 25),
    sd_outcome = c(5, 6, 7, 8),
    n = c(10, 12, 15, 8)
  )

  # Test 1: Default test against other groups' means
  test_result_1 <- mfcurve_stat_test(
    data = sample_data,
    test = "mean",
    level = 0.95
  )

  # Check if the output contains correct columns
  expect_true(all(c("t_value", "p_value", "significant", "ci_lower", "ci_upper") %in% colnames(test_result_1)))

  # Check if all p-values are calculated correctly and are numeric
  expect_true(all(!is.na(test_result_1$p_value)))
  expect_type(test_result_1$p_value, "double")

  # Check if significant results are correctly flagged
  expect_true(all(test_result_1$significant %in% c(TRUE, FALSE)))

  # Test 2: Testing against zero
  test_result_2 <- mfcurve_stat_test(
    data = sample_data,
    test = "zero",
    level = 0.95
  )

  # Check that p-values and t-values are not NA
  expect_true(all(!is.na(test_result_2$t_value)))
  expect_true(all(!is.na(test_result_2$p_value)))

  # Test 3: Handling missing values
  sample_data_with_na <- sample_data
  sample_data_with_na$sd_outcome[1] <- NA

  test_result_3 <- mfcurve_stat_test(
    data = sample_data_with_na,
    test = "mean",
    level = 0.95
  )

  # Expect NA values where standard deviations were missing
  expect_true(is.na(test_result_3$t_value[1]))
  expect_true(is.na(test_result_3$p_value[1]))

  # Test 4: Group with only one observation
  single_obs_data <- tibble::tibble(
    group = c(1, 2),
    mean_outcome = c(10, 20),
    sd_outcome = c(NA, 5),
    n = c(1, 10)
  )

  test_result_4 <- mfcurve_stat_test(
    data = single_obs_data,
    test = "mean",
    level = 0.95
  )

  # Check that the group with only one observation has NA results
  expect_true(is.na(test_result_4$t_value[1]))
  expect_true(is.na(test_result_4$p_value[1]))

  # Test 5: Different confidence levels
  test_result_5 <- mfcurve_stat_test(
    data = sample_data,
    test = "mean",
    level = 0.99
  )

  # Check that confidence intervals widen with higher confidence level
  expect_true(all(test_result_5$ci_upper - test_result_5$ci_lower > test_result_1$ci_upper - test_result_1$ci_lower))

})
